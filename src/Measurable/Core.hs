{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
--
-- The /measurable/ library provides basic facilities for defining,
-- manipulating, and - where possible - evaluating measures.
--
-- Measure theory is the foundation of formal probability.  And while measures
-- are useful for proofs and theoretical work, they're not a particularly
-- efficient way to get anything done in the real world.  That said, a solid
-- mental model of probability in terms of abstract volumes and ratios is a
-- valuable (and difficult) thing to achieve, and /measurable/ can help to
-- achieve it.
--
-- Measures are represented in their dual form as integrals.  That is, a
-- measure is represented as an /integration procedure/ that, when provided
-- with a measurable function, evaluates an integral against a measure.
-- Everything corresponds exactly to the more low-level definition of measures
-- being set functions defined on sigma-algebras and all that, but in a fashion
-- that is much more amenable to representation on a computer.  Since they are
-- /procedures/ that need a function to complete them, measures are naturally
-- represented by continuations.
--
-- As continuations, measures are instances of the @Functor@ class; fmapping a
-- function over a measure transforms that measure's support while leaving its
-- density structure unchanged.  @fmap@ corresponds to the thing that's
-- variably called a pushforward, distribution, or image measure; it adapts a
-- measure from one measureable space to another.
--
-- <image here>
--
-- Measures are also instances of @Monad@.  'return' wraps a value up as a
-- Dirac measure, and 'bind' is an integrating operator that marginalizes
-- existing measures by blending them into others.
--
-- Measures have to be created from something; /measurable/ offers four
-- functions to build them:
--
-- * 'fromPoints', for a discrete collection of points
--
-- * 'fromDensityCounting', for a density with respect to counting measure
-- (i.e. a mass function)
--
-- * 'fromDensityLebesgue', for a density with espect to Lebesgue measure
--
-- * 'fromSamplingFunction', for a sampling function that uniquely
-- characterizes a measure.
--
-- Addtionally, there are two important ways to query measures:
--
-- * 'integrate' integrates a measurable function against a measure
--
-- * 'expectation' integrates the identity function against a measure
--
-- * 'cdf' returns a cumulative distribution function for a measure space
--
-- Other queries (volume, variance, higher moments) are also available, but are
-- moreso included as curiosities.
--
-- Measures are not only implemented as stand-alone probabilistic objects, but
-- as a monad transformers as well.  This allows measure semantics to be
-- layered on top of any existing monad.

module Measurable.Core where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Functor.Identity
import Data.Traversable hiding (mapM)
import Numeric.Integration.TanhSinh

-- | We don't want to allow nonlinear things like callCC, so we roll our own
--   Continuation type that doesn't allow that sort of thing.
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

type Cont r = ContT r Identity

runCont :: Cont r a -> (a -> r) -> r
runCont m k = runIdentity (runContT m (Identity . k))

cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT (\c -> Identity (f (runIdentity . c)))

instance Functor (ContT r m) where
  fmap f m = ContT $ \c -> runContT m (c . f)

instance Applicative (ContT r m) where
    pure x  = ContT ($ x)
    f <*> v = ContT $ \c -> runContT f $ \g -> runContT v (c . g)

instance Monad (ContT r m) where
    return x = ContT ($ x)
    m >>= k  = ContT $ \c -> runContT m (\x -> runContT (k x) c)

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

-- | A measure is represented as a continuation with output restricted to the
--   reals.  Measures are *integration programs* that, when supplied with a
--   measurable function, integrate that function against some measure.
type Measure a    = Cont Double a
type MeasureT m a = ContT Double m a

-- | A more domain-specific alias for runCont.  This follows the traditional
--   mathematical form; integrate a function against a measure.
integrate :: (a -> Double) -> Measure a -> Double
integrate = flip runCont

integrateT :: Monad m => (a -> Double) -> MeasureT m a -> m Double
integrateT f = (`runContT` (return . f))

-- | Things like convolution are trivially expressed by lifted arithmetic 
--   operators.  Probability measures in particular - where things like
--   \infty - \infty are not an issue - form a ring.
--
--   Note that the complexity of integration over a sum, difference, or product
--   of 'n' measures in this situation, each encoding 'm' elements, is O(m^n)
--   in this implementation.  Operations on independent measures can
--   theoretically be implemented with drastically lower complexity, possibly
--   by using some cleverness with Arrows.
instance (Monad m, Num a) => Num (ContT Double m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = id
  signum      = const 1
  fromInteger = return . fromInteger

-- | Creates a measure from a density w/respect to counting measure.
fromDensityCounting
  :: (Functor f, Foldable f)
  => (a -> Double)
  -> f a
  -> Measure a
fromDensityCounting f support = cont $ \g ->
    Foldable.sum $ (g /* f) <$> support

fromDensityCountingT
  :: (Applicative m, Monad m, Traversable t)
  => (a -> Double)
  -> t a
  -> MeasureT m a
fromDensityCountingT p support = ContT $ \f ->
    fmap Foldable.sum . traverse (f //* pLifted) $ support
  where
    pLifted = return . p

-- | Create a measure from a density w/respect to Lebesgue measure.
--
--   NOTE The quality of this implementation depends entirely on the underlying
--        quadrature routine.  This is included moreso for interest's sake and
--        isn't particularly accurate.
fromDensityLebesgue :: (Double -> Double) -> Measure Double
fromDensityLebesgue d = cont $ \f -> quadratureTanhSinh $ f /* d where
  quadratureTanhSinh = result . last . everywhere trap

-- | Create a measure from a fixed collection of points.
fromPoints :: (Functor f, Foldable f) => f a -> Measure a
fromPoints = cont . flip weightedAverage

fromPointsT
  :: (Applicative m, Monad m, Traversable f)
  => f a
  -> MeasureT m a
fromPointsT = ContT . flip weightedAverageM

-- | Create a measure from a sampling function.  Needs access to a random
--   number supply monad, so only a monad transformer version is available.
fromSamplingFunction
  :: (Monad m, Applicative m)
  => (t -> m b)
  -> Int
  -> t
  -> MeasureT m b
fromSamplingFunction f n g = (lift $ replicateM n (f g)) >>= fromPointsT

-- | Synonyms for fmap.
push :: (a -> b) -> Measure a -> Measure b
push = fmap

pushT :: Monad m => (a -> b) -> MeasureT m a -> MeasureT m b
pushT = fmap

-- | Expectation is integration against the identity function.
expectation :: Measure Double -> Double
expectation = integrate id 

expectationT :: Monad m => MeasureT m Double -> m Double
expectationT = integrateT id

-- | Variance is obtained by the usual identity.
variance :: Measure Double -> Double
variance mu = integrate (^ 2) mu - expectation mu ^ 2

varianceT :: Monad m => MeasureT m Double -> m Double
varianceT mu = liftM2 (-) (integrateT (^ 2) mu) (liftM (^ 2) (expectationT mu))

-- | Convenience function for returning the expectation & variance as a pair.
meanVariance :: Measure Double -> (Double, Double)
meanVariance = expectation &&& variance

meanVarianceT
  :: (Applicative m, Monad m)
  => MeasureT m Double
  -> m (Double, Double)
meanVarianceT mu = (,) <$> expectationT mu <*> varianceT mu

-- | The nth raw moment of a measure.
rawMoment :: Int -> Measure Double -> Double
rawMoment n = integrate (^ n)

rawMomentT :: Monad m => Int -> MeasureT m Double -> m Double
rawMomentT n = integrateT (^ n)

-- | All raw moments of a measure.
rawMoments :: Measure Double -> [Double]
rawMoments mu = map (`rawMoment` mu) [1..]

rawMomentsT :: Monad m => MeasureT m Double -> Int -> m [Double]
rawMomentsT mu n = mapM (`rawMomentT` mu) (take n [1..])

-- | The nth central moment of a measure.
centralMoment :: Int -> Measure Double -> Double
centralMoment n mu = integrate (\x -> (x - rm) ^ n) $ mu
  where rm = rawMoment 1 mu

centralMomentT :: Monad m => Int -> MeasureT m Double -> m Double
centralMomentT n mu = integrateT (^ n) $ do
  rm <- lift $ rawMomentT 1 mu
  (\x -> x - rm) <$> mu

-- | All central moments.
centralMoments :: Measure Double -> [Double]
centralMoments mu = map (`centralMoment` mu) [1..]

centralMomentsT :: Monad m => MeasureT m Double -> Int -> m [Double]
centralMomentsT mu n = mapM (`centralMomentT` mu) (take n [1..])

-- | The moment generating function for a measure.
momentGeneratingFunction :: Measure Double -> Double -> Double
momentGeneratingFunction mu t = integrate (exp . (* t) . id) mu

-- | The cumulant generating function for a measure.
cumulantGeneratingFunction :: Measure Double -> Double -> Double
cumulantGeneratingFunction mu = log . momentGeneratingFunction mu 

-- | Apply a measure to the underlying space.  In particular, this is trivially
--   1 for any probability measure.
volume :: Measure a -> Double
volume = integrate (const 1)

volumeT :: Monad m => MeasureT m a -> m Double
volumeT = integrateT (const 1)

-- | Cumulative distribution function. 
cdf :: Measure Double -> Double -> Double
cdf mu x = expectation $ negativeInfinity `to` x <$> mu

cdfT :: Monad m => MeasureT m Double -> Double -> m Double
cdfT mu x = expectationT $ negativeInfinity `to` x <$> mu

-- | Indicator function for the interval a <= x <= b.  Useful for integrating 
--   from a to b.
to :: (Num a, Ord a) => a -> a -> a -> a
to a b x
  | x >= a && x <= b = 1
  | otherwise        = 0

-- | Integrate over a discrete, possibly unordered set.
containing :: (Num a, Eq b) => [b] -> b -> a
containing xs x 
  | x `elem` xs = 1
  | otherwise   = 0

-- | End of the line.
negativeInfinity :: Fractional a => a
negativeInfinity = negate (1 / 0)

-- | Simple average.
average :: (Fractional a, Foldable f) => f a -> a
average xs = fst $ Foldable.foldl' 
  (\(!m, !n) x -> (m + (x - m) / fromIntegral (n + 1), n + 1)) (0, 0) xs
{-# INLINE average #-}

-- | Weighted average.
weightedAverage
  :: (Functor f, Foldable f, Fractional c)
  => (a -> c)
  -> f a
  -> c
weightedAverage f = average . fmap f
{-# INLINE weightedAverage #-}

-- | Monadic weighted average.
weightedAverageM 
  :: (Fractional c, Traversable f, Monad m, Applicative m)
  => (a -> m c) 
  -> f a 
  -> m c
weightedAverageM f = liftM average . traverse f
{-# INLINE weightedAverageM #-}

-- | Lifted multiplication.
(/*) :: (Num c, Applicative f) => f c -> f c -> f c
(/*)  = liftA2 (*)

-- | Doubly-lifted multiplication.
(//*) :: (Num c, Applicative f, Applicative g) => f (g c) -> f (g c) -> f (g c)
(//*) = liftA2 (/*)

