{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Measurable.Core where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Hashable
import qualified Data.HashSet as HashSet
import Data.Traversable hiding (mapM)
import Numeric.Integration.TanhSinh

-- | A measure is represented as a continuation.
type Measure a = Cont Double a
type MeasureT m a = ContT Double m a

-- | A more appropriate version of runCont.
integrate :: (a -> Double) -> Measure a -> Double
integrate = flip runCont

integrateT :: Monad m => (a -> Double) -> MeasureT m a -> m Double
integrateT f = (`runContT` fLifted)
  where fLifted = return . f

-- | Things like convolution are trivially expressed by lifted arithmetic 
--   operators.
--
--   Note that the complexity of integration over a sum, difference, or product
--   of 'n' measures, each encoding 'm' elements, is O(m^n).
--
--   The reason for the complexity is that, in the case of dependent measures,
--   a lot of book-keeping has to be done.  Operations on independent measures
--   can (theoretically) be implemented with drastically lower complexity.
instance (Monad m, Num a) => Num (ContT r m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = id
  signum      = error "signum: not supported for Measures"
  fromInteger = return . fromInteger

-- | Create a measure from a density w/respect to counting measure.
fromDensityCounting
  :: (Functor f, Foldable f)
  => (a -> Double)
  -> f a
  -> Measure a
fromDensityCounting f support = cont $ \g ->
    Foldable.sum $ (g /* f) <$> support

fromDensityCountingT
  :: (Applicative m, Traversable t, Monad m)
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
--        quadrature routine.  As we're presently using the 
--        Numeric.Integration.TanhSinh module, the types are also constricted 
--        to Doubles.
--
--        This is included moreso for interest's sake and isn't particularly
--        accurate.
fromDensityLebesgue :: (Double -> Double) -> Measure Double
fromDensityLebesgue d = cont $ \f -> quadratureTanhSinh $ f /* d
  where quadratureTanhSinh = result . last . everywhere trap

-- | Create a measure from observations sampled from some distribution.
fromObservations
  :: (Functor f, Foldable f)
  => f a
  -> Measure a
fromObservations = cont . flip weightedAverage

-- fmap f mu = cont $ \g -> integrate (g . f) mu
-- liftM2 (+) mu nu = do
--   a <- mu
--   b <- nu
--   return $ a + b
-- 
-- mu + nu = cont $ \g -> integrate mu + integrate nu



fromObservationsT
  :: (Applicative m, Monad m, Traversable f)
  => f a
  -> MeasureT m a
fromObservationsT = ContT . flip weightedAverageM

-- | A synonym for fmap.
push :: (a -> b) -> Measure a -> Measure b
push = fmap

-- | Yet another.
pushT :: Monad m => (a -> b) -> MeasureT m a -> MeasureT m b
pushT = fmap

-- | Expectation is integration against the identity function.
expectation :: Measure Double -> Double
expectation = integrate id 

expectationT :: Monad m => MeasureT m Double -> m Double
expectationT = integrateT id

-- | The variance is obtained by integrating against the usual function.
variance :: Measure Double -> Double
variance mu = integrate (^ 2) mu - expectation mu ^ 2

varianceT :: Monad m => MeasureT m Double -> m Double
varianceT mu = liftM2 (-) (integrateT (^ 2) mu) (liftM (^ 2) (expectationT mu))

-- | Return the mean & variance in a pair.
meanVariance :: Measure Double -> (Double, Double)
meanVariance = expectation &&& variance

meanVarianceT :: Monad m => MeasureT m Double -> m (Double, Double)
meanVarianceT mu = do
  m <- expectationT mu
  v <- varianceT mu
  return (m, v)

-- | The measure applied to the underlying space.  This is trivially 1 for any 
--   probability measure.
volume :: Measure Double -> Double
volume = integrate (const 1)

volumeT :: Monad m => MeasureT m Double -> m Double
volumeT = integrateT (const 1)

-- | Cumulative distribution function.  Only makes sense for Fractional/Ord
--   inputs.
cdf :: Measure Double -> Double -> Double
cdf mu x = expectation $ negativeInfinity `to` x <$> mu

cdfT :: Monad m => MeasureT m Double -> Double -> m Double
cdfT mu x = expectationT $ negativeInfinity `to` x <$> mu

-- | Indicator function for the interval a <= x <= b.  Useful for integrating 
--   from a to b.
to :: (Num a, Ord a) => a -> a -> a -> a
to a b x | x >= a && x <= b = 1
         | otherwise        = 0

-- | Integrate over a discrete, possibly unordered set.
containing :: (Num a, Eq b, Hashable b) => [b] -> b -> a
containing xs x 
    | x `HashSet.member` set = 1
    | otherwise          = 0
  where
    set = HashSet.fromList xs

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

-- | Round to a specified number of digits.
roundTo :: Int -> Double -> Double
roundTo n f = fromIntegral (round $ f * (10 ^ n) :: Int) / (10.0 ^^ n)
{-# INLINE roundTo #-}

-- | Lifted multiplication.
(/*) :: (Num c, Applicative f) => f c -> f c -> f c
(/*)  = liftA2 (*)

-- | Doubly-lifted multiplication.
(//*) :: (Num c, Applicative f, Applicative g) => f (g c) -> f (g c) -> f (g c)
(//*) = liftA2 (/*)

