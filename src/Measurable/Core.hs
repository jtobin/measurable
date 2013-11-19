{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Measurable.Core where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.Cont
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import Data.Traversable hiding (mapM)
import Numeric.Integration.TanhSinh
import System.Random.MWC
import System.Random.Represent

-- | A measure is represented as a continuation.
type Measure a = Cont Double a
type MeasureT m a = ContT Double m a

-- | A model is a proper measure wrapped around a sampling monad.
type Model a = MeasureT IO a

-- | A more appropriate version of runCont.
integrate :: (a -> Double) -> Measure a -> Double
integrate = flip runCont

integrateT :: Monad m => (a -> Double) -> MeasureT m a -> m Double
integrateT f = (`runContT` fLifted)
  where fLifted = return . f

-- | Things like convolution are trivially expressed by lifted arithmetic 
--   operators.
instance (Monad m, Num a) => Num (ContT r m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = id
  signum      = error "signum: not supported for Measures"
  fromInteger = error "fromInteger: not supported for Measures"

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
  where quadratureTanhSinh = roundTo 2 . result . last . everywhere trap

-- | Create a measure from observations sampled from some distribution.
fromObservations
  :: (Functor f, Foldable f)
  => f a
  -> Measure a
fromObservations = cont . flip weightedAverage

fromObservationsT
  :: (Applicative m, Monad m, Traversable f)
  => f a
  -> MeasureT m a
fromObservationsT = ContT . flip weightedAverageM

-- | Create an 'approximating measure' from observations.
fromObservationsApprox
  :: (Applicative m, PrimMonad m, Traversable f, Integral n)
  => n
  -> f a
  -> Gen (PrimState m)
  -> MeasureT m a
fromObservationsApprox n xs g = ContT $ \f -> weightedAverageApprox n f xs g

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

-- | Integrate over an ordered, discrete set.
containing :: (Num a, Ord b) => [b] -> b -> a
containing xs x 
    | x `Set.member` set = 1
    | otherwise          = 0
  where
    set = Set.fromList xs

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

-- | An monadic approximate weighted average.
weightedAverageApprox 
  :: (Fractional c, Traversable f, PrimMonad m, Applicative m, Integral n)
  => n
  -> (a -> m c) 
  -> f a 
  -> Gen (PrimState m)
  -> m c
weightedAverageApprox n f xs g = 
  sampleReplace n xs g >>= (liftM average . traverse f)
{-# INLINE weightedAverageApprox #-}

-- | Round to a specified number of digits.
roundTo :: Int -> Double -> Double
roundTo n f = fromIntegral (round $ f * (10 ^ n) :: Int) / (10.0 ^^ n)
{-# INLINE roundTo #-}

-- | Lifted multiplication.
(/*) :: (Num c, Applicative f) => f c -> f c -> f c
f /* g  = liftA2 (*) f g

-- | Doubly-lifted multiplication.
(//*) :: (Num c, Applicative f, Applicative g) => f (g c) -> f (g c) -> f (g c)
f //* g = liftA2 (liftA2 (*)) f g

