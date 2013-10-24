{-# LANGUAGE BangPatterns #-}

module Measurable.Core where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Monoid
import qualified Data.Set as Set
import Data.Traversable hiding (mapM)
import Numeric.Integration.TanhSinh

-- | A measure is represented as a continuation.
type Measure r a = Cont r a
type MeasureT r m a = ContT r m a

-- | A more measure-y alias for runCont.
measure :: Measure r a -> (a -> r) -> r
measure = runCont

measureT :: MeasureT r m a -> (a -> m r) -> m r
measureT = runContT

-- | Things like convolution are trivially expressed by lifted arithmetic 
--   operators.
instance (Monad m, Num a) => Num (ContT r m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = id
  signum      = error "signum: not supported for Measures"
  fromInteger = error "fromInteger: not supported for measures"

-- | Create a measure from a density w/respect to counting measure.
fromDensityCounting
  :: (Num r, Functor f, Foldable f)
  => (a -> r)
  -> f a
  -> Measure r a
fromDensityCounting f support = cont $ \g ->
  Foldable.sum . fmap (liftA2 (*) g f) $ support

fromDensityCountingT
  :: (Num r, Applicative m, Traversable t, Monad m)
  => (a -> r)
  -> t a
  -> MeasureT r m a
fromDensityCountingT p support = ContT $ \f ->
  fmap Foldable.sum . traverse (liftA2 (liftA2 (*)) f (return . p)) $ support

-- | Create a measure from a density w/respect to Lebesgue measure.
--
--   NOTE The quality of this implementation depends entirely on the underlying
--        quadrature routine.  As we're presently using the 
--        Numeric.Integration.TanhSinh module, the types are also constricted 
--        to Doubles.
--
--        This is included moreso for interest's sake, and doesn't produce 
--        particularly accurate results.
fromDensityLebesgue :: (Double -> Double) -> Measure Double Double
fromDensityLebesgue d = cont $ \f -> quadratureTanhSinh $ liftA2 (*) f d
  where quadratureTanhSinh = result . last . everywhere trap

-- | Create a measure from observations sampled from some distribution.
fromObservations
  :: (Functor f, Foldable f, Fractional r)
  => f a
  -> Measure r a
fromObservations = cont . flip weightedAverage

fromObservationsT
  :: (Applicative m, Monad m, Fractional r, Traversable f)
  => f a
  -> MeasureT r m a
fromObservationsT = ContT . flip weightedAverageM

-- | Integration of a function against a measure is just running a 
--   continuation.
integrate :: (a -> r) -> Measure r a -> r
integrate = flip measure

integrateT :: Monad m => (a -> r) -> MeasureT r m a -> m r
integrateT f = (`measureT` (return . f))

-- | Expectation is integration against the identity function.
expectation :: Measure r r -> r
expectation = integrate id 

expectationT :: Monad m => MeasureT r m r -> m r
expectationT = integrateT id

-- | The variance is obtained by integrating against the usual function.
variance :: Num r => Measure r r -> r
variance mu = measure mu (^ 2) - expectation mu ^ 2

varianceT :: (Monad m, Num r) => MeasureT r m r -> m r
varianceT mu = liftM2 (-) (measureT mu (return . (^ 2))) 
                          (liftM (^ 2) (expectationT mu))

-- | The measure of the underlying space.  This is trivially 1 for any 
--   probability measure.
volume :: Num r => Measure r r -> r
volume = integrate (const 1)

volumeT :: (Num r, Monad m) => MeasureT r m r -> m r
volumeT = integrateT (const 1)

-- | Cumulative distribution function.  Only makes sense for Fractional/Ord
--   inputs.
cdf :: (Fractional r, Ord r) => Measure r r -> r -> r
cdf mu x = expectation $ negativeInfinity `to` x <$> mu

cdfT :: (Fractional r, Ord r, Monad m) => MeasureT r m r -> r -> m r
cdfT mu x = expectationT $ negativeInfinity `to` x <$> mu

-- | Indicator function for the interval a <= x <= b.  Useful for integrating 
--   from a to b.
to :: (Num a, Ord a) => a -> a -> a -> a
to a b x | x >= a && x <= b = 1
         | otherwise        = 0

-- | Integrate over an ordered, discrete set.
containing :: (Num a, Ord b) => [b] -> b -> a
containing xs x | x `Set.member` set = 1
                | otherwise          = 0
  where set = Set.fromList xs

-- | End of the line.
negativeInfinity :: Fractional a => a
negativeInfinity = negate (1 / 0)

-- | Simple average.
average :: (Fractional a, Foldable f) => f a -> a
average xs = fst $ Foldable.foldl' 
  (\(!m, !n) x -> (m + (x - m) / fromIntegral (n + 1), n + 1)) (0, 0) xs
{-# INLINE average #-}

-- | Weighted average.
weightedAverage :: (Functor f, Foldable f, Fractional c) => (a -> c) -> f a -> c
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

