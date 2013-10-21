{-# LANGUAGE BangPatterns #-}

module Measurable.Generic where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.List
import Data.Monoid
import qualified Data.Set as Set
import Data.Traversable hiding (mapM)
import Numeric.Integration.TanhSinh

type MeasureT r m a = ContT r m a

-- | A more measure-y alias for runContT.
measureT :: MeasureT r m a -> (a -> m r) -> m r
measureT = runContT

-- | Create a measure from observations (samples) from some distribution.
fromObservations 
  :: (Applicative m, Monad m, Fractional r, Traversable f)
  => f a
  -> MeasureT r m a
fromObservations xs = ContT (`weightedAverageM` xs)

-- A mass function is close to universal when dealing with discrete objects, but
-- the problem is that we need to create it over the entire support.  In terms 
-- of getting answers out, that breaks down immediately for something as trivial
-- as the natural numbers.
--
-- Maybe we can use something like an 'observed support'.  You can probably get
-- inspiration from how the Dirichlet process is handled in practice.
fromMassFunction
  :: (Num r, Applicative f, Traversable t)
  => (a -> f r)
  -> t a
  -> MeasureT r f a
fromMassFunction p support = ContT $ \f ->
  fmap Foldable.sum . traverse (liftA2 (liftA2 (*)) f p) $ support

-- | Expectation is obtained by integrating against the identity function.  We
--   provide an additional function for mapping the input type to the output 
--   type -- if these are equivalent, this would just `id`.
--
--   NOTE should we have this transformation handled elsewhere?  I.e. make fmap
--        responsible for transforming the type?
expectation :: Monad m => (a -> r) -> MeasureT r m a -> m r
expectation f = (`measureT` (return . f))

-- | The volume is obtained by integrating against a constant.  This is '1' for
--   any probability measure.
volume :: (Num r, Monad m) => MeasureT r m r -> m r
volume mu = measureT mu (return . const 1)

-- | Cumulative distribution function.  Only makes sense for Fractional/Ord
--   inputs.  Lots of potentially interesting cases where this isn't necessarily
--   true.
cdf :: (Fractional r, Ord r, Monad m) => MeasureT r m r -> r -> m r
cdf mu x = expectation id $ (negativeInfinity `to` x) <$> mu

-- | Integrate from a to b.
to :: (Num a, Ord a) => a -> a -> a -> a
to a b x | x >= a && x <= b = 1
         | otherwise        = 0

-- | End of the line.
negativeInfinity :: Fractional a => a
negativeInfinity = negate (1 / 0)

-- | Integrate over an ordered, discrete set.
containing :: (Num a, Ord b) => [b] -> b -> a
containing xs x | x `Set.member` set = 1
                | otherwise          = 0
  where set = Set.fromList xs

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


