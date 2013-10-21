{-# LANGUAGE BangPatterns #-}

module Measurable.Generic where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
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
fromObservationsT :: (Monad m, Fractional r) => [a] -> ContT r m a
fromObservationsT xs = ContT (`weightedAverageM` xs)

-- A mass function is close to universal when dealing with discrete objects, but
-- the problem is that we need to create it over the entire support.  In terms 
-- of getting answers out, that breaks down immediately for something as trivial
-- as the natural numbers.
--
-- Maybe we can use something like an 'observed support'.  You can probably get
-- inspiration from how the Dirichlet process is handled in practice.
fromMassFunctionT
  :: (Num r, Applicative f)
  => (a -> f r)
  -> [a]
  -> ContT r f a
fromMassFunctionT p support = ContT $ \f ->
  fmap sum . traverse (liftA2 (liftA2 (*)) f p) $ support

-- | Expectation is obtained by integrating against the identity function.  We
--   provide an additional function for mapping the input type to the output 
--   type -- if these are equivalent, this would just `id`.
--
--   NOTE should we have this transformation handled elsewhere?  I.e. make fmap
--        responsible for transforming the type?
expectationT :: Monad m => (a -> r) -> MeasureT r m a -> m r
expectationT f = (`measureT` (return . f))

-- | The volume is obtained by integrating against a constant.  This is '1' for
--   any probability measure.
volumeT :: (Num r, Monad m) => MeasureT r m r -> m r
volumeT mu = measureT mu (return . const 1)

-- | Cumulative distribution function.  Only makes sense for Fractional/Ord
--   inputs.  Lots of potentially interesting cases where this isn't necessarily
--   true.
cdfT :: (Fractional r, Ord r, Monad m) => MeasureT r m r -> r -> m r
cdfT mu x = expectationT id $ (negativeInfinity `to` x) <$> mu

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
average :: Fractional a => [a] -> a
average xs = fst $ foldl' 
  (\(!m, !n) x -> (m + (x - m) / fromIntegral (n + 1), n + 1)) (0, 0) xs
{-# INLINE average #-}

-- | Weighted average.
weightedAverage :: Fractional c => (a -> c) -> [a] -> c
weightedAverage f = average . map f
{-# INLINE weightedAverage #-}

-- | Monadic weighted average.
weightedAverageM :: (Fractional c, Monad m) => (a -> m c) -> [a] -> m c
weightedAverageM f = liftM average . mapM f
{-# INLINE weightedAverageM #-}


