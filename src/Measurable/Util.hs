
module Measurable.Util where

import Control.Applicative
import Control.Foldl
import qualified Control.Foldl as Foldl
import Data.Foldable (Foldable)
import Data.Traversable

negativeInfinity :: Fractional a => a
negativeInfinity = negate $ 1 / 0

weightedAverage :: (Foldable f, Fractional r) => (a -> r) -> f a -> r
weightedAverage f = Foldl.fold (weightedAverageFold f)

weightedAverageM
  :: (Traversable t, Applicative f, Fractional r)
  => (a -> f r)
  -> t a
  -> f r
weightedAverageM f = fmap (Foldl.fold averageFold) . traverse f

weightedAverageFold :: Fractional r => (a -> r) -> Fold a r
weightedAverageFold f = Foldl.premap f averageFold

averageFold :: Fractional a => Fold a a
averageFold = (/) <$> Foldl.sum <*> Foldl.genericLength

-- | Lifted multiplication.
(/*) :: (Num c, Applicative f) => f c -> f c -> f c
(/*)  = liftA2 (*)

-- | Doubly-lifted multiplication.
(//*) :: (Num c, Applicative f, Applicative g) => f (g c) -> f (g c) -> f (g c)
(//*) = liftA2 (/*)

