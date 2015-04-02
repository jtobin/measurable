{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Measurable.Core where

import Control.Arrow
import Control.Applicative
import Control.Foldl (Fold)
import qualified Control.Foldl as Foldl
import Control.Monad
import Control.Monad.Trans
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Functor.Identity
import Data.Traversable
import Numeric.Integration.TanhSinh

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

type Cont r = ContT r Identity

runCont :: Cont r a -> (a -> r) -> r
runCont m k = runIdentity $ runContT m (Identity . k)

cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT $ \c -> Identity $ f (runIdentity . c)

type Measure a    = Cont Double a
type MeasureT m a = ContT Double m a

instance Functor (ContT r m) where
  fmap f m = ContT $ \c -> runContT m (c . f)

instance Applicative (ContT r m) where
  pure x  = ContT ($ x)
  f <*> v = ContT $ \c ->
    runContT f $ \g ->
      runContT v (c . g)

instance Monad (ContT r m) where
  return x = ContT ($ x)
  m >>= k  = ContT $ \c ->
    runContT m $ \x ->
      runContT (k x) c

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

integrate :: (a -> Double) -> Measure a -> Double
integrate = flip runCont

integrateT :: Applicative m => (a -> Double) -> MeasureT m a -> m Double
integrateT f = (`runContT` (pure . f))

instance (Applicative m, Num a) => Num (ContT Double m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = fmap id
  signum      = fmap signum
  fromInteger = pure . fromInteger

fromMassFunction :: Foldable f => (a -> Double) -> f a -> Measure a
fromMassFunction f support = cont $ \g -> weightedAverage (g /* f) support

fromMassFunctionT :: (Applicative m, Traversable t)
  => (a -> Double)
  -> t a
  -> MeasureT m a
fromMassFunctionT f support = ContT $ \g ->
    fmap Foldable.sum . traverse (g //* (pure . f)) $ support

fromDensityFunction :: (Double -> Double) -> Measure Double
fromDensityFunction d = cont $ \f -> quadratureTanhSinh $ f /* d where
  quadratureTanhSinh = result . last . everywhere trap

fromSamples :: Foldable f => f a -> Measure a
fromSamples = cont . flip weightedAverage

fromSamplesT
  :: (Applicative m, Traversable f)
  => f a
  -> MeasureT m a
fromSamplesT = ContT . flip weightedAverageM

fromSamplingFunction
  :: (Monad m, Applicative m)
  => (t -> m b)
  -> Int
  -> t
  -> MeasureT m b
fromSamplingFunction f n g = (lift $ replicateM n (f g)) >>= fromSamplesT

push :: (a -> b) -> Measure a -> Measure b
push = fmap

pushT :: Monad m => (a -> b) -> MeasureT m a -> MeasureT m b
pushT = fmap

-- | Expectation is integration against the identity function.
expectation :: Measure Double -> Double
expectation = integrate id

expectationT :: Applicative m => MeasureT m Double -> m Double
expectationT = integrateT id

variance :: Measure Double -> Double
variance mu = integrate (^ 2) mu - expectation mu ^ 2

varianceT :: Applicative m => MeasureT m Double -> m Double
varianceT mu = liftA2 (-) (integrateT (^ 2) mu) ((^ 2) <$> expectationT mu)

meanVariance :: Measure Double -> (Double, Double)
meanVariance = expectation &&& variance

meanVarianceT :: Applicative m => MeasureT m Double -> m (Double, Double)
meanVarianceT mu = liftA2 (,) (expectationT mu) (varianceT mu)

rawMoment :: Int -> Measure Double -> Double
rawMoment n = integrate (^ n)

rawMomentT :: (Applicative m, Monad m) => Int -> MeasureT m Double -> m Double
rawMomentT n = integrateT (^ n)

rawMoments :: Measure Double -> [Double]
rawMoments mu = (`rawMoment` mu) <$> [1..]

rawMomentsT :: (Applicative m, Monad m) => MeasureT m Double -> Int -> m [Double]
rawMomentsT mu n = traverse (`rawMomentT` mu) $ take n [1..]

centralMoment :: Int -> Measure Double -> Double
centralMoment n mu = integrate (\x -> (x - rm) ^ n) $ mu
  where rm = rawMoment 1 mu

centralMomentT :: (Applicative m, Monad m) => Int -> MeasureT m Double -> m Double
centralMomentT n mu = integrateT (^ n) $ do
  rm <- lift $ rawMomentT 1 mu
  (subtract rm) <$> mu

centralMoments :: Measure Double -> [Double]
centralMoments mu = (`centralMoment` mu) <$> [1..]

centralMomentsT :: (Applicative m, Monad m) => MeasureT m Double -> Int -> m [Double]
centralMomentsT mu n = traverse (`centralMomentT` mu) $ take n [1..]

momentGeneratingFunction :: Measure Double -> Double -> Double
momentGeneratingFunction mu t = integrate (exp . (* t)) mu

cumulantGeneratingFunction :: Measure Double -> Double -> Double
cumulantGeneratingFunction mu = log . momentGeneratingFunction mu

volume :: Measure a -> Double
volume = integrate $ const 1

volumeT :: Applicative m => MeasureT m a -> m Double
volumeT = integrateT $ const 1

cdf :: Measure Double -> Double -> Double
cdf mu x = expectation $ negativeInfinity `to` x <$> mu

cdfT :: Applicative m => MeasureT m Double -> Double -> m Double
cdfT mu x = expectationT $ negativeInfinity `to` x <$> mu

to :: (Num a, Ord a) => a -> a -> a -> a
to a b x
  | x >= a && x <= b = 1
  | otherwise        = 0

containing :: (Num a, Eq b) => [b] -> b -> a
containing xs x
  | x `elem` xs = 1
  | otherwise   = 0

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

(/*) :: (Num c, Applicative f) => f c -> f c -> f c
(/*)  = liftA2 (*)

(//*) :: (Num c, Applicative f, Applicative g) => f (g c) -> f (g c) -> f (g c)
(//*) = liftA2 (/*)

