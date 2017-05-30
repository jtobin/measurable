{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Measurable.Core where

import Control.Applicative
import Control.Foldl (Fold)
import qualified Control.Foldl as Foldl
import Data.List (foldl')
import Numeric.Integration.TanhSinh

-- | A measure can be represented by nothing more than a continuation with a
--   restricted output type corresponding to the reals.
--
--   A @Functor@ instance implements pushforward or image measures - merely
--   @fmap@ a measurable function over a measure to create one.
--
--   An @Applicative@ instance adds product measure, and in turn measure
--   convolution, subtraction, and multiplication by enabling a @Num@ instance.
--
--   A @Monad@ instance lumps the ability to create measures from graphs of
--   measures on top of that.
newtype Measure a = Measure ((a -> Double) -> Double)

instance Functor Measure where
  fmap f nu = Measure $ \g ->
    integrate (g . f) nu

instance Applicative Measure where
  pure x = Measure (\f -> f x)
  Measure h <*> Measure g = Measure $ \f ->
    h (\k -> g (f . k))

instance Monad Measure where
  return x  = Measure (\f -> f x)
  rho >>= g = Measure $ \f ->
    integrate (\nu -> integrate f (g nu)) rho

instance Num a => Num (Measure a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

-- | The 'integrate' function is just 'runCont' with its arguments reversed
--   in order to resemble the conventional mathematical notation, in which one
--   integrates a measurable function against a measure.
--
--   >>> let mu = fromSample [-1, 0, 1]
--   >>> expectation mu
--   0.0
--   >>> variance mu
--   0.6666666666666666
integrate :: (a -> Double) -> Measure a -> Double
integrate f (Measure nu) = nu f

-- | The expectation of a measure is typically understood to be its expected
--   value, which is found by integrating it against the identity function.
expectation :: Measure Double -> Double
expectation = integrate id

-- | The variance of a measure, as per the usual formula
--   @var X = E^2 X - EX^2@.
variance :: Measure Double -> Double
variance mu = integrate (^ 2) mu - expectation mu ^ 2

-- | Calculates the volume of a 'Measure' over its entire space.  Trivially 1
--   for any probability measure.
--
--   >>> let mu = fromSample [1..10]
--   >>> volume mu
--   1.0
volume :: Measure a -> Double
volume = integrate $ const 1

-- | Create a 'Measure' from a probability mass function and its support,
--   provided as a foldable container.
--
--   The requirement to supply the entire support is restrictive but necessary;
--   for approximations, consider using 'fromSample'.
--
--   >>> let mu = fromMassFunction (binomialPmf 10 0.2) [0..10]
--   >>> integrate fromIntegral mu
--   2.0
fromMassFunction :: Foldable f => (a -> Double) -> f a -> Measure a
fromMassFunction f support = Measure $ \g ->
  foldl' (\acc x -> acc + f x * g x) 0 support

-- | Create a 'Measure' from a probability density function.
--
--   Note that queries on measures constructed with @fromDensityFunction@ are
--   subject to numerical error due to the underlying dependency on quadrature!
--
--   >>> let f x = 1 / (sqrt (2 * pi)) * exp (- (x ^ 2) / 2)
--   >>> let mu = fromDensityFunction f
--   >>> expectation mu
--   0.0
--   >>> variance mu
--   1.0000000000000002
fromDensityFunction :: (Double -> Double) -> Measure Double
fromDensityFunction d = Measure $ \f ->
    quadratureTanhSinh (\x -> f x * d x)
  where
    quadratureTanhSinh = result . last . everywhere trap

-- | Create a measure from a collection of observations.
--
--   Useful for creating general purpose empirical measures.
--
--   >>> let mu = fromSample [(1, 2), (3, 4)]
--   >>> integrate (uncurry (+)) mu
--   5.0
fromSample :: Foldable f => f a -> Measure a
fromSample = Measure . flip weightedAverage where
  weightedAverage :: (Foldable f, Fractional r) => (a -> r) -> f a -> r
  weightedAverage f = Foldl.fold (weightedAverageFold f)

  weightedAverageFold :: Fractional r => (a -> r) -> Fold a r
  weightedAverageFold f = Foldl.premap f averageFold

  averageFold :: Fractional a => Fold a a
  averageFold = (/) <$> Foldl.sum <*> Foldl.genericLength

-- | The @nth@ raw moment of a 'Measure'.
rawMoment :: Int -> Measure Double -> Double
rawMoment n = integrate (^ n)

-- | The @nth@ central moment of a 'Measure'.
centralMoment :: Int -> Measure Double -> Double
centralMoment n mu = integrate (\x -> (x - rm) ^ n) mu where
  rm = rawMoment 1 mu

-- | The moment generating function corresponding to a 'Measure'.
--
--   >>> let mu = fromSample [1..10]
--   >>> let mgfMu = mgf mu
--   >>> fmap mgfMu [0, 0.5, 1]
--   [1.0,37.4649671547254,3484.377384533132]
mgf :: Measure Double -> Double -> Double
mgf mu t = integrate (\x -> exp (t * x)) mu

-- | The cumulant generating function corresponding to a 'Measure'.
--
--   >>> let mu = fromSample [1..10]
--   >>> let cgfMu = cgf mu
--   >>> fmap cgfMu [0, 0.5, 1]
--   [0.0,3.6234062871236543,8.156044651432666]
cgf :: Measure Double -> Double -> Double
cgf mu = log . mgf mu

-- | The cumulative distribution function corresponding to a 'Measure'
--
--   >>> let mu = fromSample [1..10]
--   >>> let cdfMu = cdf mu
--   >>> fmap cdfMu [0..10]
--   [0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]
cdf :: Measure Double -> Double -> Double
cdf nu x = integrate (negativeInfinity `to` x) nu where
  negativeInfinity = negate (1 / 0)


-- | A helpful utility for calculating the volume of a region in a measure
--   space.
--
--   >>> let mu = fromSample [1..10]
--   >>> integrate (2 `to` 8) mu
--   0.7
to :: (Num a, Ord a) => a -> a -> a -> a
to a b x
  | x >= a && x <= b = 1
  | otherwise        = 0

-- | An analogue of 'to' for measures defined over non-ordered domains.
--
--   >>> data Group = A | B | C deriving Eq
--   >>> let mu = fromSample [A, A, A, B, A, B, C]
--   >>> integrate (containing [B]) mu
--   0.2857142857142857
--   >>> integrate (containing [A,C]) mu
--   0.7142857142857143
--   >>> integrate (containing [A,B,C]) mu
--   1.0
containing :: (Num a, Eq b) => [b] -> b -> a
containing xs x
  | x `elem` xs = 1
  | otherwise   = 0

