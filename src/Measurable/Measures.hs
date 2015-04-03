
module Measurable.Measures where

import Measurable.Core
import Numeric.SpecFunctions (choose)
import Statistics.Distribution
import qualified Statistics.Distribution.Beta as Statistics
import qualified Statistics.Distribution.Binomial as Statistics
import qualified Statistics.Distribution.ChiSquared as Statistics
import qualified Statistics.Distribution.Gamma as Statistics
import qualified Statistics.Distribution.Exponential as Statistics
import qualified Statistics.Distribution.Normal as Statistics

standard :: Measure Double
standard = fromDensityFunction pdf where
  pdf = density Statistics.standard

normal :: Double -> Double -> Measure Double
normal m s = fromDensityFunction pdf where
  pdf = density $ Statistics.normalDistr m s

logNormal :: Double -> Double -> Measure Double
logNormal m s = fmap exp (normal m s)

exponential :: Double -> Measure Double
exponential r = fromDensityFunction pdf where
  pdf = density $ Statistics.exponential r

gamma :: Double -> Double -> Measure Double
gamma a b = fromDensityFunction pdf where
  pdf = density $ Statistics.gammaDistr a b

inverseGamma :: Double -> Double -> Measure Double
inverseGamma a b = fmap recip (gamma a b)

chiSquare :: Int -> Measure Double
chiSquare k = fromDensityFunction pdf where
  pdf = density $ Statistics.chiSquared k

beta :: Double -> Double -> Measure Double
beta a b = fromDensityFunction pdf where
  pdf = density $ Statistics.betaDistr a b

binomial :: Int -> Double -> Measure Int
binomial n p = fromMassFunction pmf [0..n] where
  pmf = binomialMass n p

bernoulli :: Double -> Measure Int
bernoulli = binomial 1

binomialMass :: Int -> Double -> Int -> Double
binomialMass n p k = bc * p ^ k * (1 - p) ^ (n - k) where
  bc = n `choose` k

