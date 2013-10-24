{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Control.Arrow
import Control.Error
import Control.Lens hiding (to)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Vector (singleton)
import qualified Data.Traversable as Traversable
import Measurable.Core
import Numeric.SpecFunctions
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Distribution.Beta
import Statistics.Distribution.ChiSquared
import System.Random.MWC
import System.Random.MWC.Distributions

-- | Some workhorse densities (with respect to Lebesgue measure).
genNormal m v = density $ normalDistr m v
genBeta   a b = density $ betaDistr a b
genChiSq  d   = density $ chiSquared d

-- | Measures created from densities.  Notice that the binomial measure has to
--   be treated differently than the measures absolutely continuous WRT Lebesgue
--   measure.
normalMeasure m v = fromDensityLebesgue $ genNormal m v
betaMeasure   a b = fromDensityLebesgue $ genBeta a b
chiSqMeasure  d   = fromDensityLebesgue $ genChiSq d

-- | And a measure represented directly over a sampler.
altBetaMeasure epochs a b g = do
  bs <- lift $ replicateM epochs (genContVar (betaDistr a b) g)
  fromObservationsT bs


-- | A standard beta-binomial conjugate model.  Notice how naturally it's 
--   expressed using do-notation!
betaBinomialConjugate :: Double -> Double -> Int -> Measure Double Int
betaBinomialConjugate a b n = do
  p <- betaMeasure a b
  binomMeasure n p

-- | Observe a gamma distribution.
genGammaSamples 
  :: (Applicative m, PrimMonad m)
  => Int
  -> Double
  -> Double
  -> Gen (PrimState m) 
  -> m [Double]
genGammaSamples n a b g  = replicateM n $ gamma a b g

-- | Observe a normal distributions.
genNormalSamples
  :: (Applicative m, PrimMonad m)
  => Int
  -> Double
  -> Double
  -> Gen (PrimState m)
  -> m [Double]
genNormalSamples n m t g = replicateM n $ normal m (1 / t) g

-- | Normal-gamma model.  Note the resulting type is a probability measure on
--   tuples.
--
--   X | t  ~ N(mu, 1/(lambda * t))
--   t      ~ gamma(a, b)
--   (X, t) ~ NormalGamma(mu, lambda, a, b)
normalGammaMeasure 
  :: (Fractional r, Applicative m, PrimMonad m) 
  => Int
  -> Double
  -> Double
  -> Double
  -> Double
  -> Gen (PrimState m)
  -> MeasureT r m (Double, Double)
normalGammaMeasure n a b mu lambda g = do
  gammaSamples <- lift $ genGammaSamples n a b g
  precision    <- fromObservationsT gammaSamples

  normalSamples <- lift $ genNormalSamples n mu (lambda * precision) g
  location      <- fromObservationsT normalSamples

  return (location, precision)

-- | Alternate Normal-gamma model, to demonstrate probability measures over 
--   various return types.  Here we have a probability distribution over hash 
--   maps.
altNormalGammaMeasure 
  :: (Fractional r, Applicative m, PrimMonad m) 
  => Int
  -> Double
  -> Double
  -> Double
  -> Double
  -> Gen (PrimState m)
  -> MeasureT r m (HashMap String Double)
altNormalGammaMeasure n a b mu lambda g = do
  gammaSamples <- lift $ genGammaSamples n a b g
  precision    <- fromObservationsT gammaSamples

  normalSamples <- lift $ genNormalSamples n mu (lambda * precision) g
  location      <- fromObservationsT normalSamples

  return $ HashMap.fromList [("location", location), ("precision", precision)]

-- | A normal-normal gamma conjugate model
normalNormalGammaMeasure 
  :: (Fractional r, Applicative m, PrimMonad m) 
  => Int
  -> Double
  -> Double
  -> Double
  -> Double
  -> Gen (PrimState m)
  -> MeasureT r m Double
normalNormalGammaMeasure n a b mu lambda g = do
  (m, t) <- normalGammaMeasure n a b mu lambda g
  normalSamples <- lift $ genNormalSamples n m t g
  fromObservationsT normalSamples

-- | Alternate normal-normal gamma conjugate model.
altNormalNormalGammaMeasure
  :: (Fractional r, Applicative m, PrimMonad m) 
  => Int
  -> Double
  -> Double
  -> Double
  -> Double
  -> Gen (PrimState m)
  -> MeasureT r m Double
altNormalNormalGammaMeasure n a b mu lambda g = do
  parameterHash <- altNormalGammaMeasure n a b mu lambda g
  let m = fromMaybe (error "no location!") $ 
            HashMap.lookup "location" parameterHash
      t = fromMaybe (error "no precision!") $ 
            HashMap.lookup "precision" parameterHash
  normalSamples <- lift $ genNormalSamples n m t g
  fromObservationsT normalSamples

-- | The binomial density.
binom :: Double -> Int -> Int -> Double
binom p n k
 | n <= 0    = 0
 | k <  0    = 0
 | n < k     = 0
 | otherwise = n `choose` k * p ^ k * (1 - p) ^ (n - k)

-- | Generate a measure from the binomial density.
binomMeasure 
  :: (Applicative m, Monad m)
  => Int
  -> Double
  -> MeasureT Double m Int
binomMeasure n p = fromDensityCountingT (binom p n) [0..n]

-- | Note that we can handle all sorts of things that have densities w/respect
--   to counting measure.  They don't necessarily have to have domains that 
--   are instances of Num (or even have Ordered domains, though that's the case
--   here).
data Group = A | B | C deriving (Enum, Eq, Ord, Show)

-- | Density of a categorical measure.
categoricalOnGroupDensity :: Fractional a => Group -> a
categoricalOnGroupDensity g
  | g == A = 0.3
  | g == B = 0.6
  | g == C = 0.1

-- | Here's a measure defined on the Group data type. 
categoricalOnGroupMeasure 
  :: (Applicative m, Monad m, Fractional r)
  => MeasureT r m Group
categoricalOnGroupMeasure = 
  fromDensityCountingT categoricalOnGroupDensity [A, B, C]

-- | A gaussian mixture model, with mixing probabilities based on observed 
--   groups.  Again, note that Group is not an instance of Num!   We can compose
--   measures of various types, so long as our 'end type' is Fractional.
--
--   X | S ~ case S of
--             A -> observed from N(-2, 1)
--             B -> observed from N( 0, 1)
--             C -> observed from N( 1, 1)
--
--   S     ~ observed from categorical
--
gaussianMixtureModel
  :: (Fractional r, Applicative m, PrimMonad m)
  => Int
  -> [Group] 
  -> Gen (PrimState m)
  -> MeasureT r m Double
gaussianMixtureModel n observed g = do
  s       <- fromObservationsT observed
  samples <- case s of
               A -> lift $ genNormalSamples n (-2) 1 g
               B -> lift $ genNormalSamples n 0 1 g
               C -> lift $ genNormalSamples n 1 1 g

  fromObservationsT samples

