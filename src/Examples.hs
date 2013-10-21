import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Vector (singleton)
import Measurable.Generic
import Numeric.SpecFunctions
import Statistics.Distribution
import System.Random.MWC
import System.Random.MWC.Distributions

genGammaSamples 
  :: PrimMonad m 
  => Int
  -> Double
  -> Double
  -> Gen (PrimState m) 
  -> m [Double]
genGammaSamples n a b g  = replicateM n $ gamma a b g

genNormalSamples
  :: PrimMonad m
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
  :: (Fractional r, PrimMonad m) 
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
  :: (Fractional r, PrimMonad m) 
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

normalNormalGammaMeasure 
  :: (Fractional r, PrimMonad m) 
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

altNormalNormalGammaMeasure
  :: (Fractional r, PrimMonad m) 
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

-- | A binomial density (with respect to counting measure).
binom :: Double -> Int -> Int -> Double
binom p n k
 | n <= 0    = 0
 | k <  0    = 0
 | n < k     = 0
 | otherwise = n `choose` k * p ^ k * (1 - p) ^ (n - k)

-- | Generate a binomial measure from its mass function.
binomMeasure 
  :: (Applicative m, Monad m)
  => Int
  -> Double
  -> MeasureT Double m Int
binomMeasure n p = fromMassFunctionT (return . binom p n) [0..n]

-- | Note that we can handle all sorts of things that are densities w/respect
--   to counting measure.  They don't necessarily have to have integral 
--   domains (or even have Ordered domains, though that's the case here).
data Group = A | B | C deriving (Enum, Eq, Ord, Show)

categoricalOnGroupDensity :: Fractional a => Group -> a
categoricalOnGroupDensity g
  | g == A = 0.3
  | g == B = 0.6
  | g == C = 0.1

-- | Here's a measure defined on the Group data type. 
categoricalOnGroupMeasure 
  :: (Applicative m, Monad m, Fractional a)
  => MeasureT a m Group
categoricalOnGroupMeasure = 
  fromMassFunctionT (return . categoricalOnGroupDensity) [A, B, C]

main :: IO ()
main = do
  let nng  = normalNormalGammaMeasure 30 2 6 1 0.5
      anng = altNormalNormalGammaMeasure 30 2 6 1 0.5
  m0 <- withSystemRandom . asGenIO $ \g ->
          expectationT id $ nng g

  m1 <- withSystemRandom . asGenIO $ \g ->
          expectationT id $ anng g

  p0 <- withSystemRandom . asGenIO $ \g ->
          expectationT id $ 2 `to` 3 <$> nng g

  p1 <- withSystemRandom . asGenIO $ \g ->
          expectationT id $ 2 `to` 3 <$> anng g

  binomialMean <- expectationT fromIntegral (binomMeasure 10 0.5)

  groupProbBC <- expectationT id
                   (containing [B, C] <$> categoricalOnGroupMeasure)

  print m0
  print m1

  print p0
  print p1

  print binomialMean

  print groupProbBC

