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
  :: (Applicative m, PrimMonad m)
  => Int
  -> Double
  -> Double
  -> Gen (PrimState m) 
  -> m [Double]
genGammaSamples n a b g  = replicateM n $ gamma a b g

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
  precision    <- fromObservations gammaSamples

  normalSamples <- lift $ genNormalSamples n mu (lambda * precision) g
  location      <- fromObservations normalSamples

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
  precision    <- fromObservations gammaSamples

  normalSamples <- lift $ genNormalSamples n mu (lambda * precision) g
  location      <- fromObservations normalSamples

  return $ HashMap.fromList [("location", location), ("precision", precision)]

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
  fromObservations normalSamples

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
  fromObservations normalSamples

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
binomMeasure n p = fromMassFunction (return . binom p n) [0..n]

-- | Note that we can handle all sorts of things that have densities w/respect
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
  :: (Applicative m, Monad m, Fractional r)
  => MeasureT r m Group
categoricalOnGroupMeasure = 
  fromMassFunction (return . categoricalOnGroupDensity) [A, B, C]

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
  s       <- fromObservations observed
  samples <- case s of
               A -> lift $ genNormalSamples n (-2) 1 g
               B -> lift $ genNormalSamples n 0 1 g
               C -> lift $ genNormalSamples n 1 1 g

  fromObservations samples

-- | A bizarre measure.
weirdMeasure
  :: Fractional r
  => [Group]
  -> [Bool]
  -> MeasureT r IO Bool
weirdMeasure [] acc     = fromObservations acc
weirdMeasure (m:ms) acc
  | m == A = do
      j <- lift $ withSystemRandom . asGenIO $ uniform
      k <- lift $ withSystemRandom . asGenIO $ uniform
      if   j
      then weirdMeasure ms (k : acc)
      else weirdMeasure ms acc
  | otherwise = weirdMeasure ms acc

main :: IO ()
main = do
  let nng  = normalNormalGammaMeasure 30 2 6 1 0.5
      anng = altNormalNormalGammaMeasure 30 2 6 1 0.5
  m0 <- withSystemRandom . asGenIO $ \g ->
          expectation id $ nng g

  m1 <- withSystemRandom . asGenIO $ \g ->
          expectation id $ anng g

  p0 <- withSystemRandom . asGenIO $ \g ->
          expectation id $ 2 `to` 3 <$> nng g

  p1 <- withSystemRandom . asGenIO $ \g ->
          expectation id $ 2 `to` 3 <$> anng g

  binomialMean <- expectation fromIntegral (binomMeasure 10 0.5)

  groupProbBC <- expectation id
                   (containing [B, C] <$> categoricalOnGroupMeasure)

  let groupsObserved = [A, A, A, B, A, B, B, A, C, B, B, A, A, B, C, A, A]
      categoricalFromObservationsMeasure = fromObservations groupsObserved

  groupsObservedProbBC <- expectation id
    (containing [B, C] <$> categoricalFromObservationsMeasure)

  mixtureModelMean <- withSystemRandom . asGenIO $ \g -> 
    expectation id (gaussianMixtureModel 30 groupsObserved g)

  print m0
  print m1

  print p0
  print p1

  print binomialMean

  print groupProbBC
  print groupsObservedProbBC

  print mixtureModelMean

