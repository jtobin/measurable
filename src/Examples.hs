import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Vector (singleton)
import Measurable.Generic
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
--   X | t ~ N(mu, 1/(lambda * t))
--   t     ~ gamma(a, b)
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

main :: IO ()
main = do
  let nng  = normalNormalGammaMeasure 100 2 6 1 0.5
      anng = altNormalNormalGammaMeasure 100 2 6 1 0.5
  m0 <- withSystemRandom . asGenIO $ \g ->
          expectationT id $ nng g

  m1 <- withSystemRandom . asGenIO $ \g ->
          expectationT id $ anng g

  p0 <- withSystemRandom . asGenIO $ \g ->
          expectationT id $ 2 `to` 3 <$> nng g

  p1 <- withSystemRandom . asGenIO $ \g ->
          expectationT id $ 2 `to` 3 <$> anng g

  print m0
  print m1

  print p0
  print p1

