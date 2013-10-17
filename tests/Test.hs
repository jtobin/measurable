-- Simple example that demonstrates some measure-foo.
--
-- Create a measure from a standard normal pdf, and another from a collection of
-- samples drawn from an exponential distribution.
--
-- Push trigonometric functions on each and convolute the results.
--
-- Push an exponential function on that, and calculate the mean of the resulting 
-- distribution.
--

import Control.Monad
import Measurable
import Statistics.Distribution hiding (mean, variance)
import Statistics.Distribution.Normal
import System.Random.MWC
import System.Random.MWC.Distributions

standardNormal = density $ normalDistr 0 1

main = do
  expSamples <- withSystemRandom . asGenIO $ \g -> 
                  replicateM 100 $ exponential 1 g

  normSamples <- withSystemRandom . asGenIO $ \g ->
                  replicateM 100 $ normal 0 1 g

  let mu  = fromDensity standardNormal
      nu  = fromObservations expSamples
      rho = convolute (push cos mu) (push sin nu)
      eta = push exp rho

  putStrLn $ "mean of normal samples (should be around 0):                " ++ 
               show (mean . fromObservations $ normSamples)
  putStrLn $ "variance of normal samples (should be around 1):            " ++ 
               show (variance . fromObservations $ normSamples)
  putStrLn $ "let X ~ N(0, 1), Y ~ observed.  mean of exp(cos X + sin Y): " ++
               show (mean eta)

