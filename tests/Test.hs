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
import Statistics.Distribution.ChiSquared
import System.Random.MWC
import System.Random.MWC.Distributions

standardNormal      = density $ normalDistr 0 1
genLocationNormal m = density $ normalDistr m 1

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

  putStrLn ""
  putStrLn "and now some 'woah, this actally seems to make sense' examples:"
  putStrLn ""

  -- Subtraction of measures?

  let iota = mu `msubtract` mu
  
  putStrLn $ "let X, Y be independent N(0, 1).  mean of X - Y:            " ++
               show (mean iota)
  putStrLn $ "let X, Y be independent N(0, 1).  variance of X - Y:        " ++
               show (variance iota)

  -- Product of measures?  *pops out of cake* YEAH WE CAN DO THAT

  let phi  = fromDensity $ genLocationNormal 2
      xi   = fromDensity $ genLocationNormal 3
      zeta = mproduct phi xi

  putStrLn $ "let X ~ N(2, 1), Y ~ N(3, 1). mean of XY (should be 6)      " ++
               show (mean zeta)
  putStrLn $ "let X ~ N(2, 1), Y ~ N(3, 1). variance of XY (should be 14) " ++
               show (variance zeta)

