-- Simple example that demonstrates some measure-foo.
--
-- Create a measure from a standard normal pdf, and another from a collection of
-- samples drawn from an exponential distribution.
--
-- Push trigonometric functions on each and convolute the results.
--
-- Push an exponential function on that, and calculate the mean of that 
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
  g       <- create
  samples <- replicateM 30 $ exponential 1 g

  let mu  = fromDensity standardNormal
      nu  = fromObservations samples
      rho = convolute (push cos mu) (push sin nu)
      eta = push exp rho

  print $ mean eta

