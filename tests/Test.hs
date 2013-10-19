-- Simple examples that demonstrate some measure-foo.

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
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
      rho = (cos <$> mu) + (sin <$> nu)
      eta = exp <$> rho

  putStrLn $ "mean of normal samples (should be around 0):                " ++ 
               show (expectation . fromObservations $ normSamples)
  putStrLn $ "variance of normal samples (should be around 1):            " ++ 
               show (variance . fromObservations $ normSamples)
  putStrLn $ "let X ~ N(0, 1), Y ~ observed.  mean of exp(cos X + sin Y): " ++
               show (expectation eta)

  putStrLn ""
  putStrLn "and now some 'woah, this actally seems to make sense' examples:"
  putStrLn ""

  -- Subtraction of measures?

  let iota = mu - mu
  
  putStrLn $ "let X, Y be independent N(0, 1).  mean of X - Y:            " ++
               show (expectation iota)
  putStrLn $ "let X, Y be independent N(0, 1).  variance of X - Y:        " ++
               show (variance iota)

  -- Product of measures?  *pops out of cake* YEAH WE CAN DO THAT

  let phi  = fromDensity $ genLocationNormal 2
      xi   = fromDensity $ genLocationNormal 3
      zeta = phi * xi

  putStrLn $ "let X ~ N(2, 1), Y ~ N(3, 1). mean of XY (should be 6)      " ++
               show (expectation zeta)
  putStrLn $ "let X ~ N(2, 1), Y ~ N(3, 1). variance of XY (should be 14) " ++
               show (variance zeta)

  let alpha = fromDensity $ density $ chiSquared 5
      beta  = (exp . tanh) <$> (phi * alpha)

  putStrLn $ "let X ~ N(2, 1), Y ~ chisq(5).  variance of exp (tanh XY)   " ++
               show (variance beta)

  putStrLn ""
  putStrLn "Some probability examples:"
  putStrLn ""

  putStrLn $ "let X ~ N(0, 1).  P(X < 0) (should be ~ 0.5):               " ++
               show (cdf mu 0)

  putStrLn $ "let X ~ N(0, 1).  P(0 < X < 1) (should be ~ 0.341):         " ++
               show (expectation $ 0 `to` 1 <$> mu)

  putStrLn $ "let X ~ N(0, 1), Y ~ observed.  P(0 < X < 0.8):             " ++
               show (expectation $ 0 `to` 0.8 <$> (mu + nu))


