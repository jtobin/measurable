-- Simple examples that demonstrate some measure-fu.

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Measurable
import Numeric.SpecFunctions
import Statistics.Distribution hiding (mean, variance)
import Statistics.Distribution.Normal
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.ChiSquared
import System.Random.MWC
import System.Random.MWC.Distributions

-- | A standard beta-binomial conjugate model.
betaBinomialConjugate :: Double -> Double -> Int -> Measure Double
betaBinomialConjugate a b n = do
  p <- betaMeasure a b
  binomMeasure n p

-- | Workhorse densities.
standardNormal      = density $ normalDistr 0 1
genLocationNormal m = density $ normalDistr m 1
basicBeta a b       = density $ betaDistr a b

-- | A beta measure.
betaMeasure a b     = fromDensity $ basicBeta a b

-- | A binomial mass function.
binom p n k | n <= 0    = 0
            | k <  0    = 0
            | n < k     = 0
            | otherwise = n `choose` k * p ^ k * (1 - p) ^ (n - k)

-- | Binomial measure.
binomMeasure n p = fromMassFunction (\x -> binom p n (truncate x)) 
                                    (map fromIntegral [0..n] :: [Double])

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

  putStrLn ""
  putStrLn "Creating from a mass function:"
  putStrLn ""

  
  let kappa = binomMeasure 10 0.5

  putStrLn $ "let X ~ binom(10, 0.5).  mean of X (should be 5):           " ++
                show (expectation kappa)
  putStrLn $ "let X ~ binom(10, 0.5).  variance of X (should be 2.5):     " ++
                show (variance kappa)

  putStrLn ""
  putStrLn "Bayesian inference"
  putStrLn ""

  let omega = betaBinomialConjugate 1 4 10

  putStrLn $ 
    "let X | p ~ binomial(10, p), p ~ beta(1, 4).  mean of posterior pred.:\n"
    ++ show (expectation omega)
  putStrLn $ 
    "let X | p ~ binomial(10, p), p ~ beta(1, 4).  variance of posterior pred:\n" 
    ++ show (variance omega)
  
