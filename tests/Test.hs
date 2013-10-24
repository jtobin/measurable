-- Simple examples that demonstrate some measure-fu.

import Control.Applicative
import Control.Monad
import Data.Vector (singleton)
import Measurable.Core
import Numeric.SpecFunctions
import Statistics.Distribution hiding (mean, variance)
import Statistics.Distribution.Normal
import Statistics.Distribution.Beta
import Statistics.Distribution.ChiSquared
import System.Random.MWC
import System.Random.MWC.Distributions

-- | Some workhorse densities (with respect to Lebesgue measure).
genNormal m v = density $ normalDistr m v
genBeta   a b = density $ betaDistr a b
genChiSq  d   = density $ chiSquared d

-- | A binomial density (with respect to counting measure).
binom p n k
  | n <= 0    = 0
  | k <  0    = 0
  | n < k     = 0
  | otherwise = n `choose` k * p ^ k * (1 - p) ^ (n - k)

-- | Measures created from densities.  Notice that the binomial measure has to
--   be treated differently than the measures absolutely continuous WRT Lebesgue
--   measure.
normalMeasure m v = fromDensityLebesgue $ genNormal m v
betaMeasure   a b = fromDensityLebesgue $ genBeta a b
chiSqMeasure  d   = fromDensityLebesgue $ genChiSq d
binomMeasure  n p = fromDensityCounting (binom p n) [0..n]

-- | Sampling functions.
generateExpSamples n l g      = replicateM n (exponential l g)
generateNormalSamples n m v g = replicateM n (normal m v g)

-- | A standard beta-binomial conjugate model.  Notice how naturally it's 
--   expressed using do-notation!
betaBinomialConjugate :: Double -> Double -> Int -> Measure Double Int
betaBinomialConjugate a b n = do
  p <- betaMeasure a b
  binomMeasure n p

main :: IO ()
main = do
  -- Initialize our PRNG.
  g <- initialize (singleton 42)

  -- Generate some samples (in practice we'd usually create measures directly
  -- from samples).
  expSamples  <- generateExpSamples 1000 1 g
  normSamples <- generateNormalSamples 1000 0 1 g

  -- Create a couple of measures from those.
  let observedExpMeasure    = fromObservations expSamples
      observedNormalMeasure = fromObservations normSamples

  putStrLn "X ~ N(0, 1)"
  putStrLn "Y ~ empirical (observed from exponential(1))"
  putStrLn "Z ~ empirical (observed from N(0, 1))"
  putStrLn "W ~ ChiSquared(5)"
  putStrLn ""

  -- We can mingle our empirical measures with those created directly from
  -- densities.  We can literally just add measures together (there's a 
  -- convolution happening under the hood).

  let mu = normalMeasure 0 1 + observedExpMeasure
  putStrLn $ "E(X + Y):             " ++ show (expectation mu)

  -- We can create pushforward/image measures by.. pushing functions onto
  -- measures.  
  --
  -- The pushforward operator happens to be trusty old 'fmap', (as infix, <$>).

  let nu = (cos <$> normalMeasure 0 1) * (sin <$> observedNormalMeasure)
  putStrLn $ "E(cos X * sin Z):     " ++ show (expectation nu)

  let eta = exp <$> nu
  putStrLn $ "E[e^(cos X * sin Z)]: " ++ show (expectation eta)

  -- At present the complexity of each Measure operation seems to *explode*, so
  -- you can't do more than a few of them without your machine locking up.  I
  -- have to look into what could be done to make this reasonably efficient. 
  -- But hey, experiments and such..

  let zeta = (exp . tanh) <$> (chiSqMeasure 5 * normalMeasure 0 1)
  putStrLn $ "E[e^(tanh (X * W))]:  " ++ show (expectation zeta)

  putStrLn ""

  -- We can do probability by just taking the expectation of an indicator 
  -- function, and there's a built-in cumulative distribution function.
  --
  -- P(X < 0) for example.  It should be 0.5, but there is some error due to 
  -- quadrature.

  putStrLn $ "P(X < 0):             " ++ show (cdf (normalMeasure 0 1) 0)

  -- Everyone knows that for X ~ N(0, 1), P(0 < X < 1) is about 0.341..

  putStrLn $ "P(0 < X < 1):         " 
    ++ show (expectation $ 0 `to` 1 <$> normalMeasure 0 1)

  putStrLn ""

  -- The coolest trick of all is that monadic bind is Bayesian inference.  
  -- Getting posterior predictive expectations & probabilities is thus really
  -- declarative.

  putStrLn "X | p ~ binomial(10, p)"
  putStrLn "p     ~ beta(1, 4)"

  let phi = fromIntegral <$> betaBinomialConjugate 1 4 10

  putStrLn $ "E(X) (unconditional): " 
    ++ show (expectation phi)

  putStrLn $ "P(X == 5):            " 
    ++ show (expectation $ 5 `to` 5 <$> phi)

  putStrLn $ "P(1 <= X <= 5):       " 
    ++ show (expectation $ 1 `to` 5 <$> phi)

  putStrLn $ "var(X):               " ++ show (variance phi)

  -- Lots of kinks to be worked out, but this is a cool concept.

