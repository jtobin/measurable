module Measurable where

import Control.Applicative
import Numeric.Integration.TanhSinh

-- | A measure is a set function from some sigma-algebra to the extended real
--   line.  In practical terms we define probability in terms of measures; for a
--   probability measure /P/ and some measurable set /A/, the measure of the set
--   is defined to be that set's probability.
--
--   For any sigma field, there is a one-to-one correspondence between measures
--   and increasing linear functionals on its associated space of positive
--   measurable functions.  That is,
--
--   P(A) = f(I_A)
--
--   For A a measurable set and I_A its indicator function.  So we can generally
--   abuse notation to write something like P(I_A), even though we don't
--   directly apply the measure to a function.
--
--   So we can actually deal with measures in terms of measurable functions,
--   rather than via the sigma algebra or measurable sets.  Instead of taking
--   a set and returning a probability, we can take a function and return a
--   probability.

type Measure a = (a -> Double) -> Double

-- | Once we have a measure, we use it by integrating against it.  Take a 
--   real-valued random variable (i.e., measurable function) /X/.  The mean of X
--   is E X = int_R X d(Px), for Px the distribution (image measure) of X.
--
--   We can generalize this to work with arbitrary measurable functions and 
--   measures.  Expectation can be defined by taking a measurable function and
--   applying a measure to it - i.e., it's just function application.
--
--   expectation :: Measure Double -> (Double -> Double) -> Double
--   expectation 
--     :: ((Double -> Double) -> Double)   -- (a -> b)
--     -> (Double -> Double) -> Double     --  a -> b
--   
--   ($) :: (a -> b) -> a -> b
--
--   expectation == ($)
--
--   So there's no need to express expectation as its own operator.  We can just
--   define particular expectations that are interesting.  Moments, for example.

-- NOTE want to add cumulants

-- | The nth raw moment.
rawMoment :: Integral a => Measure Double -> a -> Double
rawMoment mu n = mu (^^ n)

-- | All positive raw moments.
rawMoments :: Measure Double -> [Double]
rawMoments mu = map (rawMoment mu) [1..]

-- | Alias for first raw moment.
mean :: Measure Double -> Double
mean mu = rawMoment mu 1

-- | The nth central moment.
--
-- NOTE slow-as in ghci.  Need to memoize or something, or this might just 
--      disappear when compiling.
centralMoment :: Integral a => Measure Double -> a -> Double
centralMoment mu n = mu $ (^^ n) . \x -> x - rawMoment mu 1

-- | All positive central moments.
centralMoments :: Measure Double -> [Double]
centralMoments mu = map (centralMoment mu) [1..]

-- | Alias for second central moment.
variance :: Measure Double -> Double
variance mu = centralMoment mu 2

-- | The nth normalized moment.
normalizedMoment :: Integral a => Measure Double -> a -> Double
normalizedMoment mu n = (/ (sd ^ n)) $ centralMoment mu n
  where sd = sqrt $ centralMoment mu 2

-- | All normalized moments.
normalizedMoments :: Measure Double -> [Double]
normalizedMoments mu = map (normalizedMoment mu) [1..]

-- | The moment generating function about a point.
momentGeneratingFunction :: Double -> Measure Double -> Double
momentGeneratingFunction t mu = mu $ exp . (* t) . id

-- | The cumulant generating function about a point.
cumulantGeneratingFunction :: Double -> Measure Double -> Double
cumulantGeneratingFunction t mu = log $ momentGeneratingFunction t mu

-- | We want two ways to create measures; empirically (i.e. from observations) 
--   or directly from some integrable function (i.e. a density).  

-- | Construct an empirical measure from observations.
fromObservations :: [Double] -> Measure Double
fromObservations xs f = normalize . sum . map f $ xs
  where normalize = (/ fromIntegral (length xs))

-- | Construct a measure from a density function.
fromDensity :: (Double -> Double) -> (Double -> Double) -> Double
fromDensity d f = quadratureTanhSinh $ liftA2 (*) f d
  where quadratureTanhSinh = result . last . everywhere trap

-- | For a random variable X, measurable function f, and measure P, we can 
--   construct the image (pushforward) measure P_(f X).
push :: (Double -> Double) -> Measure Double -> Measure Double
push f mu g = mu $ g . f

-- | Measure composition is convolution.  This allows us to compose measures,
--   independent of how they were constructed. 
--
--   This is (.) on the category of measures.
convolute :: Measure Double -> Measure Double -> Measure Double
convolute mu nu f = nu $ \y -> mu $ \x -> f $ x + y

-- | The identity measure.
--
--   This is 'id' on the category of measures.
identity :: Measure Double
identity = fromObservations [0]

-- instance Functor Measure where
--   fmap f mu = push f mu

