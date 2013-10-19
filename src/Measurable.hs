{-# LANGUAGE BangPatterns #-}

module Measurable where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
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
--
--   Once we have a measure, we use it by integrating against it.  Take a 
--   real-valued random variable (i.e., measurable function) /X/.  The mean of X
--   is E X = int_R X d(Px), for Px the distribution (image measure) of X.
--
--   We can generalize this to work with arbitrary measurable functions and 
--   measures.  Expectation can be defined by taking a measurable function and
--   applying a measure to it - i.e., just function application.
--
--   So really, a Measure in this sense is an expression of a particular 
--   computational process - expectation.  We leave a callback to be
--   plugged in - a measurable function - and from there, we can finish the 
--   computation and return a value.
--
--   This is equivalent to the type that forms the Continuation monad, albeit
--   with constant (Double) result type.  The functor and monad instances follow
--   suit.  The strength of the Monad instance is that it allows us to meld
--   together measures with different input types.

newtype Measure a = Measure { measure :: (a -> Double) -> Double }

instance Fractional a => Monoid (Measure a) where
  mempty  = identityMeasure
  mappend = convolute

instance Functor Measure where
  fmap = push

instance Applicative Measure where
  pure  = return
  (<*>) = ap

instance Monad Measure where
  return x = Measure (\f -> f x)
  mu >>= f = Measure $ \d ->
               measure mu $ \g ->
                 measure (f g) d

-- | The pushforward measure is obtained by 'pushing' a function onto an 
--   existing measure. 
push :: (a -> b) -> Measure a -> Measure b
push f mu = Measure pushforward
  where pushforward g = measure mu $ g . f

-- | The volume is obtained by integrating against a constant.  This is '1' for
--   any probability measure.
volume :: Measure a -> Double
volume mu = measure mu (const 1)

-- | The mean is obtained by integrating against the identity function.
mean :: Measure Double -> Double
mean mu = measure mu id

-- | The variance is obtained by integrating against the usual function.
variance :: Measure Double -> Double
variance mu = measure mu (^ 2) - mean mu ^ 2

-- | Create a measure from a collection of observations from some distribution.
fromObservations :: Fractional a => [a] -> Measure a
fromObservations xs = Measure $ \f -> 
                        average . map f $ xs

-- | Create a measure from a density function.
fromDensity :: (Double -> Double) -> Measure Double
fromDensity d = Measure $ \f -> quadratureTanhSinh $ liftM2 (*) f d
  where quadratureTanhSinh = result . last . everywhere trap

-- | Measure addition is convolution.  
convolute :: Num a => Measure a -> Measure a -> Measure a
convolute mu nu = Measure $ \f -> measure nu
                            $ \y -> measure mu
                              $ \x -> f (x + y)

-- | Measure subtraction. (does this make sense?)
msubtract :: Num a => Measure a -> Measure a -> Measure a
msubtract mu nu = Measure $ \f -> measure nu
                            $ \y -> measure mu
                              $ \x -> f (x - y)

-- | Measure multiplication. (does this make sense?)
mproduct :: Num a => Measure a -> Measure a -> Measure a
mproduct mu nu = Measure $ \f -> measure nu
                           $ \y -> measure mu
                             $ \x -> f (x * y)

-- | The (sum) identity measure.
identityMeasure :: Fractional a => Measure a
identityMeasure = fromObservations []

-- | Simple average.
average :: Fractional a => [a] -> a
average xs = fst $ foldl' 
  (\(!m, !n) x -> (m + (x - m) / fromIntegral (n + 1), n + 1)) (0, 0) xs
{-# INLINE mean #-}

