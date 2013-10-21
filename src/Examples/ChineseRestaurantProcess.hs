{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Control.Lens
import Control.Monad.Trans
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Measurable.Generic

data Table = Table { 
    _number :: {-# UNPACK #-} !Int
  , _people :: {-# UNPACK #-} !Int
  } deriving (Eq, Show)

instance Ord Table where
  t1 < t2 = _people t1 < _people t2

makeLenses ''Table

-- | Mass function for a given table.  It's dependent on the state of the 
--   restaurant via 'n' and 'newestTable'.
tableMass n a newestTable table 
  | table^.number == newestTable^.number = a / (fromIntegral n + a)
  | otherwise = fromIntegral (table^.people) / (fromIntegral n + a)

-- | A dependent measure over tables.
tableMeasure n a newestTable = 
  fromMassFunction (return . tableMass n a newestTable)

-- | A probability measure over restaurants, represented by IntMaps.
restaurantMeasure a restaurant = do
  let numberOfCustomers   = sumOf (traverse.people) restaurant
      numberOfTables      = lengthOf traverse restaurant
      nextTableNum        = succ numberOfTables
      possibleTable       = Table nextTableNum 1
      possibleRestaurant  = IntMap.insert nextTableNum possibleTable restaurant

  table <- tableMeasure numberOfCustomers a possibleTable possibleRestaurant

  let newTable | table^.number == possibleTable^.number = table
               | otherwise                              = table&people %~ succ

  return $ IntMap.insert (newTable^.number) newTable restaurant

-- | The Chinese Restaurant process.  
--
--   This implementation is dismally inefficient as-is, but appears to be
--   correct.  I think I need to look at doing memoization under the hood.
chineseRestaurantProcess n a = go n IntMap.empty
  where go 0 restaurant = return restaurant
        go j restaurant = restaurantMeasure a restaurant >>= go (pred j)

main :: IO ()
main = do
  -- Easily verified by hand.
  meanTinyRestaurant <- expectation (fromIntegral . lengthOf traverse)
                          (chineseRestaurantProcess 2 1)

  -- Easily verified by hand.
  meanBiggerRestaurant <- expectation (fromIntegral . lengthOf traverse)
                            (chineseRestaurantProcess 3 1)

  meanBigRestaurant <- expectation (fromIntegral . lengthOf traverse)
                         (chineseRestaurantProcess 9 1)

  meanBigRestaurantAntisocial <- expectation (fromIntegral . lengthOf traverse)
                                   (chineseRestaurantProcess 9 3)

  -- We can answer other questions by changing our transformation function. 
  -- Trivial example: the expected number of customers for a CRP observed for n
  -- epochs is always n.

  differentQuestionSameMeasure <- 
    expectation (fromIntegral . sumOf (traverse.people)) 
                (chineseRestaurantProcess 9 3)

  print meanTinyRestaurant
  print meanBiggerRestaurant
  print meanBigRestaurant
  print meanBigRestaurantAntisocial
  print differentQuestionSameMeasure

