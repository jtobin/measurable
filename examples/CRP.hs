{-# LANGUAGE TemplateHaskell #-}

-- | An example defining measures for various finite realizations of a Chinese
--   Restaurant Process.

import Control.Applicative
import Control.Lens
import Control.Monad.Trans
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Measurable.Core

data Table = Table {
    _number :: {-# UNPACK #-} !Int
  , _people :: {-# UNPACK #-} !Int
  } deriving (Eq, Show)

instance Ord Table where
  t1 <  t2 = _people t1 < _people t2
  t1 <= t2 = _people t1 <= _people t2

makeLenses ''Table

-- | Mass function for a given table.  It's dependent on the state of the
--   restaurant via @n@ and @newestTable@.
tableMass :: (Fractional a, Integral b) => b -> a -> Table -> Table -> a
tableMass n a newestTable table
  | table^.number == newestTable^.number = a / (fromIntegral n + a)
  | otherwise = fromIntegral (table^.people) / (fromIntegral n + a)

-- | A measure defined over tables.
tableMeasure
  :: (Integral b, Applicative m, Monad m, Traversable t)
  => b
  -> Double
  -> Table
  -> t Table
  -> MeasureT m Table
tableMeasure n a newestTable =
  fromDensityCountingT (tableMass n a newestTable)

-- | A probability measure over restaurants, represented by IntMaps.
restaurantMeasure
  :: (Monad m, Applicative m)
  => Double
  -> IntMap Table
  -> MeasureT m (IntMap Table)
restaurantMeasure a restaurant = do
  let numCustomers  = sumOf (traverse.people) restaurant
      numTables     = lengthOf traverse restaurant
      nextTableNum  = succ numTables
      possibleTable = Table nextTableNum 1
      possibleRestaurant =
        IntMap.insert nextTableNum possibleTable restaurant

  table <- tableMeasure numCustomers a
             possibleTable possibleRestaurant

  let newTable
        | table^.number == possibleTable^.number = table
        | otherwise = table&people %~ succ

  return (IntMap.insert (newTable^.number) newTable restaurant)

-- | A measure for a finite realization of a CRP measure with a given number of
--   customers and concentration parameter.
chineseRestaurantProcess
  :: (Enum a, Eq a, Monad m, Applicative m, Num a)
  => a
  -> Double
  -> MeasureT m (IntMap Table)
chineseRestaurantProcess n a = go n IntMap.empty where
  go 0 restaurant = return restaurant
  go j restaurant = restaurantMeasure a restaurant >>= go (pred j)

main :: IO ()
main = do
  let numTables = fromIntegral . lengthOf traverse
      tinyRestaurant  = chineseRestaurantProcess 2 1
      smallRestaurant = chineseRestaurantProcess 3 1
      bigRestaurant   = chineseRestaurantProcess 9 1
      bigRestaurantAntisocial = chineseRestaurantProcess 9 3

  meanTinyRestaurant  <- integrate numTables tinyRestaurant
  meanSmallRestaurant <- integrate numTables smallRestaurant
  meanBigRestaurant   <- integrate numTables bigRestaurant
  meanBigRestaurantAntisocial <- integrate numTables bigRestaurantAntisocial

  let numCustomers = fromIntegral . sumOf (traverse.people)

  differentQuestion <- integrate numCustomers bigRestaurantAntisocial

  print meanTinyRestaurant
  print meanSmallRestaurant
  print meanBigRestaurant
  print meanBigRestaurantAntisocial
  print differentQuestion

