{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Lib
    ( doPackage
    ) where

import Prelude hiding (length)

import Data.Maybe (mapMaybe)
import qualified Data.List as L

type Length = Int
type Width = Int
type Height = Int
type Name = String

data Item = Item Name Length Width Height
  deriving (Eq, Show)

data Box
  = Small [Item]
  | Medium [Item]
  | Large [Item]
  deriving (Eq, Show)

newtype Package = Package [Box]
  deriving (Eq, Show)

class Dimensions a where
  length :: a -> Int
  width :: a -> Int
  height :: a -> Int
  volume :: a -> Int

class HasPermutations a where
  permutations :: a -> [a]


instance Dimensions Item where
  length  (Item _ l _ _) = l
  width   (Item _ _ w _) = w
  height  (Item _ _ _ h) = h
  volume  (Item _ l w h) = l * w * h 

instance HasPermutations Item where
  permutations (Item n l w h) =
    [ Item n l w h
    , Item n w l h
    , Item n w h l
    , Item n h w l
    , Item n h l w
    , Item n l h w
    ]

instance HasPermutations [Item] where
  permutations :: [Item] -> [[Item]]
  permutations [] = [[]]
  permutations items = mconcat $ zipWith (curry handle) [0..] items
    where
      handle (index, item) = do
        let permutedItems = permutations item
            permutedRemaining = permutations $ deleteAt index items
        do
          i <- permutedItems
          a <- permutedRemaining
          [i : a]

      deleteAt index xs = left ++ right
        where (left, _:right) = splitAt index xs

instance HasPermutations Box where
  permutations box =
    [ box
    , Small []
    , Medium []
    , Large []
    ]

instance Dimensions Package where
  length  (Package boxes) = sum $ map length boxes
  width   (Package boxes) = sum $ map width boxes
  height  (Package boxes) = sum $ map height boxes
  volume  (Package boxes) = sum $ map volume boxes

instance Dimensions Box where
  length box = case box of
                Small _   -> 10
                Medium _  -> 20
                Large _   -> 30

  width box = case box of
                Small _   -> 10
                Medium _  -> 20
                Large _   -> 30

  height box = case box of
                Small _   -> 10
                Medium _  -> 20
                Large _   -> 30
  volume box = case box of
                Small _   -> 1000
                Medium _  -> 8000
                Large _   -> 27000


_items :: Box -> [Item]
_items (Small i) = i
_items (Medium i) = i
_items (Large i) = i


small :: Box
small = Small []

medium :: Box
medium = Medium []

large :: Box
large = Large []


canFit :: Item -> Box -> Bool
canFit item box = do
  let items = _items box
      remainingLength = length box - sum (map length items)
      remainingWidth  = width box - sum (map width items)
      remainingHeight = height box - sum (map height items)
  remainingLength >= length item && remainingWidth >= width item && remainingHeight >= height item

addItemToBox :: Item -> Box -> Maybe Box
addItemToBox i b | not $ canFit i b = Nothing
addItemToBox i (Small is) = Just $ Small (i : is)
addItemToBox i (Medium is) = Just $ Medium (i : is)
addItemToBox i (Large is) = Just $ Large (i : is)


package :: [Item] -> Package
package [] = Package []
package items = do
  let itemPermutations = permutations items
      solutions = map (`go` []) itemPermutations
  bestPackage items solutions
  where
    go :: [Item] -> [Box] -> Package
    go [] boxes = Package boxes
    go (i:items') boxes = do
      let boxesToTry      = mapMaybe (addItemToBox i) [small, medium, large]
          newBoxSolutions = map (\box -> go items' (box : boxes)) boxesToTry
          solutions       = firstBoxSolution <> newBoxSolutions
      bestPackage items solutions
      where
        firstBoxSolution = case boxes of
          []    -> []
          (b:tail) -> foldMap (\box -> [ go items' (box:tail) ]) (addItemToBox i b)


bestPackage :: [Item] -> [Package] -> Package
bestPackage items packages = do
  let initialPackage = Package $ mapMaybe (`addItemToBox` large) items
  foldl compare initialPackage packages
  where
    compare :: Package -> Package -> Package
    compare bestSolution@(Package bestBoxes) thisSolution@(Package thisBoxes) = do
      let volumeBest = volume bestSolution
          volumeThis = volume thisSolution
          boxCountBest = L.length bestBoxes
          boxCountThis = L.length thisBoxes
      if volumeBest < volumeThis
        then bestSolution
        else if volumeBest > volumeThis
            then thisSolution
            else if boxCountBest < boxCountThis
              then bestSolution
              else thisSolution

doPackage :: IO ()
doPackage = do
  let item1 = Item "one" 1 1 1
      item2 = Item "two" 2 2 2
      item3 = Item "three" 4 4 4
      item4 = Item "four" 8 8 8 
      item5 = Item "five" 16 16 16
      item6 = Item "six" 30 30 30
      item7 = Item "seven" 1 2 3
      item8 = Item "eight" 1 2 3
      item9 = Item "nine" 1 2 3
      item10 = Item "ten" 1 2 3
      items =
        [ item1
        , item2
        , item3
        , item4
        , item5
        , item6
        --, item7
        --, item8
        --, item9
        --, item10
        ]

  print $ package items

