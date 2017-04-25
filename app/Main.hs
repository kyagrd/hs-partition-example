module Main where

import           Lib

import           Data.Partition
import           Data.Set       (singleton)

p :: Partition Int
p = fromDisjointSets (fmap singleton [1..10])

q1 = joinElems 4 9 p
p1 = joinElems 2 3 p
p2 = joinElems 8 4 p1
p3 = joinElems 2 4 p2

main :: IO ()
-- main = someFunc
main = do
  print q1
  print p1
  print p2
  print p3
