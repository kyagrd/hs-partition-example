{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Lib

import           Control.Monad                     (unless)
import           Data.Graph.Inductive
import           Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph        as Graph
import           Data.Graph.Inductive.NodeMap
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.GraphViz                     as GV
import           Data.List                         (nub, nubBy)
import           Data.Partition
import qualified Data.Partition                    as Partition
import           Data.Set                          (singleton, toList)

{-# ANN module "HLint: ignore Use mappend" #-}
{-# ANN module "HLint: ignore Use fmap" #-}

maxVal :: Int
maxVal = 3

p0 :: Partition Int
p0 = fromDisjointSets (fmap singleton [0..maxVal])

p1 = joinElems 1 3 p0
p2 = joinElems 2 4 p0
p3 = joinElems 2 3 p1
p4 = joinElems 3 4 p2
p5 = joinElems 1 2 p4

type G = Gr (Partition Int) (Int,Int)

emptyG :: G
emptyG = Graph.empty

g0' :: G
g0' = mkGraph [(0,p0)] []

nodeMap0' = fromGraph g0'
-- mmm = new
-- Graph
-- joinElemsG g x1 x2 p =
--  where p' = joinElems x1 x2 p

(g0,nodeMap0,lnode0) = insMapNode new p0 emptyG

(g1,nodeMap1) = (insMapEdge nm (p0,p1,(1,3)) g, nm)
  where (g,nm,_) = insMapNode nodeMap0 p1 g0

(g2,nodeMap2) = (insMapEdge nm (p0,p2,(2,4)) g, nm)
  where (g,nm,_) = insMapNode nodeMap0 p2 g0

(g3,nodeMap3) = (insMapEdge nm (p1,p3,(2,3)) g, nm)
  where (g,nm,_) = insMapNode nodeMap1 p3 g1

(g4,nodeMap4) = (insMapEdge nm (p2,p4,(3,4)) g, nm)
  where (g,nm,_) = insMapNode nodeMap2 p4 g2

(g5,nodeMap5) = (insMapEdge nm (p2,p4,(1,2)) g, nm)
  where (g,nm,_) = insMapNode nodeMap4 p5 g4


-- all join pairs (assuming input list is sorted and uniq)
joinPairs []     = []
joinPairs [_]    = []
joinPairs (x:xs) = [(x,y) | y<-xs] ++ joinPairs xs

type M t = NodeMapM (Partition Int) (Int,Int) Gr t

-- join the partition containging i and the partition continaing j
addJoinFrom :: Partition Int -> (Int,Int) -> M (LNode (Partition Int))
addJoinFrom p (i,j) = do
  ln <- insMapNodeM p'
  insMapEdgeM (p,p',(i,j))
  return ln
  where p' = joinElems i j p

-- generate all possible partitions from p by a single join pair
-- and add them to the graph and nodemap
-- join pairs are generated from representative elem of each partition
addFrom :: Partition Int -> M [LNode (Partition Int)]
addFrom p = do
  lnodes <- mapM (addJoinFrom p) $ joinPairs (nub $ map (rep p) [0..maxVal])
  return $ nubBy (\x y -> fst x == fst y) lnodes

-- repeatedly apply addFrom to newly geneated partitions
-- until there are no more partitions to generate
addFromFix :: Partition Int -> M ()
addFromFix p = do
  lnodes <- addFrom p
  unless (null lnodes) $
    sequence_ [addFromFix p' | (_,p')<- lnodes]

instance GV.Labellable (Int,Int) where
  toLabelValue (i,j) = GV.toLabelValue (show i ++ "=" ++ show j)

instance GV.Labellable (Partition Int) where
  toLabelValue p = GV.toLabelValue (show $  toList <$> Partition.nontrivialSets p)

main :: IO ()
-- main = someFunc
main = do
  print p1
  print p1
  print p2
  print p3
  print p4
  print p5
  -- generate all substititions that equates variables in size 4 set
  -- where the elements are numberd as 0,1,2,3
  let (r,(nmap,g)) = run g0 (addFromFix p0)
  print r
  print nmap
  print g
  GV.preview g
