module Analyze.TreeBuilder (
  buildForest, NodeMap
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree
import Data.List


type NodeMap a = Map.Map a (Set.Set a)

buildForest :: Ord a => [(a,a)] -> Forest a
buildForest xs = map (mkTree nodeMap) roots
  where
    roots = getRoots xs
    nodeMap = pairsToMap xs

pairsToMap :: Ord a => [(a,a)] -> NodeMap a
pairsToMap = Map.fromListWith Set.union . map (\(x,y)-> (x, Set.singleton y))

mkTree :: Ord a => NodeMap a -> a -> Tree a
mkTree nm x = Node x (mkForest nm subtrees)
  where subtrees = maybe [] Set.toList $ Map.lookup x nm

mkForest :: Ord a => NodeMap a -> [a] -> Forest a
mkForest nm = map (mkTree nm)

getRoots :: Eq a => [(a,a)] -> [a]
getRoots xs = nub froms \\ tos
  where (froms, tos) = unzip xs
