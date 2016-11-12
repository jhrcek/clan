module Analyze.AstProc where
  -- ( printTopLevelClasses
  -- , inheritanceHierarchyToDotFile
  -- , inheritanceHierarchyToTree
  -- ) where

import Data.List (intercalate)
import Data.Tree (Forest, Tree(Node))
import Data.Tuple (swap)
import Language.Java.Syntax (CompilationUnit)
import Text.Printf (printf)

import Analyze.Extract
import Analyze.TreeBuilder

  -- Useless, just for parser testing - write out FQNs of parsed classes
printTopLevelClasses :: [CompilationUnit] -> IO ()
printTopLevelClasses parsedASTs = mapM_ print fqns
    where fqns = concatMap getTopLevelClasses parsedASTs


inheritanceHierarchyToDotFile :: [CompilationUnit] -> FilePath -> IO ()
inheritanceHierarchyToDotFile parsedASTs outFile = do
    writeFile outFile $ toDot extendsPairs
    putStrLn info
  where
    extendsPairs = concatMap getExtendsPairs parsedASTs

    info = unlines
      [ "----- PROCESSING FINISHED -----"
      , "Data written to file graph.dot"
      , "To render it you'll need graphviz package installed"
      , "You can render it using: tred hierarchy.dot | dot  -Tsvg -o hierarchy.svg"
      ]

toDot :: [(String, String)] -> String
toDot exPairs = "digraph G {\ngraph[overlap=false,rankdir=BT];\n" ++ edgeLines ++ "}"
  where
    edgeLines = unlines $ map pairToEdge exPairs
    pairToEdge (cls, superCls) = printf "\"%s\"->\"%s\"" cls superCls


inheritanceHierarchyToTree :: [CompilationUnit] -> Forest String
inheritanceHierarchyToTree parsedASTs =
    let extendsPairs = map swap $ concatMap getExtendsPairs parsedASTs
    in buildForest extendsPairs

treeToJson :: Tree String -> String
treeToJson (Node r cs) = "{\"n\":\"" ++ r  ++"\",\"c\":" ++ forestToJson cs ++ "}"

forestToJson :: Forest String -> String
forestToJson trees = "[" ++ intercalate "," (map treeToJson trees) ++"]"

getInheritanceHierarchyAsJson :: [CompilationUnit] -> String
getInheritanceHierarchyAsJson = treeToJson . head . inheritanceHierarchyToTree
