module Main where

import Data.Either (partitionEithers)
import System.Environment (getArgs)
import System.Exit (die)
import Analyze.AstProc
import Analyze.Parse (parseJavaFile)

main :: IO ()
main = do
    classListFile <- getCmdLineArgs
    classes <- lines <$> readFile classListFile
    (_parseErrors, parsedASTs) <- partitionEithers `fmap` mapM parseJavaFile classes
    --inheritanceHierarchyToDotFile parsedASTs "hierarchy.dot"
    --printTopLevelClasses parsedASTs
    inheritanceHierarchyToJsonFile parsedASTs "hierarchy.json"


getCmdLineArgs :: IO FilePath
getCmdLineArgs = do
    as <- getArgs
    if length as == 1
      then return $ head as
      else die cmdLineMissingArgs

cmdLineMissingArgs :: String
cmdLineMissingArgs = "ERROR: Please provide path to file containing list of classes to analyze as command line argument"
