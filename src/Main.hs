module Main where

import Control.Exception (ErrorCall, evaluate, catch)
import Data.Either (partitionEithers)
import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit)
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import System.Environment (getArgs)
import System.FilePath (takeFileName)
import System.Exit (die)
import Text.Parsec.Error (newErrorMessage, Message(Message), ParseError)
import Text.Parsec.Pos (newPos)

import Analyze.AstProc

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

parseJavaFile :: FilePath -> IO (Either ParseError CompilationUnit)
parseJavaFile javaSourceFile = do
    putStrLn $ "Processing " ++ takeFileName javaSourceFile
    sourceStr <- T.unpack <$> TO.readFile javaSourceFile
    parseResult <- evaluate (parser compilationUnit sourceStr) `catch` alexLexicalErrorHandler --evaluate to force potential error
    either (\e -> putStrLn $ "Error processing "++ javaSourceFile ++ show e) (const $ return ()) parseResult
    return parseResult
  where
    alexLexicalErrorHandler :: ErrorCall -> IO (Either ParseError CompilationUnit)
    alexLexicalErrorHandler ex = -- wrap lexical error as parse error
      let parseError = newErrorMessage (Message . head . lines $ show ex) (newPos "" 1 1)
      in return $ Left parseError

cmdLineMissingArgs :: String
cmdLineMissingArgs = "ERROR: Please provide path to file containing list of classes to analyze as command line argument"
