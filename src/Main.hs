import Control.Exception (ErrorCall, evaluate, catch)
import Data.Either (either)
import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit)
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import System.Environment (getArgs)
import System.Exit (die)
import Text.Parsec.Error (newErrorMessage, Message(Message), ParseError)
import Text.Parsec.Pos (newPos)

main :: IO ()
main = do
    classListFile <- getCmdLineArgs
    classes <- lines <$> readFile classListFile
    mapM_ processFile classes

getCmdLineArgs :: IO FilePath
getCmdLineArgs = do
    as <- getArgs
    if length as == 1
      then return $ head as
      else die "ERROR: Please provide path to file containing list of classes to analyze as command line argument"

processFile :: FilePath -> IO ()
processFile javaSourceFile = do
    sourceStr <- T.unpack <$> TO.readFile javaSourceFile
    parseResult <- evaluate (parser compilationUnit sourceStr) `catch` alexLexicalErrorHandler --evaluate to force potential error
    either printError processAST parseResult
  where
    printError parseErr = putStrLn $ "ERROR processing " ++ javaSourceFile ++ "\n" ++ (unwords . take 3 .lines . show) parseErr

    processAST _ = return () --TODO

    alexLexicalErrorHandler :: ErrorCall -> IO (Either ParseError CompilationUnit)
    alexLexicalErrorHandler ex = -- wrap lexical error as parse error
      let parseError = newErrorMessage (Message . head . lines $ show ex) (newPos "" 1 1)
      in return $ Left parseError
