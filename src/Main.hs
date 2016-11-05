import Control.Exception (ErrorCall, evaluate, catch)
import Data.Either (either)
import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit)
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import System.Environment (getArgs)
import System.FilePath (takeFileName)
import System.Exit (die)
import Text.Parsec.Error (newErrorMessage, Message(Message), ParseError)
import Text.Parsec.Pos (newPos)
import Text.Printf (printf)
import Analyze.Analyze

main :: IO ()
main = do
    classListFile <- getCmdLineArgs
    classes <- lines <$> readFile classListFile
    extendsPairs <- mapM processFile classes
    let extendsData = concat extendsPairs
    putStrLn processingFinished
    writeFile "hierarchy.dot" $ toDot extendsData

toDot :: [(String, String)] -> String
toDot exPairs =
  let
    edgeLines = unlines $ map pairToEdge exPairs
    pairToEdge (cls, superCls) = printf "\"%s\"->\"%s\"" cls superCls
  in  "digraph G {\noverlap=false\nrankdir=BT\n" ++ edgeLines ++ "}"

getCmdLineArgs :: IO FilePath
getCmdLineArgs = do
    as <- getArgs
    if length as == 1
      then return $ head as
      else die cmdLineMissingArgs

processFile :: FilePath -> IO [(String, String)]
processFile javaSourceFile = do
    putStrLn $ "Processing " ++ takeFileName javaSourceFile
    sourceStr <- T.unpack <$> TO.readFile javaSourceFile
    parseResult <- evaluate (parser compilationUnit sourceStr) `catch` alexLexicalErrorHandler --evaluate to force potential error
    either printError processAST parseResult
  where
    printError parseErr = return [] --putStrLn $ "ERROR processing " ++ javaSourceFile ++ "\n" ++ (unwords . take 3 .lines . show) parseErr

    processAST = return . getExtendsPairs

    alexLexicalErrorHandler :: ErrorCall -> IO (Either ParseError CompilationUnit)
    alexLexicalErrorHandler ex = -- wrap lexical error as parse error
      let parseError = newErrorMessage (Message . head . lines $ show ex) (newPos "" 1 1)
      in return $ Left parseError

processingFinished, cmdLineMissingArgs :: String
processingFinished = unlines
  [ "----- SOURCE PROCESSING FINISHED -----"
  , "Data written to file graph.dot"
  , "To render it you'll need graphviz package installed"
  , "You can render it using: tred hierarchy.dot | dot  -Tsvg -o hierarchy.svg"
  ]

cmdLineMissingArgs = "ERROR: Please provide path to file containing list of classes to analyze as command line argument"
