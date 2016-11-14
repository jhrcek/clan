module Analyze.Parse (parseJavaFile) where

import Control.Exception (ErrorCall, evaluate, catch)
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit)
import System.FilePath (takeFileName)
import Text.Parsec.Error (newErrorMessage, Message(Message), ParseError)
import Text.Parsec.Pos (newPos)

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
