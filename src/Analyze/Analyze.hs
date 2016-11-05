module Analyze.Analyze
  ( getTopLevelClassNames
  , getExtendsPairs
  ) where

import Data.Maybe (mapMaybe, fromMaybe)
import Language.Java.Syntax
import Data.List (intercalate)

getTopLevelClassNames :: CompilationUnit -> [String]
getTopLevelClassNames (CompilationUnit _ _ typeDecls) = mapMaybe getClassName typeDecls

getClassName :: TypeDecl -> Maybe String
getClassName td = case td of
    (ClassTypeDecl (ClassDecl _ (Ident className) _ _ _ _)) -> Just className

    _ -> Nothing

getExtendsPairs :: CompilationUnit -> [(String, String)]
getExtendsPairs (CompilationUnit _ _ typeDecls) = mapMaybe getExtendsPair typeDecls

getExtendsPair :: TypeDecl -> Maybe (String, String)
getExtendsPair td = case td of
    (ClassTypeDecl (ClassDecl _ (Ident className) _ maybeExtends _ _)) ->
      let superCls = fromMaybe "Object" (maybeExtends >>= classNameFromRefType)
      in Just (className, superCls)

    _ -> Nothing
  where
    classNameFromRefType :: RefType -> Maybe String
    classNameFromRefType (ClassRefType (ClassType identsList)) =
      --identsList looks like: ClassType [(Ident "java",[]),(Ident "lang",[]),(Ident "Object",[])]
      let className = intercalate "." $ map (\(Ident i, _) -> i) identsList
      in Just className
    classNameFromRefType _ = Nothing
