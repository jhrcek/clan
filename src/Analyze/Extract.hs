module Analyze.Extract
  ( getTopLevelClasses
  , getExtendsPairs
  , Class_ (C)
  ) where

import Data.Maybe (mapMaybe, maybe, fromMaybe)
import Language.Java.Syntax
import Data.List (intercalate)

--            C package simpleClassName
data Class_ = C String String deriving Show

getTopLevelClasses :: CompilationUnit -> [Class_]
getTopLevelClasses (CompilationUnit mPackageDecl _ typeDecls) =
    C pkg <$> mapMaybe getClassName typeDecls
  where
    pkg = maybe "" getPackage mPackageDecl

getPackage :: PackageDecl -> String
getPackage (PackageDecl (Name identList)) = intercalate "." $ map identToString identList

identToString :: Ident -> String
identToString (Ident str) = str

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
    classNameFromRefType rt = case rt of
        (ClassRefType (ClassType identsList)) ->
        --identsList looks like: ClassType [(Ident "java",[]),(Ident "lang",[]),(Ident "Object",[])]
            let className = intercalate "." $ map (\(Ident i, _) -> i) identsList
            in Just className
        _ -> Nothing
