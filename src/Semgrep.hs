{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable, DeriveFunctor #-}


import           Control.Monad
import           Control.Applicative
import           Data.Maybe
import           Text.PrettyPrint.HughesPJ
import qualified Language.C as C
import qualified Language.Python.Version2 as P
import           Data.Generics
import           Debug.Trace
import           System.Environment

import           Semgrep.Languages.Generic

data Annotated a b = Annotated a b
                   deriving (Show, Typeable, Data, Functor)

class GenExpr a where
  toExpr :: a -> Expr

type AExpr = Annotated Expr NodeInfo

data Position = Position
              { posOffset :: Maybe Int
              , posFilename :: Maybe String
              , posLineNumber :: Maybe Int
              , posColumnNumber :: Maybe Int
              } deriving (Show)


data NodeInfo = NodeInfo Position (Maybe String)
instance Show NodeInfo where
  show (NodeInfo p s) =
    "NodeInfo " ++ show p ++ " \"" ++ (maybe "??" id s) ++ "\""



conditions :: [AExpr] -> [AExpr]
conditions = filter (isCondExpr . toExpr)


process file = do
  putStrLn $ "file: " ++ file
  stream <- C.readInputStream file
  let parsedC = C.parseC stream (C.initPos file)

  case parsedC of
    Left txt -> print txt
    Right transUnit -> do
      let all   = allCExprs transUnit
      let convertedExprs = map fromCExpr' all
      let conds = conditions convertedExprs
      let ifs   = ifStmts transUnit

      putStrLn "\nAll Expressions"
      mapM_ prettyPrint all
      putStrLn "\nConditional Expressions"
      mapM_ print conds
      putStrLn "\nIf Statements"
      mapM_ prettyPrint ifs

  where
    prettyPrint = print . pretty

    allCExprs :: C.CTranslUnit -> [C.CExpr]
    allCExprs = listify (const True)

main = do
  [file] <- getArgs
  process file
