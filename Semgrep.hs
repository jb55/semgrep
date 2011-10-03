{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

import Control.Monad
import Data.Maybe
import Language.C
import Language.C.Analysis.AstAnalysis
import Data.Generics
import Debug.Trace
import System.Environment


gtrace a = trace (gshow a) a 


isIfExpr :: CStat -> Bool
isIfExpr (CIf _ _ _ _) = True
isIfExpr _ = False


isCondOp :: CBinaryOp -> Bool
isCondOp CLeOp   = True
isCondOp CGrOp   = True
isCondOp CLeqOp  = True
isCondOp CGeqOp  = True
isCondOp CEqOp   = True
isCondOp CNeqOp  = True
isCondOp _       = False


isCondExpr :: CExpr -> Bool
isCondExpr (CCond _ _ _ _ )   = True
isCondExpr (CBinary op _ _ _) = isCondOp op
isCondExpr _                  = False


isCond :: CStat -> Bool
isCond (CIf expr _ _ _)  = isCondExpr expr
isCond (CExpr expr _ )   = maybe False isCondExpr expr
isCond _                 = False


ifExprs :: CTranslUnit -> [CStat]
ifExprs = listify isIfExpr


conditions :: CTranslUnit -> [CExpr]
conditions = listify isCondExpr


process file = do
  putStrLn $ "file: " ++ file
  stream <- readInputStream file
  let parsedC = parseC stream nopos

  case parsedC of
    Left txt -> print txt
    Right transUnit -> do
      let conds = conditions transUnit
      let ifs = ifExprs transUnit

      putStrLn "\nCondition Expressions"
      mapM_ prettyPrint conds
      putStrLn "\nIf Statements"
      mapM_ prettyPrint ifs

  where
    prettyPrint = putStrLn . show . pretty


main = do
  [file] <- getArgs
  process file
