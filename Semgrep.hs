{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable #-}

import           Control.Monad
import           Data.Maybe
import           Language.C
import           Language.C.Data.Ident
import           Language.C.Analysis.AstAnalysis
import qualified Language.Python.Version2 as P
import           Data.Generics
import           Debug.Trace
import           System.Environment

data BinOp = LeOp
           | GrOp
           | LeqOp
           | GeqOp
           | EqOp
           | NeqOp
           | UnkOp String
           deriving (Show, Typeable, Data, Eq)

data Const = IntConst Int
           | CharConst Char
           | FloatConst Float
           | StringConst String
           deriving (Show, Typeable, Data, Eq)

data Expr = BinaryOp BinOp Expr Expr
          | Var String
          | Const Const
          | ConditionalOp Expr (Maybe Expr) Expr
          | UnkExpr String
          deriving (Show, Typeable, Data)


gtrace a = trace (gshow a) a 

fromCBinOp :: CBinaryOp -> BinOp
fromCBinOp CLeOp  = LeOp
fromCBinOp CGrOp  = GrOp
fromCBinOp CLeqOp = LeqOp
fromCBinOp CGeqOp = GeqOp
fromCBinOp CEqOp  = EqOp
fromCBinOp CNeqOp = NeqOp
fromCBinOp o      = UnkOp (gshow o)

fromCConst :: CConst -> Const
fromCConst (CIntConst cint _) = IntConst (fromInteger $ getCInteger cint)
fromCConst (CCharConst cchar _) = CharConst (head $ getCChar cchar)
fromCConst (CFloatConst (CFloat s) _) = FloatConst (read s)
fromCConst (CStrConst cstring _) = StringConst (getCString cstring)

fromCExpr :: CExpr -> Expr
fromCExpr (CVar (Ident s _ _) _) = Var s
fromCExpr (CConst c)             = Const (fromCConst c)
fromCExpr (CCond e1 e2 e3 _) = ConditionalOp (fromCExpr e1) 
                                             (fmap fromCExpr e2) 
                                             (fromCExpr e3)

fromCExpr (CBinary op e1 e2 _) = BinaryOp (fromCBinOp op) 
                                          (fromCExpr e1)
                                          (fromCExpr e2)
fromCExpr e = UnkExpr (show e)


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


ifStmts :: CTranslUnit -> [CStat]
ifStmts = listify isIfExpr


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
      let all   = allExprs transUnit
      let ifs   = ifStmts transUnit
      let convertedExprs = map fromCExpr all

      putStrLn "\nCondition Expressions"
      mapM_ prettyPrint conds
      putStrLn "\nIf Statements"
      mapM_ prettyPrint ifs
      putStrLn "\nConverted Expressions"
      mapM_ print convertedExprs

  where
    prettyPrint = putStrLn . show . pretty

    allExprs :: CTranslUnit -> [CExpr]
    allExprs = listify (const True)


main = do
  [file] <- getArgs
  process file
