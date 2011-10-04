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

data AssignOp = AssignOp
              | MulAssignOp
              | DivAssignOp
              | UnkAssignOp String
              deriving (Show, Typeable, Data, Eq)

data Const = IntConst Int
           | CharConst Char
           | FloatConst Float
           | StringConst String
           deriving (Show, Typeable, Data, Eq)

data Expr = BinaryOp BinOp Expr Expr
          | Var String
          | Assign AssignOp Expr Expr
          | Const Const
          | ConditionalOp Expr (Maybe Expr) Expr
          | UnkExpr String
          deriving (Show, Typeable, Data)

gtrace a = trace (gshow a) a

fromCAssignOp :: CAssignOp -> AssignOp
fromCAssignOp CAssignOp = AssignOp
fromCAssignOp CMulAssOp = MulAssignOp
fromCAssignOp CDivAssOp = DivAssignOp
fromCAssignOp o = UnkAssignOp (show o)


fromCBinOp :: CBinaryOp -> BinOp
fromCBinOp CLeOp  = LeOp
fromCBinOp CGrOp  = GrOp
fromCBinOp CLeqOp = LeqOp
fromCBinOp CGeqOp = GeqOp
fromCBinOp CEqOp  = EqOp
fromCBinOp CNeqOp = NeqOp
fromCBinOp o      = UnkOp (gshow o)


fromCConst :: CConst -> Const
fromCConst (CIntConst cint _)         = IntConst (fromInteger $ getCInteger cint)
fromCConst (CCharConst cchar _)       = CharConst (head $ getCChar cchar)
fromCConst (CFloatConst (CFloat s) _) = FloatConst (read s)
fromCConst (CStrConst cstring _)      = StringConst (getCString cstring)


fromCExpr :: CExpr -> Expr
fromCExpr (CVar ident _)       = Var (identToString ident)
fromCExpr (CAssign op e1 e2 _) = Assign (fromCAssignOp op)
                                        (fromCExpr e1)
                                        (fromCExpr e2)
fromCExpr (CConst c)           = Const (fromCConst c)
fromCExpr (CCond e1 e2 e3 _)   = ConditionalOp (fromCExpr e1)
                                               (fmap fromCExpr e2)
                                               (fromCExpr e3)
fromCExpr (CBinary op e1 e2 _) = BinaryOp (fromCBinOp op)
                                          (fromCExpr e1)
                                          (fromCExpr e2)
fromCExpr e = UnkExpr (show e)


isIfExpr :: CStat -> Bool
isIfExpr (CIf _ _ _ _) = True
isIfExpr _ = False


isCondOp :: BinOp -> Bool
isCondOp LeOp   = True
isCondOp GrOp   = True
isCondOp LeqOp  = True
isCondOp GeqOp  = True
isCondOp EqOp   = True
isCondOp NeqOp  = True
isCondOp _      = False


isCondExpr :: Expr -> Bool
isCondExpr (ConditionalOp op _ _ ) = True
isCondExpr (BinaryOp op _ _ )      = isCondOp op
isCondExpr _                       = False



ifStmts :: CTranslUnit -> [CStat]
ifStmts = listify isIfExpr


conditions :: [Expr] -> [Expr]
conditions = filter isCondExpr


process file = do
  putStrLn $ "file: " ++ file
  stream <- readInputStream file
  let parsedC = parseC stream (initPos file)

  case parsedC of
    Left txt -> print txt
    Right transUnit -> do
      let all   = allCExprs transUnit
      let convertedExprs = map fromCExpr all
      let conds = conditions convertedExprs
      let ifs   = ifStmts transUnit

      putStrLn "\nAll Expressions"
      mapM_ prettyPrint all
      putStrLn "\nConditional Expressions"
      mapM_ print conds
      putStrLn "\nIf Statements"
      mapM_ prettyPrint ifs

  where
    prettyPrint = putStrLn . show . pretty

    allCExprs :: CTranslUnit -> [CExpr]
    allCExprs = listify (const True)

main = do
  [file] <- getArgs
  process file
