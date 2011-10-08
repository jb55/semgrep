{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable,
             DeriveFunctor #-}

module Semgrep.Languages.Generic where

import           Data.Generics
import           Semgrep

data BinOp = LeOp
           | GrOp
           | LeqOp
           | GeqOp
           | EqOp
           | NeqOp
           | AddOp
           | UnkOp String
           deriving (Show, Typeable, Data, Eq)

data AssignOp = AssignOp
              | MulAssignOp
              | DivAssignOp
              | UnkAssignOp String
              deriving (Show, Typeable, Data, Eq)

data ConstVal = IntConst Int
              | CharConst Char
              | FloatConst Float
              | StringConst String
           deriving (Show, Typeable, Data, Eq)

data Expr = Var String
          | ConstVal ConstVal
          | BinaryOp BinOp Expr Expr
          | Assign AssignOp Expr Expr
          | ConditionalOp Expr (Maybe Expr) Expr
          | UnkExpr String
          deriving (Show, Typeable, Data)

type AExpr = Annotated NodeInfo Expr

data Ast = Ast [AExpr]


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


exprs :: Ast -> [AExpr]
exprs (Ast exprs) = exprs

conditions :: [AExpr] -> [AExpr]
conditions = filter (strip . fmap isCondExpr)
