module Semgrep.Languages.Generic where

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

data Ast a = Ast a

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

