{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable,
             DeriveFunctor, ViewPatterns, FlexibleInstances #-}

module Semgrep.Languages.Generic where

import           Data.Generics
import           Semgrep


data Position = Position
              { posOffset :: Maybe Int
              , posFilename :: Maybe String
              , posLineNumber :: Maybe Int
              , posColumnNumber :: Maybe Int
              }
              deriving (Data, Typeable)

try' :: (Show a) => Maybe a -> String
try' = maybe "??" show

try :: Maybe String -> String -> String
try m s = maybe s id m

instance Show Position where
  show (Position o f l c) =
    try f "Unknown File" ++ ": (" ++ try' l ++ ", " ++ try' c ++ ")"

instance Show NodeInfo where
  show (NodeInfo p s) =
    "NodeInfo " ++ show p ++ maybe "" (\x -> " \"" ++ x ++ "\"") s

data NodeInfo = NodeInfo (Maybe Position) (Maybe String)
              deriving (Data, Typeable)

class MaybeInfo a where
  info :: a -> Maybe NodeInfo

class MaybePos a where
  posOf :: a -> Maybe Position

instance MaybeInfo NodeInfo where
  info = Just

instance MaybePos NodeInfo where
  posOf (NodeInfo p _) = p

instance MaybePos (Maybe NodeInfo) where
  posOf (Just ni) = posOf ni
  posOf Nothing   = Nothing

instance MaybeInfo (Maybe NodeInfo) where
  info (Just ni) = info ni
  info Nothing   = Nothing

type Annotation = Maybe NodeInfo


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

data Expr = Var String Annotation
          | ConstVal ConstVal Annotation
          | BinaryOp BinOp Expr Expr Annotation
          | Assign AssignOp Expr Expr Annotation
          | ConditionalOp Expr (Maybe Expr) Expr Annotation
          | UnkExpr String Annotation
       -- | CaseExpr
          deriving (Show, Typeable, Data)

data Type = Type String
          deriving (Show, Typeable, Data)

data Stmt = ExprStmt (Maybe Expr) Annotation
          | Label String Stmt Annotation
          | DeclStmt Decl
          | CaseStmt Expr Stmt Annotation
          | CaseStmtDefault Stmt Annotation
          | IfStmt Expr Stmt (Maybe Stmt) Annotation
          | SwitchStmt Expr Stmt Annotation
          | CompoundStmts [Stmt] Annotation
          | UnkStmt String Annotation
          deriving (Show, Typeable, Data)


data Decl = Class [Decl] Annotation
          | DataDecl (Maybe Type) Annotation
          | Function (Maybe String) Stmt Annotation
          | UnkDecl String Annotation
          deriving (Show, Typeable, Data)

data Module = Module [Decl] Annotation
            deriving (Show, Typeable, Data)

data Project = Project [Module]
             deriving (Show, Typeable, Data)

instance MaybeInfo Expr where
  info (Var _ n)            = n
  info (ConstVal _ n)       = n
  info (BinaryOp _ _ _ n)   = n
  info (Assign _ _ _ n)     = n
  info (ConditionalOp _ _ _ n) = n
  info (UnkExpr _ n)           = n

instance MaybeInfo Decl where
  info (Class _ n)    = n
  info (DataDecl _ n) = n
  info (Function _ _ n) = n
  info (UnkDecl _ n) = n

instance MaybeInfo Module where
  info (Module _ n) = n

type LanguageParser = FilePath -> IO (Either String Project)

isCondOp :: BinOp -> Bool
isCondOp LeOp   = True
isCondOp GrOp   = True
isCondOp LeqOp  = True
isCondOp GeqOp  = True
isCondOp EqOp   = True
isCondOp NeqOp  = True
isCondOp _      = False


isCondExpr :: Expr -> Bool
isCondExpr (ConditionalOp op _ _ _) = True
isCondExpr (BinaryOp op _ _ _)      = isCondOp op
isCondExpr _                       = False


exprs :: Project -> [Expr]
exprs = listify (const True)

conditions :: [Expr] -> [Expr]
conditions = filter isCondExpr
