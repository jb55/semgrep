{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable,
             DeriveFunctor, ViewPatterns, FlexibleInstances #-}

module Semgrep.Languages.Generic where

import           Data.Generics
import           Data.Maybe(isJust)
import           Control.Monad(foldM)
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

addParens :: (a -> String) -> Maybe a -> String
addParens f = maybe "" (\x -> " (" ++ f x ++ ")")

addParens' :: Maybe String -> String
addParens' = addParens id

instance Show Position where
  show (Position o f l c) =
    try f "Unknown File" ++ ": (" ++ try' l ++ ", " ++ try' c ++ ")"

instance Show NodeInfo where
  show (NodeInfo p s) =
    "NodeInfo " ++ try' p ++ maybe "" (\x -> " \"" ++ x ++ "\"") s

data NodeInfo = NodeInfo (Maybe Position) (Maybe String)
              deriving (Data, Typeable)

class Named a where
  name :: a -> String

class MaybeInfo a where
  info :: a -> Maybe NodeInfo

class Unknown a where
  unk :: a -> Maybe String

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

data AssignOp = DefaultAssign
              | DivAssign
              | MulAssign
              | PlusAssign
              | MinusAssign
              | ModAssign
              | PowAssign
              | BinAndAssign
              | BinOrAssign
              | BinXorAssign
              | LeftShiftAssign
              | RightShiftAssign
              | FloorDivAssign
              | UnkAssign String
              deriving (Show, Typeable, Data, Eq)

data LiteralValue = IntLiteral Int
              | CharLiteral Char
              | FloatLiteral Float
              | StringLiteral String
              deriving (Show, Typeable, Data, Eq)

data Expr = Var String Annotation
          | CompoundExpr [Expr] Annotation
          | LiteralValue LiteralValue (Maybe String) Annotation
          | BinaryOp BinOp Expr Expr Annotation
          | Assign AssignOp Expr Expr Annotation
          | DestructuringAssign [Expr] Expr Annotation
          | ConditionalOp Expr (Maybe Expr) Expr Annotation
          | FunApp Expr [Expr] Annotation
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
          | Block [Stmt] Annotation
          | UnkStmt String Annotation
          deriving (Show, Typeable, Data)

data DeclProp = Result Expr
              deriving (Show, Typeable, Data)

data Decl = Class [Decl] Annotation
          | DataDecl (Maybe Type) Annotation
          | Function [DeclProp] (Maybe String) Stmt Annotation
          | UnkDecl String Annotation
          deriving (Show, Typeable, Data)

data Module = Module [Stmt] Annotation
            deriving (Show, Typeable, Data)

data Project = Project [Module]
             deriving (Show, Typeable, Data)

instance Unknown Expr where
  unk (UnkExpr s _) = Just s
  unk _             = Nothing

instance Unknown Decl where
  unk (UnkDecl s _) = Just s
  unk _             = Nothing

instance Unknown Stmt where
  unk (DeclStmt decl)      = unk decl
  unk (UnkStmt s _)        = Just s
  unk _                    = Nothing

instance Named LiteralValue where
  name (IntLiteral {})       = "Integer"
  name (CharLiteral {})      = "Char"
  name (FloatLiteral {})     = "Float"
  name (StringLiteral {})    = "String"
  name _                     = "Unknown"

instance Named Expr where
  name (Var {})            = "Variable"
  name (CompoundExpr {})   = "Compound Expression"
  name (LiteralValue c _ _ )   = "Constant Value (" ++ name c ++ ")"
  name (BinaryOp {})       = "Binary Operator"
  name (Assign {})         = "Assignment"
  name (DestructuringAssign {}) = "DestructuringAssignment"
  name (ConditionalOp {})  = "Conditional Operator"
  name (FunApp {})         = "Function Call"
  name (UnkExpr {})        = "Unknown Expression"

instance Named Decl where
  name (Class {})          = "Class"
  name (DataDecl {})       = "Data"
  name (Function {})       = "Function"
  name (UnkDecl {})        = "Unknown Declaration"

instance Named Stmt where
  name (ExprStmt mExpr _)   = "Expression Statement" ++ addParens name mExpr
  name (Label {})           = "Label"
  name (DeclStmt decl)      = "Declaration Statement (" ++ name decl ++ ")"
  name (CaseStmt {})        = "Case"
  name (CaseStmtDefault {}) = "Default Case"
  name (IfStmt {})          = "If"
  name (SwitchStmt {})      = "Switch"
  name (Block {})           = "Block"
  name (UnkStmt {})         = "Unknown Statement"

instance MaybeInfo Stmt where
  info (ExprStmt _ n)       = n
  info (Label _ _ n)        = n
  info (DeclStmt decl)      = info decl
  info (CaseStmt _ _ n)     = n
  info (CaseStmtDefault _ n) = n
  info (IfStmt _ _ _ n)     = n
  info (SwitchStmt _ _ n)   = n
  info (Block _ n)  = n
  info (UnkStmt _ n)        = n

instance MaybeInfo Expr where
  info (Var _ n)            = n
  info (CompoundExpr _ n)   = n
  info (LiteralValue _ _ n)     = n
  info (BinaryOp _ _ _ n)   = n
  info (Assign _ _ _ n)     = n
  info (ConditionalOp _ _ _ n) = n
  info (FunApp _ _ n)       = n
  info (UnkExpr _ n)        = n

instance MaybeInfo Decl where
  info (Class _ n)    = n
  info (DataDecl _ n) = n
  info (Function _ _ _ n) = n
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
isCondExpr _                        = False

isStringLit :: Expr -> Bool
isStringLit (LiteralValue (StringLiteral {}) _ _) = True
isStringLit _ = False

stringLit :: Expr -> Maybe String
stringLit (LiteralValue (StringLiteral s) _ _) = Just s
stringLit _ = Nothing

fromCompoundedStrings :: Expr -> Maybe Expr
fromCompoundedStrings (CompoundExpr [] _)  = Nothing
fromCompoundedStrings (CompoundExpr [a] _) = Just a
fromCompoundedStrings (CompoundExpr (e1:es) _) = foldM joinTwo e1 es
  where
    joinTwo :: Expr -> Expr -> Maybe Expr
    joinTwo l1@(LiteralValue s1 m1 a1) l2@(LiteralValue s2 m2 a2) = do
      str1 <- stringLit l1
      str2 <- stringLit l2
      return $ LiteralValue (StringLiteral $ str1 ++ str2) m1 a1
    joinTwo _ _ = Nothing


isFunctionCall :: Expr -> Bool
isFunctionCall (FunApp {}) = True
isFunctionCall _           = False

isCompound :: Stmt -> Bool
isCompound (Block {}) = True
isCompound _          = False

isExpressionStmt :: Stmt -> Bool
isExpressionStmt (ExprStmt {}) = True
isExpressionStmt _             = False

isDullStmt :: Stmt -> Bool
isDullStmt stmt = or $ map ($stmt) dulls
  where dulls = [isExpressionStmt, isCompound]

statements :: Project -> [Stmt]
statements = listify (const True)

expressions :: Project -> [Expr]
expressions = listify (const True)

calls :: [Expr] -> [Expr]
calls = filter isFunctionCall

conditions :: [Expr] -> [Expr]
conditions = filter isCondExpr

isUnk = isJust . unk

infos :: (MaybeInfo a) => [a] -> [NodeInfo]
infos ms = [a | Just a <- map info ms]

unks :: (Unknown a) => [a] -> [String]
unks unks = [a | Just a <- map unk unks]

namedInfo :: (Named a, MaybeInfo a, Unknown a) => a -> String
namedInfo a =
  let n  = name a
      i  = info a
      u  = unk a
      mu = addParens' u
  in maybe ("No NodeInfo for " ++ n ++ mu) id $ do
    NodeInfo mPos mPretty <- i
    let ps = maybe "" (\pos -> show pos ++ " ") mPos
    let pr = maybe "" (\pr  -> ": \"" ++ pr ++ "\"") mPretty
    return $ ps ++ n ++ pr
