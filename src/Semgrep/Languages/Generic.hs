{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable,
             DeriveFunctor, ViewPatterns, FlexibleInstances #-}

module Semgrep.Languages.Generic where

import           Data.Generics
import           Data.Maybe(isJust)
import           Control.Monad(foldM)
import           Semgrep()


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

--------------------------------------------------------------------------------
-- | NodeInfo contains generic node information such as the position of the node
--   in the document
--------------------------------------------------------------------------------
data NodeInfo = NodeInfo (Maybe Position) (Maybe String)
              deriving (Data, Typeable)

--------------------------------------------------------------------------------
-- | Named types
--------------------------------------------------------------------------------
class Named a where
  name :: a -> String

--------------------------------------------------------------------------------
-- | Types that may produce a node info
--------------------------------------------------------------------------------
class MaybeInfo a where
  info :: a -> Maybe NodeInfo

--------------------------------------------------------------------------------
-- | Error reporting unconverted nodes
--------------------------------------------------------------------------------
class Unknown a where
  unk :: a -> Maybe String

--------------------------------------------------------------------------------
-- | Types that may produce position information
--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
-- | Binary operators
--------------------------------------------------------------------------------
data BinOp = LeOp
           | GrOp
           | LeqOp
           | GeqOp
           | EqOp
           | NeqOp
           | AddOp
           | UnkOp String
           deriving (Show, Typeable, Data, Eq)

--------------------------------------------------------------------------------
-- | Assignment operators
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- | Literal values
--------------------------------------------------------------------------------
data LiteralValue = IntLiteral Int
                  | CharLiteral Char
                  | FloatLiteral Float
                  | StringLiteral String
                  deriving (Show, Typeable, Data, Eq)

--------------------------------------------------------------------------------
-- | Expressions
--------------------------------------------------------------------------------
data Expr = Var Identifier Annotation
          | CompoundExpr [Expr] Annotation
          | LiteralValue LiteralValue (Maybe String) Annotation
          | BinaryOp BinOp Node Node Annotation
          | Assign AssignOp Node Node Annotation
          | DestructuringAssign [Node] Node Annotation
          | ConditionalOp Node (Maybe Node) Node Annotation
          | Call Node [Node] Annotation
          | UnkExpr String Annotation
       -- | CaseExpr
          deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Function/type constructor arguments
--------------------------------------------------------------------------------
--data Argument =

--------------------------------------------------------------------------------
-- | Statements
--------------------------------------------------------------------------------
data Stmt = ExprStmt (Maybe Expr) Annotation
          | Label Identifier Stmt Annotation
          | DeclStmt Decl
          | CaseStmt Node Node Annotation
          | CaseStmtDefault Node Annotation
          | IfStmt Node Node (Maybe Stmt) Annotation
          | SwitchStmt Node Node Annotation
          | Block [Node] Annotation
          | Return (Maybe Node) Annotation
          | Import [ImportItem] (Maybe Identifier) Annotation
          | UnkStmt String Annotation
          deriving (Show, Typeable, Data)

data Node = Stmt Stmt
          | Expr Expr
          | Decl Decl
          deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Import items
--------------------------------------------------------------------------------
data ImportItem = ImportItem [Identifier] (Maybe Identifier) Annotation
                deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Identifiers
--------------------------------------------------------------------------------
data Identifier = Ident String (Maybe Int) Annotation
                deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Declaration properties
--------------------------------------------------------------------------------
data DeclProp = Result Expr
              deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Declarations
--------------------------------------------------------------------------------
data Decl = Class { declName :: Identifier
                  , declArgs :: [Stmt]
                  , declInfo :: Annotation
                  }
          | DataDecl Annotation
          | Function [DeclProp] (Maybe Identifier) Stmt Annotation
          | UnkDecl String Annotation
          deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Generic module
--------------------------------------------------------------------------------
data Module = Module { moduleNodes :: [Node]
                     , moduleName  :: Maybe String
                     , moduleAnno  :: Annotation
                     } deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Generic projects, contains zero or more modules
--------------------------------------------------------------------------------
data Project = Project [Module]
             deriving (Show, Typeable, Data)

instance Unknown Node where
  unk (Stmt x) = unk x
  unk (Expr x) = unk x
  unk (Decl x) = unk x

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

instance Unknown Identifier where
  unk _ = Nothing

instance Named LiteralValue where
  name (IntLiteral {})       = "Integer"
  name (CharLiteral {})      = "Char"
  name (FloatLiteral {})     = "Float"
  name (StringLiteral {})    = "String"
  name _                     = "Unknown"

instance Named Expr where
  name (Var {})                 = "Variable"
  name (CompoundExpr {})        = "Compound Expression"
  name (LiteralValue c _ _ )    = "Literal Value (" ++ name c ++ ")"
  name (BinaryOp {})            = "Binary Operation"
  name (Assign {})              = "Assignment"
  name (DestructuringAssign {}) = "DestructuringAssignment"
  name (ConditionalOp {})       = "Conditional Operator"
  name (Call {})                = "Function Call"
  name _                        = "Unknown Expression"

instance Named Decl where
  name (Class {})          = "Class"
  name (DataDecl {})       = "Data"
  name (Function {})       = "Function"
  name _                   = "Unknown Declaration"

instance Named ImportItem where
  name (ImportItem {})     = "Import Item"

instance Named Identifier where
  name (Ident {})          = "Identifier"

instance Named Stmt where
  name (ExprStmt expr _)    = maybe "" (\e -> e ++ " ") (fmap name expr) ++ "Expression"
  name (Label {})           = "Label"
  name (DeclStmt decl)      = name decl ++ " Declaration"
  name (CaseStmt {})        = "Case"
  name (CaseStmtDefault {}) = "Default Case"
  name (IfStmt {})          = "If"
  name (SwitchStmt {})      = "Switch"
  name (Block {})           = "Block"
  name (Import {})          = "Import"
  name (Return {})          = "Return"
  name (UnkStmt {})         = "Unknown"

instance Named Node where
  name (Stmt x) = name x ++ " Statement"
  name (Expr x) = name x ++ " Expression"
  name (Decl x) = name x ++ " Declaration"

instance MaybeInfo Node where
  info (Stmt x) = info x
  info (Expr x) = info x
  info (Decl x) = info x

instance MaybeInfo Stmt where
  info (ExprStmt _ n)       = n
  info (Label _ _ n)        = n
  info (DeclStmt decl)      = info decl
  info (CaseStmt _ _ n)     = n
  info (CaseStmtDefault _ n) = n
  info (IfStmt _ _ _ n)     = n
  info (SwitchStmt _ _ n)   = n
  info (Block _ n)          = n
  info (Import _ _ n)       = n
  info (Return _ n)         = n
  info (UnkStmt _ n)        = n

instance MaybeInfo ImportItem where
  info (ImportItem _ _ n)   = n

instance MaybeInfo Identifier where
  info (Ident _ _ n)        = n

instance MaybeInfo Expr where
  info (Var _ n)            = n
  info (CompoundExpr _ n)   = n
  info (LiteralValue _ _ n) = n
  info (BinaryOp _ _ _ n)   = n
  info (Assign _ _ _ n)     = n
  info (DestructuringAssign _ _ n) = n
  info (ConditionalOp _ _ _ n) = n
  info (Call _ _ n)       = n
  info (UnkExpr _ n)        = n

instance MaybeInfo Decl where
  info (Class _ _ n)      = n
  info (DataDecl n)       = n
  info (Function _ _ _ n) = n
  info (UnkDecl _ n)      = n

instance MaybeInfo Module where
  info (Module _ _ n) = n

type LanguageParser = FilePath -> IO (Either String Project)

isCondOp :: BinOp -> Bool
isCondOp LeOp   = True
isCondOp GrOp   = True
isCondOp LeqOp  = True
isCondOp GeqOp  = True
isCondOp EqOp   = True
isCondOp NeqOp  = True
isCondOp _      = False

projectModules :: Project -> [Module]
projectModules (Project mods) = mods

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


--------------------------------------------------------------------------------
-- | Converts a compound expression consisting of only string literals into
--   a single string literal.
--
--   eg. "hello""world" becomes "helloworld"
--
--   This results in Nothing if the compound expression contains expressions
--   other than String literals
--------------------------------------------------------------------------------
fromCompoundedStrings :: Expr -> Maybe Expr
fromCompoundedStrings (CompoundExpr [] _)  = Nothing
fromCompoundedStrings (CompoundExpr [e] a) = updateNode e a
  where
    updateNode (LiteralValue s m _) newA = Just $ LiteralValue s m newA
    updateNode _ _                       = Nothing

fromCompoundedStrings (CompoundExpr (e1:es) a) = foldM joinTwo e1 es
  where
    joinTwo :: Expr -> Expr -> Maybe Expr
    joinTwo l1@(LiteralValue _ m1 _) l2 = do
      str1 <- stringLit l1
      str2 <- stringLit l2
      return $ LiteralValue (StringLiteral $ str1 ++ str2) m1 a
    joinTwo _ _ = Nothing
fromCompoundedStrings _ = Nothing


isFunctionCall :: Expr -> Bool
isFunctionCall (Call {}) = True
isFunctionCall _         = False

isImport :: Stmt -> Bool
isImport (Import {}) = True
isImport _           = False

isCompound :: Stmt -> Bool
isCompound (Block {}) = True
isCompound _          = False

isExpressionStmt :: Stmt -> Bool
isExpressionStmt (ExprStmt {}) = True
isExpressionStmt _             = False

isDullStmt :: Stmt -> Bool
isDullStmt stmt = or $ map ($stmt) dulls
  where dulls = [isExpressionStmt, isCompound]

statements :: (Data a) => a -> [Stmt]
statements = listify (const True)

imports :: (Data a) => a -> [Stmt]
imports = filter isImport . statements

nodes :: (Data a) => a -> [Node]
nodes = listify (const True)

expressions :: (Data a) => a -> [Expr]
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


--------------------------------------------------------------------------------
-- | Pretty print nodes of all kinds!
--------------------------------------------------------------------------------
namedInfo :: (Named a, MaybeInfo a, Unknown a) => a -> String
namedInfo a =
  let n  = name a
      i  = info a
      u  = unk a
      mu = maybe "" (\x -> " '" ++ x ++ "'") u
  in maybe ("No NodeInfo for " ++ n ++ mu) id $ do
    NodeInfo mPos mPretty <- i
    let ps = maybe "" (\pos -> show pos ++ " ") mPos
    let pr = maybe "" (\pr  -> ": '" ++ pr ++ "'") mPretty
    return $ ps ++ n ++ pr
