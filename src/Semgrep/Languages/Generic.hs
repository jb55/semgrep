{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable,
             DeriveFunctor, ViewPatterns, FlexibleInstances, UndecidableInstances #-}

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

instance Show NInfo where
  show (NInfo p s) =
    "NInfo "
    ++ try' p
    ++ maybe "" (\x -> " \"" ++ x ++ "\"") s
    ++ " "

--------------------------------------------------------------------------------
-- | NInfo contains generic node information such as the position of the node
--   in the document
--------------------------------------------------------------------------------
data NInfo = NInfo { info_pos :: Maybe Position
                   , info_pretty :: Maybe String
                   }
                   deriving (Data, Typeable)

--------------------------------------------------------------------------------
-- | Named types
--------------------------------------------------------------------------------
class Named a where
  name :: a -> String

--------------------------------------------------------------------------------
-- | Error reporting unconverted nodes
--------------------------------------------------------------------------------
class Unknown a where
  unk :: a -> Maybe String

--------------------------------------------------------------------------------
-- | Position and pretty printing node info
--------------------------------------------------------------------------------
class Info a where
  info :: a -> NInfo

--------------------------------------------------------------------------------
-- | Node kinds (Expression/Statement)
--------------------------------------------------------------------------------
class Kind a where
  kind :: a -> NKind

--------------------------------------------------------------------------------
-- | Types that may produce position information
--------------------------------------------------------------------------------
class MaybePos a where
  posOf :: a -> Maybe Position

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
data Literal = IntLiteral Int
             | CharLiteral Char
             | FloatLiteral Float
             | StringLiteral String
             deriving (Show, Typeable, Data, Eq)

data CaseType = Match | Default
              deriving (Show, Typeable, Data, Eq)

--------------------------------------------------------------------------------
-- | Elements
--------------------------------------------------------------------------------
data Node = Label { lbl_ident :: Identifier
                  , lbl_node  :: Node
                  , lbl_info  :: NInfo
                  , lbl_kind  :: NKind
                  }
          | Var { var_ident :: Identifier
                , var_info  :: NInfo
                , var_kind  :: NKind
                }
          | Case { case_label :: Maybe Node
                 , case_type  :: CaseType
                 , case_body  :: Node
                 , case_info :: NInfo
                 , case_kind :: NKind
                 }
          | Singleton { sing_node :: Node
                      , sing_info :: NInfo
                      , sing_kind :: NKind
                      }
          | If { if_cond :: Node
               , if_body :: Node
               , if_else :: Maybe Node
               , if_info :: NInfo
               , if_kind :: NKind
               }
          | Switch { switch_cond :: Node
                   , switch_body :: Node
                   , switch_info :: NInfo
                   , switch_kind :: NKind
                   }
          | Compound { cmp_nodes :: [Node]
                     , cmp_info  :: NInfo
                     , cmp_kind  :: NKind
                     }
          | Block { block_nodes :: [Node]
                  , block_info  :: NInfo
                  , block_kind  :: NKind
                  }
          | Return { ret_body :: Maybe Node
                   , ret_info :: NInfo
                   , ret_kind :: NKind
                   }
          | Literal { lit_type :: Literal
                    , lit_str  :: Maybe String
                    , lit_info :: NInfo
                    , lit_kind :: NKind
                    }
          | BinaryOp { bin_op :: BinOp
                     , bin_node1 :: Node
                     , bin_node2 :: Node
                     , bin_info :: NInfo
                     , bin_kind :: NKind
                     }
          | Assign { asn_op :: AssignOp
                   , asn_to :: Node
                   , asn_from :: Node
                   , asn_info :: NInfo
                   , asn_kind :: NKind
                   }
          | DestructuringAssign { dasn_to :: [Node]
                                , dasn_from :: Node
                                , dasn_info :: NInfo
                                , dasn_kind :: NKind
                                }
          | ConditionalOp { condop_cond :: Node
                          , condop_body :: Maybe Node
                          , condop_else :: Node
                          , condop_info :: NInfo
                          , condop_kind :: NKind
                          }
          | Call { call_body :: Node
                 , call_args :: [Node]
                 , call_info :: NInfo
                 , call_kind :: NKind
                 }
          | Import { import_items :: [ImportItem]
                   , import_ident :: Maybe Identifier
                   , import_info :: NInfo
                   , import_kind :: NKind
                   }
          | Function { fun_props :: [DeclProp]
                     , fun_ident :: Maybe Identifier
                     , fun_body :: Node
                     , fun_info :: NInfo
                     , fun_kind :: NKind
                     }
          | Class { cls_name :: Identifier
                  , cls_body :: [Node]
                  , cls_info :: NInfo
                  , cls_kind :: NKind
                  }
          | UnkNode { unk_name :: String
                    , unk_info :: NInfo
                    , unk_kind :: NKind
                    }
          deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Node Kind
--------------------------------------------------------------------------------
data NKind = Statement | Expression | Declaration
           deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Declaration Properties
--------------------------------------------------------------------------------
data DeclProp = Result Node
              deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Import items
--------------------------------------------------------------------------------
data ImportItem = ImportItem [Identifier] (Maybe Identifier) NInfo
                deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Identifiers
--------------------------------------------------------------------------------
data Identifier = Ident String (Maybe Int) NInfo
                deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Generic module
--------------------------------------------------------------------------------
data Module = Module { module_nodes :: [Node]
                     , module_name  :: Maybe String
                     , module_info  :: NInfo
                     } deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Generic projects, contains zero or more modules
--------------------------------------------------------------------------------
data Project = Project [Module]
             deriving (Show, Typeable, Data)

instance Unknown Identifier where
  unk _ = Nothing

instance Named Literal where
  name IntLiteral {}    = "Integer"
  name CharLiteral {}   = "Char"
  name FloatLiteral {}  = "Float"
  name StringLiteral {} = "String"
  name _                = "Unknown"

instance Named Node where
  name Var {}                   = "Variable"
  name Literal { lit_type = c } = name c ++ " Literal"
  name BinaryOp {}              = "Binary Operation"
  name Assign {}                = "Assignment"
  name DestructuringAssign {}   = "Destructuring Assignment"
  name ConditionalOp {}         = "Conditional Operator"
  name Call {}                  = "Function Call"
  name Class {}                 = "Class"
  name Function {}              = "Function"
  name Label {}                 = "Label"
  name Case {}                  = "Case"
  name If {}                    = "If"
  name Switch {}                = "Switch"
  name Block {}                 = "Block"
  name Import {}                = "Import"
  name Return {}                = "Return"
  name _                        = "Unknown"

instance Kind Node where
  kind = nodeKind

instance Named ImportItem where
  name ImportItem {} = "Import Item"

instance Named Identifier where
  name Ident {} = "Identifier"

instance Info Node where
  info = nodeInfo

instance Info NInfo where
  info = id

instance MaybePos NInfo where
  posOf (info_pos -> p) = p

instance MaybePos Node where
  posOf (info -> n) = posOf n

instance MaybePos (Maybe Node) where
  posOf (Just a) = posOf a
  posOf Nothing  = Nothing

type LanguageParser = FilePath -> IO (Either String Project)


nodeKind :: Node -> NKind
nodeKind Var { var_kind = k }                  = k
nodeKind Literal { lit_kind = k }              = k
nodeKind BinaryOp { bin_kind = k }             = k
nodeKind Assign { asn_kind = k }               = k
nodeKind DestructuringAssign { dasn_kind = k } = k
nodeKind ConditionalOp { condop_kind = k }     = k
nodeKind Call { call_kind = k }                = k
nodeKind Class { cls_kind = k }                = k
nodeKind Function { fun_kind = k }             = k
nodeKind Label { lbl_kind = k }                = k
nodeKind Case { case_kind = k }                = k
nodeKind If { if_kind = k }                    = k
nodeKind Switch { switch_kind = k }            = k
nodeKind Block { block_kind = k }              = k
nodeKind Import { import_kind = k }            = k
nodeKind Return { ret_kind = k }               = k
nodeKind Singleton { sing_kind = k }           = k
nodeKind Compound { cmp_kind = k }             = k
nodeKind UnkNode { unk_kind = k }              = k

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

nodeInfo :: Node -> NInfo
nodeInfo Var { var_info = i }                  = i
nodeInfo Literal { lit_info = i }              = i
nodeInfo BinaryOp { bin_info = i }             = i
nodeInfo Assign { asn_info = i }               = i
nodeInfo DestructuringAssign { dasn_info = i } = i
nodeInfo ConditionalOp { condop_info = i }     = i
nodeInfo Call { call_info = i }                = i
nodeInfo Class { cls_info = i }                = i
nodeInfo Function { fun_info = i }             = i
nodeInfo Label { lbl_info = i }                = i
nodeInfo Case { case_info = i }                = i
nodeInfo If { if_info = i }                    = i
nodeInfo Switch { switch_info = i }            = i
nodeInfo Block { block_info = i }              = i
nodeInfo Import { import_info = i }            = i
nodeInfo Return { ret_info = i }               = i
nodeInfo Singleton { sing_info = i }           = i
nodeInfo Compound { cmp_info = i }             = i
nodeInfo UnkNode { unk_info = i }              = i


infoPretty :: NInfo -> Maybe String
infoPretty (NInfo _ p) = p

isStmt :: Node -> Bool
isStmt (kind -> Statement) = True
isStmt _                   = False

statements :: (Data a) => a -> [Node]
statements = listify isExpr

expressions :: (Data a) => a -> [Node]
expressions = listify isStmt

isExpr :: Node -> Bool
isExpr (kind -> Expression) = True
isExpr _                    = False

isCond :: Node -> Bool
isCond ConditionalOp {}         = True
isCond BinaryOp { bin_op = op } = isCondOp op
isCond _                        = False

isStringLit :: Node -> Bool
isStringLit Literal { lit_type = StringLiteral {} } = True
isStringLit _                                       = False

stringLit :: Node -> Maybe String
stringLit Literal { lit_type = StringLiteral s } = Just s
stringLit _                                      = Nothing

isCall :: Node -> Bool
isCall (Call {}) = True
isCall _         = False

isImport :: Node -> Bool
isImport (Import {}) = True
isImport _           = False

nullInfo :: NInfo
nullInfo = NInfo Nothing Nothing

isCompound :: Node -> Bool
isCompound (Block {}) = True
isCompound _          = False

isDullNode :: Node -> Bool
isDullNode node = or $ map ($node) dulls
  where dulls = []

imports :: [Node] -> [Node]
imports = filter isImport

nodes :: (Data a) => a -> [Node]
nodes = listify (const True)

calls :: [Node] -> [Node]
calls = filter isCall

conditions :: [Node] -> [Node]
conditions = filter isCond

isUnk = isJust . unk

unks :: (Unknown a) => [a] -> [String]
unks unks = [a | Just a <- map unk unks]

--------------------------------------------------------------------------------
-- | Pretty print nodes
--------------------------------------------------------------------------------
namedInfo :: Node -> String
namedInfo a =
  let n       = name a
      i       = info a
      k       = kind a
      mPos    = posOf a
      mPretty = infoPretty i
      ps      = maybe "" (\pos -> show pos ++ " ") mPos
      pr      = maybe "" (\pr  -> ": '" ++ pr ++ "'") mPretty
  in ps ++ n ++ (' ' : show k) ++ pr
