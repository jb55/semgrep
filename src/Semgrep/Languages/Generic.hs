{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable,
             DeriveFunctor, ViewPatterns, FlexibleInstances, UndecidableInstances #-}

module Semgrep.Languages.Generic where

import           Data.Generics
import           Data.Monoid
import           Data.Maybe(isJust)
import           Control.Monad(foldM)
import           Semgrep()


data Position = PosSpanLine { pos_filename :: String
                            , pos_line     :: Int
                            , pos_col      :: Int
                            , pos_col_end  :: Int
                            }
              | PosPoint { pos_filename :: String
                         , pos_line     :: Int
                         , pos_col      :: Int
                         }
              | PosSpanLines { pos_filename :: String
                             , pos_line     :: Int
                             , pos_line_end :: Int
                             , pos_col      :: Int
                             , pos_col_end  :: Int
                             }
              deriving (Data, Typeable)

posLineEnd :: Position -> Int
posLineEnd p@(PosSpanLine {})  = pos_line p
posLineEnd p@(PosSpanLines {}) = pos_line_end p
posLineEnd p@(PosPoint {})     = pos_line p

posColEnd :: Position -> Int
posColEnd p@(PosSpanLine {})  = pos_col_end p
posColEnd p@(PosSpanLines {}) = pos_col_end p
posColEnd p@(PosPoint {})     = pos_col p

try' :: (Show a) => Maybe a -> String
try' = maybe "??" show

try :: Maybe String -> String -> String
try m s = maybe s id m

addParens :: (a -> String) -> Maybe a -> String
addParens f = maybe "" (\x -> " (" ++ f x ++ ")")

addParens' :: Maybe String -> String
addParens' = addParens id

instance Show Position where
  show (PosSpanLine f l s e) =
    concat [f, ": (", show l, ", ", show s, "-", show e, ")"]

  show (PosPoint f l c) =
    concat [f, ": (", show l, ", ", show c, ")"]

  show (PosSpanLines f ls le cs ce) =
    concat [f, ": (", show ls, "-", show le, ", ", show cs, "-", show ce, ")"]


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
data NInfo = NInfo { info_pos    :: Maybe Position
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
                  , node_info  :: NInfo
                  , node_kind  :: NKind
                  }
          | Var { var_ident :: Identifier
                , node_info  :: NInfo
                , node_kind  :: NKind
                }
          | Case { case_label :: Maybe Node
                 , case_type  :: CaseType
                 , case_body  :: Node
                 , node_info :: NInfo
                 , node_kind :: NKind
                 }
          | Singleton { sing_node :: Node
                      , node_info :: NInfo
                      , node_kind :: NKind
                      }
          | If { if_cond :: Node
               , if_body :: Node
               , if_else :: Maybe Node
               , node_info :: NInfo
               , node_kind :: NKind
               }
          | Switch { switch_cond :: Node
                   , switch_body :: Node
                   , node_info :: NInfo
                   , node_kind :: NKind
                   }
          | Compound { cmp_nodes :: [Node]
                     , node_info  :: NInfo
                     , node_kind  :: NKind
                     }
          | Block { block_nodes :: [Node]
                  , node_info  :: NInfo
                  , node_kind  :: NKind
                  }
          | Return { ret_body :: Maybe Node
                   , node_info :: NInfo
                   , node_kind :: NKind
                   }
          | Literal { lit_type :: Literal
                    , lit_str  :: Maybe String
                    , node_info :: NInfo
                    , node_kind :: NKind
                    }
          | BinaryOp { bin_op :: BinOp
                     , bin_node1 :: Node
                     , bin_node2 :: Node
                     , node_info :: NInfo
                     , node_kind :: NKind
                     }
          | Assign { asn_op :: AssignOp
                   , asn_to :: Node
                   , asn_from :: Node
                   , node_info :: NInfo
                   , node_kind :: NKind
                   }
          | DestructuringAssign { dasn_to :: [Node]
                                , dasn_from :: Node
                                , node_info :: NInfo
                                , node_kind :: NKind
                                }
          | ConditionalOp { condop_cond :: Node
                          , condop_body :: Maybe Node
                          , condop_else :: Node
                          , node_info :: NInfo
                          , node_kind :: NKind
                          }
          | Call { call_body :: Node
                 , call_args :: [Node]
                 , node_info :: NInfo
                 , node_kind :: NKind
                 }
          | Import { import_items :: [ImportItem]
                   , import_ident :: Maybe Identifier
                   , node_info :: NInfo
                   , node_kind :: NKind
                   }
          | Function { fun_props :: [DeclProp]
                     , fun_ident :: Maybe Identifier
                     , fun_body :: Node
                     , node_info :: NInfo
                     , node_kind :: NKind
                     }
          | Class { cls_name :: Identifier
                  , cls_body :: [Node]
                  , node_info :: NInfo
                  , node_kind :: NKind
                  }
          | UnkNode { unk_name :: String
                    , node_info :: NInfo
                    , node_kind :: NKind
                    }
          deriving (Show, Typeable, Data)

--------------------------------------------------------------------------------
-- | Node Kind
--------------------------------------------------------------------------------
data NKind = Statement | Expression | Declaration | Unknown
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
  name Compound {}              = "Compound"
  name Function {}              = "Function"
  name Singleton {}             = "Singleton"
  name Label {}                 = "Label"
  name Case {}                  = "Case"
  name If {}                    = "If"
  name Switch {}                = "Switch"
  name Block {}                 = "Block"
  name Import {}                = "Import"
  name Return {}                = "Return"
  name UnkNode {}               = "Unknown"

instance Kind Node where
  kind = node_kind

instance Named ImportItem where
  name ImportItem {} = "Import Item"

instance Named Identifier where
  name Ident {} = "Identifier"

instance Info Node where
  info = node_info

instance Info NInfo where
  info = id

instance MaybePos NInfo where
  posOf (info_pos -> p) = p

instance MaybePos (Maybe Position) where
  posOf = id

instance MaybePos Node where
  posOf (info -> n) = posOf n

instance MaybePos (Maybe Node) where
  posOf (Just a) = posOf a
  posOf Nothing  = Nothing

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

mergeSpan' :: (MaybePos a) => [a] -> Maybe Position
mergeSpan' ps = mergeSpan [a | Just a <- map posOf ps]

mergeSpan :: [Position] -> Maybe Position
mergeSpan []     = Nothing
mergeSpan [a]    = Just a
mergeSpan (a:as) = foldM append a as
  where
    append p1 p2
      | f1 /= f2       = Nothing
      | startC == endC = Just $ PosPoint f1 startL startC
      | startL /= endL = Just $ PosSpanLines f1 startL endL startC endC
      | otherwise      = Just $ PosSpanLine f1 startL startC endC
      where
        f1     = pos_filename p1
        f2     = pos_filename p2
        startL = pos_line p1   `min` pos_line p2
        startC = pos_col p1    `min` pos_col p2
        endL   = posLineEnd p1 `max` posLineEnd p2
        endC   = posColEnd p1  `max` posColEnd p2


nodesFromData :: (Data a) => a -> [Node]
nodesFromData = listify (const True)

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
