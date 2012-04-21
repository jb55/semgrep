{-# LANGUAGE ViewPatterns #-}

module Semgrep.Languages.Javascript where

import qualified Language.JavaScript.Pretty.Printer as PP
import qualified Language.JavaScript.Parser.AST as J
import qualified Language.JavaScript.Parser as P
import           Data.String.Utils (lstrip)
import           System.IO
import           Semgrep.Languages.Generic
import           Debug.Trace (trace)
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)

pp = PP.renderToString

fromTokPos :: P.TokenPosn -> Maybe Position
fromTokPos (P.TokenPn _ r c) = Just $ PosPoint "" r c

maybeTok :: J.JSNode -> Maybe P.TokenPosn
maybeTok (J.NT _ tok _) = Just tok
maybeTok _              = Nothing

infoFromNode :: J.JSNode -> NInfo
infoFromNode jn = NInfo (maybeTok jn >>= fromTokPos) s
  where
    s = Just $ lstrip $ pp jn

jnode :: J.JSNode -> J.Node
jnode (J.NT n _ _) = n
jnode (J.NN n)     = n

--------------------------------------------------------------------------------
-- | Convert from JSNode to Node
--------------------------------------------------------------------------------
fromJsNode :: J.JSNode -> Node

--------------------------------------------------------------------------------
-- | Block statements
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSBlock _ nodes _) =
  Block (map fromJsNode nodes) (infoFromNode n) Statement

fromJsNode n@(jnode -> J.JSExpression nodes) =
  Compound (map fromJsNode nodes) (infoFromNode n) expression

--------------------------------------------------------------------------------
-- | Top level node for holding nodes
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSSourceElementsTop nodes) =
  Compound (map fromJsNode nodes) (infoFromNode n) Statement

--------------------------------------------------------------------------------
-- | If statements
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSIf _ _ cond _ body el) =
  If {
    if_cond   = fromJsNode cond
  , if_body   = disregardTokens' body
  , if_else   = disregardTokens el
  , node_info = infoFromNode n
  , node_kind = Statement
  }
  where
    traceNodes nodes = trace (show $ map (lstrip . pp) nodes) nodes

--------------------------------------------------------------------------------
-- | Binary expression
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSExpressionBinary _ lhs op rhs) =
  BinaryOp {
    bin_op    = fromJsBinOp op
  , bin_node1 = disregardTokens' lhs
  , bin_node2 = disregardTokens' rhs
  , node_info = infoFromNode n
  , node_kind = expression
  }

fromJsNode n@(jnode -> J.JSDecimal s) =
  Literal {
    lit_type = IntLiteral (read s)
  , lit_str  = Just s
  , node_info = infoFromNode n
  , node_kind = expression
  }

--------------------------------------------------------------------------------
-- | Variables
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSVariables _ decls _) =
  Compound (map fromJsNode decls) (infoFromNode n) Statement

--------------------------------------------------------------------------------
-- | Variable declaration
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSVarDecl nn v) =
  case disregardTokens v of
    Nothing -> updateKind convertedNode declaration
    Just ns -> updateKind convertedNode Declaration {
      decl_init = Just Assign { asn_op   = DefaultAssign
                              , asn_to   = convertedNode
                              , asn_from = ns
                              , node_info = infoFromNode n
                              , node_kind = declaration
                              }
    , kind_props = []
    }
  where
    updateKind var@(Var{}) d = var { node_kind = d }
    updateKind _ _           = error "JSVarDecl node1 is not an identifier"
    convertedNode = fromJsNode nn
    convertedName = ident_name . var_ident $ convertedNode

--------------------------------------------------------------------------------
-- | Identifiers
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSIdentifier s) = jsVarDecl s info
  where info = infoFromNode n

--------------------------------------------------------------------------------
-- | language-javascript mixes tokens with the ast for accurate representation
--   when printing. We can safely ignore them for our purposes
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSLiteral _) = discard

--------------------------------------------------------------------------------
-- | Unknown node
--------------------------------------------------------------------------------
fromJsNode n = UnkNode (P.showStripped n) (infoFromNode n) Unknown

jsVarDecl s i = Var (Ident s Nothing i) i expression

ppNodes :: [J.JSNode] -> String
ppNodes = intercalate "," . map pp

disregardTokens' nodes =
  let err = error $ "Only tokens in node list" ++ ppNodes nodes
  in fromMaybe err $ disregardTokens nodes

disregardTokens :: [J.JSNode] -> Maybe Node
disregardTokens nodes =
  let nodes' = filter isntDiscard $ map fromJsNode nodes
      ppNodes' = ppNodes nodes
  in case nodes' of
    [n]  -> Just n
    []   -> Nothing
    n:_  -> error $ "More than one non-discarded node in disregardTokens " ++ ppNodes'

--------------------------------------------------------------------------------
-- | Binary Operators
--------------------------------------------------------------------------------
fromJsBinOp :: J.JSNode -> BinOp
fromJsBinOp jn = UnkOp (pp jn)

--------------------------------------------------------------------------------
-- | Simple module constructor
--------------------------------------------------------------------------------
moduleFromNode :: J.JSNode -> Module
moduleFromNode n@(jnode -> J.JSSourceElementsTop nodes) =
  Module (map fromJsNode nodes) Nothing (infoFromNode n)

moduleFromNode node =
  Module [fromJsNode node] Nothing (infoFromNode node)


--------------------------------------------------------------------------------
-- | Parse javascript from file
--------------------------------------------------------------------------------
parse :: FilePath -> IO (Either String Project)
parse file = do
  content <- openFile file ReadMode >>= hGetContents
  return $ case P.parse content file of
    Left parseError -> Left $ show parseError
    Right node      -> Right $ Project [moduleFromNode node]
    --Right node      -> Left $ show node
