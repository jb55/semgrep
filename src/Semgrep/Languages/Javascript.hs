{-# LANGUAGE ViewPatterns #-}

module Semgrep.Languages.Javascript where

import qualified Language.JavaScript.Pretty.Printer as PP
import qualified Language.JavaScript.Parser.AST as J
import qualified Language.JavaScript.Parser as P
import           Data.String.Utils (lstrip)
import           System.IO
import           Semgrep.Languages.Generic
import           Debug.Trace (trace)

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
  Block (map fromJsNode nodes) (infoFromNode n) Statement

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
  , if_body   = Compound (map fromJsNode (traceNodes body)) (infoFromNode n) Statement
  , if_else   = Just $ Compound (map fromJsNode (traceNodes el)) (infoFromNode n) Statement
  , node_info = infoFromNode n
  , node_kind = Statement
  }
  where
    traceNodes nodes = trace (show $ map (lstrip . pp) nodes) nodes

--------------------------------------------------------------------------------
-- | Binary Expression
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSExpressionBinary _ lhs op rhs) =
  BinaryOp {
    bin_op    = fromJsBinOp op
  , bin_node1 = grp lhs
  , bin_node2 = grp rhs
  , node_info = infoFromNode n
  , node_kind = Expression
  }
  where
    grp [] = error "No nodes in javascript binary expression"
    grp nodes@(n:_) =
      Compound (map fromJsNode nodes) (infoFromNode n) Expression


--------------------------------------------------------------------------------
-- | Unknown node
--------------------------------------------------------------------------------
fromJsNode n = let nn = jnode n in
  UnkNode (show nn) (infoFromNode n) Unknown

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
