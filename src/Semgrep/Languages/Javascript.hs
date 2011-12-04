{-# LANGUAGE ViewPatterns #-}

module Semgrep.Languages.Javascript where

import qualified Language.JavaScript.Parser.AST as J
import qualified Language.JavaScript.Parser.Parser as P
import           System.IO
import           Semgrep.Languages.Generic

fromSpan' :: J.SrcSpan -> Maybe Position
fromSpan' (J.SpanCoLinear f r s e)        = Just $ PosSpanLine f r s e
fromSpan' (J.SpanMultiLine f rs cs re ce) = Just $ PosSpanLines f rs re cs ce
fromSpan' (J.SpanPoint f r c)             = Just $ PosPoint f r c
fromSpan' (J.SpanEmpty {})                = Nothing

infoFromNode :: J.JSNode -> NInfo
infoFromNode (J.NS n span) = 
  NInfo (fromSpan' span) (Just $ take 20 $ show n)

jnode :: J.JSNode -> J.Node
jnode (J.NS n _) = n

--------------------------------------------------------------------------------
-- | Convert from JSNode to Node
--------------------------------------------------------------------------------
fromJsNode :: J.JSNode -> Node

--------------------------------------------------------------------------------
-- | Block statements
--------------------------------------------------------------------------------
fromJsNode n@(jnode -> J.JSBlock node) =
  Block [fromJsNode node] (infoFromNode n) Statement

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
fromJsNode n@(jnode -> J.JSIfElse n1 n2 n3) =
  If (fromJsNode n1)
     (fromJsNode n2)
     (Just $ fromJsNode n3)
     (infoFromNode n)
     Statement

fromJsNode n@(jnode -> J.JSIf n1 n2) =
  If (fromJsNode n1) 
     (fromJsNode n2)
     Nothing
     (infoFromNode n)
     Statement

--------------------------------------------------------------------------------
-- | Unknown node
--------------------------------------------------------------------------------
fromJsNode n@(J.NS ns span) = UnkNode (show ns) (infoFromNode n) Unknown

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
