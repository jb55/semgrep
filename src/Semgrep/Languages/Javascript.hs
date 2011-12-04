
import qualified Languages.Javascript.Parser.AST as J

fromSrcSpan' :: J.SrcSpan -> Position
fromSrcSpan' (J.SpanCoLinear f r s e)        = PosSpanLine f r s e
fromSrcSpan' (J.SpanMultiLine f rs cs re ce) = PosSpanLines f rs re cs ce
fromSrcSpan' (J.SpanPoint f r c)             = PosPoint f r c

fromSrcSpan :: J.SrcSpan -> NInfo
fromSrcSpan span = NInfo (fromSrcSpan' span) (

fromJsNode :: J.JSNode -> Node
fromJsNode (J.NS (J.JSBlock node) span) = Block [fromJsNode node]
                                                (fromSrcSpan span)
                                                Statement
fromJsNode (J.NS (J.JSIf node) span) = Block [fromJsNode node] ni Statement

