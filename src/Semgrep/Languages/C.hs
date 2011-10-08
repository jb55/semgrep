module Semgrep.Languages.C (
  parse
) where


import qualified Language.C as C
import           Semgrep.Languages.Generic
import           Data.Generics
import           Semgrep

-- Converts a C assignment operator to a generic one
fromCAssignOp :: C.CAssignOp -> AssignOp
fromCAssignOp C.CAssignOp = AssignOp
fromCAssignOp C.CMulAssOp = MulAssignOp
fromCAssignOp C.CDivAssOp = DivAssignOp
fromCAssignOp o = UnkAssignOp (show o)


-- Converts a C binary operator to a generic one
fromCBinOp :: C.CBinaryOp -> BinOp
fromCBinOp C.CLeOp  = LeOp
fromCBinOp C.CGrOp  = GrOp
fromCBinOp C.CLeqOp = LeqOp
fromCBinOp C.CGeqOp = GeqOp
fromCBinOp C.CEqOp  = EqOp
fromCBinOp C.CNeqOp = NeqOp
fromCBinOp C.CAddOp = AddOp
fromCBinOp o        = UnkOp (gshow o)


-- Converts a C const value to a generic const value
fromCConst :: C.CConst -> ConstVal
fromCConst (C.CIntConst cint _)         = IntConst (fromInteger $ C.getCInteger cint)
fromCConst (C.CCharConst cchar _)       = CharConst (head $ C.getCChar cchar)
fromCConst (C.CFloatConst (C.CFloat s) _) = FloatConst (read s)
fromCConst (C.CStrConst cstring _)      = StringConst (C.getCString cstring)


-- Converts a C expression to a generic expression
fromCExpr :: C.CExpr -> Expr
fromCExpr (C.CVar ident _)       = Var (C.identToString ident)
fromCExpr (C.CAssign op e1 e2 _) = Assign (fromCAssignOp op)
                                          (fromCExpr e1)
                                          (fromCExpr e2)
fromCExpr (C.CConst c)           = ConstVal (fromCConst c)
fromCExpr (C.CCond e1 e2 e3 _)   = ConditionalOp (fromCExpr e1)
                                                 (fmap fromCExpr e2)
                                                 (fromCExpr e3)
fromCExpr (C.CBinary op e1 e2 _) = BinaryOp (fromCBinOp op)
                                            (fromCExpr e1)
                                            (fromCExpr e2)
fromCExpr e = UnkExpr (gshow e)

-- Takes a CNode and returns a generic NodeInfo
cNodeInfo :: (C.CNode a) => a -> NodeInfo
cNodeInfo n = NodeInfo (nodeInfo $ C.nodeInfo n)
  where
    nodeInfo n  = makePos (C.posOfNode n)
    makePos pos = Position Nothing
                           (Just $ C.posFile pos)
                           (Just $ C.posRow pos)
                           (Just $ C.posColumn pos)


-- Annotate a Expr with NodeInfo from a C.Expr
annotCNode :: C.CExpr -> Expr -> AExpr
annotCNode cexpr expr = Annotated (cNodeInfo cexpr) expr


-- Converts a C expression to a generic expression and annotates the generic
-- expression with position information from C node
fromCExpr' :: C.CExpr -> AExpr
fromCExpr' ce = annotCNode ce (fromCExpr ce)


-- Takes a C statement a returns if it's an 'If' statement or not
isIfStmt :: C.CStat -> Bool
isIfStmt (C.CIf _ _ _ _) = True
isIfStmt _ = False


-- Traverses a C translation unit and returns a list of C if statements
ifStmts :: C.CTranslUnit -> [C.CStat]
ifStmts = listify isIfStmt

allCExprs :: C.CTranslUnit -> [C.CExpr]
allCExprs = listify (const True)


-- Takes a FilePath string and returns a generic AST, annotated with NodeInfo
-- from the C expressions.
parse :: FilePath -> IO (Maybe Ast)
parse file = do
  stream <- C.readInputStream file
  let parsedC = C.parseC stream (C.initPos file)

  case parsedC of
    Left txt        -> return Nothing
    Right transUnit -> do
      let all = allCExprs transUnit
      let exprs = map fromCExpr' all
      return $ Just $ Ast exprs

