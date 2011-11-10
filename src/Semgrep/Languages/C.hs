{-# LANGUAGE ViewPatterns #-}

module Semgrep.Languages.C (
  parse
) where


import qualified Language.C as C
import           Language.C.System.GCC(newGCC)
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
fromCConst (C.CIntConst cint _)  = IntConst (fromInteger $ C.getCInteger cint)
fromCConst (C.CCharConst cchar _) = CharConst (head $ C.getCChar cchar)
fromCConst (C.CFloatConst (C.CFloat s) _) = FloatConst (read s)
fromCConst (C.CStrConst cstring _) = StringConst (C.getCString cstring)


-- Converts a C expression to a generic expression
fromCExpr :: C.CExpr -> Expr
fromCExpr n@(C.CVar ident _) = Var (C.identToString ident) (makeNodeInfo n)

fromCExpr n@(C.CAssign op e1 e2 _) = Assign (fromCAssignOp op)
                                          (fromCExpr e1)
                                          (fromCExpr e2)
                                          (makeNodeInfo n)

fromCExpr n@(C.CConst c)           = ConstVal (fromCConst c) (makeNodeInfo n)

fromCExpr n@(C.CCond e1 e2 e3 _)   = ConditionalOp (fromCExpr e1)
                                                 (fmap fromCExpr e2)
                                                 (fromCExpr e3)
                                                 (makeNodeInfo n)

fromCExpr n@(C.CBinary op e1 e2 _) = BinaryOp (fromCBinOp op)
                                              (fromCExpr e1)
                                              (fromCExpr e2)
                                              (makeNodeInfo n)

fromCExpr n@(C.CCall expr exprs _) = FunApp (fromCExpr expr)
                                            (map fromCExpr exprs)
                                            (makeNodeInfo n)

fromCExpr e = UnkExpr (gshow e) (makeNodeInfo e)

-- Takes a CNode and returns a generic NodeInfo
makeNodeInfo :: (C.Pretty a, C.Pos a) => a -> Maybe NodeInfo
makeNodeInfo p = Just $ NodeInfo (makePos p) (makePretty p)
  where
    makePretty   = Just . show . C.pretty
    makePos      = Just . makePos' . C.posOf
    makePos' pos = Position Nothing
                            (Just $ C.posFile pos)
                            (Just $ C.posRow pos)
                            (Just $ C.posColumn pos)


-- Takes a C statement a returns if it's an 'If' statement or not
isIfStmt :: C.CStat -> Bool
isIfStmt (C.CIf _ _ _ _) = True
isIfStmt _ = False


-- Traverses a C translation unit and returns a list of C if statements
ifStmts :: C.CTranslUnit -> [C.CStat]
ifStmts = listify isIfStmt


allCExprs :: C.CTranslUnit -> [C.CExpr]
allCExprs = listify (const True)


fromCDecl :: C.CDecl -> Decl
fromCDecl (C.CDecl {}) = UnkDecl "not implemented" Nothing


fromCCompoundBlock :: C.CBlockItem -> Either Stmt Decl
fromCCompoundBlock (C.CBlockStmt stmt) = Left $ fromCStmt stmt
fromCCompoundBlock (C.CBlockDecl decl) = Right $ fromCDecl decl
fromCCompoundBlock (C.CNestedFunDef funDef) = Right $ fromCFunctionDef funDef


-- | Convert a C statement into a generic statement
fromCStmt :: C.CStat -> Stmt
fromCStmt n@(C.CLabel ident stmt attrs _) = Label (C.identToString ident)
                                                  (fromCStmt stmt)
                                                  (makeNodeInfo n)

fromCStmt n@(C.CCase e1 s1 _) = CaseStmt (fromCExpr e1)
                                         (fromCStmt s1)
                                         (makeNodeInfo n)

fromCStmt n@(C.CDefault s1 _) = CaseStmtDefault (fromCStmt s1) (makeNodeInfo n)

fromCStmt n@(C.CExpr mExpr _) = ExprStmt (fmap fromCExpr mExpr) (makeNodeInfo n)

fromCStmt n@(C.CIf e1 s1 ms1 _) = IfStmt (fromCExpr e1)
                                         (fromCStmt s1)
                                         (fmap fromCStmt ms1)
                                         (makeNodeInfo n)

fromCStmt n@(C.CSwitch e1 s1 _) = SwitchStmt (fromCExpr e1)
                                             (fromCStmt s1)
                                             (makeNodeInfo n)

fromCStmt n@(C.CCompound localLabels blockItems _) =
  let converted = map (stmtOrDecl . fromCCompoundBlock) blockItems
  in CompoundStmts converted (makeNodeInfo n)
    where
      stmtOrDecl (Left stmt) = stmt
      stmtOrDecl (Right decl)  = DeclStmt decl

fromCStmt n = UnkStmt (gshow n) (makeNodeInfo n)


fromCFunctionDef :: C.CFunDef -> Decl
fromCFunctionDef n@(C.CFunDef declSpecs d1 d2 stmt _) =
  let name  = Nothing
      stmt' = fromCStmt stmt
      node  = makeNodeInfo n
  in Function name stmt' node

fromCExtDecl :: C.CExtDecl -> Decl
fromCExtDecl (C.CFDefExt d) = fromCFunctionDef d
fromCExtDecl (C.CDeclExt d) = fromCDecl d
fromCExtDecl n = UnkDecl (gshow n) (makeNodeInfo n)


fromCTranslUnit :: C.CTranslUnit -> Module
fromCTranslUnit n@(C.CTranslUnit extDecls _) =
  Module (map fromCExtDecl extDecls) (makeNodeInfo n)


-- Takes a FilePath string and returns a generic AST, annotated with NodeInfo
-- from the C expressions.
parse :: FilePath -> [String] -> IO (Either String Project)
parse file incs = do
  let gcc = newGCC "gcc"
  parsedC <- C.parseCFile gcc Nothing incs file

  case parsedC of
    Left txt        -> return $ Left $ show txt
    Right transUnit -> do
      let all = allCExprs transUnit
      let exprs = map fromCExpr all
      return $ Right $ Project [fromCTranslUnit transUnit]

