module Language.Granule.Codegen.MarkGlobals where
import Language.Granule.Codegen.NormalisedDef
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Type
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Pretty
import Data.Bifunctor.Foldable
import Language.Granule.Codegen.Builtins (builtinIds)

data GlobalMarker =
    GlobalVar Type Id
    | BuiltinVar Type Id
    | PairConstr Type Type
    | Unit
    deriving (Show, Eq)

instance Pretty GlobalMarker where
    pretty (GlobalVar _ x) = pretty x
    pretty (BuiltinVar _ x) = pretty x
    pretty (PairConstr _ _) = "(,)"
    pretty Unit = "()"

markGlobals :: NormalisedAST () Type -> NormalisedAST GlobalMarker Type
markGlobals (NormalisedAST dataDecls functionDefs valueDefs) =
    let globals = (functionDefIdentifier <$> functionDefs) ++ (valueDefIdentifier <$> valueDefs)
        functionDefs' = map (markGlobalsInFunctionDef globals) functionDefs
        valueDefs'    = map (markGlobalsInValueDef globals) valueDefs
    in NormalisedAST dataDecls functionDefs' valueDefs'

markGlobalsInFunctionDef :: [Id] -> FunctionDef () Type -> FunctionDef GlobalMarker Type
markGlobalsInFunctionDef globals def@FunctionDef { functionDefBody = body } =
    def { functionDefBody = markGlobalsInExpr globals body }

markGlobalsInValueDef :: [Id] -> ValueDef () Type -> ValueDef GlobalMarker Type
markGlobalsInValueDef globals def@ValueDef { valueDefInitializer = initializer } =
    def { valueDefInitializer = markGlobalsInExpr globals initializer }

markGlobalsInExpr :: [Id] -> Expr () Type -> Expr GlobalMarker Type
markGlobalsInExpr globals =
    bicata fixMapExtExpr markInValue
    where markInValue (VarF ty ident)
              | ident `elem` builtinIds = Ext ty (BuiltinVar ty ident)
              | ident `elem` globals = Ext ty (GlobalVar ty ident)
              | otherwise = Var ty ident
          markInValue (ConstrF ty@(FunTy _ _ leftTy (FunTy _ _ rightTy _)) (Id "," _) []) =
            Ext ty (PairConstr leftTy rightTy)
          markInValue (ConstrF ty@(TyCon (Id "()" _)) _ []) =
            Ext ty Unit
          markInValue other =
              fixMapExtValue (\ty ev -> error "Extension value in AST before global marking.") other

fixMapExtExpr :: ExprF eva a (Expr evb a) (Value evb a) -> Expr evb a
fixMapExtExpr (AppF sp ty rf fn arg) = App sp ty rf fn arg
fixMapExtExpr (BinopF sp ty rf op lhs rhs) = Binop sp ty rf op lhs rhs
fixMapExtExpr (LetDiamondF sp ty rf pat mty now next) = LetDiamond sp ty rf pat mty now next
fixMapExtExpr (ValF sp ty rf val) = Val sp ty rf val
fixMapExtExpr (CaseF sp ty rf swexp arms) = Case sp ty rf swexp arms
fixMapExtExpr (HoleF sp ty rf ids hints) = Hole sp ty rf ids hints
fixMapExtExpr (AppTyF sp ty rf fn tye) = AppTy sp ty rf fn tye
fixMapExtExpr (TryCatchF sp ty rf e1 pt mty e2 e3) = TryCatch sp ty rf e1 pt mty e2 e3
fixMapExtExpr (UnpackF sp ty rf id1 id2 e1 e2) = Unpack sp ty rf id1 id2 e1 e2

fixMapExtValue :: (a -> eva -> Value evb a)
               -> ValueF eva a (Value evb a) (Expr evb a)
               -> Value evb a
fixMapExtValue f (VarF ty ident) = Var ty ident
fixMapExtValue f (AbsF ty pat mty body) = Abs ty pat mty body
fixMapExtValue f (PromoteF ty ex) = Promote ty ex
fixMapExtValue f (PureF ty ex) = Pure ty ex
fixMapExtValue f (ConstrF ty ident vals) = Constr ty ident vals
fixMapExtValue f (NumIntF n) = NumInt n
fixMapExtValue f (NumFloatF n) = NumFloat n
fixMapExtValue f (CharLiteralF ch) = CharLiteral ch
fixMapExtValue f (StringLiteralF txt) = StringLiteral txt
fixMapExtValue f (ExtF ty ev) = f ty ev
fixMapExtValue f (NecF ty ex) = Nec ty ex
fixMapExtValue f (PackF sp ty t1 ex ident k t2) = Pack sp ty t1 ex ident k t2
fixMapExtValue f (TyAbsF ty idents e) = TyAbs ty idents e
fixMapExtValue f (RefF ty ex) = Ref ty ex
