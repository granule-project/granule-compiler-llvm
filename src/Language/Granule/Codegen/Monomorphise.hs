module Language.Granule.Codegen.Monomorphise (monomorphiseAST) where

import Control.Monad.Identity (runIdentity)
import Data.Bifunctor (Bifunctor (bimap), second)
import qualified Data.Map as Map
import Language.Granule.Syntax.Annotated (annotation)
import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Expr hiding (subst)
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Pattern
import Language.Granule.Syntax.Type

-- polymorphic id -> [monomorphic id, [(ty var, ty subst)]]
type PolyInstances = Map.Map Id [(Id, [(Id, Type)])]

-- polymorphic id -> [ty var]
type PolyFuncs = Map.Map Id [Id]

-- TODO: support tyvar in any argument position and support more than 1 tyvar per function
-- currently only supports single tyvars in first argument

-- create monomorphic versions for each required instance of polymorphic function and rewrite ast
monomorphiseAST :: AST ev Type -> AST ev Type
monomorphiseAST ast =
  let polymorphicFuncs = getPolymorphicFunctions ast
      env = collectInstances ast polymorphicFuncs
      monoDefs = makeMonoDefs ast env
      rewritten = rewriteCalls ast env
   in rewritten {definitions = filter (not . isPolymorphic) (definitions rewritten) ++ monoDefs}

isPolymorphic :: Def ev Type -> Bool
isPolymorphic def =
  case defTypeScheme def of
    Forall _ ((_, Type 0) : _) _ _ -> True
    _ -> False

-- e.g. id -> __id_3856
makeMonoId :: Id -> [Type] -> Id
makeMonoId (Id id _) types =
  let hash = abs $ sum $ map fromEnum (show types)
      name = "__" ++ id ++ "_" ++ show hash
   in Id name name

-- create map of polymorphic function id to its ty vars
getPolymorphicFunctions :: AST ev Type -> PolyFuncs
getPolymorphicFunctions ast =
  Map.fromList $ map getPolyInfo $ filter isPolymorphic $ definitions ast
  where
    getPolyInfo :: Def ev Type -> (Id, [Id])
    getPolyInfo def =
      case defTypeScheme def of
        Forall _ bindings _ _ ->
          let tyVars = map fst bindings
           in (defId def, tyVars)

-- collect all insts of polymorphic functions with their concrete type substitutions
collectInstances :: AST ev Type -> PolyFuncs -> PolyInstances
collectInstances ast fns =
  foldl collectDef Map.empty (definitions ast)
  where
    collectDef env def =
      let defInstances = foldl collectEquation Map.empty (equations $ defEquations def)
       in Map.unionWith (++) env defInstances

    collectEquation env eq = collectExpr (equationBody eq)

    collectExpr :: Expr ev Type -> PolyInstances
    collectExpr (App _ _ _ e1 e2) =
      let inst = case getPolymorphicCall fns e1 e2 of
            Just (id, tyVarSubsts) ->
              Map.singleton id [(makeMonoId id (map snd tyVarSubsts), tyVarSubsts)]
            Nothing -> Map.empty
       in Map.unionWith (++) (collectExprs [e1, e2]) inst
    collectExpr (Val _ _ _ val) = collectVal val
    collectExpr (Binop _ _ _ _ e1 e2) = collectExprs [e1, e2]
    collectExpr (Case _ _ _ e bs) = collectExprs (e : map snd bs)
    collectExpr (AppTy _ _ _ e _) = collectExpr e
    collectExpr (LetDiamond _ _ _ _ _ e1 e2) = collectExprs [e1, e2]
    collectExpr (TryCatch _ _ _ e1 _ _ e2 e3) = collectExprs [e1, e2, e3]
    collectExpr (Unpack _ _ _ _ _ e1 e2) = collectExprs [e1, e2]
    collectExpr (Hole {}) = Map.empty

    collectVal :: Value ev Type -> PolyInstances
    collectVal (Abs _ _ _ body) = collectExpr body
    collectVal (Constr _ _ vals) = foldr (Map.unionWith (++) . collectVal) Map.empty vals
    collectVal (Pure _ e) = collectExpr e
    collectVal (Promote _ e) = collectExpr e
    collectVal (Nec _ e) = collectExpr e
    collectVal (Ref _ e) = collectExpr e
    collectVal (Pack _ _ _ e _ _ _) = collectExpr e
    collectVal (TyAbs _ _ e) = collectExpr e
    collectVal _ = Map.empty

    -- combine results from multiple expressions
    collectExprs :: [Expr ev Type] -> PolyInstances
    collectExprs = foldr (Map.unionWith (++) . collectExpr) Map.empty

-- identify polymorphic function calls and get substitution info
getPolymorphicCall :: PolyFuncs -> Expr ev Type -> Expr ev Type -> Maybe (Id, [(Id, Type)])
getPolymorphicCall fns (Val _ _ _ (Var _ id)) arg =
  case Map.lookup id fns of
    Just tyVars ->
      let argType = annotation arg
          substitutions = case argType of
            TyVar _ -> []
            _ -> [(head tyVars, argType)]
       in Just (id, substitutions)
    Nothing -> Nothing
getPolymorphicCall _ _ _ = Nothing

-- create monomorphised definitions for all polymorphic function insts
makeMonoDefs :: AST ev Type -> PolyInstances -> [Def ev Type]
makeMonoDefs ast env = concatMap (monoDefsForFunc ast) (Map.toList env)

monoDefsForFunc :: AST ev Type -> (Id, [(Id, [(Id, Type)])]) -> [Def ev Type]
monoDefsForFunc ast (id, instances) =
  let og = head (filter (\def -> defId def == id) (definitions ast))
   in map (monoDef og) instances

monoDef :: Def ev Type -> (Id, [(Id, Type)]) -> Def ev Type
monoDef (Def s _ r spec eqs ts) (id', typeSubsts) =
  let subs = Map.fromList typeSubsts
      subst = substTy subs
      eqs' = monoEqList eqs id' subs subst
      ts' = substTypeScheme ts subs subst
   in Def s id' r spec eqs' ts'

monoEqList :: EquationList ev Type -> Id -> Map.Map Id Type -> (Type -> Type) -> EquationList ev Type
monoEqList (EquationList s _ r eqs) id' subs applySubst =
  let eqs' = map (monoEq id' subs applySubst) eqs
   in EquationList s id' r eqs'

monoEq :: Id -> Map.Map Id Type -> (Type -> Type) -> Equation ev Type -> Equation ev Type
monoEq id' subs applySubst (Equation s id a r ps b) =
  let a' = applySubst a
      ps' = map (substPat subs) ps
      b' = substExpr subs b
   in Equation s id' a' r ps' b'

substTypeScheme :: TypeScheme -> Map.Map Id Type -> (Type -> Type) -> TypeScheme
substTypeScheme (Forall s bs cs ty) subs applySubst =
  let bs' = filter (\(tyVar, _) -> not (Map.member tyVar subs)) bs
      ty' = applySubst ty
   in Forall s bs' cs ty'

-- use typeFold with our substitution map
substTy :: Map.Map Id Type -> Type -> Type
substTy subs ty =
  runIdentity $ typeFoldM (baseTypeFold {tfTyVar = substVar}) ty
  where
    substVar id = return $ Map.findWithDefault (TyVar id) id subs

substPat :: Map.Map Id Type -> Pattern Type -> Pattern Type
substPat subs =
  patternFold
    (\s ty r id -> PVar s (substTy subs ty) r id)
    (\s ty r -> PWild s (substTy subs ty) r)
    (\s ty r pat -> PBox s (substTy subs ty) r pat)
    (\s ty r i -> PInt s (substTy subs ty) r i)
    (\s ty r f -> PFloat s (substTy subs ty) r f)
    (\s ty r id ids pats -> PConstr s (substTy subs ty) r id ids pats)

substExpr :: Map.Map Id Type -> Expr ev Type -> Expr ev Type
substExpr subs expr =
  case expr of
    App s ty r f arg -> App s (apply ty) r (subExp f) (subExp arg)
    Val s ty r val -> Val s (apply ty) r (fmap apply val)
    Binop s ty r op e1 e2 -> Binop s (apply ty) r op (subExp e1) (subExp e2)
    Case s ty r e ps -> Case s (apply ty) r (subExp e) (map (bimap (substPat subs) subExp) ps)
    Hole s ty r ids hs -> Hole s (apply ty) r ids hs
    AppTy s ty r e t -> AppTy s (apply ty) r (subExp e) t
    TryCatch s ty r e p mt e1 e2 -> TryCatch s (apply ty) r e p mt (subExp e1) (subExp e2)
    Unpack s ty r tyVar var e1 e2 -> Unpack s (apply ty) r tyVar var (subExp e1) (subExp e2)
    LetDiamond s ty r ps mt e1 e2 -> LetDiamond s (apply ty) r ps mt (subExp e1) (subExp e2)
  where
    apply = substTy subs
    subExp = substExpr subs

-- rewrite polymorphic function calls to use the monomorphised versions
rewriteCalls :: AST ev Type -> PolyInstances -> AST ev Type
rewriteCalls ast env = ast {definitions = map rewriteDef (definitions ast)}
  where
    rewriteDef def = def {defEquations = rewriteEqList (defEquations def)}
    rewriteEqList eqs = eqs {equations = map rewriteEq (equations eqs)}
    rewriteEq eq = eq {equationBody = rewriteExpr (equationBody eq)}

    rewriteExpr :: Expr ev Type -> Expr ev Type
    rewriteExpr expr@(App s ty r f arg) =
      let rewrittenF = rewriteExpr f
          rewrittenArg = rewriteExpr arg
          newF = case rewrittenF of
            Val s' t' r' (Var vt id) ->
              -- Only rewrite if this is a polymorphic function in our map
              if Map.member id env
                then
                  let argTy = annotation rewrittenArg
                      ty' = FunTy Nothing Nothing argTy ty
                   in Val s' ty' r' (Var ty' (makeMonoId id [argTy]))
                else rewrittenF
            _ -> rewrittenF
       in App s ty r newF rewrittenArg
    rewriteExpr (Val s ty r val) = Val s ty r (rewriteVal val)
    rewriteExpr (Binop s ty r op e1 e2) = Binop s ty r op (rewriteExpr e1) (rewriteExpr e2)
    rewriteExpr (Case s ty r e ps) = Case s ty r (rewriteExpr e) (map (second rewriteExpr) ps)
    rewriteExpr (Hole s a b ids hs) = Hole s a b ids hs
    rewriteExpr (AppTy s a b e t) = AppTy s a b (rewriteExpr e) t
    rewriteExpr (TryCatch s a b e p mt e1 e2) = TryCatch s a b (rewriteExpr e) p mt (rewriteExpr e1) (rewriteExpr e2)
    rewriteExpr (Unpack s a rf tyVar var e1 e2) = Unpack s a rf tyVar var (rewriteExpr e1) (rewriteExpr e2)
    rewriteExpr (LetDiamond s a b ps mt e1 e2) = LetDiamond s a b ps mt (rewriteExpr e1) (rewriteExpr e2)

    rewriteVal :: Value ev Type -> Value ev Type
    rewriteVal (Abs a pat mt e) = Abs a pat mt (rewriteExpr e)
    rewriteVal (Constr a idv vals) = Constr a idv (map rewriteVal vals)
    rewriteVal (Promote a e) = Promote a (rewriteExpr e)
    rewriteVal (Pure a e) = Pure a (rewriteExpr e)
    rewriteVal (Nec a e) = Nec a (rewriteExpr e)
    rewriteVal (Pack s a ty e v k ty') = Pack s a ty (rewriteExpr e) v k ty'
    rewriteVal (TyAbs a v e) = TyAbs a v (rewriteExpr e)
    rewriteVal (NumInt n) = NumInt n
    rewriteVal (NumFloat n) = NumFloat n
    rewriteVal (CharLiteral ch) = CharLiteral ch
    rewriteVal (StringLiteral str) = StringLiteral str
    rewriteVal (Ext a ev) = Ext a ev
    rewriteVal (Var a id) = Var a id
