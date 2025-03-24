module Language.Granule.Codegen.RewriteAST where

import Data.Bifunctor (bimap)
import Data.List (mapAccumL)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Identifiers (Id)
import Language.Granule.Syntax.Pattern
import Language.Granule.Syntax.Type

-- Rewrite Unpack ASTs into App Abs ASTs which our
-- compiler already knows how to handle. WIP.

rewriteAST :: AST ev Type -> AST ev Type
rewriteAST ast = ast {definitions = map rewriteDef (definitions ast)}
  where
    rewriteDef def = def {defEquations = rewriteEquationList (defEquations def)}
    rewriteEquationList eqs = eqs {equations = map rewriteEquation (equations eqs)}
    rewriteEquation eq = eq {equationBody = rewriteExpr (equationBody eq)}

rewriteExpr :: Expr ev Type -> Expr ev Type
rewriteExpr (Unpack s retTy b tyVar var e1 e2) =
  let e1' = e1
      e1Ty = exprTy e1'
      e2' = e2
      absTy = FunTy Nothing Nothing e1Ty retTy
   in fixTypes (App s retTy b (Val s absTy b (Abs absTy (PVar s e1Ty b var) Nothing e2')) e1')
  where
    fixTypes expr = snd $ substExpr emptyEnv expr
rewriteExpr exp = exp

exprTy :: Expr ev Type -> Type
exprTy (App _ ty _ _ _) = ty
exprTy (Val _ ty _ _) = ty
exprTy (Binop _ ty _ _ _ _) = ty
exprTy (LetDiamond _ ty _ _ _ _ _) = ty
exprTy (Case _ ty _ _ _) = ty
exprTy (Hole _ ty _ _ _) = ty
exprTy (AppTy _ ty _ _ _) = ty
exprTy (TryCatch _ ty _ _ _ _ _ _) = ty
exprTy (Unpack _ ty _ _ _ _ _) = ty

-- `let (x, y) = ` inside of an Unpack seems to leave TyVars in the AST, and these
-- are not already handled by the compiler. Here we find the correct types and substitute
-- the TyVars. WIP.

--          val var -> Type, type var -> Type
type Env = (Map.Map Id Type, Map.Map Id Type)

emptyEnv :: Env
emptyEnv = (Map.empty, Map.empty)

insertEnv :: Env -> Either Id Id -> Type -> Env
insertEnv (vals, tys) (Left id) ty = (Map.insert id ty vals, tys)
insertEnv (vals, tys) (Right id) ty = (vals, Map.insert id ty tys)

lookupEnv :: Env -> Either Id Id -> Maybe Type
lookupEnv (vals, tys) (Left id) = Map.lookup id vals
lookupEnv (vals, tys) (Right id) = Map.lookup id tys

substExpr :: Env -> Expr ev Type -> (Env, Expr ev Type)
substExpr env (App s ty b e1 e2) =
  let (env', e2') = substExpr env e2
      (env'', e1') = substExpr env' e1
      ty' = substTy env ty
   in (env'', App s ty' b e1' e2')
substExpr env (Val s ty b v) =
  let (env', v') = substVal env v
      ty' = substTy env' ty
   in (env', Val s ty' b v')
substExpr env exp = error "TODO expr"

substVal :: Env -> Value ev Type -> (Env, Value ev Type)
substVal env (Var (TyVar id) var) =
  -- see if we already have it
  case lookupEnv env (Right id) of
    Just ty -> (env, Var ty var)
    Nothing ->
      -- see if the value variable has it
      case lookupEnv env (Left var) of
        -- and update
        Just ty -> (insertEnv env (Right id) ty, Var ty var)
        -- we wont always win
        Nothing -> (env, Var (TyVar id) var)
substVal env (Var ty var) = (insertEnv env (Left var) ty, Var ty var)
substVal env (Abs ty p mt e) =
  let (env', p') = substPat env p
      (env'', e') = substExpr env' e
      ty' = substTy env'' ty
   in (env'', Abs ty' p' mt e')
substVal env (Constr ty id vals) =
  let (env', vals') = mapAccumL substVal env vals
      ty' = substTy env' ty
   in (env', Constr ty' id vals')
substVal env (NumInt v) = (env, NumInt v)
substVal env (NumFloat v) = (env, NumFloat v)
substVal env (Promote t v) = (env, Promote t v)
substVal env val = error "TODO val"

substPat :: Env -> Pattern Type -> (Env, Pattern Type)
substPat env (PVar s (TyVar id) b var) =
  case lookupEnv env (Right id) of
    Just ty -> (env, PVar s ty b var)
    Nothing ->
      case lookupEnv env (Left var) of
        Just ty -> (insertEnv env (Right id) ty, PVar s ty b var)
        Nothing -> (env, PVar s (TyVar id) b var)
substPat env (PVar s ty b var) = (insertEnv env (Left var) ty, PVar s ty b var)
substPat env (PConstr s ty b id ids ps) =
  let (env', ps') = mapAccumL substPat env ps
      ty' = substTy env' ty
   in (env', PConstr s ty' b id ids ps')
substPat env p = error "TODO pat"

substTy :: Env -> Type -> Type
substTy env (TyVar id) = fromMaybe (TyVar id) (lookupEnv env (Right id))
substTy env (Type i) = Type i
substTy env (FunTy id mc arg ret) = FunTy id mc (substTy env arg) (substTy env ret)
substTy env (TyCon id) = TyCon id
substTy env (Box c t) = substTy env t
substTy env (Diamond e t) = Diamond (substTy env e) (substTy env t)
substTy env (Star g t) = substTy env t
substTy env (Borrow p t) = substTy env t
substTy env (TyApp t1 t2) = TyApp (substTy env t1) (substTy env t2)
substTy env (TyGrade mt i) = TyGrade mt i
substTy env (TyInfix op t1 t2) = TyInfix op (substTy env t1) (substTy env t2)
substTy env (TySet p ts) = TySet p (map (substTy env) ts)
substTy env (TyCase t tps) = TyCase (substTy env t) (map (bimap (substTy env) (substTy env)) tps)
substTy env (TySig t k) = TySig (substTy env t) (substTy env k)
substTy env (TyExists id k t) = substTy env t
substTy env (TyForall id k t) = substTy env t
substTy env t = t
