module Language.Granule.Codegen.RetypeAST where

import Data.Bifunctor (bimap)
import Data.List (mapAccumL)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Identifiers (Id)
import Language.Granule.Syntax.Pattern (Pattern (..))
import Language.Granule.Syntax.Type

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

retypeAST :: AST ev Type -> AST ev Type
retypeAST ast = ast {definitions = map retypeDef (definitions ast)}
  where
    retypeDef def = def {defEquations = retypeEquationList (defEquations def)}
    retypeEquationList eqs = eqs {equations = map retypeEquation (equations eqs)}
    retypeEquation eq = eq {equationBody = snd (retypeExpr emptyEnv (equationBody eq))}

retypeExpr :: Env -> Expr ev Type -> (Env, Expr ev Type)
retypeExpr env (App s ty b e1 e2) =
  let (env', e2') = retypeExpr env e2
      (env'', e1') = retypeExpr env' e1
      ty' = subsTy env ty
   in (env'', App s ty' b e1' e2')
retypeExpr env (Val s ty b v) =
  let (env', v') = retypeVal env v
      ty' = subsTy env' ty
   in (env', Val s ty' b v')
retypeExpr env exp = error "TODO expr"

retypeVal :: Env -> Value ev Type -> (Env, Value ev Type)
retypeVal env (Var (TyVar id) var) =
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
retypeVal env (Var ty var) = (insertEnv env (Left var) ty, Var ty var)
retypeVal env (Abs ty p mt e) =
  let (env', p') = retypePat env p
      (env'', e') = retypeExpr env' e
      ty' = subsTy env'' ty
   in (env'', Abs ty' p' mt e')
retypeVal env (Constr ty id vals) =
  let (env', vals') = mapAccumL retypeVal env vals
      ty' = subsTy env' ty
   in (env', Constr ty' id vals')
retypeVal env (NumInt v) = (env, NumInt v)
retypeVal env (NumFloat v) = (env, NumFloat v)
retypeVal env (Promote t v) = (env, Promote t v)
retypeVal env val = error "TODO val"

retypePat :: Env -> Pattern Type -> (Env, Pattern Type)
retypePat env (PVar s (TyVar id) b var) =
  case lookupEnv env (Right id) of
    Just ty -> (env, PVar s ty b var)
    Nothing ->
      case lookupEnv env (Left var) of
        Just ty -> (insertEnv env (Right id) ty, PVar s ty b var)
        Nothing -> (env, PVar s (TyVar id) b var)
retypePat env (PVar s ty b var) = (insertEnv env (Left var) ty, PVar s ty b var)
retypePat env (PConstr s ty b id ids ps) =
  let (env', ps') = mapAccumL retypePat env ps
      ty' = subsTy env' ty
   in (env', PConstr s ty' b id ids ps')
retypePat env p = error "TODO pat"

subsTy :: Env -> Type -> Type
subsTy env (TyVar id) = fromMaybe (TyVar id) (lookupEnv env (Right id))
subsTy env (Type i) = Type i
subsTy env (FunTy id mc arg ret) = FunTy id mc (subsTy env arg) (subsTy env ret)
subsTy env (TyCon id) = TyCon id
subsTy env (Box c t) = subsTy env t
subsTy env (Diamond e t) = Diamond (subsTy env e) (subsTy env t)
subsTy env (Star g t) = subsTy env t
subsTy env (Borrow p t) = subsTy env t
subsTy env (TyApp t1 t2) = TyApp (subsTy env t1) (subsTy env t2)
subsTy env (TyGrade mt i) = TyGrade mt i
subsTy env (TyInfix op t1 t2) = TyInfix op (subsTy env t1) (subsTy env t2)
subsTy env (TySet p ts) = TySet p (map (subsTy env) ts)
subsTy env (TyCase t tps) = TyCase (subsTy env t) (map (bimap (subsTy env) (subsTy env)) tps)
subsTy env (TySig t k) = TySig (subsTy env t) (subsTy env k)
subsTy env (TyExists id k t) = subsTy env t
subsTy env (TyForall id k t) = subsTy env t
subsTy env t = t
