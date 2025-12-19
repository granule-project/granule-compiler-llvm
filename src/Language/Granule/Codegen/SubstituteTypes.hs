module Language.Granule.Codegen.SubstituteTypes where

import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import Debug.Trace
import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Pattern
import Language.Granule.Syntax.Type

-- Many Typed ASTs contain unnecessary TyVars
-- There are a few strategies to substitue these
-- 1. From parent / sibling node i.e.
--      Val (concrete) (Var (variable)),
--      App (variable) (Val (variable -> concrete)) (Val (concrete))
--      etc.
-- 2. From value bindings
--  i.e.
--      Somewhere:      Var (concrete) x
--      Somewhere else: Var (variable) x
-- 3. From type bindings
--  i.e.
--      Somewhere:      Val (concrete) (Var (x)) (using )
--      Somewhere else: Val (x) (Var (variable)) ()
-- We do 1 and 3 here
--
-- TODO: clean up, do in 1 pass if possible
substituteTypes :: AST ev Type -> AST ev Type
substituteTypes ast = ast {definitions = map retypeDef (definitions ast)}
  where
    retypeDef def = def {defEquations = retypeEquationList (defEquations def)}
    retypeEquationList eqs = eqs {equations = map retypeEquation (equations eqs)}
    retypeEquation eq =
      let expr = equationBody eq
          substs = collect expr
          expr' = replace substs expr
       in eq {equationBody = expr'}

type VMap = Map.Map Id Type

diff :: Map.Map Id Type -> Type -> Type -> Map.Map Id Type
diff env t1 t2 = case (t1, t2) of
  (TyVar v1, TyVar v2) ->
    case (Map.lookup v1 env, Map.lookup v2 env) of
      (Nothing, Nothing) -> env
      (Nothing, Just t2') -> Map.insert v1 t2' env
      (Just t1', Nothing) -> Map.insert v2 t1' env
      (Just t1', Just t2') -> diff env t1' t2'
  (TyVar v1, t2) ->
    case Map.lookup v1 env of
      Nothing -> Map.insert v1 t2 env
      Just t1 -> diff env t1 t2
  (t1, TyVar v2) ->
    case Map.lookup v2 env of
      Nothing -> Map.insert v2 t1 env
      Just t2 -> diff env t1 t2
  (FunTy _ _ a b, FunTy _ _ a' b') -> diff (diff env b b') a a'
  (TyApp a b, TyApp a' b') -> diff (diff env b b') a a'
  (Box _ a, Box _ a') -> diff env a a'
  (Diamond _ a, Diamond _ a') -> diff env a a'
  (Star _ a, Star _ a') -> diff env a a'
  (Borrow _ a, Borrow _ a') -> diff env a a'
  (TySig a _, TySig a' _) -> diff env a a'
  (TyInfix _ a b, TyInfix _ a' b') -> diff (diff env b b') a a'
  (TyExists _ _ a, TyExists _ _ a') -> diff env a a'
  (TyForall _ _ a, TyForall _ _ a') -> diff env a a'
  (TyCase a as, TyCase a' as') ->
    foldl
      (\e ((p1, r1), (p2, r2)) -> diff (diff e p1 p2) r1 r2)
      (diff env a a')
      (zip as as')
  (TySet _ ts, TySet _ ts') ->
    foldl
      (\e (t, t') -> diff e t t')
      env
      (zip ts ts')
  (TyGrade (Just t) _, TyGrade (Just t') _) -> diff env t t'
  _ -> env

collect :: Expr ev Type -> Map.Map Id Type
collect expr =
  let (env', _) = inExpr Map.empty expr
   in let (env'', _) = inExpr env' expr
       in env''
  where
    inExpr :: Map.Map Id Type -> Expr ev Type -> (Map.Map Id Type, Type)
    inExpr env expr =
      case expr of
        App s retTy r f arg ->
          let (env', argTy) = inExpr env arg
              (env'', fTy) = inExpr env' f
              env''' = diff env'' (FunTy Nothing Nothing argTy retTy) fTy
           in (env''', retTy)
        Val s ty r val ->
          let (env', ty') = inVal env val
              env'' = diff env' ty ty'
           in (env'', ty)
        Binop s ty r op e1 e2 -> (fst $ inExpr (fst $ inExpr env e2) e1, ty)
        Case s ty r e ps ->
          ( foldl
              (\env (p, e) -> fst $ inExpr (fst $ inPat env p) e)
              (fst $ inExpr env e)
              ps,
            ty
          )
        AppTy s ty r e t -> error "TODO: AppTy"
        LetDiamond s ty r ps mt e1 e2 -> error "TODO: LetDiamond"
        TryCatch s ty r e p mt e1 e2 -> error "TODO: TryCatch"
        Unpack s ty r tyVar var e1 e2 ->
          let (env', _) = inExpr env e1
              (env'', retTy) = inExpr env' e2
              env''' = diff env'' ty retTy
           in (env''', ty)
        Hole s ty r ids hs -> error "TODO: Hole"

    inVal :: Map.Map Id Type -> Value ev Type -> (Map.Map Id Type, Type)
    inVal env val =
      case val of
        Abs funTy p mt e ->
          let (env', argTy) = inPat env p
              (env'', retTy) = inExpr env' e
              env''' = diff env'' (FunTy Nothing Nothing argTy retTy) funTy
           in (env''', funTy)
        Constr a id vs ->
          (foldl (\env v -> fst $ inVal env v) env vs, a)
        Promote a e -> (fst $ inExpr env e, a)
        Pure a e -> (fst $ inExpr env e, a)
        Nec a e -> error "TODO: Nec"
        Pack s a ty e v k ty' ->
          let (env', eTy) = inExpr env e

              env'' = diff env' (TyExists v k eTy) a
           in trace
                (show a)
                trace
                (show (TyExists v k eTy))
                trace
                ""
                (env'', a)
        TyAbs a v e -> error "TODO: TyAbs"
        NumInt n -> (env, TyCon (Id "Int" "Int"))
        NumFloat n -> (env, TyCon (Id "Float" "Float"))
        CharLiteral ch -> (env, TyCon (Id "Char" "Char"))
        StringLiteral str -> (env, TyCon (Id "String" "String"))
        Ext a ev -> error "TODO: Ext"
        Var a id -> (env, a)

    inPat :: Map.Map Id Type -> Pattern Type -> (Map.Map Id Type, Type)
    inPat env pat =
      case pat of
        PVar s ty r id -> (env, ty)
        PWild s ty r -> (env, ty)
        PBox s ty r pat' -> (fst $ inPat env pat', ty)
        PInt s ty r i -> (env, ty)
        PFloat s ty r f -> (env, ty)
        PConstr s ty r id ids ps -> (foldl (\env p -> fst $ inPat env p) env ps, ty)

replace :: Map.Map Id Type -> Expr ev Type -> Expr ev Type
replace env expr = inExpr env expr
  where
    inExpr :: Map.Map Id Type -> Expr ev Type -> Expr ev Type
    inExpr env expr =
      case expr of
        App s ty r f arg ->
          let ty' = inTy env ty
              arg' = inExpr env arg
              f' = inExpr env f
           in App s ty' r f' arg'
        Val s ty r val ->
          let ty' = inTy env ty
              val' = inVal env val
           in Val s ty' r val'
        Binop s ty r op e1 e2 ->
          let ty' = inTy env ty
              e2' = inExpr env e2
              e1' = inExpr env e1
           in Binop s ty' r op e1' e2'
        Case s ty r e ps ->
          let ty' = inTy env ty
              e' = inExpr env e
              ps' = map (bimap (inPat env) (inExpr env)) ps
           in Case s ty' r e' ps'
        AppTy s ty r e t -> error "TODO: AppTy"
        LetDiamond s ty r ps mt e1 e2 -> error "TODO: LetDiamond"
        TryCatch s ty r e p mt e1 e2 -> error "TODO: TryCatch"
        Unpack s ty r tyVar var e1 e2 ->
          let ty' = inTy env ty
              e1' = inExpr env e1
              e2' = inExpr env e2
           in Unpack s ty' r tyVar var e1' e2'
        Hole s ty r ids hs -> error "TODO: Hole"

    inVal :: Map.Map Id Type -> Value ev Type -> Value ev Type
    inVal env val =
      case val of
        Abs a pat mt e ->
          let a' = inTy env a
              pat' = inPat env pat
              e' = inExpr env e
           in Abs a' pat' mt e'
        Constr a id vs ->
          let a' = inTy env a
              vs' = map (inVal env) vs
           in Constr a' id vs'
        Promote a e ->
          let a' = inTy env a
              e' = inExpr env e
           in Promote a' e'
        Pure a e ->
          let a' = inTy env a
              e' = inExpr env e
           in Pure a' e'
        Nec a e -> error "TODO: Nec"
        Pack s a t1 e v k t2 ->
          let a' = inTy env a
              t1' = inTy env t1
              e' = inExpr env e
              t2' = inTy env t2
           in Pack s a' t1' e' v k t2'
        TyAbs a v e -> error "TODO: TyAbs"
        NumInt n -> val
        NumFloat n -> val
        CharLiteral ch -> val
        StringLiteral str -> val
        Ext a ev -> error "TODO: Ext"
        Var a id -> Var (inTy env a) id

    inPat :: Map.Map Id Type -> Pattern Type -> Pattern Type
    inPat env pat =
      case pat of
        PVar s ty r id -> PVar s (inTy env ty) r id
        PWild s ty r -> PWild s (inTy env ty) r
        PBox s ty r p -> PBox s (inTy env ty) r (inPat env p)
        PInt s ty r i -> PInt s (inTy env ty) r i
        PFloat s ty r f -> PFloat s (inTy env ty) r f
        PConstr s ty r id ids ps -> PConstr s (inTy env ty) r id ids (map (inPat env) ps)

    inTy :: Map.Map Id Type -> Type -> Type
    inTy env ty =
      case ty of
        TyVar id ->
          case Map.lookup id env of
            Nothing -> ty
            Just ty' -> inTy env ty'
        Type i -> Type i
        FunTy id mc arg ret -> FunTy id mc (inTy env arg) (inTy env ret)
        Box c t -> Box c (inTy env t)
        Diamond e t -> Diamond e (inTy env t)
        Star g t -> Star g (inTy env t)
        Borrow p t -> Borrow p (inTy env t)
        TyApp t1 t2 -> TyApp (inTy env t1) (inTy env t2)
        TyInfix op t1 t2 -> TyInfix op (inTy env t1) (inTy env t2)
        TyCase t tys -> TyCase (inTy env t) (map (bimap (inTy env) (inTy env)) tys)
        TySig t k -> TySig (inTy env t) k
        TyExists id k t -> TyExists id k (inTy env t)
        TyForall id k t -> TyForall id k (inTy env t)
        TyGrade (Just t) i -> TyGrade (Just (inTy env t)) i
        TySet p ts -> TySet p (map (inTy env) ts)
        TyCon {} -> ty
        TyInt {} -> ty
        TyRational {} -> ty
        TyFraction {} -> ty
        TyName {} -> ty
        TyGrade {} -> ty
