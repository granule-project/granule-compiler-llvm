{-# LANGUAGE LambdaCase #-}
module Language.Granule.Codegen.ConvertClosures where

import Language.Granule.Codegen.ClosureFreeDef
import Language.Granule.Codegen.NormalisedDef
import Language.Granule.Codegen.MarkGlobals
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Pattern
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type
import Language.Granule.Syntax.Span

import Data.Set (Set)
import Data.List (findIndex)
import Data.Maybe (fromJust, fromMaybe)
import Data.Bifunctor.Foldable
import Data.Bifoldable
import qualified Data.Set as Set

import Control.Monad.State
import Control.Monad.Writer

convertClosures :: NormalisedAST GlobalMarker Type -> ClosureFreeAST
convertClosures (NormalisedAST dataDecl functionDefs valueDefs) =
    let globals = (functionDefIdentifier <$> functionDefs) ++ (valueDefIdentifier <$> valueDefs)
        ((functionDefs', valueDefs'), lambdaDefs) =
            evalLiftLambda $ do
                values <- mapM (convertClosuresInValueDef globals) valueDefs
                functions <- mapM (convertClosuresInFunctionDef globals) functionDefs
                return (functions, values)
    in ClosureFreeAST dataDecl (functionDefs' ++ lambdaDefs) valueDefs'

convertClosuresInFunctionDef :: [Id] -> FunctionDef GlobalMarker Type -> LiftLambdaM ClosureFreeFunctionDef
convertClosuresInFunctionDef globals (FunctionDef sp ident body arg ts) =
    do
        let locals = boundVars arg
        body' <- convertClosuresInExpression globals locals body
        return $ ClosureFreeFunctionDef sp ident Nothing body' arg ts

convertClosuresInValueDef :: [Id] -> ValueDef GlobalMarker Type -> LiftLambdaM ClosureFreeValueDef
convertClosuresInValueDef globals (ValueDef sp ident initExpr ts) =
    do
        initExpr' <- convertClosuresInExpression globals [] initExpr
        return $ ValueDef sp ident initExpr' ts

type LiftLambdaM a = StateT Int (Writer [ClosureFreeFunctionDef]) a

evalLiftLambda :: LiftLambdaM a -> (a, [ClosureFreeFunctionDef])
evalLiftLambda s = runWriter $ evalStateT s 0

convertClosuresInExpression :: [Id]
                            -> [Id]
                            -> Expr GlobalMarker Type
                            -> LiftLambdaM ClosureFreeExpr
convertClosuresInExpression globals locals =
    bicataPM (convertClosuresFromExpr, boundFromExpr)
             (convertClosuresFromValue, boundFromValue)
             (Nothing, Nothing, locals)
    where boundFromExpr (Case _ _ _ _ arms) (x, y, bound) =
              (x, y, bound ++ boundByArms arms)
          boundFromExpr _ bound = bound
          boundFromValue (Abs _ arg _ body) (_, parentEnvironment, parentLocals) =
              let locals           = boundVars arg
                  initializer      = environmentInitializer parentEnvironment parentLocals locals body
                  maybeInitializer = if null initializer then Nothing else Just initializer
              in (parentEnvironment, maybeInitializer, locals)
          boundFromValue _ ctx = ctx

environmentInitializer :: Maybe [ClosureVariableInit]
                       -> [Id]
                       -> [Id]
                       -> Expr GlobalMarker Type
                       -> [ClosureVariableInit]
environmentInitializer parentEnvironment parentLocals locals expr =
    let capturedVars = Set.toList (captures locals expr)
    in map (variableInitializer parentLocals parentEnvironment) capturedVars

variableInitializer :: [Id]
                    -> Maybe [ClosureVariableInit]
                    -> (Type, Id)
                    -> ClosureVariableInit
variableInitializer locals _ (ty, ident)
    | ident `elem` locals =
        FromLocalScope ident ty
variableInitializer _ (Just parentEnvironment) (ty, ident) =
    case findCaptureIndex parentEnvironment ident of
        Just n -> FromParentEnv ident ty n
        Nothing -> error $ "Attempt to capture " ++ show ident ++
                           " which is not in the parent environment or local scope."
variableInitializer locals parentEnv var =
    error $ "Invalid combination of arguments to generate initializer \n"
            ++ show locals ++ "\n" ++ show parentEnv ++ "\n" ++ show var

findCaptureIndex :: [ClosureVariableInit] -> Id -> Maybe Int
findCaptureIndex env ident =
    findIndex hasIdent env
    where hasIdent (FromParentEnv i _ _) = ident == i
          hasIdent (FromLocalScope i _)  = ident == i

captures :: [Id] -> Expr GlobalMarker Type -> Set (Type, Id)
captures = bicataP (capturesInExpr, accumBoundExpr) (capturesInValue, accumBoundValue)
    where capturesInExpr _ expr = bifold expr
          capturesInValue bound (VarF ty ident)
              | ident `elem` bound = Set.empty
              | otherwise = Set.singleton (ty, ident)
          capturesInValue _ val = bifold val
          accumBoundValue (Abs _ arg _ expr) bound = bound ++ boundVars arg
          -- NOTE: This marks all names bound by every match as bound in every arm.
          -- Which is not technically correct but should be ok because of the
          -- freshener.
          accumBoundValue _ bound = bound
          accumBoundExpr (Case _ _ _ _ arms) bound = bound ++ boundByArms arms
          accumBoundExpr (LetDiamond _ _ _ pat _ _ _) bound = bound ++ boundVars pat
          accumBoundExpr _ bound = bound


boundByArms :: [(Pattern a, b)] -> [Id]
boundByArms = concatMap (boundVars . fst)


convertClosuresFromExpr :: (Maybe [ClosureVariableInit], Maybe [ClosureVariableInit], [Id])
                        -> ExprF GlobalMarker Type ClosureFreeExpr ClosureFreeValue
                        -> LiftLambdaM ClosureFreeExpr
convertClosuresFromExpr _ expr =
    return $ fixMapExtExpr expr

freshLambdaIdentifiers :: LiftLambdaM (Id, String)
freshLambdaIdentifiers =
    do
        index <- get
        modify (+1)
        let lambdaName = "lambda#" ++ show index
        let envName = "env." ++ lambdaName
        return (mkId lambdaName, envName)

environmentType :: String
                -> Maybe [ClosureVariableInit]
                -> Maybe NamedClosureEnvironmentType
environmentType name maybeVariableInitializers =
    maybeVariableInitializers >>=
        \case
            [] -> Nothing
            variableInitializers ->
                let types = map closureVariableInitType variableInitializers
                in Just (name, TyClosureEnvironment types)

convertClosuresFromValue :: (Maybe [ClosureVariableInit], Maybe [ClosureVariableInit], [Id])
                         -> ValueF GlobalMarker Type ClosureFreeValue ClosureFreeExpr
                         -> LiftLambdaM ClosureFreeValue
convertClosuresFromValue (_, maybeCurrentEnv, _) (AbsF ty@FunTy {} arg mty expr) =
    do
        (lambdaIdent, envName) <- freshLambdaIdentifiers
        let lambdaTypeScheme = Forall nullSpanNoFile [] [] ty
        let envTy = environmentType envName maybeCurrentEnv
        let lambdaDef = ClosureFreeFunctionDef nullSpanNoFile lambdaIdent envTy expr arg lambdaTypeScheme
        lift $ tell [lambdaDef]
        return $ Ext ty $ Right $
            case maybeCurrentEnv of
                Just env -> MakeClosure lambdaIdent (ClosureEnvironmentInit envName env)
                Nothing  -> MakeTrivialClosure lambdaIdent

convertClosuresFromValue (_, maybeCurrentEnv, locals) (VarF ty ident)
    | ident `notElem` locals =
        let currentEnv = fromJust maybeCurrentEnv
            indexInEnv = fromMaybe errorMessage (findCaptureIndex currentEnv ident)
                         where errorMessage = error $ "Could not find captured variable "
                                              ++ sourceName ident ++ " in environment."
        in return $ Ext ty (Right (CapturedVar ty ident indexInEnv))

convertClosuresFromValue _ other =
    return $ fixMapExtValue (\ty gv -> Ext ty $ Left gv) other
