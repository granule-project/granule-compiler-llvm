{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Language.Granule.Codegen.Emit.EmitLLVM where

import qualified LLVM.AST as IR

import LLVM.AST (Operand, mkName)
import LLVM.AST.Constant (Constant(..))
import LLVM.AST.Type hiding (Type)
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import Language.Granule.Codegen.Emit.Types (GrType, IrType)
import Language.Granule.Codegen.Emit.LLVMHelpers
import Language.Granule.Codegen.Emit.EmitableDef
import Language.Granule.Codegen.Emit.EmitterState
import Language.Granule.Codegen.Emit.EmitBuiltins (emitBuiltins)
import Language.Granule.Codegen.Emit.Names
import Language.Granule.Codegen.Emit.LowerClosure (emitEnvironmentType, emitTrivialClosure)
import Language.Granule.Codegen.Emit.LowerType (llvmTopLevelType, llvmType)
import Language.Granule.Codegen.Emit.LowerExpression (emitExpression)
import Language.Granule.Codegen.Emit.MainOut (emitMainOut, findMainReturnType, loadMainValue, mainOut)

import Language.Granule.Codegen.ClosureFreeDef
import Language.Granule.Codegen.NormalisedDef

import Language.Granule.Syntax.Pattern (boundVars, Pattern(..))
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Def (DataDecl)
import Language.Granule.Syntax.Type hiding (Type)

import Data.String (fromString)
import qualified Data.Map.Strict as Map

import Control.Monad.Fix
import Control.Monad.State.Strict hiding (void)
import LLVM.IRBuilder (int32)

emitLLVM :: String -> ClosureFreeAST -> Either String IR.Module
emitLLVM moduleName (ClosureFreeAST dataDecls functionDefs valueDefs) =
    let buildModule name m = evalState (buildModuleT name m) (EmitterState { localSymbols = Map.empty })
    in Right $ buildModule (fromString moduleName) $ do
        _ <- extern (mkName "malloc") [i64] (ptr i8)
        _ <- extern (mkName "abort") [] void
        _ <- externVarArgs (mkName "printf") [ptr i8] i32
        _ <- extern (mkName "llvm.memcpy.p0.p0.i32") [ptr i8, ptr i8, i32, i1] void
        _ <- extern (mkName "free") [ptr i8] void
        _ <- emitBuiltins
        let mainTy = findMainReturnType valueDefs
        _ <- emitMainOut mainTy
        mapM_ emitDataDecl dataDecls
        mapM_ emitEnvironmentType functionDefs
        mapM_ emitFunctionDef functionDefs
        valueInitPairs <- mapM emitValueDef valueDefs
        emitGlobalInitializer valueInitPairs mainTy

emitGlobalInitializer :: (MonadModuleBuilder m) => [(Operand, Operand)] -> GrType -> m Operand
emitGlobalInitializer valueInitPairs mainTy =
    function (mkName "main") [] i32 $ \[] -> do
        mapM_ (\(global, initializer) -> do
            value <- call initializer []
            store global 4 value) valueInitPairs
        mainValue <- loadMainValue mainTy
        _ <- call (IR.ConstantOperand $ mainOut mainTy) [(mainValue, [])]
        ret (int32 0)

emitValueDef :: MonadState EmitterState m
             => MonadModuleBuilder m
             => MonadFix m
             => ClosureFreeValueDef
             -> m (Operand, Operand)
emitValueDef def@(ValueDef sp ident initExpr typeScheme) =
    do
        clearLocals
        let name = definitionNameFromId ident
        let valueType = llvmTopLevelType (type_ def)
        let initializerName = mkName $ "init." ++ internalName ident
        initializer <- privateFunction initializerName [] valueType $ \[] -> do
            returnValue <- emitExpression Nothing initExpr
            ret returnValue
        value <- global name valueType (Undef valueType)
        return (value, initializer)
    where
      type_ ValueDef { valueDefTypeScheme = (Forall _ _ _ ty) } = ty

maybeEnvironment :: Maybe NamedClosureEnvironmentType -> Maybe IrType
maybeEnvironment = fmap (\(name, _) -> NamedTypeReference (mkName name))

emitFunctionDef :: (MonadState EmitterState m, MonadModuleBuilder m, MonadFix m)
                => ClosureFreeFunctionDef
                -> m (Operand, Operand)
emitFunctionDef def@(ClosureFreeFunctionDef sp ident environment body argument typeScheme) =
    do
        clearLocals
        let maybeEnvironmentType = maybeEnvironment environment -- maybeEmitEnvironmentType environment
        function <- emitFunction ident maybeEnvironmentType body argument (type_ def)
        trivialClosure <- emitTrivialClosure (ident, type_ def)
        return (trivialClosure, function)
    where
      type_ ClosureFreeFunctionDef { closureFreeDefTypeScheme = (Forall _ _ _ ty) } = ty

emitFunction :: (MonadState EmitterState m, MonadModuleBuilder m, MonadFix m)
             => Id
             -> Maybe IrType
             -> EmitableExpr
             -> Pattern GrType
             -> GrType
             -> m Operand
emitFunction ident maybeEnvironmentType body argument (FunTy _ _ from to) =
    do
        let (parameterType, returnType) = (llvmType from, llvmType to)
        let parameter = (parameterType, paramName argument)
        let environmentParameter = (ptr i8, mkPName "env")
        privateFunction
            (functionNameFromId ident)
            [environmentParameter, parameter] returnType $ \[env, param] -> do
                emitArg argument param
                typedEnvrionmentPointer <- maybeBitcastEnvironment env maybeEnvironmentType
                returnValue <- emitExpression typedEnvrionmentPointer body
                ret returnValue
emitFunction _ _ _ _ _ = error "cannot emit function with non function type"

paramName :: Pattern GrType -> ParameterName
paramName (PConstr _ _ _ (Id "," _) _ _) = parameterNameFromId $ mkId "pair"
paramName (PConstr _ _ _ (Id "()" _) _ _) = parameterNameFromId $ mkId "unit"
paramName pat = case boundVars pat of
  [] -> parameterNameFromId $ mkId "wildcard"
  (var : _) -> parameterNameFromId var

emitArg :: (MonadState EmitterState m, MonadModuleBuilder m, MonadIRBuilder m)
          => Pattern GrType
          -> Operand
          -> m ()
emitArg (PVar _ _ _ var) param = addLocal var param
emitArg (PWild {}) param = pure ()
emitArg (PBox _ _ _ pat) param = emitArg pat param
emitArg (PConstr _ _ _ (Id "," _) _ [lpat, rpat]) param = do
  left <- extractValue param [0]
  emitArg lpat left
  rightV <- extractValue param [1]
  emitArg rpat rightV
emitArg (PConstr _ _ _ (Id "()" _) _ []) param = pure ()
emitArg p _ = error ("Unsupported param pattern:\n" ++ show p)

maybeBitcastEnvironment :: (MonadIRBuilder m)
                        => Operand
                        -> Maybe IrType
                        -> m (Maybe Operand)
maybeBitcastEnvironment environmentPointerUntyped =
    traverse emitBitcast
    where
      emitBitcast environmentType =
        bitcast environmentPointerUntyped (ptr environmentType)

emitDataDecl :: {-(MonadModuleBuilder m) =>-} DataDecl -> m ()
emitDataDecl = error "Cannot emit data decls yet!"
