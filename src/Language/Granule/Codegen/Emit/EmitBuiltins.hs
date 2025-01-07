{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Granule.Codegen.Emit.EmitBuiltins where

import Control.Monad (zipWithM)
import LLVM.AST (Operand)
import qualified LLVM.AST as IR
import qualified LLVM.AST.Constant as C
import LLVM.AST.Name
import LLVM.AST.Type hiding (Type)
import LLVM.IRBuilder (MonadIRBuilder)
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad (IRBuilderT)
import Language.Granule.Codegen.Builtins
import Language.Granule.Codegen.Emit.LLVMHelpers
import Language.Granule.Codegen.Emit.LowerClosure (mallocEnvironment)
import Language.Granule.Codegen.Emit.LowerType (llvmType, llvmTypeForClosure, llvmTypeForFunction)
import Language.Granule.Codegen.Emit.Names (functionNameFromId)
import Language.Granule.Syntax.Identifiers

-- TODO: only emit builtins as required
emitBuiltins :: MonadModuleBuilder m => m [Operand]
emitBuiltins = mapM emitBuiltin builtins

emitBuiltin :: (MonadModuleBuilder m) => Builtin -> m Operand
emitBuiltin builtin =
    let argTys = map llvmType (builtinArgTys builtin)
        retTy = llvmType (builtinRetTy builtin)
        impl = builtinImpl builtin
    in emitClosureChain (builtinId builtin) argTys retTy impl

emitClosureChain ::
    MonadModuleBuilder m =>
    Id ->
    [IR.Type] ->
    IR.Type ->
    ([Operand] -> IRBuilderT m Operand) ->
    m Operand
emitClosureChain id argTys retTy impl = case argTys of
    [] -> error "Cannot create closure chain with no arguments"
    [argTy] -> do
        let name = functionNameFromId id
        privateFunction name [(ptr i8, "env"), (argTy, "x")] retTy $ \[env, x] -> do
            previousArgs <- loadPreviousArgs env (init argTys)
            result <- impl (previousArgs ++ [x])
            ret result
    -- TODO: need further work >2 args
    (argTy:argTys) -> do
        let innerName = mkName $ internalName id ++ ".arg" ++ show (length argTys)
        let envTy = StructureType False [argTy]
        _ <- privateFunction innerName [(ptr i8, "env"), (head argTys, "y")] retTy $ \[env, y] -> do
            typedEnv <- bitcast env (ptr argTy)
            x <- load typedEnv 4
            result <- impl [x, y]
            ret result

        emitBuiltinClosure id innerName envTy argTys retTy

loadPreviousArgs :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> [IR.Type] -> m [Operand]
loadPreviousArgs env = zipWithM (\i ty -> do
        envPtr <- bitcast env (ptr ty)
        load envPtr 4) [0..]

emitBuiltinClosure ::
    (MonadModuleBuilder m) =>
    Id ->
    Name ->
    IR.Type ->
    [IR.Type] ->
    IR.Type ->
    m Operand
emitBuiltinClosure id innerName envTy argTys retTy = do
    let nextFunTy = foldr llvmTypeForFunction retTy argTys
    let nextClosureTy = llvmTypeForClosure nextFunTy
    privateFunction
        (functionNameFromId id)
        [(ptr i8, "env"), (head argTys, "x")] nextClosureTy $ \[env, x] -> do
            envVoidPtr <- mallocEnvironment envTy
            envTypedPtr <- bitcast envVoidPtr (ptr envTy)
            addr <- gep envTypedPtr [IR.ConstantOperand $ intConstant 0, IR.ConstantOperand $ intConstant 0]
            store addr 4 x
            let functionPtr = C.GlobalReference (ptr nextFunTy) innerName
            closure <- insertValue (IR.ConstantOperand $ C.Undef nextClosureTy) (IR.ConstantOperand functionPtr) [0]
            closure' <- insertValue closure envVoidPtr [1]
            ret closure'
