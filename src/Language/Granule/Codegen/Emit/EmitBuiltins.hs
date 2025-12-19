{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Granule.Codegen.Emit.EmitBuiltins where

import Control.Monad (forM)
import LLVM.AST (Operand)
import qualified LLVM.AST as IR
import qualified LLVM.AST.Constant as C
import LLVM.AST.Type hiding (resultType, Type)
import LLVM.IRBuilder.Constant (int32)
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad (IRBuilderT)
import Language.Granule.Codegen.Builtins.Builtins
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Codegen.Emit.LLVMHelpers
import Language.Granule.Codegen.Emit.LowerClosure (mallocEnvironment)
import Language.Granule.Codegen.Emit.LowerType (llvmType, llvmTypeForClosure, llvmTypeForFunction)
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type


emitBuiltins :: (MonadModuleBuilder m) => [(Id, Type)] -> m ()
emitBuiltins uses = mapM_ emitBuiltin (monos ++ polys)
  where
    monos =
      [b | (id, _) <- uses, b <- builtins, builtinId b == sourceName id]
    polys =
      [specialise b (internalName id) ty | (id, ty) <- uses, b <- specialisable, specialisableId b == sourceName id]

emitBuiltin :: (MonadModuleBuilder m) => Builtin -> m Operand
emitBuiltin builtin =
    let argTys = map llvmType (builtinArgTys builtin)
        retTy = llvmType (builtinRetTy builtin)
        impl = builtinImpl builtin
        envTy = StructureType False argTys
    in emitClosureChain (builtinId builtin) argTys retTy impl envTy 0

emitClosureChain ::
    MonadModuleBuilder m =>
    String ->
    [IR.Type] ->
    IR.Type ->
    ([Operand] -> IRBuilderT m Operand) ->
    IR.Type ->
    Int ->
    m Operand
emitClosureChain id argTys retTy impl envTy pos =
    case argTys of
        [] -> error "Cannot create closure chain with no args - use constant"
        [argTy] ->
            privateFunction (funcName id pos) [(ptr i8, "env"), (argTy, "x")] retTy $ \[env, x] -> do
                envPtr <- bitcast env (ptr envTy)
                prevArgs <- forM [0..pos-1] $ \i -> do
                    argPtr <- gep envPtr [IR.ConstantOperand $ intConstant 0, IR.ConstantOperand $ intConstant i]
                    load argPtr 4
                result <- impl (prevArgs ++ [x])
                ret result

        (argTy:restArgTys) -> do
            _ <- emitClosureChain id restArgTys retTy impl envTy (pos + 1)
            let nextRetTy = if length restArgTys == 1 then retTy else llvmTypeForClosure $ foldr llvmTypeForFunction retTy (tail restArgTys)
            let nextFunTy = llvmTypeForFunction (head restArgTys) nextRetTy
            let nextClosureTy = llvmTypeForClosure nextFunTy
            privateFunction (funcName id pos) [(ptr i8, "env"), (argTy, "x")] nextClosureTy $ \[env, x] -> do
                envPtr <- if pos == 0 then mallocEnvironment envTy else pure env
                envPtr' <- bitcast envPtr (ptr envTy)

                argPtr <- gep envPtr' [int32 0, int32 $ fromIntegral pos]
                store argPtr 4 x

                let functionPtr = C.GlobalReference (ptr nextFunTy) (funcName id (pos + 1))
                closure <- insertValue (IR.ConstantOperand $ C.Undef nextClosureTy) (IR.ConstantOperand functionPtr) [0]
                closure' <- insertValue closure envPtr [1]
                ret closure'

funcName :: String -> Int -> IR.Name
funcName id 0 = IR.mkName $ "fn." ++ id
funcName id x = IR.mkName $ "internal.fn." ++ id ++ show x
