{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Shared where

import LLVM.AST
import LLVM.AST.Type as IR
import LLVM.IRBuilder.Constant (bit, int32)
import LLVM.IRBuilder.Instruction (bitcast, call, gep, load, store, insertValue, mul, sext)
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import Language.Granule.Codegen.Emit.LLVMHelpers (sizeOf)
import Language.Granule.Codegen.Emit.Primitives as P
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type as Gr
import qualified LLVM.AST.Constant as C

data Builtin = Builtin {
    builtinId :: String,
    builtinArgTys :: [Gr.Type],
    builtinRetTy :: Gr.Type,
    builtinImpl :: forall m. (MonadModuleBuilder m, MonadIRBuilder m) => [Operand] -> m Operand}

-- LLVM helpers

allocate :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> IR.Type -> m Operand
allocate len ty = do
  pointer <- call (ConstantOperand P.malloc) [(len, [])]
  bitcast pointer (ptr ty)

allocateStruct :: (MonadIRBuilder m, MonadModuleBuilder m) => IR.Type -> m Operand
allocateStruct ty = allocate (ConstantOperand $ sizeOf ty) ty

allocateFloatArray :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
allocateFloatArray len = do
  dataSize <- mul len (int32 8)
  dataSize64 <- sext dataSize i64
  allocate dataSize64 IR.double

copy :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Operand -> Operand -> m Operand
copy dst src len = do
  dst' <- bitcast dst (ptr i8)
  src' <- bitcast src (ptr i8)
  call (ConstantOperand P.memcpy) [(dst', []), (src', []), (len, []), (bit 0, [])]

free :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
free memPtr = do
  memPtr' <- bitcast memPtr (ptr i8)
  call (ConstantOperand P.free) [(memPtr', [])]

makePair :: (MonadIRBuilder m, MonadModuleBuilder m) => (IR.Type, Operand) -> (IR.Type, Operand) -> m Operand
makePair (leftTy, leftVal) (rightTy, rightVal) = do
  let pairTy = StructureType False [leftTy, rightTy]
  let pair = ConstantOperand $ C.Undef pairTy
  pair' <- insertValue pair leftVal [0]
  insertValue pair' rightVal [1]

-- Arrays

arrayStruct :: IR.Type -> IR.Type
arrayStruct ty = StructureType False [i32, ptr ty]

floatArrayStruct :: IR.Type
floatArrayStruct = arrayStruct IR.double

getArrayLen :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
getArrayLen arrPtr = do
  lenField <- gep arrPtr [int32 0, int32 0]
  load lenField 0

getArrayDataPtr ::  (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
getArrayDataPtr arrPtr = do
  dataField <- gep arrPtr [int32 0, int32 1]
  load dataField 0

makeArray :: (MonadIRBuilder m, MonadModuleBuilder m) => IR.Type -> Operand -> Operand -> m Operand
makeArray ty len dataPtr = do
  arrPtr <- allocateStruct (arrayStruct ty)

  lenField <- gep arrPtr [int32 0, int32 0]
  store lenField 0 len

  dataField <- gep arrPtr [int32 0, int32 1]
  store dataField 0 dataPtr

  return arrPtr

-- Granule types

mkFunType :: [Gr.Type] -> Gr.Type -> Gr.Type
mkFunType args ret = foldr (FunTy Nothing Nothing) ret args

tyInt :: Gr.Type
tyInt = TyCon (Id "Int" "Int")

tyFloat :: Gr.Type
tyFloat = TyCon (Id "Float" "Float")

tyChar :: Gr.Type
tyChar = TyCon (Id "Char" "Char")

tyUnit :: Gr.Type
tyUnit = TyCon (Id "()" "()")

tyPair :: (Gr.Type, Gr.Type) -> Gr.Type
tyPair (l, r) = TyApp (TyApp (TyCon (Id "," ",")) l) r

tyFloatArray :: Gr.Type
tyFloatArray = TyApp (TyCon (Id "FloatArray" "FloatArray")) (TyVar (Id "id" "id"))
