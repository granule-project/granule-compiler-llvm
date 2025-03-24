{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Shared where

import LLVM.AST
import LLVM.AST.Type as IR
import LLVM.IRBuilder.Constant (bit, int32)
import LLVM.IRBuilder.Instruction (bitcast, call, gep, load, store, insertValue)
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

writeStruct :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Int -> Operand -> m ()
writeStruct struct index value = do
  field <- gep struct [int32 0, int32 $ fromIntegral index]
  store field 0 value

readStruct :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Int -> m Operand
readStruct struct index = do
  field <- gep struct [int32 0, int32 $ fromIntegral index]
  load field 0

-- Arrays

arrayStruct :: IR.Type -> IR.Type
arrayStruct ty = StructureType False [i32, ptr ty]

-- creates a struct for len and array data
makeArrayStruct :: (MonadIRBuilder m, MonadModuleBuilder m) => IR.Type -> Operand -> Operand -> m Operand
makeArrayStruct ty len dataPtr = do
  arrPtr <- allocateStruct (arrayStruct ty)
  writeStruct arrPtr 0 len
  writeStruct arrPtr 1 dataPtr
  return arrPtr

writeData :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Operand -> Operand -> m ()
writeData dataPtr index value = do
  valuePtr <- gep dataPtr [index]
  store valuePtr 0 value

readData :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Operand -> m Operand
readData dataPtr index = do
  valuePtr <- gep dataPtr [index]
  load valuePtr 0

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

tyString :: Gr.Type
tyString = TyCon (Id "String" "String")

tyPair :: (Gr.Type, Gr.Type) -> Gr.Type
tyPair (l, r) = TyApp (TyApp (TyCon (Id "," ",")) l) r

tyFloatArray :: Gr.Type
tyFloatArray = TyApp (TyCon (Id "FloatArray" "FloatArray")) (TyVar (Id "id" "id"))
