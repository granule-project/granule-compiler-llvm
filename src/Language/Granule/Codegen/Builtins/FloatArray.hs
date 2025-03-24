{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.FloatArray where

import qualified LLVM.AST.Constant as C
import LLVM.AST.Operand (Operand (ConstantOperand))
import LLVM.AST.Type as IR
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module (MonadModuleBuilder)
import LLVM.IRBuilder.Monad (MonadIRBuilder)
import Language.Granule.Codegen.Builtins.Shared

-- Mutable FloatArray builtins
newFloatArrayDef, readFloatArrayDef, writeFloatArrayDef, lengthFloatArrayDef, deleteFloatArrayDef :: Builtin
newFloatArrayDef =
  Builtin "newFloatArray" [tyInt] tyFloatArray impl
  where
    impl [len] = newFloatArray len
readFloatArrayDef =
  Builtin "readFloatArray" [tyFloatArray, tyInt] (tyPair (tyFloat, tyFloatArray)) impl
  where
    impl [arrPtr, idx] = readFloatArray arrPtr idx
writeFloatArrayDef =
  Builtin "writeFloatArray" [tyFloatArray, tyInt, tyFloat] tyFloatArray impl
  where
    impl [arrPtr, idx, val] = do
      dataPtr <- readStruct arrPtr 1
      writeData dataPtr idx val
      return arrPtr
lengthFloatArrayDef =
  Builtin "lengthFloatArray" [tyFloatArray] (tyPair (tyInt, tyFloatArray)) impl
  where
    impl [arrPtr] = lengthFloatArray arrPtr
deleteFloatArrayDef =
  Builtin "deleteFloatArray" [tyFloatArray] tyUnit impl
  where
    impl [arrPtr] = do
      dataPtr <- readStruct arrPtr 1
      _ <- free dataPtr
      _ <- free arrPtr
      -- return unit (need to check)
      return $ ConstantOperand (C.Struct Nothing False [])

-- Immutable FloatArray builtins
newFloatArrayIDef, readFloatArrayIDef, writeFloatArrayIDef, lengthFloatArrayIDef :: Builtin
newFloatArrayIDef =
  Builtin "newFloatArrayI" [tyInt] tyFloatArray impl
  where
    impl [len] = newFloatArray len
readFloatArrayIDef =
  Builtin "readFloatArrayI" [tyFloatArray, tyInt] (tyPair (tyFloat, tyFloatArray)) impl
  where
    impl [arrPtr, idx] = readFloatArray arrPtr idx
writeFloatArrayIDef =
  Builtin "writeFloatArrayI" [tyFloatArray, tyInt, tyFloat] tyFloatArray impl
  where
    impl [arrPtr, idx, val] = do
      len <- readStruct arrPtr 0
      dataPtr <- readStruct arrPtr 1

      -- len * double precision
      size <- mul len (int32 8)
      -- malloc wants i64
      size' <- sext size i64
      newDataPtr <- allocate size' IR.double

      -- copy existing data to new array
      _ <- copy newDataPtr dataPtr size

      -- write value at index
      writeData newDataPtr idx val

      -- return a new array struct
      makeArrayStruct IR.double len newDataPtr
lengthFloatArrayIDef =
  Builtin "lengthFloatArrayI" [tyFloatArray] (tyPair (tyInt, tyFloatArray)) impl
  where
    impl [arrPtr] = lengthFloatArray arrPtr

-- arrayStruct specialisation
floatArrayStruct :: IR.Type
floatArrayStruct = arrayStruct IR.double

-- creates a new float array of length
newFloatArray :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
newFloatArray len = do
  -- len * double precision
  size <- mul len (int32 8)
  -- malloc wants i64
  size' <- sext size i64
  dataPtr <- allocate size' IR.double
  makeArrayStruct IR.double len dataPtr

-- read the value at index of float array
readFloatArray :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Operand -> m Operand
readFloatArray arrPtr idx = do
  dataPtr <- readStruct arrPtr 1
  value <- readData dataPtr idx
  makePair (IR.double, value) (ptr floatArrayStruct, arrPtr)

-- read the length of float array
lengthFloatArray :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
lengthFloatArray arrPtr = do
  len <- readStruct arrPtr 0
  makePair (i32, len) (ptr floatArrayStruct, arrPtr)
