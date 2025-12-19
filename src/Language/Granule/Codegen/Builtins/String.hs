{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.String where

import LLVM.AST
import LLVM.AST.Type as IR
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module (MonadModuleBuilder)
import LLVM.IRBuilder.Monad (MonadIRBuilder)
import Language.Granule.Codegen.Builtins.Shared

stringAppendDef, stringConsDef, stringSnocDef :: Builtin
stringAppendDef =
  Builtin "stringAppend" [tyString, tyString] tyString impl
  where
    impl [strPtrA, strPtrB] = do
      (lenA, dataPtrA) <- readString strPtrA
      (lenB, dataPtrB) <- readString strPtrB

      len <- add lenA lenB
      dataPtr <- allocate len i8

      -- copy x into new arr
      _ <- copy dataPtr dataPtrA lenA

      -- offset pointer to end of string A
      nextPos <- gep dataPtr [lenA]

      -- copy y into new arr from offset
      _ <- copy nextPos dataPtrB lenB

      makeArrayStruct i8 len dataPtr
stringConsDef =
  Builtin "stringCons" [tyChar, tyString] tyString impl
  where
    impl [char, strPtr] = do
      (len, dataPtr) <- readString strPtr
      newLen <- add len (int32 1)
      newDataPtr <- allocate newLen i8
      writeData newDataPtr (int32 0) char
      firstPos <- gep newDataPtr [int32 1]
      _ <- copy firstPos dataPtr len
      makeArrayStruct i8 newLen newDataPtr
stringSnocDef =
  Builtin "stringSnoc" [tyString, tyChar] tyString impl
  where
    impl [strPtr, char] = do
      (len, dataPtr) <- readString strPtr
      newLen <- add len (int32 1)
      newDataPtr <- allocate newLen i8
      _ <- copy newDataPtr dataPtr len
      writeData newDataPtr len char
      makeArrayStruct i8 newLen newDataPtr

stringStruct :: IR.Type
stringStruct = StructureType False [i32, ptr i8]

-- read length and data ptr from string
readString :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m (Operand, Operand)
readString strPtr = do
  len <- readStruct strPtr 0
  dataPtr <- readStruct strPtr 1
  return (len, dataPtr)
