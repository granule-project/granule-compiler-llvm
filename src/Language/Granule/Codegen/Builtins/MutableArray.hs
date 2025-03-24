{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.MutableArray where

import LLVM.AST
import LLVM.AST.Type as IR
import qualified LLVM.AST.Constant as C
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import Language.Granule.Codegen.Builtins.Shared

-- newFloatArray :: Int -> FloatArray id
newFloatArrayDef :: Builtin
newFloatArrayDef =
    Builtin "newFloatArray" args ret impl
    where
        args = [tyInt]
        ret = tyFloatArray
        impl [len] = do
          -- length * double precision 8 bytes
          dataSize <- mul len (int32 8)
          dataSize64 <- sext dataSize i64
          dataPtr <- allocate dataSize64 IR.double
          makeArray IR.double len dataPtr

-- readFloatArray :: (FloatArray id) -> Int -> (Float, FloatArray id)
readFloatArrayDef :: Builtin
readFloatArrayDef =
    Builtin "readFloatArray" args ret impl
    where
        args = [tyFloatArray, tyInt]
        ret = tyPair (tyFloat, tyFloatArray)
        impl [arrPtr, idx] = do
          dataPtr <- getArrayDataPtr arrPtr
          valuePtr <- gep dataPtr [idx]
          value <- load valuePtr 0
          makePair (IR.double, value) (ptr floatArrayStruct, arrPtr)

-- writeFloatArray :: (FloatArray id) -> Int -> Float -> FloatArray id
writeFloatArrayDef :: Builtin
writeFloatArrayDef =
    Builtin "writeFloatArray" args ret impl
    where
        args = [tyFloatArray, tyInt, tyFloat]
        ret = tyFloatArray
        impl [arrPtr, idx, val] = do
            dataPtr <- getArrayDataPtr arrPtr
            valuePtr <- gep dataPtr [idx]
            store valuePtr 0 val
            return arrPtr

-- lengthFloatArray :: (FloatArray id) -> (Int -> FloatArray id)
lengthFloatArrayDef :: Builtin
lengthFloatArrayDef =
    Builtin "lengthFloatArray" args ret impl
    where
        args = [tyFloatArray]
        ret = tyPair (tyInt, tyFloatArray)
        impl [arrPtr] = do
          len <- getArrayLen arrPtr
          makePair (i32, len) (ptr floatArrayStruct, arrPtr)

deleteFloatArrayDef :: Builtin
deleteFloatArrayDef =
    Builtin "deleteFloatArray" args ret impl
        where
            args = [tyFloatArray]
            ret = tyUnit
            impl [arrPtr] = do
                dataPtr <- getArrayDataPtr arrPtr
                _ <- free dataPtr
                _ <- free arrPtr

                -- return unit (need to check)
                return $ ConstantOperand (C.Struct Nothing False [])
