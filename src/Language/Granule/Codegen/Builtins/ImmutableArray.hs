{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.ImmutableArray where

import LLVM.AST.Type as IR
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import Language.Granule.Codegen.Builtins.Shared

-- newFloatArrayI :: Int -> FloatArray id
newFloatArrayIDef :: Builtin
newFloatArrayIDef =
    Builtin "newFloatArrayI" args ret impl
    where
        args = [tyInt]
        ret = tyFloatArray
        impl [len] = do
            -- length * double precision 8 bytes
            dataSize <- mul len (int32 8)
            dataSize64 <- sext dataSize i64
            dataPtr <- allocate dataSize64 IR.double
            makeArray IR.double len dataPtr

-- readFloatArrayI :: (FloatArray id) -> Int -> (Float, FloatArray id)
readFloatArrayIDef :: Builtin
readFloatArrayIDef =
    Builtin "readFloatArrayI" args ret impl
    where
        args = [tyFloatArray, tyInt]
        ret = tyPair (tyFloat, tyFloatArray)
        impl [arrPtr, idx] = do
            dataPtr <- getArrayDataPtr arrPtr
            valuePtr <- gep dataPtr [idx]
            value <- load valuePtr 0
            makePair (IR.double, value) (ptr floatArrayStruct, arrPtr)

-- writeFloatArrayI :: (FloatArray id) -> Int -> Float -> FloatArray id
writeFloatArrayIDef :: Builtin
writeFloatArrayIDef =
    Builtin "writeFloatArrayI" args ret impl
    where
        args = [tyFloatArray, tyInt, tyFloat]
        ret = tyFloatArray
        impl [arrPtr, idx, val] = do
            len <- getArrayLen arrPtr
            dataPtr <- getArrayDataPtr arrPtr

            dataSize <- mul len (int32 8)
            dataSize64 <- sext dataSize i64
            newDataPtr <- allocateFloatArray len

            _ <- copy newDataPtr dataPtr dataSize

            valuePtr <- gep newDataPtr [idx]
            store valuePtr 0 val

            makeArray IR.double len newDataPtr

-- lengthFloatArrayI :: (FloatArray id) -> (Int -> FloatArray id)
lengthFloatArrayIDef :: Builtin
lengthFloatArrayIDef =
    Builtin "lengthFloatArrayI" args ret impl
    where
        args = [tyFloatArray]
        ret = tyPair (tyInt, tyFloatArray)
        impl [arrPtr] = do
            len <- getArrayLen arrPtr
            makePair (i32, len) (ptr floatArrayStruct, arrPtr)
