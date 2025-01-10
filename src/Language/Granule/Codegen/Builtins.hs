{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins where

import LLVM.AST
import qualified LLVM.AST.Constant as C
import LLVM.AST.Type as IR
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import Language.Granule.Codegen.Emit.LLVMHelpers (sizeOf)
import Language.Granule.Codegen.Emit.Primitives (malloc, memcpy)
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type as Gr

data Builtin = Builtin {
    builtinId :: String,
    builtinArgTys :: [Gr.Type],
    builtinRetTy :: Gr.Type,
    builtinImpl :: forall m. (MonadModuleBuilder m, MonadIRBuilder m) => [Operand] -> m Operand}

mkFunType :: [Gr.Type] -> Gr.Type -> Gr.Type
mkFunType args ret = foldr (FunTy Nothing Nothing) ret args

builtins :: [Builtin]
builtins =  [charToIntDef, divDef, newFloatArrayIDef, readFloatArrayIDef, writeFloatArrayIDef, lengthFloatArrayIDef ]


builtinIds :: [Id]
builtinIds = map (mkId . builtinId) builtins

-- charToInt :: Char -> Int
charToIntDef :: Builtin
charToIntDef =
    Builtin "charToInt" args ret impl
    where
        args = [TyCon (Id "Char" "Char")]
        ret = TyCon (Id "Int" "Int")
        impl [x] = zext x i32

-- div :: Int -> Int -> Int
divDef :: Builtin
divDef =
    Builtin "div" args ret impl
    where
        args = [TyCon (Id "Int" "Int"), TyCon (Id "Int" "Int")]
        ret = TyCon (Id "Int" "Int")
        impl [x, y] = sdiv x y

-- newFloatArrayI :: Int -> FloatArray id
newFloatArrayIDef :: Builtin
newFloatArrayIDef =
    Builtin "newFloatArrayI" args ret impl
    where
        args = [tyInt]
        ret = tyFloatArray
        impl [len] = do
            -- arrays are a struct {int32 len, double* data} - on heap - use stack?
            arrPtr <- call (ConstantOperand malloc) [(ConstantOperand $ sizeOf structTy, [])]
            arrPtr' <- bitcast arrPtr (ptr structTy)

            -- not 100% on the size and if we need to do anything for alignment, but it works
            dataSize <- mul len (int32 8)
            dataPtr <- call (ConstantOperand malloc) [(dataSize, [])]

            lenField <- gep arrPtr' [int32 0, int32 0]
            store lenField 0 len

            dataField <- gep arrPtr' [int32 0, int32 1]
            store dataField 0 dataPtr

            return arrPtr

-- readFloatArrayI :: (FloatArray id) -> Int -> (Float, FloatArray id)
readFloatArrayIDef :: Builtin
readFloatArrayIDef =
    Builtin "readFloatArrayI" args ret impl
    where
        args = [tyFloatArray, tyInt]
        ret = tyPair (tyFloat, tyFloatArray)
        impl [arrPtr, idx] = do
            -- arr -> data -> idx -> val

            arrPtr' <- bitcast arrPtr (ptr structTy)

            dataField <- gep arrPtr' [int32 0, int32 1]
            dataPtr <- load dataField 0

            valuePtr <- gep dataPtr [idx]
            value <- load valuePtr 0

            -- pair return (float, floatArray) on stack
            let pairTy = StructureType False [IR.double, ptr structTy]
            let pair = ConstantOperand $ C.Undef pairTy
            pair' <- insertValue pair value [0]
            insertValue pair' arrPtr [1]



-- writeFloatArrayI :: (FloatArray id) -> Int -> Float -> FloatArray id
writeFloatArrayIDef :: Builtin
writeFloatArrayIDef =
    Builtin "writeFloatArrayI" args ret impl
    where
        args = [tyFloatArray, tyInt, tyFloat]
        ret = tyFloatArray
        impl [arrPtr, idx, val] = do
            arrPtr' <- bitcast arrPtr (ptr structTy)

            lenField <- gep arrPtr' [int32 0, int32 0]
            len <- load lenField 0

            dataField <- gep arrPtr' [int32 0, int32 1]
            dataPtr <- load dataField 0

            -- need to create a new array as in newFloatArrayI
            newArrPtr <- call (ConstantOperand malloc) [(ConstantOperand $ sizeOf structTy, [])]
            newArrPtr' <- bitcast newArrPtr (ptr structTy)

            dataSize <- mul len (int32 8)
            newDataPtr <- call (ConstantOperand malloc) [(dataSize, [])]
            newDataPtr' <- bitcast newDataPtr (ptr IR.double)

            -- copy the existing data to new array
            _ <- call (ConstantOperand memcpy)
                [ (newDataPtr, [])
                , (dataPtr, [])
                , (dataSize, [])
                , (bit 0, [])
                ]

            -- write the val to new copy
            valuePtr <- gep newDataPtr' [idx]
            store valuePtr 0 val

            newLenField <- gep newArrPtr' [int32 0, int32 0]
            store newLenField 0 len

            newDataField <- gep newArrPtr' [int32 0, int32 1]
            store newDataField 0 newDataPtr

            return newArrPtr

-- lengthFloatArrayI :: (FloatArray id) -> (Int -> FloatArray id)
lengthFloatArrayIDef :: Builtin
lengthFloatArrayIDef =
    Builtin "lengthFloatArrayI" args ret impl
    where
        args = [tyFloatArray]
        ret = tyPair (tyInt, tyFloatArray)
        impl [arrPtr] = do
            arrPtr' <- bitcast arrPtr (ptr structTy)

            lenField <- gep arrPtr' [int32 0, int32 0]
            len <- load lenField 0

            -- pair return (int, floatArray) on stack
            let pairTy = StructureType False [i32, ptr structTy]
            let pair = ConstantOperand $ C.Undef pairTy
            pair' <- insertValue pair len [0]
            insertValue pair' arrPtr [1]

structTy :: IR.Type
structTy =  StructureType False [i32, ptr IR.double]

tyInt :: Gr.Type
tyInt = TyCon (Id "Int" "Int")

tyFloat :: Gr.Type
tyFloat = TyCon (Id "Float" "Float")

tyChar :: Gr.Type
tyChar = TyCon (Id "Char" "Char")

tyPair :: (Gr.Type, Gr.Type) -> Gr.Type
tyPair (l, r) = TyApp (TyApp (TyCon (Id "," ",")) l) r

tyFloatArray :: Gr.Type
tyFloatArray = TyApp (TyCon (Id "FloatArray" "FloatArray")) (TyVar (Id "id" "id"))
