{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Granule.Codegen.Emit.MainOut where

import LLVM.AST (Operand)
import qualified LLVM.AST as IR
import LLVM.AST.Constant
import qualified LLVM.AST.Constant as C
import LLVM.AST.Name (mkName)
import LLVM.AST.Type
import LLVM.IRBuilder hiding (double)
import Language.Granule.Codegen.ClosureFreeDef
import Language.Granule.Codegen.Emit.LLVMHelpers (mkPName)
import Language.Granule.Codegen.Emit.Types (GrType)
import Language.Granule.Codegen.NormalisedDef
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type
import Language.Granule.Codegen.Emit.LowerType (llvmType)
import Language.Granule.Codegen.Emit.Primitives (printf)

findMainReturnType :: [ClosureFreeValueDef] -> GrType
findMainReturnType defs =
    case filter isMainDef defs of
        [ValueDef _ _ _ (Forall _ _ _ ty)] -> ty
        _ -> error "No main function found"
    where
        isMainDef (ValueDef _ ident _ _) = internalName ident == "main"

loadMainValue :: (MonadModuleBuilder m, MonadIRBuilder m) => GrType -> m Operand
loadMainValue mainTy = load mainRef 4
    where
        mainRef = IR.ConstantOperand $ GlobalReference (ptr $ llvmType mainTy) (mkName "def.main")

emitMainOut :: (MonadModuleBuilder m) => GrType -> m Operand
emitMainOut ty =
    function "internal_mainOut" [(llvmType ty, mkPName "x")] void $ \[x] -> do
        _ <- emitPrint ty [x]
        retVoid

mainOut :: GrType -> Constant
mainOut ty = GlobalReference functionType name
    where
        name = mkName "internal_mainOut"
        functionType = ptr (FunctionType void [llvmType ty] False)

emitPrint :: (MonadModuleBuilder m, MonadIRBuilder m) => GrType -> [Operand] -> m Operand
emitPrint ty vals = do
    let str = fmtStrForTy ty ++ "\n"
    let strLen = length str + 1 -- newline 2 chars
    fmt <- globalStringPtr str (mkName "internal_mainOut.fmtStr")
    let fmtPtr = IR.ConstantOperand $ C.GetElementPtr True
                    (GlobalReference (ptr (ArrayType (fromIntegral strLen) i8)) "internal_mainOut.fmtStr")
                    [C.Int 32 0, C.Int 32 0]
    let args = (fmtPtr, []) : map (\v -> (v, [])) vals
    _ <- call (IR.ConstantOperand printf) args
    return fmtPtr

fmtStrForTy :: GrType -> String
fmtStrForTy x =
    case x of
        (TyCon (Id "Int" _)) -> "%d"
        (TyCon (Id "Float" _)) -> "%.6f"
        (TyApp (TyApp (TyCon (Id "," _)) leftTy) rightTy) ->
            "(" ++ fmtStrForTy leftTy ++ ", " ++ fmtStrForTy rightTy ++ ")"
        (TyApp (TyCon (Id "FloatArray" _)) _) -> "<array>"
        (TyCon (Id "()" _)) -> "()"
        _ -> error "Unsupported"
