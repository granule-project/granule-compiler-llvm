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
        _ <- emitPrint ty x
        _ <- printFmt "\n" []
        retVoid

mainOut :: GrType -> Constant
mainOut ty = GlobalReference functionType name
    where
        name = mkName "internal_mainOut"
        functionType = ptr (FunctionType void [llvmType ty] False)

emitPrint :: (MonadModuleBuilder m, MonadIRBuilder m) => GrType -> Operand -> m Operand
emitPrint ty val = case ty of
    (TyCon (Id "Int" _)) -> printFmt "%d" [val]
    (TyCon (Id "Float" _)) -> printFmt "%.6f" [val]
    (TyCon (Id "Char" _)) -> printFmt "'%c'" [val]
    (TyCon (Id "()" _)) -> printFmt "()" []
    (TyApp (TyApp (TyCon (Id "," _)) leftTy) rightTy) -> do
        left <- extractValue val [0]
        right <- extractValue val [1]
        _ <- printFmt "(" []
        _ <- emitPrint leftTy left
        _ <- printFmt ", " []
        _ <- emitPrint rightTy right
        printFmt ")" []
    (TyCon (Id "String" _)) -> do
        lenPtr <- gep val [int32 0, int32 0]
        len <- load lenPtr 4
        elemsPtr <- gep val [int32 0, int32 1]
        elems <- load elemsPtr 8
        printFmt "\"%.*s\"" [len, elems]
    _ -> error "Unsupported"

printFmt :: (MonadModuleBuilder m, MonadIRBuilder m) => String -> [Operand] -> m Operand
printFmt fmt args = do
    let name = mkName $ "fmt." ++ fmt
    fmtPtr <- mkFmtPtr fmt name (length fmt + 1)
    call (IR.ConstantOperand printf) ((fmtPtr, []) : map (\a -> (a, [])) args)

mkFmtPtr :: (MonadModuleBuilder m, MonadIRBuilder m) => String -> IR.Name -> Int -> m Operand
mkFmtPtr str name len = do
    _ <- globalStringPtr str name
    return $ IR.ConstantOperand $ C.GetElementPtr True
              (GlobalReference (ptr (ArrayType (fromIntegral len) i8)) name)
              [C.Int 32 0, C.Int 32 0]
