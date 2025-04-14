module Language.Granule.Codegen.Emit.LowerType where
import Language.Granule.Codegen.Emit.MkId
import Language.Granule.Syntax.Type
import Language.Granule.Codegen.Emit.Types
import LLVM.AST.Type

llvmTypeForFunction :: IrType -> IrType -> IrType
llvmTypeForFunction paramType returnType =
    FunctionType returnType argumentTypes isVarArg
    where argumentTypes = [ptr i8, paramType]
          isVarArg = False

llvmTypeForClosure :: IrType -> IrType
llvmTypeForClosure functionType =
    StructureType {
        isPacked = False,
        elementTypes = [ptr functionType, ptr i8] }

{-|
A function type can be resolved to two different types
depending on the context. e.g. for
@
    Int -> Int -> Int
@
If it is a parameter type / return value it should be
@
    {{i32(i8*, i32), u8*}(i8*, i32), i8*}
@
If it is a definition type it should be
@
    {i32(i8*, i32), u8*}(i8*, i32)
@
For the former use `llvmSubType`, for the latter use `llvmTopLevelType`
|-}
llvmType :: GrType -> IrType
llvmType (FunTy _ _ from to) =
    llvmTypeForClosure $ llvmTypeForFunction (llvmType from) (llvmType to)
llvmType (TyApp (TyApp (TyCon (MkId ",")) left) right) =
    StructureType False [llvmType left, llvmType right]
llvmType (TyApp (TyCon (MkId "FloatArray")) _) =
    ptr $ StructureType False [i32, ptr double]
llvmType (TyCon (MkId "()")) =
    StructureType False []
llvmType (TyApp (TyApp (TyCon (MkId "Ref")) _) ty) =
  ptr $ StructureType False [llvmType ty]
-- rather than ignore as below we might use this in future
llvmType (TyApp (TyApp (TyCon (MkId "Rename")) _) ty) = llvmType ty
llvmType (TyCon (MkId "Int")) = i32
llvmType (TyCon (MkId "Float")) = double
llvmType (TyCon (MkId "Char")) = i8
llvmType (TyCon (MkId "Handle")) = i8
llvmType (TyCon (MkId "Bool")) = i1
llvmType (Box coeffect ty) = llvmType ty
llvmType (TyExists _ _ ty) = llvmType ty
llvmType (Borrow _ ty) = llvmType ty

-- TODO: clean these up
llvmType (TyApp (TyCon (MkId "LChan")) (TyCon (MkId "End"))) =
    ptr $ StructureType False []
llvmType (TyApp (TyCon (MkId "LChan")) (TyApp (TyCon (MkId "Dual")) (TyCon (MkId "End")))) =
    ptr $ StructureType False []
llvmType (TyApp (TyCon (MkId "LChan")) (TyApp (TyApp (TyCon (MkId "Send")) ty) (TyCon (MkId "End")))) =
    ptr $ StructureType False [llvmType ty]
llvmType (TyApp (TyCon (MkId "LChan")) (TyApp (TyCon (MkId "Dual")) (TyApp (TyApp (TyCon (MkId "Send")) ty) (TyCon (MkId "End"))))) =
    ptr $ StructureType False [llvmType ty]

llvmType ty = error $ "Cannot lower the type " ++ show ty

llvmTopLevelType :: GrType -> IrType
llvmTopLevelType (FunTy _ _ from to) = llvmTypeForFunction (llvmType from) (llvmType to)
llvmTopLevelType other = llvmType other

refInnerTy :: GrType -> IrType
refInnerTy (TyApp (TyApp (TyCon (MkId "Ref")) _) ty) = llvmType ty
refInnerTy (Borrow _ ty) = refInnerTy ty
refInnerTy (Box _ ty) = refInnerTy ty
refInnerTy (Diamond _ ty) = refInnerTy ty
refInnerTy (Star _ ty) = refInnerTy ty
refInnerTy ty = error ("Unsupported ref ty:\n" ++ show ty)
