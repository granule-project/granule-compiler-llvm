{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Language.Granule.Codegen.Compile where

import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Type
import Language.Granule.Codegen.NormalisedDef
import Language.Granule.Codegen.TopsortDefinitions
import Language.Granule.Codegen.ConvertClosures
import Language.Granule.Codegen.Emit.EmitLLVM
import Language.Granule.Codegen.MarkGlobals
import Language.Granule.Codegen.RewriteAST
import Language.Granule.Codegen.StripAST

import qualified LLVM.AST as IR

compile :: String -> AST () Type -> Either String IR.Module
compile moduleName typedAST =
  let stripped       = stripAST typedAST
      rewritten      = rewriteAST stripped
      normalised     = normaliseDefinitions rewritten
      markedGlobals  = markGlobals normalised
      (Ok topsorted) = topologicallySortDefinitions markedGlobals
      closureFree    = convertClosures topsorted
  in emitLLVM moduleName closureFree
