module Language.Granule.Codegen.Builtins.Builtins where

import Language.Granule.Codegen.Builtins.Extras
import Language.Granule.Codegen.Builtins.FloatArray
import Language.Granule.Codegen.Builtins.Ref
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Syntax.Identifiers (Id, mkId)

builtins :: [Builtin]
builtins =
  [ charToIntDef,
    divDef,
    newFloatArrayIDef,
    readFloatArrayIDef,
    writeFloatArrayIDef,
    lengthFloatArrayIDef,
    newFloatArrayDef,
    readFloatArrayDef,
    writeFloatArrayDef,
    lengthFloatArrayDef,
    deleteFloatArrayDef,
    newRefDef,
    freezeRefDef,
    swapRefDef,
    readRefDef
  ]

builtinIds :: [Id]
builtinIds = map (mkId . builtinId) builtins
