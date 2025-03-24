module Language.Granule.Codegen.Builtins.Builtins where

import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Codegen.Builtins.ImmutableArray
import Language.Granule.Codegen.Builtins.MutableArray
import Language.Granule.Codegen.Builtins.Extras
import Language.Granule.Syntax.Identifiers (Id, mkId)

builtins :: [Builtin]
builtins = [
    charToIntDef, divDef,
    newFloatArrayIDef,
    readFloatArrayIDef,
    writeFloatArrayIDef,
    lengthFloatArrayIDef,
    newFloatArrayDef, readFloatArrayDef, writeFloatArrayDef, lengthFloatArrayDef, deleteFloatArrayDef
    ]

builtinIds :: [Id]
builtinIds = map (mkId . builtinId) builtins
