{-# LANGUAGE TypeApplications #-}

module Main where

import Data.String.Conversions
import Data.Text (Text)

import LLVM.Context
import LLVM.Module
import LLVM.Target

import Language.Granule.Checker.Checker (check)
import Language.Granule.Codegen.Compile (compile)
import Language.Granule.Syntax.Def (AST)
import Language.Granule.Syntax.Parser (parseAndDoImportsAndFreshenDefs)
import Language.Granule.Syntax.Type (Type)
import Language.Granule.Utils (Globals, globalsExtensions)
import System.Directory (removePathForcibly, withCurrentDirectory)
import System.FilePath (replaceExtension, takeBaseName, takeDirectory)
import System.Process (callProcess, readProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  grFiles <- findByExtension [".gr"] "tests/golden/positive"
  return $ testGroup "Granule Golden Tests"
    [ goldenVsString
        (takeBaseName grFile)
        goldenFile
        (cs <$> compileAndRun grFile)
    | grFile <- grFiles
    , let goldenFile = replaceExtension grFile ".golden"
    ]

parse :: FilePath -> IO (AST () Type)
parse grFile = do
    input <- readFile grFile
    let ?globals = mempty @Globals
    (ast, extensions) <- parseAndDoImportsAndFreshenDefs input
    let ?globals = ?globals { globalsExtensions = extensions }
    checkedAst <- check ast
    case checkedAst of
        Right (ast'', _) -> return ast''
        Left e -> error $ show e

compileAndRun :: FilePath -> IO Text
compileAndRun grFile = do
    let dirpath = takeDirectory grFile
        name    = takeBaseName grFile
        object  = name ++ ".o"
    ast <- parse grFile
    llvmModule <- either error return $ compile name ast
    withCurrentDirectory dirpath $ do
        withContext $ \ctx ->
            withModuleFromAST ctx llvmModule $ \modl -> do
                withHostTargetMachineDefault $ \machine ->
                    writeObjectToFile machine (File object) modl
        callProcess "clang" [object, "-o", name]
        removePathForcibly object
        result <- cs <$> readProcess ("./" ++ name) [] []
        removePathForcibly name
        return result
