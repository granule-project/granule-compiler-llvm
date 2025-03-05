{-                                       ___
                                        /\_ \
       __   _  _    __      ___   __  __\//\ \      __
     / _  \/\`'__\/ __ \  /' _ `\/\ \/\ \ \ \ \   /'__`\
    /\ \_\ \ \ \//\ \_\ \_/\ \/\ \ \ \_\ \ \_\ \_/\  __/
    \ \____ \ \_\\ \__/ \_\ \_\ \_\ \____/ /\____\ \____\
     \/___L\ \/_/ \/__/\/_/\/_/\/_/\/___/  \/____/\/____/
       /\____/
       \_/__/
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (SomeException, displayException, try)
import Control.Monad ((<=<), forM)
import Development.GitRev
import Data.Char (isSpace)
import Data.Either (isRight)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.List.Extra (breakOn)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Version (showVersion)
import System.Exit

import System.Directory (getAppUserDataDirectory, getCurrentDirectory)
import System.FilePath (takeFileName)
import System.FilePath.Posix (takeBaseName)
import "Glob" System.FilePath.Glob (glob)
import Options.Applicative
import Options.Applicative.Help.Pretty (string)

import LLVM.Target
import LLVM.Module
import LLVM.Internal.Context

import Language.Granule.Checker.Checker
import Language.Granule.Checker.Monad (CheckerError)
import Language.Granule.Codegen.Compile (compile)
import Language.Granule.Syntax.Preprocessor
import Language.Granule.Syntax.Parser
import Language.Granule.Syntax.Pretty
import Language.Granule.Utils
import Paths_granule_compiler (version)

main :: IO ()
main = do
    (globPatterns, config) <- getGrConfig
    if null globPatterns
      then runGrOnStdIn config
      else runGrOnFiles globPatterns config

-- | Run the checker and interpreter on a bunch of files
runGrOnFiles :: [FilePath] -> GrConfig -> IO ()
runGrOnFiles globPatterns config = let ?globals = grGlobals config in do
    pwd <- getCurrentDirectory
    results <- forM globPatterns $ \pat -> do
      paths <- glob pat
      case paths of
        [] -> do
          let result = Left $ NoMatchingFiles pat
          printResult result
          return [result]
        _ -> forM paths $ \path -> do
          let fileName = if pwd `isPrefixOf` path then takeFileName path else path
          let ?globals = ?globals{ globalsSourceFilePath = Just fileName } in do
            printInfo $ "Checking " <> fileName <> "..."
            src <- preprocess Nothing False path "granule"
            result <- run src
            printResult result
            return result
    if all isRight (concat results) then exitSuccess else exitFailure

runGrOnStdIn :: GrConfig -> IO ()
runGrOnStdIn GrConfig{..}
  = let ?globals = grGlobals{ globalsSourceFilePath = Just "stdin" } in do
      printInfo "Reading from stdin: confirm input with `enter+ctrl-d` or exit with `ctrl-c`"
      debugM "Globals" (show ?globals)
      result <- getContents >>= run
      printResult result
      if isRight result then exitSuccess else exitFailure

-- print the result of running the checker and interpreter
printResult
  :: (?globals :: Globals)
  => Either CompileError CompileResult
  -> IO ()
printResult = \case
    Left err -> printError err
    Right CompileSuccess -> printSuccess "Compiled!"

{-| Run the input through the type checker and evaluate.
-}
run
  :: (?globals :: Globals)
  => String
  -> IO (Either CompileError CompileResult)
run input = let ?globals = maybe mempty grGlobals (getEmbeddedGrFlags input) <> ?globals in do
    result <- try $ parseAndDoImportsAndFreshenDefs input
    case result of
      Left (e :: SomeException) -> return . Left . ParseError $ show e
      Right (ast, extensions) ->
        -- update globals with extensions
        let ?globals = ?globals { globalsExtensions = extensions } in do
        -- Print to terminal when in debugging mode:
        debugM "Pretty-printed AST:" $ pretty ast
        debugM "Raw AST:" $ show ast
        -- Check and evaluate
        checked <- try $ check ast
        case checked of
          Left (e :: SomeException) -> return .  Left . FatalError $ displayException e
          Right (Left errs) -> return . Left $ CheckerError errs
          Right (Right (ast',_)) -> do
            printSuccess "OK, compiling..."
            let moduleName = takeBaseName sourceFilePath
            case compile moduleName ast' of
              Left (e :: String) ->
                return $ Left $ CompileError e
              Right moduleAST -> do
                withHostTargetMachineDefault $ \machine ->
                  withContext $ \context ->
                    withModuleFromAST context moduleAST $ \mo -> do
                      writeBitcodeToFile (File (moduleName ++ ".bc")) mo
                      writeObjectToFile machine (File (moduleName ++ ".o")) mo
                return $ Right CompileSuccess


-- | Get the flags embedded in the first line of a file, e.g.
-- "-- gr --no-eval"
getEmbeddedGrFlags :: String -> Maybe GrConfig
getEmbeddedGrFlags
  = foldr ((<|>) . getEmbeddedGrFlagsLine) Nothing . take 3 -- only check for flags within the top 3 lines (so they are visible and at the top)
  . lines
  where
    getEmbeddedGrFlagsLine
      = parseGrFlags . dropWhile isSpace
      <=< stripPrefix "gr" . dropWhile isSpace
      <=< stripPrefix "--" . dropWhile isSpace


parseGrFlags :: String -> Maybe GrConfig
parseGrFlags
  = pure . snd
  <=< getParseResult . execParserPure (prefs disambiguate) parseGrConfig . words


data GrConfig = GrConfig
  {
    grShowVersion     :: Bool
  , grGlobals         :: Globals
  , grcNoLink         :: Bool
  , grcEmitLLVM       :: Bool
  , grcClean          :: Bool
  , grcOutput         :: Maybe String
  }
  deriving (Show)

instance Semigroup GrConfig where
  c1 <> c2 = GrConfig
    { grShowVersion     = grShowVersion c1 ||  grShowVersion c2
    , grGlobals         = grGlobals     c1 <>  grGlobals c2
    , grcNoLink         = grcNoLink     c1 ||  grcNoLink c2
    , grcEmitLLVM       = grcEmitLLVM   c1 ||  grcEmitLLVM c2
    , grcClean          = grcClean      c1 ||  grcClean c2
    , grcOutput         = grcOutput     c2 <|> grcOutput c1
    }

instance Monoid GrConfig where
  mempty = GrConfig
    { grGlobals     = mempty
    , grShowVersion = False
    , grcNoLink     = False
    , grcEmitLLVM   = False
    , grcClean      = False
    , grcOutput     = Nothing
    }

getGrConfig :: IO ([FilePath], GrConfig)
getGrConfig = do
    (globPatterns, configCLI) <- getGrCommandLineArgs
    configHome <- readUserConfig (grGlobals configCLI)
    pure (globPatterns, configCLI <> configHome)
  where
  -- TODO: UNIX specific
    readUserConfig :: Globals -> IO GrConfig
    readUserConfig globals = do
      let ?globals = globals
      try (getAppUserDataDirectory "granule") >>= \case
        Left (e :: SomeException) -> do
          debugM "Read user config" $ show e
          pure mempty
        Right configFile ->
          try (parseGrFlags <$> readFile configFile) >>= \case
            Left (e :: SomeException) -> do
              debugM "Read user config" $ show e
              pure mempty
            Right Nothing -> do
              printInfo . red . unlines $
                [ "Couldn't parse granule configuration file at " <> configFile
                , "Run `gr --help` to see a list of accepted flags."
                ]
              pure mempty
            Right (Just config) -> pure config


getGrCommandLineArgs :: IO ([FilePath], GrConfig)
getGrCommandLineArgs = customExecParser (prefs disambiguate) parseGrConfig

parseGrConfig :: ParserInfo ([FilePath], GrConfig)
parseGrConfig = info (go <**> helper) $ briefDesc
    <> (headerDoc . Just . string . unlines)
            [ "The Granule LLVM Compiler"
            , "version: "     <> showVersion version
            , "branch: "      <> $(gitBranch)
            , "commit hash: " <> $(gitHash)
            , "commit date: " <> $(gitCommitDate)
            , if $(gitDirty) then "(uncommitted files present)" else ""
            ]
    <> footer "This software is provided under a BSD3 license and comes with NO WARRANTY WHATSOEVER.\
              \ Consult the LICENSE for further information."
  where
    go = do
        globPatterns <-
          many $ argument str $ metavar "GLOB_PATTERNS" <> action "file"
          <> (help . unwords)
            [ "Glob pattern for Granule source files. If the file extension is `.md`/`.tex`, the markdown/TeX preprocessor will be used."
            , "If none are given, input will be read from stdin."
            ]

        globalsDebugging <-
          flag Nothing (Just True)
            $ long "debug"
            <> help "Debug mode"

        grShowVersion <-
          flag False True
            $ long "version"
            <> help "Show version"

        globalsSuppressInfos <-
          flag Nothing (Just True)
            $ long "no-info"
            <> help "Don't output info messages"

        globalsSuppressErrors <-
          flag Nothing (Just True)
            $ long "no-error"
            <> help "Don't output error messages"

        globalsNoColors <-
          flag Nothing (Just True)
            $ long "no-color"
            <> long "no-colour"
            <> help "Turn off colors in terminal output"

        globalsAlternativeColors <-
          flag Nothing (Just True)
            $ long "alternative-colors"
            <> long "alternative-colours"
            <> help "Print success messages in blue instead of green (may help with color blindness)"

        globalsTimestamp <-
          flag Nothing (Just True)
            $ long "timestamp"
            <> help "Print timestamp in info and error messages"

        globalsSolverTimeoutMillis <-
          (optional . option (auto @Integer))
            $ long "solver-timeout"
            <> (help . unwords)
            [ "SMT solver timeout in milliseconds (negative for unlimited)"
            , "Defaults to"
            , show solverTimeoutMillis <> "ms."
            ]

        globalsIncludePath <-
          optional $ strOption
            $ long "include-path"
            <> help ("Path to the standard library. Defaults to "
                    <> show includePath)
            <> metavar "PATH"

        globalsEntryPoint <-
          optional $ strOption
            $ long "entry-point"
            <> help ("Program entry point. Defaults to " <> show entryPoint)
            <> metavar "ID"

        grcNoLink <-
          flag False True
            $ long "no-link"
            <> help "Stop after generating object files, do not create executable"

        grcEmitLLVM <-
          flag False True
            $ long "emit-llvm"
            <> help "Generate LLVM bitcode files"

        grcClean <-
          flag False True
            $ long "clean"
            <> short 'c'
            <> help "Remove object files after successful linking"

        grcOutput <-
          optional $ strOption
            $ long "output"
            <> short 'o'
            <> help "Specify output file name"
            <> metavar "FILENAME"
        pure
          ( globPatterns
          , GrConfig
            { grShowVersion
            , grGlobals = Globals
              { globalsDebugging
              , globalsInteractiveDebugging = Nothing
              , globalsNoColors
              , globalsAlternativeColors
              , globalsNoEval = Nothing
              , globalsSuppressInfos
              , globalsSuppressErrors
              , globalsIgnoreHoles = Nothing
              , globalsTimestamp
              , globalsTesting = Nothing
              , globalsSolverTimeoutMillis
              , globalsIncludePath
              , globalsSourceFilePath = Nothing
              , globalsEntryPoint
              , globalsRewriteHoles = Nothing
              , globalsHolePosition = (,) <$> Nothing <*> Nothing
              , globalsSynthesise = Nothing
              , globalsBenchmark = Nothing
              , globalsBenchmarkRaw = Nothing
              , globalsSubtractiveSynthesis = Nothing
              , globalsAlternateSynthesisMode = Nothing
              , globalsCartesianSynth = Nothing
              , globalsHaskellSynth = Nothing
              , globalsSynthHtml = Nothing
              , globalsExampleLimit = Nothing
              , globalsExtensions = []
              , globalsDocMode = Nothing
              }
            , grcNoLink
            , grcEmitLLVM
            , grcClean
            , grcOutput
            }
          )
      where
        ?globals = mempty @Globals

data CompileResult = CompileSuccess

data CompileError
  = ParseError String
  | CheckerError (NonEmpty CheckerError)
  | CompileError String
  | FatalError String
  | NoEntryPoint
  | NoMatchingFiles String
  deriving Show

instance UserMsg CompileError where
  title ParseError {} = "Parse error"
  title CheckerError {} = "Type checking failed"
  title CompileError {} = "Error during compilation"
  title FatalError{} = "Fatal error"
  title NoEntryPoint{} = "No program entry point"
  title NoMatchingFiles{} = "User error"

  msg (ParseError m) = fst . breakOn "CallStack (from HasCallStack):" $ m -- TODO
  msg (CheckerError ms) = intercalate "\n\n" . map formatError . toList $ ms
  msg (CompileError m) = m
  msg (FatalError m) = m
  msg NoEntryPoint = "Program entry point `" <> entryPoint <> "` not found. A different one can be specified with `--entry-point`."
  msg (NoMatchingFiles p) = "The glob pattern `" <> p <> "` did not match any files."
