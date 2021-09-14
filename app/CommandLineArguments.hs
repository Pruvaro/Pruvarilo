-- |
-- This module provides the functionality for interpreting
-- command line arguments, and using those to interface
-- with the rest of the modules.
module CommandLineArguments
  ( CommandLineOptions
    ( CommandLineOptions
    , clo_modeOptions
    , clo_commonOptions
    , clo_mbConfigFile
    )
  , cloParser
  , actLocParser
  , modeExecCaller
  ) where

-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Config
  ( ProofSystemConfig
  , getConf
  )

import Modes
  ( Mode
    ( AddMode
    , BuildMode
    , DocMode
    , RemoveMode
    )
  , CommonModeOptions
    ( CommonModeOptions
    )
  , ActionLocation
    ( ActLocPath
    , ActLocDiscDir
    )
  , AddModeOptions
    ( AddModeOptions
    )
  , BuildModeOptions
    ( BuildModeOptions
    )
  , DocModeOptions
    ( DocModeOptions
    )
  , RemoveModeOptions
    ( RemoveModeOptions
    )
  )

import Modes.AddMode
  ( execAddMode
  )

import Modes.BuildMode
  ( execBuildMode
  )

import Modes.DocMode
  ( execDocMode
  )

import Modes.RemoveMode
  ( execRemoveMode
  )

import Control.Applicative
  ( (<|>)
  , optional
  )

import qualified Control.Monad.Trans.Reader as CTR
  ( runReaderT
  )

import qualified Options.Applicative as OA
  ( Parser
  , argument
  , command
  , help
  , info
  , long
  , metavar
  , progDesc
  , short
  , str
  , strOption
  , subparser
  , switch
  )

import qualified System.IO as SIO
  ( hPutStrLn
  , stderr
  )

-- MODE EXECUTION CALLER
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- The function which provides the interpretation of the
-- command line arguments as function calls to the 
-- specific mode functions.
modeExecCaller
  :: CommandLineOptions
  -> IO ()
modeExecCaller clo =
  let
    -- Divide the command line options into its
    -- mode-specific settings,
    modeOpts :: Mode
    modeOpts = clo_modeOptions clo

    -- and its common-mode options,
    cmo :: CommonModeOptions
    cmo = clo_commonOptions clo
  in
    do
      -- Try to get the configuration.
      eitConf <- getConf $ clo_mbConfigFile clo

      -- Getting the configuration can fail, which gives us
      -- an error message. If it really fails, we output that
      -- error to stderr.
      case eitConf of
        Left err -> SIO.hPutStrLn SIO.stderr err
        Right conf -> 
          case modeOpts of
            AddMode addOpts ->
              CTR.runReaderT (execAddMode cmo addOpts) conf
            BuildMode buildOpts ->
              CTR.runReaderT (execBuildMode cmo buildOpts) conf
            DocMode docOpts ->
              CTR.runReaderT (execDocMode cmo docOpts) conf
            RemoveMode removeOpts ->
              CTR.runReaderT 
                (execRemoveMode cmo removeOpts)
                conf
        

-- COMMAND LINE OPTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- The general data type corresponding to all possible
-- command line options. It is composed of a 'Mode'
-- field, which contains all the options that are
-- specific to a certain mode, a 'CommonModeOptions' field,
-- responsible for encoding information about commands that
-- work for all operation modes, and some extra fields
-- representing options that should not be passed forward 
-- to the functions which handle the actions themselves, 
-- but rather be handled immediately, such as the (possibly 
-- user-specified) location of the configuration file.
data CommandLineOptions =
  CommandLineOptions
  { 
    -- |
    -- The mode-specific options.
    clo_modeOptions :: Mode
  
    -- |
    -- The options that apply to every mode.
  , clo_commonOptions :: CommonModeOptions
 
    -- |
    -- The optional user-specified location of the
    -- configuration file. If the user does not specify
    -- it, then it is searched in a predetermined path
    -- list. See the 'Config.getConf' function for
    -- more information.
  , clo_mbConfigFile :: Maybe FilePath
  } deriving Show

-- |
-- The parser which provides the functionalities to interpret
-- the command line arguments and, if they are correct, build
-- a 'CommandLineOptions' value from them.
cloParser :: OA.Parser CommandLineOptions
cloParser =
  CommandLineOptions 
  <$> modeParser 
  <*> cmoParser
  <*> configFileParser
  where
    -- Combine all mode parsers as sub-parsers of a 
    -- single 'modeParser'.
    modeParser :: OA.Parser Mode
    modeParser =
      OA.subparser $
        ( OA.command "add" $ 
            OA.info addModeParser $ 
              OA.progDesc addModeDesc
        )
        <>
        ( OA.command "build" $
            OA.info buildModeParser $
              OA.progDesc buildModeDesc
        )
        <>
        ( OA.command "doc" $
            OA.info docModeParser $
              OA.progDesc docModeDesc
        )
        <>
        ( OA.command "remove" $
            OA.info removeModeParser $
              OA.progDesc removeModeDesc
        )

    -- Description for the @add@ mode subparser.
    addModeDesc :: String
    addModeDesc = 
      "Used to add the declarations of files to the project\
      \ files, if they are not already declared." 

    -- Description for the @build@ mode subparser.
    buildModeDesc :: String
    buildModeDesc = 
      "Used to build the object files associated with some\
      \ proof file." 

    -- Description for the @doc@ mode subparser.
    docModeDesc :: String
    docModeDesc = 
      "Used to build the documentation of proof files.\
      \\n\
      \Notice that currently, building the documentation for\
      \ specific files is not available, and can only be done\
      \ in a per-Discipline Directory basis. Therefore, this\
      \ mode will build the documentation of the entire \
      \ Discipline Directory containing the given path."

    -- Description for the @remove@ mode subparser.
    removeModeDesc :: String
    removeModeDesc = 
      "Used to remove declarations from project files.\
      \ The declarations can be removed regardless of the\
      \ existence of the files, so this mode can be run\
      \ either before or after files are deleted from\
      \ the disk.\
      \\n\
      \However, notice that this mode does not delete any\
      \ files, it just updates the project file mentions." 


-- MODE-SPECIFIC PARSERS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Notice that, since, for now, there are no mode-specific
-- options for any mode, the parsers for mode-specific
-- options are all simply a return of the default option.

-- |
-- Parser for the @add@ mode.
addModeParser :: OA.Parser Mode
addModeParser = AddMode <$> pure AddModeOptions

-- |
-- Parser for the @build@ mode.
buildModeParser :: OA.Parser Mode
buildModeParser = BuildMode <$> pure BuildModeOptions

-- |
-- Parser for the @doc@ mode.
docModeParser :: OA.Parser Mode
docModeParser = DocMode <$> pure DocModeOptions

-- |
-- Parser for the @remove@ mode.
removeModeParser :: OA.Parser Mode
removeModeParser = RemoveMode <$> pure RemoveModeOptions


-- COMMON MODE OPTIONS PARSER
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- The parser which parses all the options that are common
-- to all operation modes.
cmoParser :: OA.Parser CommonModeOptions
cmoParser = 
  CommonModeOptions <$> actLocParser <*> assumeYesParser


-- BASIC PARSERS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- The general parser for action location.
--
-- It can take two flags @--path@ or @-p@, for specifying
-- a path, or @--discdir@ or @-d@ for specifying the base
-- name of a Discipline Directory. Another option is to 
-- just write a path, without specifying any flag, which
-- means the argument is automatically assumed to be a path.
actLocParser :: OA.Parser ActionLocation
actLocParser = 
  let
    -- The parser for paths, accepting the @--path@ or
    -- @-p@ flags.
    filePathParser :: OA.Parser FilePath
    filePathParser =
      OA.strOption $
        OA.long "path"
        <> OA.short 'p'
        <> OA.metavar "PATH"
        <> OA.help filePathHelp

    -- The parser for Discipline Directory base names,
    -- accepting the @--discdir@ or @-d@ flags.
    discDirParser :: OA.Parser String
    discDirParser = 
      OA.strOption $
        OA.long "discdir"
        <> OA.short 'd'
        <> OA.metavar "DISCDIR"
        <> OA.help discDirHelp

    -- Another parser for paths, this time not having any
    -- flags at all.
    noArgsFilePathParser :: OA.Parser FilePath
    noArgsFilePathParser =
      OA.argument OA.str $
        OA.metavar "PATH"
        <> OA.help noArgsFilePathHelp

    -- Help text for the 'filePathParser' parser.
    filePathHelp :: String
    filePathHelp = 
      "Used to specify a path. If this is the path of a\
      \ directory, then the mode-specific action (add,\
      \ build, ...) will be performed on all files descending\
      \ (directly or indirectly) from this path.\
      \\n\
      \Specifically for the doc mode, since individual file\
      \ documentation building is not available, the program\
      \ will instead build the documentation of the\
      \ Discipline Directory containing the given path, if\
      \ there is one."

    -- Help text for the 'discDirParser' parser.
    discDirHelp :: String
    discDirHelp = 
      "Used to specify a Discipline Directory (by its base\
      \ name). The mode-specific action (add, build, ...)\
      \ will be performed for all files descending\
      \ (directly or indirectly) from the specified\
      \ Discipline Directly.\
      \\n\
      \Specifically for the doc mode, since individual\
      \ file documentation building is currently not\
      \ available, and has to be done in a per-Discipline\
      \ Directory basis, this command will build the\
      \ documentation of the Discipline Directory whose\
      \ base name is DISCDIR."

    -- Help text for the 'noArgsFilePathHelp' parser.
    noArgsFilePathHelp :: String
    noArgsFilePathHelp = 
      "If neither the  --path (or its alternative forms) or\
      \ the --discdir (or its alternative forms) are used,\
      \ then the argument is interpreted as if the --path\
      \ flag was used."
  in
    (ActLocPath <$> noArgsFilePathParser)
    <|> (ActLocPath <$> filePathParser)
    <|> (ActLocDiscDir <$> discDirParser)
    -- (ActLocPath <$> filePathParser)
    -- <|> (ActLocDiscDir <$> discDirParser)

-- |
-- The parser for the automatic confirmation flag.
-- This parser is a switch, meaning that it has a default
-- value of 'False', but, if the flags @-y@, @--yes@, or
-- @--assume-yes@ are used in the command, it is set to
-- 'True', and the commands will not ask for used input,
-- assuming that the answer is already yes.
assumeYesParser :: OA.Parser Bool
assumeYesParser =
  let
    -- Help text for the 'assumeYesParser' parser.
    assumeYesHelp :: String
    assumeYesHelp = 
      "Do not prompt the user for confirmation, instead,\
      \ automatically answer \"yes\" to all prompts." 
  in
    OA.switch $
      OA.long "assume-yes"
      <> OA.long "yes"
      <> OA.short 'y'
      <> OA.help assumeYesHelp

-- |
-- Parser for the (optional) specification of the 
-- configuration file location. If not given, the system
-- will look for the configuration file in a list of
-- predetermined paths. See the 'Config.getConf' function
-- for more information on that.
--
-- This parser accepts as flags @--config-file@ and @-c@.
configFileParser :: OA.Parser (Maybe FilePath)
configFileParser =
  let
    -- Help text for the 'configFileParser' parser
    configFileHelp :: String 
    configFileHelp = 
      "Use the file CONFIGFILE as a configuration file." 
  in
    optional $ OA.strOption $
      OA.long "config-file"
      <> OA.short 'c'
      <> OA.metavar "CONFIGFILE"
      <> OA.help configFileHelp
