-- |
-- The goal of the "Config" module is to provide an 
-- overarching environment data structure which contains the
-- information about how the proof files and the associated
-- databases are distributed within the filesystem itself, as
-- well as to provide a centralized place for controling how
-- the structure of the project is defined, such as which are
-- the Discipline Directories, and which Discipline 
-- Directories have access to which proofs.
module Config
  (
    -- * General configuration environment
    -- |
    -- The 'ProofSystemConfig' is the general environment
    -- data type. It contains the information about how
    -- the proof system is setup: the Discipline Directories,
    -- the in-disk paths, the known extensions for file
    -- paths, etc, as well as information about which commands
    -- to use for compiling files, rebuilding Makefiles,
    -- and building documentation, for example.
    ProofSystemConfig

    -- ** 'ProofSystemConfig' getter functions
  , psRootPath
  , disciplineDirectories
  , proofFileExtensions
  , nameValidityPredicate
  , declarativeToBuild
  , hiddenPaths
  , hiddenBaseNames
  , projectFileName
  , compilationCommand
  , makefileBuildCommand
  , docBuildCommand
  , coqbinPath

    -- ** Building a @ProofSystemConfig@ environment
  , getConf

    -- * Discipline Directories
    -- |
    -- Discipline Directories are a core aspect of how
    -- the proof system is structured. Each Discipline
    -- Directory contains the proof files that relate
    -- to a certain discipline (like Mathematics, or
    -- Physics), as well as the documentation of the
    -- files that are used for proofs in that discipline.
  , DisciplineDirectory
    ( DisciplineDirectory
    , discDirBaseName
    , accessibleDiscDirs
    )
  ) where


-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import qualified Control.Monad.Trans.Except as CTE
  ( ExceptT
    ( ExceptT
    )
  , runExceptT
  )
import qualified System.FilePath as SF
  ( (<.>)
  , (</>)
  , dropExtension
  )

import qualified Data.Ini.Config as DIC
  ( IniParser
  , field
  , fieldMb
  , parseIniFile
  , section
  )

import qualified Data.Text as DT
  ( pack
  , unpack
  )

import qualified UnliftIO.Directory as UIOD
  ( XdgDirectory
    ( XdgConfig
    )
  , canonicalizePath
  , doesFileExist
  , getHomeDirectory
  , getXdgDirectory
  , makeAbsolute
  )

import qualified UnliftIO.Exception as UIOE
  ( IOException
  , handleIO
  )


-- DATA TYPES
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- The environment configuration type. This data type has
-- hidden constructors, but for each constructor, an
-- equivalent getter function is exported. This way, the
-- only way to construct a configuration environment is
-- through the 'getConf' function, thus avoiding bugs
-- that might happen by modifying the environment passed
-- to other functions.
data ProofSystemConfig =
  ProofSystemConfig
  { _psRootPath :: FilePath
  , _disciplineDirectories :: [DisciplineDirectory]
  , _proofFileExtensions :: [String]
  , _nameValidityPredicate :: String -> Bool
  , _declarativeToBuild :: FilePath -> FilePath
  , _hiddenPaths :: [FilePath]
  , _hiddenBaseNames :: [String]
  , _projectFileName :: String
  , _compilationCommand :: String
  , _makefileBuildCommand :: String
  , _docBuildCommand :: String
  , _coqbinPath :: FilePath
  }

-- |
-- The in-disk path of the root of the Proof System.
-- This field is currently the only field that is obligatory
-- in the configuration file. If successful, the 'getConf'
-- function reads that path in the configuration file,
-- stores it in this field of the environment, and it is
-- used throughout the entire program for things such as:  
--
-- * Defining the paths to the Discipline Directories and
--   their project and Makefiles.  
-- * Checking if a provided path is within the proof system.  
-- * Displaying paths to the screen (the paths to the files
--   are all made relative to the root of the proof system,
--   since they should all be subpaths of it).
psRootPath :: ProofSystemConfig -> FilePath
psRootPath = _psRootPath

-- |
-- Discipline Directories represent a general area 
-- knowledge, like \"Mathematics\" or \"Physics\".
-- Housed within a Discipline Directory, for example, the
-- \"Physics\" one, are various subdirectories containing
-- proof files for many different topics within that
-- discipline, such as \"Thermodynamics\" (which might have
-- itself other subdirectories like \"IdealGasTheory\"),
-- \"Newtonian Mechanics\", and so on.
--
-- Discipline Directories might have access on proofs
-- within other Discipline Directories. For example, the
-- \"Physics\" Discipline Directory is allowed to use
-- the proofs from the \"Mathematics\" Discipline Directory.
-- On the other hand, the proofs in the \"Mathematics\"
-- Discipline Directory are __only allowed to depend__
-- __on proofs from the \"Mathematics\" Discipline__
-- __Directory itself__.
disciplineDirectories
  :: ProofSystemConfig -> [DisciplineDirectory]
disciplineDirectories = _disciplineDirectories

-- |
-- The recognized extensions for proof files.
-- 
-- Currently, there is only one, @".v"@, the extension for
-- Coq proof files.
proofFileExtensions
  :: ProofSystemConfig -> [String]
proofFileExtensions = _proofFileExtensions

-- |
-- The function which tests the validity of a base name.
-- 
-- In general, this function is meant to test things in
-- a "lowest common denominator" across systems, avoiding
-- problems with invalid characters in other file systems.
--
-- Files whose base names do not match these criteria are
-- not automatically reject, but they users should receive
-- warnings, asking for explicit confirmation before
-- including such files in project files.
--
-- The current criteria are:  
--
-- * Have less than 250 characters. Ideally, less than
--   50, preferably, less than 100.
-- * First character should be an uppercase character.
-- * Only ASCII letters (uppercase or lowercase), numbers,
--   and @'_'@ (underscore) (that is, __no spaces__).
--
-- These rules should be enough to avoid problems across
-- systems. Moreover, they satisfy 
-- [Coq's identifier rules](https://coq.github.io/doc/master/refman/language/core/basic.html#term-command), allowing the
-- base name of the file to be used as the name of the module
-- exported by that file.
nameValidityPredicate
  :: ProofSystemConfig -> String -> Bool
nameValidityPredicate = _nameValidityPredicate

-- |
-- Generates the build name of a proof file based
-- on its declarative path. See the 'PathInfo.PathInfo'
-- documentation for more information on build names
-- and declarative paths.
--
-- For the standard value (which applies to Coq @.v@ 
-- files), we simply substitute the @.v@ file extension
-- with a @.vo@ file extension.
declarativeToBuild
  :: ProofSystemConfig -> FilePath -> FilePath
declarativeToBuild = _declarativeToBuild

-- |
-- Paths which should not be recursed into when looking
-- for proof files.
--
-- Hidden directories can be used to store things that
-- are not directly related to the Proof System, but
-- may help it, such as templates.
--
-- These paths must be given relative to the root of the
-- Proof System, and must be such that, when preppended
-- with the canonical path of the root (using whatever
-- separator 'System.FilePath.</>' corresponds to in the
-- system), produces the canonical path of the directory.
hiddenPaths
  :: ProofSystemConfig -> [FilePath]
hiddenPaths = _hiddenPaths

-- |
-- Base names for directories which should not be recursed
-- into when looking for proof files. These apply 
-- everywhere within the Proof System.
--
-- These hidden directories can be used to store things
-- which are not directly related to the Proof System,
-- but may help it, such as textual explanations, or
-- scraps/old versions from proofs.
hiddenBaseNames
  :: ProofSystemConfig -> [String]
hiddenBaseNames = _hiddenBaseNames

-- |
-- The base name of all project files.
--
-- Project files are the files, housed directly inside
-- each Discipline Directory, which contain the paths
-- of all the proof files that other proof files in the
-- same Discipline Directory have access to.
--
-- For example, ideally, if all the proof files have been
-- correctly added, the \"Physics\" Discipline Directory's
-- project file will contain a list of all the files 
-- inside the \"Physics\" Discipline Directory itself 
-- (descending directly or indirectly), but also from 
-- all the files in the \"Mathematics\" Discipline 
-- Directory (descending directly or indirectly), 
-- since the \"Physics\" Discipline Directory has 
-- dependency permission on the \"Mathematics\"
-- Discipline Directory.
--
-- It is this declaration of the files which will allow,
-- later, the building of the correct make files, 
-- allowing for the dependencies to be built correctly.
--
-- Changing this field will require updating the
-- @Makefile@s as well.
projectFileName
  :: ProofSystemConfig -> String
projectFileName = _projectFileName

-- |
-- The command used to compile proof files. 
--
-- This field can be set using the configuration file.
--
-- Normally, the executable for this program is an
-- executable for the GNU @make@ utility.
--
-- The structure of this field (as well as other command
-- fields, such as 'makefileBuildCommand'), is a 'String'
-- with the substitution rules set by the
-- 'SubstitutionParser.subst' function (see its
-- documentation for a more throughly explanation, with
-- some examples). Here, the subsitution list consists of
-- a list of two elements:  
--
-- > [DISCDIR, BUILDNAME]
--
-- where @DISCDIR@ is the absolute path of the 
-- Discipline Directory (where the Makefile is) and
-- @BUILDNAME@ is the 'PathInfo.buildName' field of the
-- 'PathInfo.PathInfo' value associated with the proof
-- file we are trying to build. By default the command is
-- 
-- > "make --directory=%0 %1"
--
-- which, through the 'SubstitutionParser.subst' function,
-- expands to
--
-- > "make --directory=DISCDIR BUILDNAME"
--
-- With this command, we also include in the environment
-- the environment variable @COQBIN@, which is the
-- folder location where the binary executables for
-- @coqc@, @coqdoc@, and @coq_makefile@ are located.
-- This setting is defined in the @ProofSystemConfig@
-- environment's 'coqbinPath' field.
compilationCommand
  :: ProofSystemConfig -> String
compilationCommand = _compilationCommand

-- |
-- The command used to build and rebuild the @Makefile@s
-- whenever a change in the project file of a Discipline
-- Directory occurs (by, for example, adding a new file,
-- or adding new dependencies in a file).
--
-- This command can be set using the configuration file.
--
-- Normally, the executable for this program is an
-- executable for the GNU @make@ utility.
--
-- The structure of this field (as well as other command
-- fields, such as 'compilationCommand'), is a 'String'
-- with the substitution rules set by the
-- 'SubstitutionParser.subst' function (see its
-- documentation for a more throughly explanation, with
-- some examples). Here, the subsitution list consists of
-- a single element:
--
-- > [DISCDIR]
--
-- where @DISCDIR@ is the absolute path of the 
-- Discipline Directory (where the Makefile is).
--
-- By default, the command is:
--
-- > "make --directory=%0 Makefile.coq"
--
-- This command produces the CoqMakefile called 
-- @Makefile.coq@, which is the the intermediate makefile
-- created by the @coq_makefile@ program, and called upon
-- by the already present (and immutable) @Makefile@.
--
-- With this command, we also include in the environment
-- the environment variable @COQBIN@, which is the
-- folder location where the binary executables for
-- @coqc@, @coqdoc@, and @coq_makefile@ are located.
-- This setting is defined in the @ProofSystemConfig@
-- environment's 'coqbinPath' field.
--
-- Notice that, for technical reasons, @coq_makefile@
-- forbids the output file to be outside the current
-- working directory. Therefore, in this command, the
-- current working directory is set as the Discipline
-- Directory whose Makefile we want to rebuild. Keep
-- that in mind when setting the executable path, since,
-- according to the documentation of the 'UnliftIO.Process'
-- module, used for these process calls:  
--
-- * If cwd is provided, it is implementation-dependent 
--   whether relative paths are resolved with respect to 
--   cwd or the current working directory, so absolute 
--   paths should be used to ensure portability. 
makefileBuildCommand :: ProofSystemConfig -> String
makefileBuildCommand = _makefileBuildCommand

-- |
-- The command used to build (and rebuild) the 
-- documentation of the proof files.
--
-- This command can be set using the configuration file.
--
-- Normally, the executable for this program is an
-- executable for the GNU @make@ utility.
--
-- The structure of this field (as well as other command
-- fields, such as 'compilationCommand'), is a 'String'
-- with the substitution rules set by the
-- 'SubstitutionParser.subst' function (see its
-- documentation for a more throughly explanation, with
-- some examples). Here, the subsitution list consists of
-- a single element:
--
-- > [DISCDIR]
--
-- where @DISCDIR@ is the absolute path of the 
-- Discipline Directory (where the Makefile is).
--
-- By default, the command is:
--
-- > "make --directory=%0 html"
docBuildCommand :: ProofSystemConfig -> String
docBuildCommand = _docBuildCommand

-- |
-- The directory where the @coqc@, @coqdoc@, and
-- @coq_makefile@ executables are found.
--
-- This command can be set using the configuration file.
--
-- The value set here will be supplied to the environment
-- as the environment variable @COQBIN@
-- when calling commands, such as those in the
-- 'compilationCommand' and 'makefileBuildCommand' fields.
--
-- For example, in the case of the 'makefileBuildCommand',
-- this setting makes so that the versions of the Coq
-- executables located in the path defined here are the
-- ones used.
--
-- If the environment already has a @COQBIN@ environment
-- variable, this one will overwrite it (just within this
-- program, not globally), __unless__ this field is set
-- to @""@ (empty 'String'), in which case the environment
-- passed to called commands will be the same inherited
-- by this program. Before being passed to the environment
-- however, this path will be made canonical and will have
-- a trailing separator appended to it.
--
-- By default, its value is @""@ (i.e. use whatever is
-- on the system's environment).
coqbinPath :: ProofSystemConfig -> FilePath
coqbinPath = _coqbinPath

-- |
-- The 'DisciplineDirectory' data type encodes the property
-- of the Discipline Directory abstraction.
--
-- A Discipline Directory is a (direct) subdirectory of the
-- Proof System root (wherever it is in the user's actual
-- system), which indicates the area of knowledge of all
-- elements within  it.
--
-- For example, the \"Physics\" Discipline Directory may 
-- contain various subdirectories refering to more specific 
-- topics in Physics, such as \"Thermodynamics\" (which 
-- might have itself other subdirectories like 
-- \"IdealGasTheory\"), or \"Newtonian Mechanics\", 
-- and so on.
--
-- Discipline Directories might have access on proofs
-- within other Discipline Directories. For example, the
-- \"Physics\" Discipline Directory is allowed to use
-- the proofs from the \"Mathematics\" Discipline Directory.
-- On the other hand, the proofs in the \"Mathematics\"
-- Discipline Directory are __only allowed to depend__
-- __on proofs from the \"Mathematics\" Discipline__
-- __Directory itself__. All Discipline Directories are
-- allowed to depend on proofs within itself.
data DisciplineDirectory =
  DisciplineDirectory
  {
    -- |
    -- The base name of the Discipline Directory.
    --
    -- Notice that this is __just the base name__ of the
    -- directory, which __must be direct subdirectory of__
    -- __the root of the Proof System root__.
    --
    -- For example, in the \"Mathematics\" Discipline 
    -- Directory, this field is simply @\"Mathematics\"@, 
    -- and __not__ @\/path\/to\/root\/Mathematics@.
    discDirBaseName :: String

    -- |
    -- The list of base names of the other Discipline 
    -- Directories this Directory has dependency access.
    --
    -- For example: Proofs within the \"Physics\" Discipline
    -- Directory are allowed to depend on proofs in the
    -- \"Mathematics\" Discipline Directory. This way, one can
    -- use a proof of, for example, some algebraic property
    -- proved in the more abstract mathematical context
    -- within the proof of some kind of Physics theory.
    -- However, the \"Mathematics\" Discipline Directory does
    -- not have dependency acess on the \"Physics\" Discipline
    -- Directory, meaning mathematical proofs cannot depend
    -- on proofs from Physics results.
    --
    -- This serves two main purposes: The first, more
    -- practical, is to provide some organizational
    -- protection against circular dependency. The second,
    -- more philosophical, is to preserve the intention of
    -- keeping proofs dependent only on fields that have
    -- equal or higher degree of abstraction. This way, one
    -- is incentivized to construct the proofs in the most
    -- abstract and widely-used way possible, thus avoiding
    -- needless repetition. For example, it would be better
    -- to prove some useful mathematical result directly in
    -- the \"Mathematics\" context, allowing it to be acessed
    -- by future proofs in \"Physics\" and \"Mathematics\", 
    -- rather than prove it within the \"Physics\" Discipline
    -- Directory, forcing one to reprove it again if needed
    -- in a mathematical proof.
    --
    -- This list has to contain __base names__, and __not__
    -- full paths. That is, to depend on the \"Mathematics\"
    -- Discipline Directory, write only @\"Mathematics\"@,
    -- and __not__ @\/path\/to\/root\/Mathematics@.
    --
    -- Do not include the base name of the current
    -- Discipline Directory. The self-dependency access is
    -- automatically allowed.
    , accessibleDiscDirs :: [String]
  } deriving (Show, Read, Eq)


-- DEFAULT CONFIGURATION
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- The default configuration element. The 'getConf' function
-- builds up the actual configuration element based on its
-- structure, overwritting its fields based on what is
-- read in the configuration file.
defaultConfig :: ProofSystemConfig
defaultConfig =
  ProofSystemConfig
  { _psRootPath = ""
  , _disciplineDirectories =
    [ DisciplineDirectory
      { discDirBaseName = "Mathematics"
      , accessibleDiscDirs = []
      }
    , DisciplineDirectory
      { discDirBaseName = "Physics"
      , accessibleDiscDirs = ["Mathematics"]
      }
    ]
  , _proofFileExtensions = [".v"]
  , _nameValidityPredicate = const False
  , _declarativeToBuild =
      \decPath -> SF.dropExtension decPath SF.<.> "vo"
  , _hiddenPaths = ["Templates"]
  , _hiddenBaseNames = ["Scraps"]
  , _projectFileName = "_CoqProject"
  , _compilationCommand = "make --directory=%0 %1"
  , _makefileBuildCommand =
      "make --directory=%0 Makefile.coq"
  , _docBuildCommand = "make --directory=%0 html"
  , _coqbinPath = ""
  }

-- READING CONFIGURATIONS FROM THE CONFIG FILE
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This is an internal data type, used as an intermediate
-- for the configuration parser.
--
-- The fields in this record type contain only the
-- configuration fields of a 'ProofSystemConfig' which can
-- be set through the configuration file. Some of them are
-- wrapped in the 'Maybe' monad, because they are not
-- obligatory fields, and the user might wish to just
-- omit them from the configuration file, which makes the
-- default ones take place instead.
data ConfigurableFields =
  ConfigurableFields
  {
    -- The __obligatory__ field 'psRootPath', determining
    -- the in-disk location of the root of the proof system.
    set_psRootPath :: FilePath

    -- The __optional__ field 'compilationCommand',
    -- determining the shell command to compile a proof file.
  , set_compilationCommand :: Maybe String

    -- The __optional__ field 'makefileBuildCommand',
    -- determining the shell command to build or rebuild
    -- the @(Coq)Makefile@ (after, for example, a new file
    -- has been added).
  , set_makefileBuildCommand :: Maybe String

    -- The __optional__ field 'docBuildCommand', determining
    -- the shell command to build the documentation of
    -- the proof files.
  , set_docBuildCommand :: Maybe String

    -- The __optional__ field 'coqbinPath', determining the
    -- in-disk location of the directory containing the 
    -- @coqc@, @coqdoc@ and @coq_makefile@ binary executables.
  , set_coqbinPath :: Maybe FilePath
  } deriving Show

-- |
-- This function attempts to read the configuration file.
-- It can be given an explicit path to the file as argument 
-- (that is, given a 'Just'-wrapped path) which is used, for
-- example, to set the configuration file by through a
-- command line argument.
--
-- However, if given 'Nothing' as argument, it will look for
-- the configuration file (called @config.ini@) in any 
-- of the paths below, following the respective order 
-- (using @\/@ to denote the separator, but might be
-- something else on your system),  
--
-- * @XDGCONFIGDIR\/pruvarilo@ where @XDGCONFIG@ is the
--   XDG configuration directory, as returned by the
--   'UIOD.getXdgDirectory' function. According to the
--   documentation, this function looks at the 
--   @XDG_CONFIG_HOME@ environment variable, usually
--   @~\/.config@ in POSIX and @%APPDATA%@ on Windows.  
-- * @HOMEDIR/.pruvarilo@ where @HOMEDIR@ is the user's home
--   directory, as returned by the 'UIOD.getHomeDirectory'
--   function.  
-- 
-- If the configuration file cannot be found anywhere,
-- this function returns a (monadic) 'Left'-wrapped
-- 'String', detailing what went wrong.
getConf
  :: Maybe FilePath
  -- ^
  -- An optional (that is, use 'Nothing' instead if one does
  -- not which to specify the path, and instead use the
  -- default search order) path where the configuration file
  -- should be looked.
  -> IO (Either String ProofSystemConfig)
getConf mbPath =
  let
    -- First, we decide what path we need to take.
    path :: IO (Either String FilePath)
    path =
      case mbPath of
        Just providedPath -> return $ Right providedPath
        Nothing ->
          -- No path provided, so we must use what the lookup
          -- function gives us instead.
          lookupConfigFile
  in
    -- For simplicity sake, we will transform the
    -- 'IO (Either String t)' into an 'ExceptT String IO t',
    -- allowing us to use the short-circuit behavior of the
    -- 'Either' monad through 'ExceptT'.
    CTE.runExceptT $
      do
        configPath <- CTE.ExceptT path

        -- Attempts to produce a 'ConfigurableFields' value
        -- from what was read in the configuration file.
        settings <-
          CTE.ExceptT $
            UIOE.handleIO
              ( \e ->
                -- This is the error message shown if we
                -- cannot read the file for some reason.
                -- The most likely reason being insufficient
                -- permissions or system resources, since the
                -- previous step would take care of checking
                -- its existence.
                return $ Left $
                  "Failure to read the contents of the "
                  ++ "configuration file: "
                  ++ configPath
                  ++ "\n"
                  ++ "  Exception: "
                  ++ show e
              )
              (
                flip DIC.parseIniFile configParser .
                  DT.pack <$> readFile configPath
              )

        -- We now deal with each one of the settings fields.
        -- For the root path, we need to canonicalize it.
        root <-
          CTE.ExceptT $
            UIOE.handleIO
              ( \e ->
                return $ Left $
                  "Failure to canonicalize the path of the "
                  ++ "proof system root provided in the "
                  ++ "configuration file: "
                  ++ set_psRootPath settings
              )
              (
                Right <$>
                  UIOD.canonicalizePath (
                      set_psRootPath settings)
              )

        -- The optional fields.
        let
          compCmd :: Maybe String
          compCmd = set_compilationCommand settings
          makeCmd :: Maybe String
          makeCmd = set_makefileBuildCommand settings
          coqbin :: Maybe FilePath
          coqbin = set_coqbinPath settings

        -- Finally, we return the default configuration
        -- value, with the modified fields. In the case
        -- of the optional fields, only if the fields
        -- were actually found (so their are not 'Nothing').
        return
          defaultConfig
          { _psRootPath = root
          , _compilationCommand =
              case compCmd of
                Nothing -> compilationCommand defaultConfig
                Just cmd -> cmd
          , _makefileBuildCommand =
              case makeCmd of
                Nothing -> makefileBuildCommand defaultConfig
                Just cmd -> cmd
          , _coqbinPath =
              case coqbin of
                Nothing -> coqbinPath defaultConfig
                Just p -> p
          }

-- |
-- This function is the parser for the configuration file.
-- It will attempt to read the configuration file. To call
-- on this parser, use the 'DIC.parseIniFile' function.
configParser :: DIC.IniParser ConfigurableFields
configParser =
  do
    -- Get the root. It is in the section
    -- @STRUCTURAL SETTINGS@ under the field @rootLocation@.
    -- Notice that we will still have to do some treatment
    -- to this field (making it canonical, for instance) before
    -- placing it on the final 'ProofSystemConfig' value.
    root <-
      DIC.section (DT.pack "STRUCTURAL SETTINGS") $
        DIC.field (DT.pack "rootLocation")

    -- The next elements are all in the @COMMANDS@ section.
    -- They are optional.
    compCmd <-
      DIC.section (DT.pack "COMMANDS") $
        DIC.fieldMb (DT.pack "compilationCommand")

    makeCmd <-
      DIC.section (DT.pack "COMMANDS") $
        DIC.fieldMb (DT.pack "buildMakefileCommand")

    docCmd <-
      DIC.section (DT.pack "COMMANDS") $
        DIC.fieldMb (DT.pack "docBuildCommand")

    coqbin <-
      DIC.section (DT.pack "COMMANDS") $
        DIC.fieldMb (DT.pack "coqbinLocation")

    -- We now collect everything in a 'ConfigurableFields'
    -- value. However, notice that all those fields are of
    -- 'Text' type. We use 'DT.unpack' to make them 'String'
    -- (which is equal to 'FilePath') instead.
    return
      ConfigurableFields
      { set_psRootPath = DT.unpack root
      , set_compilationCommand = DT.unpack <$> compCmd
      , set_makefileBuildCommand = DT.unpack <$> makeCmd
      , set_docBuildCommand = DT.unpack <$> docCmd
      , set_coqbinPath = DT.unpack <$> coqbin
      }

-- |
-- This is an auxiliar function to the main, exported,
-- function 'getConf'. The purpose of this one is to
-- go through the list of directories that should be checked
-- for the configuration file and return the first one
-- where a file named "config" can be found.
--
-- This file __is not read__, and its (canonicalized)
-- path is returned to "getConf".
--
-- If checking the first directory produces an
-- 'UIOE.IOException', it is caught, and we attempt to
-- go to the second one instead, and so on.
--
-- If all directories fail, we return a 'String'
-- explaining all the failures.
lookupConfigFile :: IO (Either String FilePath)
lookupConfigFile =
  let
    -- Here we define a function which checks for the file
    -- in the XDG configuration directory.
    lookupXdg :: IO (Either UIOE.IOException (Bool, FilePath))
    lookupXdg =
      -- Wrap everything in a 'handleIO', since those
      -- operations can throw exceptions.
      UIOE.handleIO (return . Left) $
        do
          -- Try to get the XDG configuration directory.
          xdgDir <- UIOD.getXdgDirectory UIOD.XdgConfig ""

          -- Try to make this path canonical.
          xdgDir' <- UIOD.canonicalizePath xdgDir

          -- Add the configuration file's base name.
          let
            confPath :: FilePath
            confPath =
              xdgDir' SF.</> "pruvarilo" SF.</> "config.ini"

          -- Tests if the configuration file exists.
          hasConfFile <- UIOD.doesFileExist confPath

          if hasConfFile
          then return $ Right (True, confPath)
          else return $ Right (False, confPath)


    -- And here we define a function which checks for
    -- the file in @HOME/.pruvarilo/@.
    lookupHome
      :: IO (Either UIOE.IOException (Bool, FilePath))
    lookupHome =
      UIOE.handleIO (return . Left) $
        do
          -- Try to get the home directory.
          home <- UIOD.getHomeDirectory

          -- Try to make it canonical.
          home' <- UIOD.canonicalizePath home

          -- Add the rest of the path.
          let
            confPath :: FilePath
            confPath =
              home' SF.</> ".pruvarilo" SF.</> "config.ini"

          -- Tests if the configuration file exists.
          hasConfFile <- UIOD.doesFileExist confPath

          if hasConfFile
          then return $ Right (True, confPath)
          else return $ Right (False, confPath)
  in
    do
      -- Gets both existence test results.
      xdgTest  <- lookupXdg
      homeTest <- lookupHome

      -- The XDG test takes precedence.
      case xdgTest of
        Right (xdgFound, xdgPath) ->
          if xdgFound
          then
            return $ Right xdgPath
          else
            -- No exception was thrown, but we did not get
            -- the file in the XDG configuration directory.
            -- So, we prepare an error message.
            let
              xdgNotFound :: String
              xdgNotFound =
                "XDG configuration directory: No file found."
                ++ "\n"
                ++ "  File looked for: "
                ++ xdgPath
            in
              -- We then look for the home directory lookup.
              case homeTest of
                Right (homeFound, homePath) ->
                  if homeFound
                  then
                    return $ Right homePath
                  else
                    -- No exception was thrown, but we
                    -- did not get the file in 
                    -- @HOME/.pruvarilo/@ either. 
                    -- Prepare its error message.
                    let
                      homeNotFound :: String
                      homeNotFound =
                        ("HOME" SF.</> ".pruvarilo")
                        ++ ": No file found."
                        ++ "\n"
                        ++ "  File looked for: "
                        ++ homePath
                    in
                      return $ Left $
                        xdgNotFound ++ "\n" ++ homeNotFound
                Left homeExc ->
                  let
                    homeExcMsg :: String
                    homeExcMsg =
                      ("HOME" SF.</> ".pruvarilo")
                      ++ ": IOException thrown."
                      ++ "\n"
                      ++ "  Exception: "
                      ++ show homeExc
                  in
                    return $ Left $
                      xdgNotFound ++ "\n" ++ homeExcMsg
        Left xdgExc ->
          let
            xdgExcMsg :: String
            xdgExcMsg =
              "XDG configuration directory: "
              ++ "IOException thrown."
              ++ "\n"
              ++ "  Exception: "
              ++ show xdgExc
          in
            -- We then look for the home directory lookup.
            case homeTest of
              Right (homeFound, homePath) ->
                if homeFound
                then
                  return $ Right homePath
                else
                  -- No exception was thrown, but we
                  -- did not get the file in @HOME/.pruvarilo/@
                  -- either. Prepare its error message.
                  let
                    homeNotFound :: String
                    homeNotFound =
                      ("HOME" SF.</> ".pruvarilo")
                      ++ ": No file found."
                      ++ "\n"
                      ++ "  File looked for: "
                      ++ homePath
                  in
                    return $ Left $
                      xdgExcMsg ++ "\n" ++ homeNotFound
              Left homeExc ->
                let
                  homeExcMsg :: String
                  homeExcMsg =
                    ("HOME" SF.</> ".pruvarilo")
                    ++ ": IOException thrown."
                    ++ "\n"
                    ++ "  Exception: "
                    ++ show homeExc
                in
                  return $ Left $
                    xdgExcMsg ++ "\n" ++ homeExcMsg


