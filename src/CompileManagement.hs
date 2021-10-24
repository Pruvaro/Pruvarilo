-- |
-- The intent of this module is to provide the functions
-- which will deal with the files that handle the compiling
-- and compile order of the proof files.
--
-- The way the compilation goes, as is relevant to this
-- module, is as follows:  
--
-- * First, for every Discipline Directory, there must be a
--   list of all the Proof Files this Discipline Directory
--   can see (i.e. has dependency access permission). This
--   list is called the \"project file\", and its creation
--   and maintenance is handled by another module,
--   the "ProjectFileManagement" module.  
-- * Using the project file, we can build a @Makefile@,
--   containing the instructions on how to compile
--   every file within that Discipline Directory, including
--   its dependencies (which might even be in other Discipline
--   Directories), Those dependencies must, of course,
--   be built before the file itself, an order handled by
--   the created @Makefile@.
--
--     * In particular, in the case of Coq, this step is 
--     handled by an auxiliar Coq tool called 
--     @coq_makefile@. This
--     tool itself produces an intermediate file, which
--     we call a CoqMakefile, and it has the default base name
--     @Makefile.coq@. The CoqMakefile is
--     then called by a wrapper @Makefile@. 
--     This wrapper file does not need to be rebuilt 
--     everytime a new proof file is added/removed from the
--     Discipline Directory. It contains some definitions 
--     needed to make the compilation work (like the 
--     @INSTALLDEFAULTROOT@ field), and, as described by 
--     the [Coq documentation](https://coq.inria.fr/refman/practical-tools/utilities.html?highlight=makefile):  
--
--         * The advantage of a wrapper, compared to directly 
--         calling the generated Makefile, is that it 
--         provides a target independent of the version of 
--         Coq to regenerate a Makefile specific to the 
--         current version of Coq.
--         Additionally, the master Makefile 
--         can be extended with targets not specific to Coq. 
--         Including the generated makefile with an include 
--         directive is discouraged, since the contents of 
--         this file, including variable names and status of 
--         rules, may change in the future.  
--
-- * Finally, with the @Makefile@ correctly generated (and
--   other possible intermediate files), then we can call
--   upon the 'buildName' field of a 'PathInfo' representing
--   a file in order to build it.  
--
-- The coordination of all those steps is handled by functions
-- defined in this module.
module CompileManagement
  ( -- * Compiling proof files
    -- $compilingProofFiles
    compileProofFile

    -- * (RE)building the (Coq)Makefiles
    -- $buildingTheMakefiles
  , buildMakefileIn
  , buildMakefileDependent
  , buildMakefileAll

    -- * Auxiliar functions
  , adaptEnvironment
  ) where

-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Config
  ( ProofSystemConfig
  , DisciplineDirectory
    ( discDirBaseName
    )
  , compilationCommand
  , coqbinPath
  , disciplineDirectories
  , makefileBuildCommand
  , psRootPath
  , projectFileName
  )

import PathInfo
  ( PathInfo
  , buildName
  , housingDiscDirBaseName
  )

import ProjectFileManagement
  ( allowedDiscDirs
  )

import SubstitutionParser
  ( subst
  )

import Control.Monad.IO.Unlift as CMIOU
  ( MonadUnliftIO
  )

import qualified Control.Monad.IO.Unlift as CMIOU
  ( MonadUnliftIO
  )

import qualified Control.Monad.Trans.Class as CTC
  ( lift
  )

import qualified Control.Monad.Trans.Except as CTE
  ( ExceptT
    ( ExceptT
    )
  , runExceptT
  )

import qualified Control.Monad.Trans.Reader as CTR
  ( ReaderT
    ( runReaderT
    )
  , asks
  )

import qualified System.Exit as SE
  ( ExitCode
    ( ExitSuccess
    , ExitFailure
    )
  )

import qualified System.FilePath as SF
  ( (</>)
  , addTrailingPathSeparator
  )

import qualified UnliftIO.Directory as UIOD
  ( canonicalizePath
  )

import qualified UnliftIO.Environment as UIOEn
  ( getEnvironment
  )

import qualified UnliftIO.Exception as UIOE
  ( IOException
  , handleIO
  )

import qualified UnliftIO.Process as UIOP
  ( CreateProcess
    ( cwd
    , env
    )
  , createProcess
  , shell
  , waitForProcess
  )


-- (RE)BUILDING THE MAKEFILES
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- $buildingTheMakefiles
--
-- In this section, we have functions which handle the
-- creation of Makefiles (or, more specifically in the Coq
-- case, of CoqMakefile intermediate files) for a specific
-- Discipline Directory, or for all Discipline Directories
-- with dependency access on a given file, or even to all
-- Discipline Directories, independently of dependency access.
--
-- Rebuilding the CoqMakefiles might be necessary for
-- several reasons. For example, if one just added a new file,
-- and, using the function in the "ProjectFileManagement"
-- module, has now added a declaration of the file to the
-- project file all correct Discipline Directories, that
-- still is not enough for that file to be correctly
-- compiled, since the CoqMakefiles of those Discipline
-- Directories are still in the old state, which does not
-- know of the existence of this new file.
--
-- So, here, it is necessary to call upon these functions to
-- rebuild that CoqMakefile, now aware of the existence of
-- the new file, so that it can be built.
--
-- Another reason for rebuilding the @CoqMakefile@s is if
-- one adds a new dependency to the file, by, for example,
-- adding to some arbitrary proof file @YYYY.v@,
--
-- > Require Import XXXX
--
-- for some module @XXXX@ to some file. The issue here is,
-- previously, the @CoqMakefile@ did not have the file
-- where @XXXX@ is defined as a dependency of @YYYY.v@, which
-- means that that file will not be compiled before @YYYY.v@
-- (however, compilation still might work, if that module
-- already has an object, @.vo@ file. But, if that file is
-- edited, its @.vo@ file will __not__ be rebuilt upon call
-- to compile @YYYY.v@, as one would expect).

-- |
-- (Re)builds the (Coq)Makefile in a Discipline Directory
-- whose __base name__ (not full path) is specified by
-- a given @String@ argument.
--
-- Since this involves a call to an external, impure process,
-- it might of course fail. Any @UIOE.IOException@ thrown
-- internally is caught and reported back, encapsulated in
-- a 'Left' constructor.
--
-- Should the command not thrown an exception, this function
-- will wait for it to finish, and return its exit code.
buildMakefileIn
  :: CMIOU.MonadUnliftIO m
  => String
  -- ^
  -- The base name of the Discipline Directory whose
  -- (Coq)Makefile must be rebuilt.
  -> CTR.ReaderT 
      ProofSystemConfig 
      m 
      (Either UIOE.IOException SE.ExitCode)
buildMakefileIn discDir =
  -- Inside the 'ReaderT ProofSystemConfig m' monad.
  do
    -- Here we ask the environment for the absolute path of
    -- the root of the Proof System. This will be used to
    -- find the path of the Discipline Directory.
    root <- CTR.asks psRootPath

    -- We now build the path of the Discipline Directory.
    let
      discDirPath :: FilePath
      discDirPath = root SF.</> discDir

    -- Now we take the the command used to
    -- build the @Makefile@s, also from the environment.
    makeCmd <- CTR.asks makefileBuildCommand


    -- At this point, we have to run two procedures, both
    -- which produce a result of type
    -- > ReaderT ProofSystemConfig m (Either IOExcept t)
    -- for some type t (this is the thing that is different
    -- for both).
    --
    -- However, if the first fails, we do not have to do
    -- the second. Thus, we need the short-circuit monadic
    -- behavior of 'Either'. However, since 'Either' is in
    -- the bottom of the layer, we will have to use monad
    -- transformers to wrap everything in 'ExceptT', giving
    -- us things of type
    -- > ExceptT IOExcept (ReaderT ProofSystemConfig m) t
    -- again, for some type @t@.
    --
    -- In the end, we use @runExceptT@ to bring it back to
    -- the excepted type.
    CTE.runExceptT $
      do
        -- Get the new environment, changed with the fields
        -- of the 'ProofSystemConfig' environment.
        -- If this fails (because some exception was thrown
        -- and caught internally in 'adaptEnvironment',
        -- the next step will not even be executed.
        newEnv <- CTE.ExceptT $ adaptEnvironment

        let
          -- Using @makeCmd@, we can construct a
          -- 'CreateProcess' value using 'shell', which will
          -- put some defaults in some unnecessary fields of
          -- the 'CreateProcess' value.
          crPr :: UIOP.CreateProcess
          crPr = 
            (UIOP.shell $ subst makeCmd [discDirPath]){
              UIOP.cwd = Just discDirPath
            , UIOP.env = Just newEnv
            }

        -- Now we wrap all in a 'handleIO'. If any exception is
        -- thrown, we will encapsulate it and return.
        CTE.ExceptT $
          UIOE.handleIO
            (\e -> return $ Left e)
            (
              do
                -- We create a new process. The first three
                -- entries of the 4-tuple are 'Maybe Handle's to
                -- the 'stdout', 'stdin', and 'stderr'. Since we
                -- do not need to handle output when building
                -- Makefiles, we just ignore those.
                --
                -- The last entry is a 'ProcessHandle'. We will
                -- need that to wait for the exit code.
                (_, _, _, prcHnd) <- UIOP.createProcess crPr
                
                -- Here we get the exit code. This uses the
                -- lifted version from "UnliftIO.Process.
                -- From the original documentation:
                --
                -- GHC Note: in order to call waitForProcess 
                -- without blocking all the other threads in 
                -- the system, you must compile the program 
                -- with -threaded.
                -- (Since: 1.2.0.0) On Unix systems, a negative 
                -- value ExitFailure -signum indicates that the 
                -- child was terminated by signal signum. The 
                -- signal numbers are platform-specific, so to 
                -- test for a specific signal use the constants 
                -- provided by System.Posix.Signals in the unix 
                -- package. Note: core dumps are not reported, 
                -- use System.Posix.Process if you need 
                -- this detail.
                Right <$> UIOP.waitForProcess prcHnd
            )

-- |
-- (Re)builds the (Coq)Makefile for all in all Discipline
-- Directories that have dependency access on a given proof
-- file (represented by the argument 'PathInfo' value). 
--
-- Normally, this function is all you need to call if you 
-- add, edit, or remove a proof file, since only the
-- Discipline Directories with dependency access on a proof
-- file should have a declaration of that proof file in their
-- Discipline Directory, and, consequentially, have
-- instructions for building that file in their 
-- CoqMakefiles in the first place.
--
-- This function simply figures out which are the
-- Discipline Directories with access using the 
-- 'allowedDiscDirs' function, and maps 'buildMakefileIn'
-- over them. Then, this function returns a list of
-- 2-tuples. Each tuple contains:  
--
-- * First entry: The base name of the Discipline Directory.  
-- * Second entry: The result of the 'buildMakefileIn'
--   function over the Discipline Directory specified in
--   the first entry. Consult the documentation for the
--   'buildMakefileIn' function for information on what
--   that result means.
buildMakefileDependent
  :: CMIOU.MonadUnliftIO m
  => PathInfo
  -- ^
  -- The 'PathInfo' representing some proof file. Only the
  -- Discipline Directories which can depend on this file
  -- will have their (Coq)Makefiles rebuilt.
  -> CTR.ReaderT 
      ProofSystemConfig 
      m 
      [(String, Either UIOE.IOException SE.ExitCode)]
buildMakefileDependent info =
  do
    -- The 'allowedDiscDirs' function will produce a
    -- (monadic) @[String]@, containing all the base names
    -- of the Discipline Directories with dependency access
    -- on our given file (including the Discipline Directory
    -- which houses the file itself).
    discDirs <- allowedDiscDirs info

    -- Then, we 'mapM' a special function, which will also
    -- include the Discipline Directory's base name in a
    -- tuple, over that list.
    mapM (\d -> (,) d <$> buildMakefileIn d) discDirs

-- |
-- (Re)builds the (Coq)Makefiles for all 
-- Discipline Directories.
--
-- This is normally excessive, and, if one is rebuilding
-- (Coq)Makefiles after adding/removing/editing some proof
-- file, 'buildMakefileDependent', which only rebuilds the
-- (Coq)Makefiles in Discipline Directories which depend on 
-- that file, should be enough. However, one might want to use
-- this function for a big overall remake, for example.
--
-- The return value type follows the same logic as the
-- one for the 'buildMakefileDependent' function, but
-- now it figures out, from the 'ProofSystemConfig'
-- environment, __all__ the Discipline Directories, instead
-- of just the ones dependent on some proof file.
-- Then, it maps 'buildMakefileIn' over them, and returns a
-- list of 2-tuples. Each tuple contains:  
--
-- * First entry: The base name of the Discipline Directory.  
-- * Second entry: The result of the 'buildMakefileIn'
--   function over the Discipline Directory specified in
--   the first entry. Consult the documentation for the
--   'buildMakefileIn' function for information on what
--   that result means.
buildMakefileAll
  :: CMIOU.MonadUnliftIO m
  => CTR.ReaderT 
      ProofSystemConfig 
      m 
      [(String, Either UIOE.IOException SE.ExitCode)]
buildMakefileAll =
  do
    -- Gets a list of __all__ Discipline Directories,
    -- regardless of their dependency permissions.
    discDirs <-
      CTR.asks $ (map discDirBaseName) . disciplineDirectories

    -- Then, we 'mapM' a special function, which will also
    -- include the Discipline Directory's base name in a
    -- tuple, over that list.
    mapM (\d -> (,) d <$> buildMakefileIn d) discDirs


-- COMPILING PROOF FILES
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- $compilingProofFiles
--
-- The functions defined in this section are intended to
-- be used to compile some proof file.
--
-- They assume that all Discipline Directories (or, at the
-- very least, the one where the file is) have their
-- @(Coq)Makefile@s correctly setup, so that all the
-- dependencies are known, and setup with the correct
-- build order in the @(Coq)Makefile@. If that is __not__
-- the case, take a look at the functions in the
-- [(Re)building the Makefiles](#g:1) section, which
-- provide functions to build (and rebuild) those Makefiles.

-- |
-- Attempts to compile the proof file associated with the 
-- 'PathInfo' argument.
--
-- To do this, this function uses the 'compilationCommand'
-- field of the 'ProofSystemConfig' environment. As
-- explained in its documentation (but briefly repeated here
-- as well), it contains two obligatory arguments:  
--
-- > --directory=DISCDIR 
--
-- where DISCDIR is the (absolute) path of the Discipline
-- Directory housing the given proof file (and thus the
-- location where the relevant @Makefile@ is); and  
--
-- > BUILDNAME
--
-- where BUILDNAME is the 'buildName' field of the 'PathInfo'
-- argument. In the default conditions, 'buildName' is the
-- name of the recipe to build this specific proof file,
-- according to the rules in the @CoqMakefile@.
--
-- Calling an external process may result in exceptions to
-- be thrown. Any 'UIOE.IOException's, however, are caught,
-- and encapsulated in the 'Left' constructor.
--
-- If the command does not result in an exception, its
-- exit code will be returned.
compileProofFile
  :: MonadUnliftIO m
  => PathInfo
  -- ^
  -- The 'PathInfo' value associated with the proof file we
  -- want to attempt to compile.
  -> CTR.ReaderT
      ProofSystemConfig
      m
      (Either UIOE.IOException SE.ExitCode)
compileProofFile info =
  do
    -- Get the absolute path to the root of the Proof System.
    -- This will be used to get the (absolute) path of the
    -- Discipline Directory, which is where the @Makefile@ is.
    root <- CTR.asks psRootPath

    let
      -- The (absolute) path of the Discipline Directory.
      discDirPath :: FilePath
      discDirPath = root SF.</> (housingDiscDirBaseName info)

    -- Gets the compilation command from the environment.
    compCmd <- CTR.asks compilationCommand


    -- At this point, we have to run two procedures, both
    -- which produce a result of type
    -- > ReaderT ProofSystemConfig m (Either IOExcept t)
    -- for some type t (this is the thing that is different
    -- for both).
    --
    -- However, if the first fails, we do not have to do
    -- the second. Thus, we need the short-circuit monadic
    -- behavior of 'Either'. However, since 'Either' is in
    -- the bottom of the layer, we will have to use monad
    -- transformers to wrap everything in 'ExceptT', giving
    -- us things of type
    -- > ExceptT IOExcept (ReaderT ProofSystemConfig m) t
    -- again, for some type @t@.
    --
    -- In the end, we use @runExceptT@ to bring it back to
    -- the excepted type.
    CTE.runExceptT $
      do
        -- Get the new environment, changed with the fields
        -- of the 'ProofSystemConfig' environment.
        -- If this fails (because some exception was thrown
        -- and caught internally in 'adaptEnvironment',
        -- the next step will not even be executed.
        newEnv <- CTE.ExceptT $ adaptEnvironment

        let
          -- Using @compCmd@, we can construct a
          -- 'CreateProcess' value using 'shell', which will
          -- put some defaults in some unnecessary fields of
          -- the 'CreateProcess' value.
          crPr :: UIOP.CreateProcess
          crPr = 
            ( 
              UIOP.shell $ 
                subst compCmd [discDirPath, buildName info]
            ){
              UIOP.env = Just newEnv
            }

        -- We now run everything inside a 'handleIO', returning
        -- the lifted results enveloped in either 'Left' (for
        -- any thrown and caught exceptions) or 'Right' (for the
        -- exit code of the compilation command).
        CTE.ExceptT $ UIOE.handleIO
          (\e -> return $ Left e)
          (
            do
              -- We create a new process. The first three
              -- entries of the 4-tuple are 'Maybe Handle's to
              -- the 'stdout', 'stdin', and 'stderr'. Since we
              -- do not need to handle output when building
              -- Makefiles, we just ignore those.
              --
              -- The last entry is a 'ProcessHandle'. We will
              -- need that to wait for the exit code.
              (_, _, _, prcHnd) <- UIOP.createProcess crPr
              Right <$> UIOP.waitForProcess prcHnd
          )

-- AUXILIAR FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This auxiliar function calls for the external environment
-- and produces a new environment, replacing the @COQBIN@
-- environment variable with the variable defined in the
-- 'ProofSystemConfig' environment's 'coqbinPath' field,
-- __if it is not empty__.
--
-- First, if not empty, this function will transform that
-- field into its canonical path, and also add a trailing
-- separator to the end.
--
-- If any 'UIOE.IOException' are thrown by any functions
-- internally, they are caught and that is returned instead,
-- wrapped in the 'Left' constructor.
adaptEnvironment
  :: MonadUnliftIO m
  => CTR.ReaderT 
      ProofSystemConfig 
      m 
      (Either UIOE.IOException [(String, String)])
  -- ^
  -- If everything succeeds, this is a monadically-wrapped
  -- 'Right'-wrapped list of @(key, value)@ pairs of
  -- environment variables, with the substitutions by
  -- the fields in the 'ProofSystemConfig' environment.
adaptEnvironment =
  do
    -- Get the 'ProofSystemConfig' environment's 'coqbinPath'
    -- field, to see what we need to replace things with.
    coqbin <- CTR.asks coqbinPath

    -- We will need to ask for the external environment.
    -- This is an 'IO' process, which may throw exceptions,
    -- so we wrap everything in a 'handleIO'.
    UIOE.handleIO
      (\e -> return $ Left e)
      (
        -- In the @ReaderT ProofSystemConfig m@ monad.
        do
          -- Get the external environment.
          env <- UIOEn.getEnvironment

          if (coqbin == "")
          then
            -- Here we don't need to do anything to the
            -- environment, because the configuration field
            -- was set to empty, telling us not to overwrite
            -- anything in the external environment.
            return $ Right env
          else
            -- Here we must replace the environment variable
            -- @COQBIN@, if we find it, or add it otherwise.
            -- But first, we turn it canonical and add the
            -- trailing separator.
            do
              coqbin' <- 
                SF.addTrailingPathSeparator <$>
                  UIOD.canonicalizePath coqbin
              return $ Right $ replOrAdd "COQBIN" coqbin' env
      )
  where
    -- This function replaces the first occurence of a
    -- @(key, value)@ pair with the given ones if the key 
    -- matches, or adds the @(key, value)@ pair at the end if 
    -- no matches were found.
    replOrAdd
      :: Eq k
      => k
      -> v
      -> [(k, v)]
      -> [(k, v)]
    replOrAdd key val [] = [(key, val)]
    replOrAdd key val ((k, v) : kvs) =
      if (key == k)
      then
        (key, val) : kvs
      else
        (k, v) : replOrAdd key val kvs
        

