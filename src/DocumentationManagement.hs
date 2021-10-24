-- |
-- The goal of this module is to provide functions which
-- handle the documentation associated with the proof files.
--
-- Currently, the documentation is done through the use of
-- the @coq_makefile@ utility, and automatized using the
-- project's Makefile. Additionaly, it is currently not
-- possible to build the documentation of a single file,
-- instead, the documentation of an entire Discipline
-- Directory must be handled together. Fortunately, the
-- Makefiles make so that we do not waste time rebuilding
-- the documentation of files that were not changed.
--
-- When the documentation of a Discipline Directory is
-- built, it is stored in that directory's @html@ 
-- subdirectory. The name of the HTML file associated with
-- a module is the same as the name of the module itself,
-- with the extension @.html@.
module DocumentationManagement
  ( buildDocDiscDir
  ) where

-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import CompileManagement
  ( adaptEnvironment
  )

import Config
  ( ProofSystemConfig
  , DisciplineDirectory
    ( discDirBaseName
    )
  , disciplineDirectories
  , docBuildCommand
  , psRootPath
  )

import SubstitutionParser
  ( subst
  )

import qualified Control.Monad.IO.Unlift as CMIOU
  ( MonadUnliftIO
  )

import qualified Control.Monad.Trans.Reader as CTR
  ( ReaderT
  , asks
  )

import qualified System.Exit as SE
  ( ExitCode
  )

import qualified System.FilePath as SF
  ( (</>)
  )

import qualified UnliftIO.Exception as UIOE
  ( IOException
  , handleIO
  )

import qualified UnliftIO.Process as UIOP
  ( CreateProcess
    ( env
    )
  , createProcess
  , shell
  , waitForProcess
  )

-- BUILDING THE DOCUMENTATION
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This function attempts to build the documentation of an
-- entire Discipline Directory, given its base name.
--
-- The result (ignoring the monadic context) can either be:  
--
-- * 'Nothing': The provided base name does not correspond
--   to the base name of any Discipline Directory, as
--   list in the 'ProofSystemConfig' environment's 
--   'disciplineDirectories' field.  
-- * 'Just (Left e)' for some @e :: 'UIOE.IOException'@.
--   Here, the base name corresponds to the base name of
--   a Discipline Directory, but calling the command to
--   build the documentation yielded an 'UIOE.IOException'.
--   This exception 'e' is caught and returned this way.  
-- * 'Just (Right ec)' for some @ec :: 'SE.ExitCode'@.
--   Here, the command to build the Discipline Directory's
--   documentation was caught, and that did not cause an
--   'UIOE.IOException' to be thrown. The command ran, and
--   its exit code is reported back this way. As per the
--   documentation of the 'SE.ExitCode' data type, this
--   can either be an @ExitSuccess@, meaning that the
--   command exited with a @0@, or it can be a
--   @ExitFailure n@, for some exit code @n :: 'Int'@,
--   meaning the command exited with @n@.
--
-- It is possible to overwrite the default documentation
-- building command using the 'docBuildCommand' field of
-- the 'ProofSystemConfig' environment, which can also be
-- set using the configuration file. See the "Config" module
-- for more information on that.
buildDocDiscDir
  :: CMIOU.MonadUnliftIO m
  => String
  -- ^
  -- The __base name__ (not the full path) of some
  -- Discipline Directory.
  -> CTR.ReaderT
      ProofSystemConfig
      m
      (Maybe (Either UIOE.IOException SE.ExitCode))
buildDocDiscDir discdir = do
  -- Get the list of Discipline Directory base names.
  discdirs <-
    CTR.asks $ map discDirBaseName . disciplineDirectories

  -- Also gets the canonical path of the root, since we
  -- might need it later.
  root <- CTR.asks psRootPath

  -- And the 'String' used as the build command.
  docCmd <- CTR.asks docBuildCommand

  -- Checks if the base name is base name of 
  -- some Discipline Directory.
  if discdir `elem` discdirs
  then do
    -- We will try to get the adapted environment (using
    -- the environment variables set through the in the
    -- 'ProofSystemConfig' environment).
    -- 
    -- > eitNewEnv :: Either IOException [(String, String)]
    eitNewEnv <- adaptEnvironment

    -- If there was a failure in setting up that new
    -- environment, we return it. Otherwise, we have
    -- a usable environment.
    case eitNewEnv of
      Left e -> return $ Just $ Left e
      Right newEnv -> do
        let
          -- Build the 'CreateProcess' value associated
          -- with our shell command.
          crPr :: UIOP.CreateProcess
          crPr =
            (UIOP.shell $ subst docCmd [root SF.</> discdir])
            { UIOP.env = Just newEnv }

        -- Runs the process, but inside a 'handleIO' to
        -- catch any exceptions thrown.
        fmap Just $ UIOE.handleIO (return . Left) $ do
          -- We create a new process. The first three
          -- entries of the 4-tuple are 'Maybe Handle's to
          -- the 'stdout', 'stdin', and 'stderr'.
          --
          -- The last entry is a 'ProcessHandle'. We will
          -- need that to wait for the exit code.
          (_, _, _, prcHdl) <- UIOP.createProcess crPr
          Right <$> UIOP.waitForProcess prcHdl
  else
    return Nothing

