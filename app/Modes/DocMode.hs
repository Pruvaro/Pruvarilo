-- |
-- This module provides the functions that interface with the
-- command line call to the @doc@ mode with the functions
-- in other modules.
module Modes.DocMode
  ( execDocMode
  ) where

-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Config
  ( ProofSystemConfig
  , DisciplineDirectory
    ( discDirBaseName
    )
  , disciplineDirectories
  , psRootPath
  )

import DocumentationManagement
  ( buildDocDiscDir
  )

import Modes
  ( ActionLocation
    ( ActLocDiscDir
    , ActLocPath
    )
  , CommonModeOptions
    ( cmo_actionLocation
    , cmo_assumeYes
    )
  , DocModeOptions
  )

import Modes.Internal
  ( promptYes
  , putRebuildMakefiles
  )

import PathInfo
  ( relativePathElem
  )

import qualified Control.Monad as CM
  ( when
  )

import qualified Control.Monad.Trans.Class as CTC
  ( lift
  )

import qualified Control.Monad.Trans.Reader as CTR
  ( ReaderT
  , asks
  )

import qualified Data.String as DS
  ( fromString
  )

import qualified Prettyprinter as P
  ( annotate
  , indent
  , line
  )

import qualified Prettyprinter.Render.Terminal as PRT
  ( Color
    ( Green
    )
  , bold
  , color
  , putDoc
  , hPutDoc
  )

import qualified System.Exit as SE
  ( ExitCode
    ( ExitFailure
    , ExitSuccess
    )
  )

import qualified System.FilePath as SF
  ( (</>)
  )

import qualified UnliftIO.IO as UIOIO
  ( stderr
  )



-- MAIN FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This is the main function that performs the @doc@ mode
-- functionality, and is called upon by the command line
-- options parser.
execDocMode
  :: CommonModeOptions
  -> DocModeOptions
  -> CTR.ReaderT ProofSystemConfig IO ()
execDocMode cmo _ =
  let
    -- Whether the user decided to go with an explicit path,
    -- or with a Discipline Directory, we need to get a path.
    path :: Monad m => CTR.ReaderT ProofSystemConfig m String
    path =
      case (cmo_actionLocation cmo) of
        ActLocPath p -> return p
        ActLocDiscDir dd ->
          -- If we are instead given a Discipline Directory,
          -- we can get its path by prepending the path
          -- to the root of the proof system.
          (SF.</> dd) <$> CTR.asks psRootPath 
  in do
    -- Gets a list of all Discipline Directory base names.
    -- This is used later to test if the input actually
    -- belongs to a Discipline Directory.
    --
    -- > discdirs :: [String]
    discdirs <- 
      CTR.asks $ (map discDirBaseName) . disciplineDirectories

    -- Currently, we can only build the documentation of
    -- an entire Discipline Directory.
    -- So, differently from other modes, where we recurse
    -- into the provided path and build up a list of 
    -- 'PathInfo's from all proof files we find along the
    -- way, we will instead take the path and determine
    -- which Discipline Directory it is in.
    --
    -- To do this, we will use the 'relativePathElem' function
    -- from the "PathInfo" module.
    --
    -- > eitPathElems :: (Either IOException (Maybe [String]))
    eitPathElems <- relativePathElem =<< path
    case eitPathElems of
      Left e ->
        -- This case means we had an exception thrown (and
        -- caught) when trying to canonicalize the provided
        -- path. We print that error message and stop.
        CTC.lift $ PRT.hPutDoc UIOIO.stderr $
          ( DS.fromString $
            "Failure to canonicalize the path "
            ++ "(Exception thrown)."
          )
          <> P.line
          <>
          ( P.indent 2 $ DS.fromString $
            "Exception: " ++ show e
          )
          <> P.line
      Right Nothing ->
        -- In this case, the provided path was properly
        -- canonicalized, but does not correspond to a path
        -- in the proof system. We report this failure
        -- and also stop.
        CTC.lift $ PRT.hPutDoc UIOIO.stderr $
          ( DS.fromString $
            "Failure "
            ++ "(Path is not a subpath of the proof system)."
          )
          <> P.line
      Right (Just []) ->
        -- Here, we were able to get the path elements,
        -- however, the path corresponds exactly to the
        -- root of the proof system.
        CTC.lift $ PRT.hPutDoc UIOIO.stderr $
          ( DS.fromString $
            "Failure (Path is not a subpath of a "
            ++ "Discipline Directory)."
          )
          <> P.line
      Right (Just (p : _)) -> do
        -- Here, we were able to get the path elements, and
        -- the path is not __exactly__ the root of the proof
        -- system. The head of the list of path elements,
        -- 'p', should be a Discipline Directory. We will
        -- test if it is or not.
        --
        -- Since the 'False' case is shorter (just print a
        -- message), we will put a 'not' in the 'if' so
        -- that it comes first.
        if (not $ p `elem` discdirs)
        then
          -- Here, 'p' is not a Discipline Directory.
          CTC.lift $ PRT.hPutDoc UIOIO.stderr $
            ( DS.fromString $
              "Failure (Path is not a subpath of a "
              ++ "Discipline Directory)."
            )
            <> P.line
        else do
          -- Here, 'p' is a Discipline Directory.
          --
          -- First, we will print a message explaining what
          -- we will do, and then ask if they want to 
          -- continue (respecting the @assume-yes@ flag).
          CTC.lift $ PRT.putDoc $
            DS.fromString "The documentation for the "
            <> (P.annotate PRT.bold $ DS.fromString p)
            <> 
            ( DS.fromString
              " Discipline Directory will be built."
            )
            <> P.line
          proceedDoc <-
            CTC.lift $ promptYes (cmo_assumeYes cmo) $
              DS.fromString "Do you want to continue? [Y/n] "

          -- The 'buildDoc' function does the actual job of 
          -- building the documentation.
          -- We just have to handle its (many-cases) 
          -- return value.
          --
          -- > mbDocRes 
          -- >   :: Maybe (Either UIOE.IOException SE.ExitCode)
          CM.when proceedDoc $ do
            -- Rebuilds the (Coq)Makefiles so that we use
            -- the updated dependency order.
            CTC.lift $ PRT.putDoc $
              P.line
              <> DS.fromString "Updating (Coq)Makefiles..."
              <> P.line
            putRebuildMakefiles
            -- Line breaks for prettiness.
            CTC.lift $ PRT.putDoc $ P.line <> P.line
            mbDocRes <- buildDocDiscDir p
            -- More line breaks for prettiness.
            CTC.lift $ PRT.putDoc $ P.line <> P.line
            CTC.lift $
              case mbDocRes of
                Nothing ->
                  -- This case should never happen, because
                  -- we already tested it out.
                  PRT.hPutDoc UIOIO.stderr $
                    ( DS.fromString $
                      "Failure (Path is not a subpath of a "
                      ++ "Discipline Directory)."
                    )
                    <> P.line
                    <> 
                    ( DS.fromString 
                      "Please report how you got this error."
                    )
                    <> P.line
                Just (Left e) ->
                  -- Here, 'p' is a Discipline Directory, but
                  -- while calling the command, some exception
                  -- was thrown (and caught).
                  PRT.hPutDoc UIOIO.stderr $
                    ( DS.fromString $
                      "Failure to build the documentation "
                      ++ "for the "
                    )
                    <> (P.annotate PRT.bold $ DS.fromString p)
                    <> 
                    ( DS.fromString $
                      " Discipline Directory "
                      ++ " (Exception thrown)."
                    )
                    <> P.line
                    <>
                    ( P.indent 2 $ DS.fromString $
                      "Exception: " ++ show e
                    )
                    <> P.line
                Just (Right (SE.ExitFailure n)) ->
                  -- Here, 'p' is a Discipline Directory, and the
                  -- command was properly called. However, the
                  -- command returned an 'ExitFailure', exiting
                  -- with the exit code 'n'.
                  PRT.hPutDoc UIOIO.stderr $
                    ( DS.fromString $
                      "Failure to build the documentation "
                      ++ "for the "
                    )
                    <> (P.annotate PRT.bold $ DS.fromString p)
                    <> 
                    ( DS.fromString $
                      " Discipline Directory"
                      ++ " (Process exited with " ++ show n ++ ")."
                    )
                    <> P.line
                Just (Right SE.ExitSuccess) ->
                  -- Finally, here is the case where 'p' is a
                  -- Discipline Directory, the command was 
                  -- properly called, and exit successfully.
                  -- In this case, we print a success message.
                  PRT.putDoc $
                    ( DS.fromString 
                      "Building documentation for the "
                    )
                    <> (P.annotate PRT.bold $ DS.fromString p)
                    <> (DS.fromString " Discipline Directory: ")
                    <> 
                    ( P.annotate (PRT.color PRT.Green) $
                      DS.fromString "Success."  
                    )
                    <> P.line
