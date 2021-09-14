-- |
-- This modules provides the functions that interface the
-- command line call to the @remove@ mode with the functions
-- in the other modules.
module Modes.RemoveMode
  ( execRemoveMode
  ) where

-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Config
  ( ProofSystemConfig
  , psRootPath
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
  , RemoveModeOptions
  )

import Modes.Internal
  ( ppPathsFromDecPaths
  , promptYes
  , putRebuildMakefiles
  )

import ProjectFileManagement
  ( previewRemoveSubpaths
  , removeSubpaths
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

import Prettyprinter
  ( (<+>)
  )

import qualified Prettyprinter as P
  ( Doc
  , annotate
  , indent
  , line
  , vsep
  )

import qualified Prettyprinter.Render.Terminal as PRT
  ( AnsiStyle
  , Color
    ( Green
    , Red
    )
  , bold
  , color
  , putDoc
  , hPutDoc
  )

import qualified System.FilePath as SF
  ( (</>)
  )

import qualified UnliftIO.Exception as UIOE
  ( IOException
  )

import qualified UnliftIO.IO as UIOIO
  ( stderr
  )


-- MAIN FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This is the main function that performs the @remove@ mode
-- functionality, and is called upon by the command line
-- options parser.
execRemoveMode
  :: CommonModeOptions
  -> RemoveModeOptions
  -> CTR.ReaderT ProofSystemConfig IO ()
execRemoveMode cmo _ =
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
    -- Try to get the preview of the deletion results.
    -- > eitPreview 
    -- >   :: Either 
    -- >        IOException 
    -- >        [(String, Either IOException [String])]
    eitPreview <- path >>= previewRemoveSubpaths

    -- We check if the preview succeeded or if it
    -- generated an exception.
    case eitPreview of
      Left exc ->
        let
          -- The error message to output.
          canonicalizeFail :: P.Doc PRT.AnsiStyle
          canonicalizeFail =
            DS.fromString "Failure to canonicalize path."
            <> P.line
            <> ( P.indent 2 $ DS.fromString $
                  "Exception: " ++ show exc
               )
            <> P.line
        in
          CTC.lift $ PRT.hPutDoc UIOIO.stderr canonicalizeFail
      Right preview -> do
        -- preview :: [(String, Either IOException [String])]
        -- The value 'preview' is a list of pairs, where
        --
        -- - The first entry is the base name a
        --   Discipline Directory.
        -- - The second entry is either an exception or
        --   a list of all declarative paths that will be
        --   removed from the the Discipline Directory's
        --   project file.
        CTC.lift $ PRT.putDoc $ 
          ( DS.fromString $
              "The following declarative paths will be  "
              ++ "removed from the project files: "
          ) <> P.line
        CTC.lift $ putPrevList preview
        CTC.lift $ PRT.putDoc P.line

      -- Now we ask for user confirmation, unless the
      -- @assume-yes@ flag was set.
        proceedRemove <-
          CTC.lift $ promptYes (cmo_assumeYes cmo) $
            DS.fromString "Do you want to continue? [Y/n] "

        CM.when proceedRemove $ do
          -- We can proceed trying to remove the subpaths.
          -- To do this, we use this the 'removeSubpaths'
          -- function. Its result is (ignoring the monadic
          -- context) of type
          -- > Either
          -- >   IOException
          -- >   [(String, Either IOException [String])]
          -- just like the one for the preview. However,
          -- it would only return a 'Left'-wrapped 
          -- 'IOException' if the path fails 
          -- canonicalization, but, to get here, it could
          -- not have failed.
          eitRes <- path >>= removeSubpaths
          case eitRes of
            Left exc ->
              -- This case should never happen.
              CTC.lift $ PRT.hPutDoc UIOIO.stderr $
                ( DS.fromString $
                  "Failure to canonicalize the path. "
                  ++ "This error should not have "
                  ++ "happned, please report it."         
                )
                <> P.line
                <> 
                ( P.indent 2 $ DS.fromString $
                  "Exception: " ++ show exc
                )
            Right res -> do
              -- A few line breaks for spacing.
              CTC.lift $ PRT.putDoc $ P.line <> P.line

              -- Prints a message explaining the result
              -- list output,
              CTC.lift $ PRT.putDoc $
                ( DS.fromString $
                    "Project file editing results: "
                )
                <> P.line

              -- Prints the result list with the auxiliar
              -- function 'putResList'.
              CTC.lift $ putResList res

              -- A few line breaks.
              CTC.lift $ PRT.putDoc $ P.line <> P.line

              -- And then tries to rebuild the (Coq)Makefiles.
              putRebuildMakefiles
  where
    -- We will define a function which takes a list
    -- such as 'preview', and, for every Discipline
    -- Directory in the list:
    -- * If it has an exception, print the name and
    --   the exception to stderr.
    -- * If it has a list of deletion, check if the
    --   list is empty. If it is, do nothing.
    --   If it has elements, pretty print them.
    putPrevList
      :: [(String, Either UIOE.IOException [String])]
      -> IO ()
    -- Recursion base case.
    putPrevList [] = return ()
    -- Exception case.
    putPrevList ((dd, Left exc) : xs) = do
      PRT.hPutDoc UIOIO.stderr $ P.indent 2 $
        (P.annotate PRT.bold $ DS.fromString dd)
        <> P.line
        <> ( P.indent 2 $ DS.fromString $ "Exception: "
              ++ show exc
           )
        <> P.line
      putPrevList xs
    -- No deletion case.
    putPrevList ((dd, Right []) : xs) = putPrevList xs
    -- Discipline Directory with deletions case.
    putPrevList ((dd, Right l) : xs) = do
      -- putStrLn $ "This is my list: " ++ show l
      PRT.putDoc $ P.indent 2 $
        (P.annotate PRT.bold $ DS.fromString dd)
        <> P.line
        <> ( P.indent 2 $ ppPathsFromDecPaths l $ 
              Just PRT.Red
           )
        <> P.line
      putPrevList xs

    -- A similar function to 'putPrevList', but this
    -- time, to show the results.
    putResList 
      :: [(String, Either UIOE.IOException [String])]
      -> IO ()
    -- Base case.
    putResList [] = return ()
    -- Exception found case.
    putResList ((dd, Left exc) : xs) = do
      PRT.hPutDoc UIOIO.stderr $ P.indent 2 $
        (P.annotate PRT.bold $ DS.fromString dd)
        <> P.line
        <> ( P.indent 2 $ DS.fromString $ "Exception: "
              ++ show exc
           )
        <> P.line
      putPrevList xs
    -- If no exception was found, but the deletion list
    -- is empty, then ignore that directory.
    putResList ((dd, Right []) : xs) = putResList xs
    -- If no exception was found, but there were deletions
    -- in that file, print "Success".
    putResList ((dd, Right l) : xs) = do
      PRT.putDoc $ P.indent 2 $
        (P.annotate PRT.bold $ DS.fromString dd)
        <> DS.fromString ": "
        <> (P.annotate (PRT.color PRT.Green) $
            DS.fromString "Success"
           )
        <> P.line
      putResList xs
    
