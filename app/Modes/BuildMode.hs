-- |
-- This module provides the functions that interface with the
-- command line call to the @build@ mode with the functions
-- in other modules.
module Modes.BuildMode
  ( execBuildMode
  ) where

-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import CompileManagement
  ( compileProofFile
  )

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
  , BuildModeOptions
  )

import Modes.Internal
  ( groupInfosExtraPerDiscDir
  , groupInfosPerDiscDir
  , ppPathsFromPathInfo
  , promptYes
  , putRebuildMakefiles
  )

import PathInfo
  ( PathInfo
  , canonicalPath
  , pathInfos
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
  )

import qualified Prettyprinter.Render.Terminal as PRT
  ( AnsiStyle
  , Color
    ( Green
    , Yellow
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
  , makeRelative
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
-- This is the main function that performs the @build@ mode
-- functionality, and is called upon by the command line
-- options parser.
execBuildMode
  :: CommonModeOptions
  -> BuildModeOptions
  -> CTR.ReaderT ProofSystemConfig IO ()
execBuildMode cmo _ =
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
    -- Recurse into the given path and get build 'PathInfo'
    -- values for all the proof files it finds.
    infos <- pathInfos =<< path

    -- Group the found 'PathInfo's per Discipline Directory
    -- for printing purposes.
    --
    -- infosPerDD :: [(String, [PathInfo])]
    infosPerDD <- groupInfosPerDiscDir infos

    -- Prints a message about which files will be built.
    CTC.lift $ PRT.putDoc $
      DS.fromString "Files to be compiled:"
      <> P.line

    -- And then prints the files themselves.
    putToBeBuilt infosPerDD

    -- Some line breaks.
    CTC.lift $ PRT.putDoc $ P.line <> P.line

    -- Asks for user confirmation to proceed.
    proceedBuild <- CTC.lift $ promptYes (cmo_assumeYes cmo) $
      DS.fromString "Do you want to continue? [Y/n] "

    -- Uses 'when', which returns 'pure ()' if we 
    -- 'proceedBuild' fails.
    CM.when proceedBuild $ do
      -- Prints a message explaining that the (Coq)Makefiles
      -- are being rebuilt.
      CTC.lift $ PRT.putDoc $
        P.line
        <> P.line
        <> DS.fromString "Updating (Coq)Makefiles..."
        <> P.line

      -- Rebuilds the (Coq)Makefiles, so that the compilation
      -- is done with updated build order.
      putRebuildMakefiles


      -- Prints a message since building output will
      -- start to appear on the screen.
      CTC.lift $ PRT.putDoc $
        P.line
        <> P.line
        <> DS.fromString "Building files..."
        <> P.line

      -- Builds the files, and gets the building results
      -- grouped by Discipline Directory.
      -- > buildRes
      -- >   :: [ ( String
      -- >        , [(PathInfo, Either IOException ExitCode)]
      -- >        )
      -- >      ]
      buildRes <- 
        groupInfosExtraPerDiscDir =<< buildShowName infos

      -- Informs that the building results will be printed.
      CTC.lift $ PRT.putDoc $
        P.line
        <> P.line
        <> DS.fromString "Building summary:"
        <> P.line

      -- Prints the building summary.
      putBuildRes buildRes
  where
    -- Auxiliar function used to print the 'PathInfo's that
    -- have to be compiled. It is assumed that they are
    -- grouped per Discipline Directory. Discipline
    -- Directories with no building targets are omitted.
    putToBeBuilt
      :: [(String, [PathInfo])]
      -> CTR.ReaderT ProofSystemConfig IO ()
    -- Base case of the recursion.
    putToBeBuilt [] = return ()
    -- No building targets.
    putToBeBuilt ((_, []) : xs) = putToBeBuilt xs
    -- Discipline Directory has build targets.
    putToBeBuilt ((dd, l) : xs) =
      let
        -- Pretty-printing 'Doc' for the Discipline
        -- Directory's base name.
        ddDoc :: P.Doc PRT.AnsiStyle
        ddDoc = P.annotate PRT.bold $ DS.fromString dd
      in do
        -- Pretty-printing 'Doc' elements to be built.
        -- 
        -- ddInfos :: P.Doc PRT.AnsiStyle
        infosDoc <- ppPathsFromPathInfo l $ Just PRT.Yellow
        -- Print everything, indenting the name 2 spaces and
        -- the list in 4 spaces.
        CTC.lift $ PRT.putDoc $
          (P.indent 2 ddDoc)
          <> P.line
          <> (P.indent 4 infosDoc)
          <> P.line
        -- Recurse.
        putToBeBuilt xs

    -- An auxiliar function which, besides building the
    -- files, also outputs their name.
    --
    -- In the end, it returns a list of pairs containing the
    -- initial 'PathInfo', and their building results
    -- (provided by the 'compileProofFile' function).
    buildShowName
      :: [PathInfo]
      -> CTR.ReaderT 
          ProofSystemConfig
          IO
          [(PathInfo, Either UIOE.IOException SE.ExitCode)]
    -- Recursion base case.
    buildShowName [] = return []
    buildShowName (i : is) = do
      -- Gets the path of the root. We will print the
      -- path of the build target relative to the root.
      root <- CTR.asks psRootPath
      let
        relPath :: FilePath
        relPath = SF.makeRelative root $ canonicalPath i
      -- Prints the relative path in yellow, followed
      -- by a ":" and a line break.
      CTC.lift $ PRT.putDoc $ 
        P.line
        <> (P.annotate (PRT.color PRT.Yellow) $ 
            DS.fromString $ relPath ++ ":"
           )
        <> P.line
      -- Builds the file.
      -- compRes :: Either IOException ExitCode
      compRes <- compileProofFile i
      -- Recurses.
      (:) (i, compRes) <$> buildShowName is


    -- An auxiliar function to print a 'PathInfo' associated
    -- with a certain build information output.
    putInfoAndBuildRes
      :: (PathInfo, Either UIOE.IOException SE.ExitCode)
      -> CTR.ReaderT ProofSystemConfig IO ()
    putInfoAndBuildRes (i, eit) =
      let
        -- The path relative to the root.
        relPath :: CTR.ReaderT ProofSystemConfig IO FilePath
        relPath = do
          -- Gets the root path
          root <- CTR.asks psRootPath
          return $ SF.makeRelative root $ canonicalPath i
      in do
        relPath' <- relPath
        CTC.lift $
          case eit of
            Left exc ->
              PRT.hPutDoc UIOIO.stderr $ P.indent 4 $
                ( P.annotate (PRT.color PRT.Yellow) $
                  DS.fromString relPath'
                )
                <> DS.fromString " (Exception): "
                <> (DS.fromString $ show exc)
                <> P.line
            Right (SE.ExitFailure n) ->
              PRT.hPutDoc UIOIO.stderr $ P.indent 4 $
                ( P.annotate (PRT.color PRT.Yellow) $
                  DS.fromString relPath'
                )
                <> (DS.fromString $ 
                    " (Failure): Process exit with "
                    ++ show n
                   )
                <> P.line
            Right SE.ExitSuccess ->
              PRT.putDoc $ P.indent 4 $
                ( P.annotate (PRT.color PRT.Yellow) $
                  DS.fromString $ relPath'
                )
                <> DS.fromString ": "
                <> (P.annotate (PRT.color PRT.Green) $
                    DS.fromString "Success"
                   )
                <> P.line
      
    -- We now define a function that prints the building
    -- summary for all files.
    putBuildRes
      :: [ ( String
           , [(PathInfo, Either UIOE.IOException SE.ExitCode)]
           )
         ]
      -> CTR.ReaderT ProofSystemConfig IO ()
    -- Recursion base case.
    putBuildRes [] = return ()
    -- Discipline Directory had no build targets, so we
    -- just skip it.
    putBuildRes ((_, []) : xs) = putBuildRes xs
    -- Discipline Directory has build targets.
    putBuildRes ((dd, l) : xs) = do
      -- Print the name of the Discipline Directory.
      CTC.lift $ PRT.putDoc $ P.indent 2 $
        (P.annotate PRT.bold $ DS.fromString dd)
        <> P.line
      -- Prints the targets build information using 
      -- the function 'putInfoAndBuildRes'.
      mapM_ putInfoAndBuildRes l
      -- Recurses
      putBuildRes xs
