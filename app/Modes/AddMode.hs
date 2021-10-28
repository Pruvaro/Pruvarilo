-- |
-- This module provides the functions that interface the
-- command line call to the @add@ mode with the functions in
-- the other modules.
module Modes.AddMode
  ( execAddMode
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

import Modes
  ( ActionLocation
    ( ActLocDiscDir
    , ActLocPath
    )
  , AddModeOptions
  , CommonModeOptions
    ( cmo_actionLocation
    , cmo_assumeYes
    )
  )

import Modes.Internal
  ( adjustOutputFormat
  , ppPathsFromPathInfo
  , ppPathsAndExtra
  , promptYes
  , putRebuildMakefiles
  )

import PathInfo
  ( PathInfo
  , validName
  , pathInfos
  , noNameConflict
  )

import ProjectFileManagement
  ( addDeclarations
  , isDeclared
  )

import qualified Control.Monad as CM
  ( unless
  , when
  )

import qualified Control.Monad.Trans.Class as CTC
  ( lift
  )

import qualified Control.Monad.Trans.Reader as CTR
  ( ReaderT
  , asks
  )

import qualified Data.Either as DE
  ( isLeft
  )

import qualified Data.String as DS
  ( fromString
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
    )
  , bold
  , color
  , putDoc
  )

import qualified System.FilePath as SF
  ( (</>)
  )

import qualified UnliftIO.Exception as UIOE
  ( IOException
  )


-- MAIN FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This is the main function that performs the @add@ mode
-- functionality, and is called upon by the command line
-- options parser.
execAddMode
  :: CommonModeOptions
  -> AddModeOptions
  -> CTR.ReaderT ProofSystemConfig IO ()
execAddMode cmo _ =
  let
    -- Whether the user decided to go with an explicit path,
    -- or with a Discipline Directory, we need to get a path.
    path :: Monad m => CTR.ReaderT ProofSystemConfig m String
    path =
      case cmo_actionLocation cmo of
        ActLocPath p -> return p
        ActLocDiscDir dd ->
          -- If we are instead given a Discipline Directory,
          -- we can get its path by prepending the path
          -- to the root of the proof system.
          (SF.</> dd) <$> CTR.asks psRootPath
  in do
    -- Recurse into the specified path, and get all
    -- proof files in that path.
    --
    -- infos :: [PathInfo]
    infos <- pathInfos =<< path

    -- Part of those infos is already declared everywhere,
    -- so they are not relevant for us. We filter those
    -- out, and stay with only the ones that are still
    -- pending addition.
    --
    -- The value
    -- > infoDec
    -- > :: ( PathInfo
    -- >    , [(String, Either UIOE.IOException Bool)]
    -- >    )
    -- is the list of those 'PathInfo's still pending
    -- operation, but also incremented with their declaration
    -- condition (the list of places they should be declared,
    -- if they are declared there, or if verification of
    -- declaration caused an exception).
    infoDec <- onlyNotFullyDeclared infos

    -- One important thing about 'infoDec' is that, if
    -- we discard the extra information next to the
    -- 'PathInfo's, we get the sublist of 'infos' containing
    -- only the 'PathInfo's that are not fully declared,
    -- or that the declaration verification failed. This
    -- will be useful later, so that we can ask for only
    -- those 'PathInfo's to be added, instead of asking for
    -- all elements of 'infos'.
    -- For this reason, we derive that list.
    let
      infosNotFullyDec :: [PathInfo]
      infosNotFullyDec = map fst infoDec

    -- However, it is nicer to group the information of
    -- where the additions should happen per Discipline
    -- Directory, instead of per 'PathInfo'. We can use
    -- the auxiliar function 'matchDecStatus' to help
    -- us create (monadic) lists of type
    -- > [ ( String
    -- >   , [(PathInfo, Either UIOE.IOException Bool)]
    -- >   )
    -- > ]
    -- that is, a list indexed first by the Discipline
    -- Directory base name (the 'String' element) containing
    -- the list of all 'PathInfo's such that their declaration
    -- status (which comes package with the 'PathInfo')
    -- matches the given predicate.
    -- This way, we can get a list indexed by Discipline
    -- Directory of all entries that failed testing
    -- (together with their exception):
    infoDecFails <- matchDecStatus infoDec DE.isLeft

    -- And similarly, one for the ones that are pending
    -- addition.
    infoDecPending <- matchDecStatus infoDec (== Right False)

    -- Now, we can print the files we intend on adding,
    docToBeAdded <- ppDiscardExtra infoDecPending
    CTC.lift $ PRT.putDoc $
      DS.fromString "Files whose declaration will be added:"
      <> P.line
      <> P.indent 2 (P.vsep docToBeAdded)
      <> P.line

    -- We must also show the files which failed verification.
    -- However, it is silly to print anything about this if
    -- nothing failed verification. So we will test if
    -- there are any fails.
    CM.unless (all (null . snd) infoDecFails) $ do
      docFailVerif <- ppDisplayExceptions infoDecFails
      CTC.lift $ PRT.putDoc $
        DS.fromString
          "Files whose declaration verification failed:"
        <> P.line
        <> P.indent 2 (P.vsep docFailVerif)
        <> P.line

    -- Now that we showed the user what will be done, we
    -- can ask for their input.
    proceedAdd <- CTC.lift $ promptYes (cmo_assumeYes cmo) $
                  DS.fromString "Do you want to continue? [Y/n] "

    CM.when proceedAdd $ do
      -- Specifically for the @add@ mode, we might be adding
      -- 'PathInfo's with names that do not conform to the
      -- project's validity predicate. That is not a discarding
      -- condition, but the user should be alerted.
      let
        -- Gets the list of 'PathInfo's we want to add that
        -- have invalid names.
        invNameInfos :: [PathInfo]
        invNameInfos = filter (not . validName) infosNotFullyDec

        -- Also gets a list of the 'PathInfo's we want to add
        -- which have case-insensitive name clashes.
        caseNameClashInfos :: [PathInfo]
        caseNameClashInfos =
          filter (not . noNameConflict) infosNotFullyDec

      -- Unless the list of invalid names is empty, we alert
      -- the user.
      CM.unless (null invNameInfos) $ do
        docInvNameInfos <- ppPathsFromPathInfo invNameInfos $
          Just PRT.Green
        CTC.lift $ PRT.putDoc $
          P.line
          <> P.line
          <> DS.fromString
             "These files have non-conforming names:"
          <> P.line
          <> P.indent 2 docInvNameInfos
          <> P.line

      -- Unless the list of case-insensitive name clashes is
      -- empty, we allert the user.
      CM.unless (null caseNameClashInfos) $ do
        docCaseNameClashInfos <-
          ppPathsFromPathInfo caseNameClashInfos $
            Just PRT.Green
        CTC.lift $ PRT.putDoc $
          DS.fromString
            "These files have case-insensitive name conflicts:"
          <> P.line
          <> P.indent 2 docCaseNameClashInfos
          <> P.line
        
      -- Gets user input if we should proceed anyway, even
      -- with invalid names.
      -- Notice that this is not asked if there are no
      -- invalid names.
      proceedInvNames <-
        if null invNameInfos
        then return True
        else
          CTC.lift $ promptYes (cmo_assumeYes cmo) $
            DS.fromString "Proceed anyway? [Y/n] "

      CM.when proceedInvNames $ do
        -- At this point, the user signaled that they
        -- really want to proceed, so we must add the
        -- declarations of the 'PathInfo's in the 
        -- 'infosNotFullyDec' list.
        --
        -- Normally, if we were to simply 'mapM' the
        -- 'addDeclarations' function over that list, we would
        -- end up with a list (ignoring monadic context)
        -- of type
        -- > [[(String, Either IOException Bool)]]
        -- which is a issue, since we lose the information
        -- of which 'PathInfo' had each result. We can
        -- slightly modify the function used in the 'mapM'
        -- to save this information. This produces
        -- > [ ( PathInfo 
        -- >   , [(String, Either IOException Bool)]
        -- >   )
        -- > ]
        -- which is better, but still fails in one thing:
        -- Before, we were showing the paths grouped by their
        -- Discipline Directories. Here, the list is 
        -- indexed by 'PathInfo'. We would then need to
        -- transpose this list, to get back something of
        -- type (ignoring the monadic context)
        -- > [ ( String
        -- >   , [(PathInfo, Either IOException Bool)]
        -- >   )
        -- > ]
        -- Which would be easier to print. Luckily, the
        -- function 'adjustOutputFormat' can produce this
        -- kind of output for us.
        --
        -- However, we must do one slight adjustment:
        -- Imagine we have two Discipline Directories,
        -- D0 and D1, and a proof file F that should be
        -- declared in both, however, F is only declared in
        -- one, say, D0. Suppose that F is within the
        -- argument path.
        --
        -- Then F (or, more precisely, the 'PathInfo' value
        -- associated with F) is not fully declared, so
        -- F is in the list 'infosNotFullyDec'.
        --
        -- If everything runs successfully (no exceptions
        -- thrown) then the output of F through the
        -- 'addDeclarations' function will be (putting aside
        -- the monadic context)
        -- [("D0", Right False), ("D1", Right True)]
        -- because D0's project file did not have to
        -- be edited (hence the 'False') and D1's had
        -- (hence the 'True'). So, if we adjusted the
        -- output with 
        -- > adjustOutputFormat 
        -- >   infosNotFullyDec 
        -- >   addDeclarations
        -- then the result would be (ignoring monadic
        -- context) a list similar to (denoting by 'piF' the
        -- 'PathInfo' value associated with the file F)
        -- > [ ( "D0"
        -- >   , [ ...
        -- >     , (piF, Right False)
        -- >     , ...
        -- >     ]
        -- >   )
        -- > , ( "D1"
        -- >   , [ ...
        -- >     , (piF, Right True)
        -- >     , ...
        -- >     ]
        -- >   )
        -- > ]
        -- The fact that D0's project file did not have to
        -- be edited means that, when we were previewing the
        -- actions, F was not shown as a file that was
        -- going to get included in D0's project file, so
        -- it would be weird to show it as successfully 
        -- addeded there.
        --
        -- Thus, the solution here is slightly filter
        -- the output of 'addDeclarations', removing all
        -- 'Right False's from there.
        let
          removeRightFalse :: (a, Either b Bool) -> Bool
          removeRightFalse (_, Right False) = False
          removeRightFalse _ = True

        addRes <- adjustOutputFormat infosNotFullyDec
          (fmap (filter removeRightFalse) . addDeclarations)

        -- We get the 'Doc' used to print 'addRes'.
        -- > ppAddRes :: [Doc AnsiStyle]
        ppAddRes <- ppDisplayRes addRes
        CTC.lift $ PRT.putDoc $
          P.line
          <> P.line
          <> DS.fromString "Project file editing results:"
          <> P.line
          <> P.indent 2 (P.vsep ppAddRes)
          <> P.line
          <> P.line

        -- Now we call 'putRebuildMakefiles', which calls the
        -- functions to rebuild all (Coq)Makefiles and 
        -- prints their output. 
        putRebuildMakefiles
  where
    -- Takes a list of 'PathInfo's, and tests if they are
    -- declared everywhere. If they are, they are simply
    -- excluded from the list.
    -- The return result is (in the monadic context),
    -- a list of
    -- > ( PathInfo
    -- > , [(String, Either IOException Bool)]
    -- > )
    -- That is, the original 'PathInfo', and the list of
    -- pairs Discipline Directory - Declaration verification.
    onlyNotFullyDeclared
      :: [PathInfo]
      -> CTR.ReaderT
          ProofSystemConfig
          IO
          [ ( PathInfo
            , [(String, Either UIOE.IOException Bool)]
            )
          ]
    -- Base case of the recursion.
    onlyNotFullyDeclared [] = return []
    onlyNotFullyDeclared (i : is) = do
      -- Gets the declaration condition of 'i'.
      -- iDec :: [(String, Either IOException Bool)]
      iDec <- isDeclared i
      let
        -- Sees if 'i' is declared everywhere.
        everywhereDec :: Bool
        everywhereDec =
          all
            ( \(_, eit) ->
              case eit of
                Right True -> True
                _          -> False
            )
            iDec
      -- If it is declared everywhere, discard it from the
      -- recursion. Otherwise, add (i, iDec) to the head
      -- of the list and recurse.
      if everywhereDec
      then onlyNotFullyDeclared is
      else (:) (i, iDec) <$> onlyNotFullyDeclared is

    -- This function consumes the output of 
    -- 'onlyNotFullyDeclared' (i.e. a list of 'PathInfo's
    -- and declaration status per Discipline Directory);
    -- a declaration status predicate, and the base name of 
    -- a Discipline Directory.
    -- It then returns a list of 'PathInfo's such that:
    -- - The given Discipline Directory was in its list, and
    -- - The declaration result from that Discipline Directory
    --   satisfies the given declaration status predicate.
    matchDiscDirAndDecStatus
      :: [ ( PathInfo
           , [(String, Either UIOE.IOException Bool)]
           )
         ]
      -> (Either UIOE.IOException Bool -> Bool)
      -> String
      -> [(PathInfo, Either UIOE.IOException Bool)]
    matchDiscDirAndDecStatus [] _ _ = []
    matchDiscDirAndDecStatus ((i, ddsts) : xs) predicate dd =
      let
        -- Gets of the elements in the list of
        -- Discipline Directory-Declaration satus pairs
        -- that matches the predicate.
        -- For all those we find, we replace the first
        -- entry (which is the name of the Discipline
        -- Directory), with the current 'PathInfo'.
        matches :: [(PathInfo, Either UIOE.IOException Bool)]
        matches =
          map (\(_, sts) -> (i, sts)) $
            filter
              (\(dd', sts) -> (dd == dd') && predicate sts)
              ddsts
      in
        -- Prepend the matches to the list, and recurse
        -- onwards to the rest of the 'xs'.
        matches ++ matchDiscDirAndDecStatus xs predicate dd

    -- And this function (basically) maps 
    -- 'matchDiscDirAndDecStatus' over all Discipline 
    -- Directories, allowing one to sort the result of
    -- a 'onlyNotFullyDeclared' by Discipline Directory and
    -- declaration status predicate.
    matchDecStatus
      :: Monad m
      => [ ( PathInfo
           , [(String, Either UIOE.IOException Bool)]
           )
         ]
      -> (Either UIOE.IOException Bool -> Bool)
      -> CTR.ReaderT
          ProofSystemConfig
          m
          [ ( String
            , [(PathInfo, Either UIOE.IOException Bool)]
            )
          ]
    matchDecStatus xs predicate = do
      -- Gets the list of all Discipline Directory base names.
      discdirs <- CTR.asks $
        map discDirBaseName . disciplineDirectories
      -- Maps 'matchDiscDirAndDecStatus', but retaining the
      -- information of the Discipline Directory.
      return $ map
        (\dd -> (dd, matchDiscDirAndDecStatus xs predicate dd))
        discdirs

    -- This function is used to produce a list of 
    -- 'Doc' (when printing it, just use 'vsep' to combine
    -- the lines into a single 'Doc') for pretty-printing 
    -- from a list indexed by Discipline Directory (such as 
    -- the ones produced by 'matchDecStatus') while completely 
    -- ignoring declaration information.
    --
    -- Paths are printed in green.
    --
    -- Discipline Directories with empty declaration lists
    -- are also omitted).
    ppDiscardExtra
      :: Monad m
      => [(String, [(PathInfo, a)])]
      -> CTR.ReaderT ProofSystemConfig m [P.Doc PRT.AnsiStyle]
    -- Base recursion case.
    ppDiscardExtra [] = return []
    -- Empty declaration list.
    ppDiscardExtra ((_, []) : _) = return []
    -- Non-empty declaration list.
    ppDiscardExtra ((dd, l) : xs) = do
      -- Get the pretty-printing 'Doc' of the declaration list.
      ppl <- ppPathsAndExtra l (const mempty) (Just PRT.Green)
      -- Recurse.
      (:)
        ( P.annotate PRT.bold (DS.fromString dd)
          <> P.line
          <> P.indent 2 ppl
        )
        <$> ppDiscardExtra xs

    -- Similar to 'ppDiscardExtra', but this one will show
    -- exceptions indented 2 spaces, in the line below.
    ppDisplayExceptions
      :: (Show a, Monad m)
      => [(String, [(PathInfo, Either a b)])]
      -> CTR.ReaderT ProofSystemConfig m [P.Doc PRT.AnsiStyle]
    -- Base recursion case.
    ppDisplayExceptions [] = return []
    -- Empty declaration list.
    ppDisplayExceptions ((_, []) : xs) = ppDisplayExceptions xs
    -- Non-empty declaration list.
    ppDisplayExceptions ((dd, l) : xs) = do
      let
        -- The pretty-printing function for the "extras".
        extraDoc
          :: Show a
          => Either a b
          -> P.Doc PRT.AnsiStyle
        extraDoc (Right _) = mempty
        extraDoc (Left e)  =
          P.line
          <>
          P.indent 2
            (DS.fromString $
              "Exception: " ++ show e
            )
      -- Get the pretty-printing 'Doc' of the declaration list.
      ppl <- ppPathsAndExtra l extraDoc (Just PRT.Green)
      -- Recurse.
      (:)
        ( P.annotate PRT.bold (DS.fromString dd)
          <> P.line
          <> P.indent 2 ppl
        )
        <$> ppDisplayExceptions xs

    -- Similar to 'ppDiscardExtra' and 'ppDisplayExceptions',
    -- but this one will show "Success" to successful builds,
    -- and will show the exception for failed ones.
    --
    -- One slight issue of this approach is the lack of
    -- separation between the failed and successful results,
    -- so that we can't output things to stdout and stderr
    -- as before.
    ppDisplayRes
      :: (Show a, Monad m)
      => [(String, [(PathInfo, Either a Bool)])]
      -> CTR.ReaderT ProofSystemConfig m [P.Doc PRT.AnsiStyle]
    -- Base recursion case.
    ppDisplayRes [] = return []
    -- Empty declaration list.
    ppDisplayRes ((_, []) : xs) = ppDisplayRes xs
    -- Non-empty declaration list.
    ppDisplayRes ((dd, l) : xs) = do
      let
        -- The pretty-printing function for the "extras"
        extraDoc
          :: Show a
          => Either a Bool
          -> P.Doc PRT.AnsiStyle
        extraDoc (Left e) =
          DS.fromString $ ": Failure (Exception): " ++ show e
        extraDoc (Right True) =
          DS.fromString ": "
          <>
          P.annotate (PRT.color PRT.Green)
            (DS.fromString "Success.")
        extraDoc (Right False) =
          DS.fromString ": "
          <>
          P.annotate (PRT.color PRT.Green)
            ( DS.fromString
              "Success (Unexpected result. Please report)."
            )
      -- Get the pretty-printing 'Doc' of the declaration list.
      ppl <- ppPathsAndExtra l extraDoc (Just PRT.Green)
      -- Recurse.
      (:)
        ( P.annotate PRT.bold (DS.fromString dd)
          <> P.line
          <> P.indent 2 ppl
        )
        <$> ppDisplayRes xs
