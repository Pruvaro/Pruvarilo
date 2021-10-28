-- |
-- This module is meant to provide functions and data types
-- that are used in internal representations of the functions
-- in the @Modes.@ modules. It is very unlikely that any
-- external user might need to use those functions.
module Modes.Internal
  ( 
    -- * Prompt
    promptYes

    -- * Adjusting output value format
  , adjustOutputFormat
  , groupInfosPerDiscDir
  , groupInfosExtraPerDiscDir

    -- * Pretty-printing paths
  , ppPathsFromPathInfo 
  , ppPathsAndExtra
  , ppPathsFromDecPaths

    -- * Declaration
    -- $DECLARATION
  , decStatus

    -- * Rebuilding (Coq)Makefiles
  , putRebuildMakefiles
  ) where

-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import CompileManagement
  ( buildMakefileAll
  )

import Config
  ( ProofSystemConfig
  , DisciplineDirectory
    ( discDirBaseName
    )
  , disciplineDirectories
  , psRootPath
  )

import PathInfo
  ( PathInfo
  , canonicalPath
  , housingDiscDirBaseName
  )

import ProjectFileManagement
  ( isDeclared
  )

import qualified Control.Monad.Trans.Class as CTC
  ( lift
  )

import qualified Control.Monad.Trans.Reader as CTR
  ( ReaderT
  , asks
  )

import qualified Data.Char as DC
  ( toLower
  )

import qualified Data.List as DL
  ( find
  , partition
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
    )
  , bold
  , color
  , putDoc
  )

import qualified System.Exit as SE
  ( ExitCode
    ( ExitSuccess
    , ExitFailure
    )
  )

import qualified System.FilePath as SF
  ( makeRelative
  )

import qualified UnliftIO.Exception as UIOE
  ( IOException
  )

import qualified UnliftIO.IO as UIOIO
  ( hFlush
  , stdout
  )


-- PROMPT
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- A simple prompt function, which asks for user confirmation.
-- If the user inputs:  
--
-- * @y@ or @Y@;  
-- * Any case-independent version of "yes" (such as @yes@ and
--   @YES@, but also @yeS@, @Yes@, etc);    
-- * An empty line (just presses enter).
-- 
-- The input will be considered as a confirmation. This
-- function also receives as input an extra 'Bool' value,
-- which is meant to be the @assume-yes@ flag. So, if that
-- argument is 'True', the function automatically returns
-- 'True', and does not output anything.
promptYes
  :: Bool
  -- ^
  -- The @assume-yes@ value. If @True@, this function 
  -- returns 'True' automatically.
  -> P.Doc PRT.AnsiStyle
  -- ^
  -- The prompt message.
  -> IO Bool
  -- ^
  -- 'True' if the returned answer can be interpreted as an
  -- yes, 'False' otherwise.
promptYes True _ = return True
promptYes _ msg =
  do
    -- We flush stdout to make sure that the message
    -- appears before the input.
    PRT.putDoc msg
    UIOIO.hFlush UIOIO.stdout
    ans <- getLine
    return ((map DC.toLower ans) `elem` ["", "y", "yes"])


-- ADJUSTING OUTPUT VALUE FORMAT
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- Many functions in the "ProjectFileManagement" take
-- a 'PathInfo' input and produce a list of type
-- @[(String, a)]@ for some kind of extra information @a@,
-- where the @String@ entry represents the base name of
-- some Discipline Directory. This list is also generally
-- within some other monadic context @m@ (often derived from
-- the 'CTR.ReaderT' transformer).
--
-- However, for printing, it is much nicer to group things
-- by the Discipline Directory, instead of by the files.
-- So, what this function aims to do, but with a more general
-- type signature, is to group things by the first tuple entry,
-- provided some list (which, in practical cases, will be
-- a list of 'PathInfo's) and a function that takes each
-- element of the list and produces a list of 2-tuples.
adjustOutputFormat
  :: (Eq b, Applicative f)
  => [a]
  -> (a -> f [(b, c)])
  -> f [ ( b , [(a, c)] ) ]
adjustOutputFormat [] _ = pure []
adjustOutputFormat (x : xs) g = 
  addMap
  <$> ((makeMap x) <$> (g x))
  <*> adjustOutputFormat xs g
  where
    -- This function produces the map-like structure we
    -- want to return, but with no functorial context.
    makeMap
      :: Eq b
      => a
      -> [(b, c)]
      -> [ ( b , [(a, c)] ) ]
    makeMap _ [] = []
    makeMap a ( (b, c) : ys ) =
      addSingleEntry (b, [(a, c)]) $ makeMap a ys
        

    -- This function combines two of the map-like structures
    -- we want to produce.
    addMap
      :: Eq b
      => [ ( b , [(a, c)] ) ]
      -> [ ( b , [(a, c)] ) ]
      -> [ ( b , [(a, c)] ) ]
    addMap [] zs = zs
    addMap (y : ys) zs = addMap ys (addSingleEntry y zs)

    -- This function adds a single entry map to a previous
    -- map-like structure.
    addSingleEntry
      :: Eq b
      =>   (b, [(a, c)])
      -> [ (b, [(a, c)]) ]
      -> [ (b, [(a, c)]) ]
    addSingleEntry (b, l) [] = [(b, l)]
    addSingleEntry (b, l) ( (b', l') : zs ) =
      if (b == b')
      then (b, l ++ l') : zs
      else (b', l') : addSingleEntry (b, l) zs

-- |
-- This function takes a list of 'PathInfo's, and groups
-- them based on Discipline Directory.
groupInfosPerDiscDir
  :: Monad m
  => [PathInfo]
  -> CTR.ReaderT ProofSystemConfig m [(String, [PathInfo])]
groupInfosPerDiscDir infos = do
  -- Gets the list of all Discipline Directory base names.
  discdirs <- CTR.asks $ 
      (map discDirBaseName) . disciplineDirectories

  -- We map a function which takes a Discipline Directory
  -- base name, and produces the 2-tuple with its base name
  -- and the list of 'PathInfo's that are in that Discipline
  -- Directory, obtained from filtering 'infos'.
  return $ map
    ( \dd ->
      (dd, filter ((== dd) . housingDiscDirBaseName) infos)
    )
    discdirs

-- |
-- A function similar to 'groupInfosPerDiscDir', but this
-- one carries an extra information together with the
-- 'PathInfo' when grouping (like build results for example).
groupInfosExtraPerDiscDir
  :: Monad m
  => [(PathInfo, a)]
  -> CTR.ReaderT 
      ProofSystemConfig 
      m 
      [(String, [(PathInfo, a)])]
groupInfosExtraPerDiscDir infoExtras = do
  -- Gets the list of all Discipline Directory base names.
  discdirs <- CTR.asks $
    (map discDirBaseName) . disciplineDirectories

  -- Maps over the list of all Discipline Directories
  -- a function that takes a Discipline Directory,
  -- filters a list of '(PathInfo, a)' for the 'PathInfo's
  -- in that Directory, and creates a 2-tuple with the first
  -- entry being the base name.
  return $ map
    ( \dd ->
      ( dd
      , filter 
          ((== dd) . housingDiscDirBaseName . fst)
          infoExtras
      )
    )
    discdirs


-- PRETTY PRINTING PATHS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This function takes a list of 'PathInfo's, and produces
-- a pretty-printing 'Doc' that is meant to be printed
-- to show that those paths in an optional colour, relative
-- to the root of the proof system.
-- 
-- * Extracts the absolute path of all of those 'PathInfo'
--   values, by mapping the getter 'canonicalPath' on the
--   argument list.  
-- * If everything is running as expected, those 'PathInfo'
--   values should have been generated by the 'pathInfo'
--   function, and so they should be subpaths of the
--   root of the proof system. Thus, by using the
--   'SF.makeRelative' function, the initial part of their
--   canonical path, corresponding to the path of the root,
--   should be removed, leaving only the part relative to
--   the root of the proof system (the 'SF.makeRelative'
--   function is pure, so, even if things are going wrong 
--   and the paths are not subpaths of the root, the
--   output of the 'SF.makeRelative' function will simply
--   be the original path, unchanged, and no exception
--   will be raised).  
-- * Colour all of the relative paths obtained in the 
--   previous step with the argument colour, and group 
--   them as lines.
-- * Group the paths, one per line, and, if a colour was
--   specified, colour them with it.
ppPathsFromPathInfo
  :: Monad m
  => [PathInfo]
  -> Maybe PRT.Color
  -> CTR.ReaderT ProofSystemConfig m (P.Doc PRT.AnsiStyle)
ppPathsFromPathInfo infos mbClr =
  let
    -- Get the canonical paths of the 'PathInfo's.
    canPaths :: [FilePath]
    canPaths = map canonicalPath infos
  in
    do
      -- Get the root of the proof system.
      root <- CTR.asks psRootPath
      
      let
        -- Make the paths relative to the root. Notice that
        -- this does not fail, and, if the paths are not
        -- subpaths of the root, they just are unchanged.
        relPaths :: [FilePath]
        relPaths = map (SF.makeRelative root) canPaths

      -- Group all paths and, if a colour was provided, 
      -- colour them with that colour.
      return $ 
        case mbClr of
          Nothing -> P.vsep $ map DS.fromString relPaths
          Just clr ->
            P.annotate (PRT.color clr) $ P.vsep $
              map DS.fromString relPaths

-- |
-- A function very similar to 'ppPathsFromPathInfo', except
-- that this one handles lists with pairs of 'PathInfo's
-- and some extra information.
--
-- A function has to be provided which transforms the
-- elements of that extra information type into some
-- kind of 'P.Doc' to be printed.
--
-- For example, for a list  
--
-- > l = [(p0, a0), (p1, a1), (p2, a2)]
--
-- where @p0@, @p1@, and @p2@ are are 'PathInfo' values
-- with relative paths (relative to the root of the
-- proof system, as set in the 'ProofSystemConfig' 
-- environment's 'psRootPath' field) @rp0@, @rp1@, and
-- @rp2@, respectively, and @toDoc@ is the function
-- which handles the transformation of the @a0@, @a1@,
-- and @a2@ values into 'P.Doc' pretty text, then
-- the resulting output (within the monadic context) of  
--
-- > ppPathsAndExtra l toDoc Nothing
--
-- will be, simplifying the isomorphisms 'DS.fromString',
--
-- >    p0 <> (toDoc a0)
-- > <> line
-- > <> p1 <> (toDoc a1)
-- > <> line
-- > <> p2 <> (toDoc a2)
-- > <> line
--
-- Notice that the @toDoc a0@, @toDoc a1@, and @toDoc a2@
-- values might themselves contain new lines, if one wants
-- to print their information in some other line.
--
-- In this example, we gave the optional colour value a
-- 'Nothing'. However, if one gives it some 'Just'-wrapped
-- 'PRT.Color', then that colour would affect the colour of
-- @p0@, @p1@, and @p2@. The @toDoc@ function could be
-- used to set colour to the 'Doc's from @a0@, @a1@, and
-- @a2@ independently.
ppPathsAndExtra
  :: Monad m
  => [(PathInfo, a)]
  -> (a -> P.Doc PRT.AnsiStyle)
  -> Maybe PRT.Color
  -> CTR.ReaderT ProofSystemConfig m (P.Doc PRT.AnsiStyle)
ppPathsAndExtra infoExtras toDoc mbClr =
  let
    -- Gets only the canonical paths of the 'PathInfo's 
    -- (and also their associated exceptions).
    canPathExtras = 
      map (\(i, e) -> (canonicalPath i, e)) infoExtras
  in do
    -- Get the root of the proof system.
    root <- CTR.asks psRootPath

    let
      -- 
      relPathExtras = 
        map 
          (\(p, e) -> (SF.makeRelative root p, e))
          canPathExtras

      -- 
      ppPair (p, e) =
        ( case mbClr of
            Nothing -> DS.fromString p
            Just clr -> 
              P.annotate (PRT.color clr) $ DS.fromString p
        ) <> toDoc e

    return $ P.vsep $ map ppPair relPathExtras


-- |
-- This function does something similar to the 
-- 'ppPathsFromPathInfo' function, but works with a list of
-- declarative paths instead of a list of 'PathInfo's.
ppPathsFromDecPaths
  :: [FilePath]
  -- ^
  -- A list of declarative paths.
  -> Maybe PRT.Color
  -- ^
  -- An optional colour.
  -> P.Doc PRT.AnsiStyle
ppPathsFromDecPaths decPaths mbClr =
  ( case mbClr of
      Nothing  -> id
      Just clr -> P.annotate (PRT.color clr)
  ) $ P.vsep $ map DS.fromString decPaths


-- DECLARATION
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- $DECLARATION
-- In this section we define some data types and functions
-- which encode the declaration status of some proof file
-- on the Discipline Directories, that is, where it
-- is declared, where it should be but is not declared,
-- and where it should be declared, but checking failed.

-- |
-- Details how a file is declared. This data type is composed
-- of three fields, representing:  
--
-- * The list of the base names of all Discipline Directories
--   where this file should be declared, and is declared.    
-- * The list of the base names of all Discipline Directories
--   where this file should be declared, but
--   __is not__ declared.  
-- * The list of base name-exception pairs of all Discipline
--   Directories where the file should be declared, but
--   checking caused an 'UIOE.IOException' to be thrown
--   (and caught), so actual status is unknown.
data DeclarationStatus =
  DeclarationStatus
  {
    -- | 
    -- Should be declared and is.
    ds_shouldBeIs :: [String]

    -- |
    -- Should be but is not.
  , ds_shouldBeIsNot :: [String]

    -- |
    -- Should be but some exception was thrown during the
    -- checking process.
  , ds_shouldBeFail :: [(String, UIOE.IOException)]
  }

-- |
-- This function builds a 'DeclarationStatus' value out of
-- a 'PathInfo' value, by calling the 'isDeclared' function
-- and moving the output information to the correct fields.
pathInfoToDecStatus
  :: PathInfo
  -> CTR.ReaderT ProofSystemConfig IO DeclarationStatus
pathInfoToDecStatus info = 
  let
    -- This auxiliar function takes a list of tuples,
    -- where the second entry is an @Either@, and 
    -- creates a tuple of lists of tuples.
    -- The first list contains the first entries of the
    -- original tuples, and the 'Left'-enclosed value
    -- of their associated 'Either', while the second list
    -- contains the first entries of the original tuples,
    -- and their associated 'Right'-enclosed values.
    -- For example, when called on
    -- > [(1, Left 'a'), (2, Right 'b'), (3, Left 'c')]
    -- this function produces
    -- > ([(1, 'a'), (3, 'c')], [(2, 'b')])
    unfoldEither
      :: [(a, Either b c)]
      -> ([(a, b)], [(a, c)])
    unfoldEither [] = ([], [])
    unfoldEither ((a, eit) : xs) =
      let
        (recL, recR) = unfoldEither xs
      in
        case eit of
          Left  b -> ( (a, b) : recL,          recR )
          Right c -> (          recL, (a, c) : recR )
  in
    do
      -- Gets the declared information from the 'isDeclared'
      -- function, which provides a (monadic) list of type
      -- > (String, Either IOException Bool)]
      -- the 'String' being the base name of the Discipline
      -- Directory, and the second entry being either
      -- an exception thrown (and caught) when trying to
      -- check for declaration, or a 'Bool', 'True' if
      -- the file is declared, and 'False' it is is not.
      decs <- isDeclared info

      -- Breaks the declarations into two lists, one
      -- containing the ones it successfully verified, and
      -- the other containing the ones that it failed to
      -- verify the declarations.
      let
        -- fails :: [(String, IOException)]
        -- succs :: [(String, Bool)]
        (fails, succs) = unfoldEither decs

        -- We break the successfully verified into two
        -- lists, the one where it is declared ('Bool' entry
        -- is a 'True') and the one where it is not ('Bool'
        -- entry is a 'False').
        -- 
        -- isMent    :: [String]
        -- isNotMent :: [String]
        (isMent, isNotMent) =
          (\(x, y) -> ( map fst x , map fst y )) $
            DL.partition snd succs

      -- Finally, we combine all those lists into the
      -- returning 'DeclarationStatus' value.
      return
        DeclarationStatus
        { ds_shouldBeIs    = isMent
        , ds_shouldBeIsNot = isNotMent
        , ds_shouldBeFail  = fails
        }

-- |
-- This function produces declaration information in
-- from a list of 'PathInfo' values, in a per-Discipline
-- Directory basis. That is, it produces, (putting aside the
-- 'ReaderT ProofSystemConfig IO' monadic context), a list
-- of 4-tuples. The entries mean:
--
-- * First entry: A 'String' value corresponding to the
--   base name of the Discipline Directory. The returning
--   list ranges over all Discipline Directories.  
-- * Second entry: A @['PathInfo']@ corresponding to the
--   provided 'PathInfo' values of the argument list that
--   should be declared in the project file of the
--   Discipline Directory whose base name
--   is the first entry of the 4-tuple, and are declared.  
-- * Third entry: A @['PathInfo']@ corresponding to the
--   provided 'PathInfo' values of the argument list that
--   should be declared in the project file of the
--   Discipline Directory whose base name
--   is the first entry of the 4-tuple, but are 
--   not declared.  
-- * Fourth entry: A @[('PathInfo', 'UIOE.IOException')]@
--   corresponding to the provided 'PathInfo' values in the
--   argument list that should be declared in the project
--   file of the Discipline Directory whose base name is the
--   first entry of the 4-tuple, however an exception was
--   thrown (and caught) while trying to check, so 
--   the declaration could not be decided.
decStatus
  :: [PathInfo]
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      [ ( String
        , [PathInfo]
        , [PathInfo]
        , [(PathInfo, UIOE.IOException)]
        )
      ]
decStatus infos =
  -- Below, we define the 'decStatusPerDD', which builds
  -- the individual element of the returning list when
  -- provided the base name of the Discipline Directory.
  -- So, by getting the entire list of Discipline Directories
  -- from the environment, we can simply return the mapping
  -- of the that function and finish.
  CTR.asks ((map discDirBaseName) . disciplineDirectories)
  >>= mapM decStatusPerDD
  where
    -- The 'DeclarationStatus' value associated with the
    -- 'PathInfo', as build by the 'pathInfoToDecStatus'
    -- function, conveniently provides a list of the
    -- Discipline Directories a 'PathInfo' should be in,
    -- and is, is not, or yielded a failure.
    --
    -- So, we pair up the original 'PathInfo' with their
    -- associated declaration status, in order to quickly
    -- access that information and discriminate between
    -- 'PathInfo's in a simpler way.
    decs 
      :: CTR.ReaderT 
          ProofSystemConfig 
          IO 
          [(PathInfo, DeclarationStatus)]
    decs = mapM (\i -> (,) i <$> pathInfoToDecStatus i) infos
    
    -- This function produces the individual entry in the
    -- returning list. It takes as an argument the base
    -- name of the Discipline Directory.
    decStatusPerDD
      :: String
      -> CTR.ReaderT
          ProofSystemConfig
          IO
          ( String
          , [PathInfo]
          , [PathInfo]
          , [(PathInfo, UIOE.IOException)]
          )
    decStatusPerDD dd = 
      do
        piDs <- decs
        let
          -- We define each entry of the tuple by filtering
          -- the list 'decs', which contains pairs of the
          -- given 'PathInfo' values and their associated
          -- 'DeclarationStatus' information.
          -- 
          -- The first entry is simply the provided 
          -- Discipline Directory @dd@.
          --
          -- The second entry are the 'PathInfo's of the
          -- declared files.
          declared :: [PathInfo]
          declared = 
            map fst $ filter
              (\(_, dec) -> elem dd $ ds_shouldBeIs dec)
              piDs

          -- The third entry contains the 'PathInfo's of
          -- the not declared files.
          notDeclared :: [PathInfo]
          notDeclared = 
            map fst $ filter
              (\(_, dec) -> elem dd $ ds_shouldBeIsNot dec)
              piDs

          -- And the fourth entry contains the 'PathInfo'
          -- and 'IOException' pairs of the files that should
          -- have been declared, but the verification
          -- yielded an exception throw.
          --
          -- This one is a bit more complicated, so we
          -- will define an auxiliar function.
          failDeclaredAux
            :: [(PathInfo, DeclarationStatus)]
            -> [(PathInfo, UIOE.IOException)]
          failDeclaredAux [] = []
          failDeclaredAux ((i, ds) : xs) =
            -- The 'find' function will try to return
            -- the first element in the list which
            -- matches the predicate, that being
            -- 'dd' is a name in the list of fails (and
            -- each fail is paired with its exception).
            case 
              ( DL.find ((dd ==) . fst) $ ds_shouldBeFail ds
              )
            of
              Nothing -> failDeclaredAux xs
              Just (_, exc) -> (i, exc) : failDeclaredAux xs

          -- And using the auxiliar function, we get the
          -- fourth entry.
          failDeclared :: [(PathInfo, UIOE.IOException)]
          failDeclared = failDeclaredAux piDs

        -- We now package everything together and
        -- return it.
        return (dd, declared, notDeclared, failDeclared)


-- REBUILDING (COQ)MAKEFILES
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- Outputs (to @stdout@) a message that all the CoqMakefiles 
-- will be rebuilt. Tries to rebuild them, allowing the
-- command output to be shown, and signaling which Discipline
-- Directory's file is being rebuilt.
--
-- This function collects the exit codes of the rebuilding
-- commands and, at the end, presents a summary of all
-- the built files.
--
-- Internally, this function uses the 'buildMakefileAll'
-- function from the "CompileManagement" module.
putRebuildMakefiles :: CTR.ReaderT ProofSystemConfig IO ()
putRebuildMakefiles = do
  -- Output a message explaining what will happen.
  CTC.lift $ PRT.putDoc $ 
    P.line
    <> DS.fromString "Rebuilding CoqMakefiles:"
    <> P.line
    
  -- > mkfBuildRes 
  -- >  :: [(String, Either IOException ExitCode)]
  mkfBuildRes <- buildMakefileAll

  CTC.lift $ PRT.putDoc $
    P.line
    <> P.line
    <> DS.fromString "CoqMakefile rebuild results:"
    <> P.line
    <> (P.indent 2 $ P.vsep $ ppMkfBuild mkfBuildRes)
    <> P.line
  where
    -- An auxiliar function to pretty-print the
    -- Makefile (re)building results.
    ppMkfBuild
      :: [(String, Either UIOE.IOException SE.ExitCode)]
      -> [P.Doc PRT.AnsiStyle]
    ppMkfBuild [] = []
    ppMkfBuild ((dd, Right SE.ExitSuccess) : xs) =
      ( ( P.annotate PRT.bold $
            DS.fromString $ dd ++ ":"
        ) 
        <+> 
        ( P.annotate (PRT.color PRT.Green) $
            DS.fromString "Success."
        )
      ) : ppMkfBuild xs
    ppMkfBuild ((dd, Right (SE.ExitFailure n)) : xs) =
      ( ( P.annotate PRT.bold $
            DS.fromString $ dd ++ ":"
        ) 
        <+> 
        ( DS.fromString $ 
          "Failure (process returned " ++ show n ++ ")."
        )
      ) : ppMkfBuild xs
    ppMkfBuild ((dd, Left e) : xs) =
      ( ( P.annotate PRT.bold $
            DS.fromString $ dd ++ ": "
        ) 
        <+> (DS.fromString $ "Exception:" ++ show e)
      ) : ppMkfBuild xs
