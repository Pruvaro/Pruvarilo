-- |
-- The goal of this module is to provide functions which
-- interact with the project files.
--
-- \"Project files\" are the files, housed directly inside
-- each Discipline Directory, which contain declarations for 
-- all the proof files that other proof files in the same 
-- Discipline Directory have access to.
--
-- For example, ideally, if all the proof files have been
-- correctly added, the \"Physics\" Discipline Directory's
-- project file will contain a list of the declarative
-- paths (see the "PathInfo" module for an explanation
-- on declarative paths) of all the proof files in the
-- the \"Physics\" Discipline Directory itself (descending
-- directly or indirectly), but also from all the proof files
-- in the \"Mathematics\" Discipline Directory (descending
-- directly or indirectly), since the \"Physics\" Discipline
-- Directory has dependency permission on the \"Mathematics\"
-- Discipline Directory.
--
-- It is this declaration of the files which will allow,
-- later, the building of the correct make files, allowing
-- for the dependencies to be built correctly.
module ProjectFileManagement
  ( allowedDiscDirs
  , isDeclared
  , addDeclarations
  , removeDeclarations
  , removeSubpaths
  , previewRemoveSubpaths
  , isDeclaredIn
  , nonExistentPaths
  ) where


-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Config
  ( ProofSystemConfig
  , DisciplineDirectory
    ( discDirBaseName
    , accessibleDiscDirs
    )
  , disciplineDirectories
  , proofFileExtensions
  , projectFileName
  , psRootPath
  )

import PathInfo
  ( PathInfo
  , declarativePath
  , housingDiscDirBaseName
  , relativePathElem
  , declarativeToAbsPath
  )

-- import qualified Control.DeepSeq as CDS
--   ( rnf
--   )

import qualified Control.Monad as CM
  ( filterM
  )

import qualified Control.Monad.Trans.Class as CTC
  ( lift
  )

import qualified Control.Monad.Trans.Reader as CTR
  ( ReaderT
  , asks
  )

import qualified Data.List as DL
  ( isPrefixOf
  )

import qualified System.IO as SIO
  ( IOMode
    ( ReadMode
    )
  , Handle
  , hClose
  , hGetContents
  , hPutStr
  , hPutStrLn
  , openFile
  , openTempFile
  )

import qualified System.FilePath as SF
  ( (</>)
  , takeExtension
  )

import qualified UnliftIO.Directory as UIOD
  ( doesFileExist
  , getTemporaryDirectory
  , removeFile
  , renameFile
  )

import qualified UnliftIO.Exception as UIOE
  ( IOException
  , bracket
  , handleIO
  , evaluateDeep
  )

import qualified UnliftIO.IO as UIOIO
  ( hClose
  )


-- MAIN FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- Tests if a proof file, associated with the given 'PathInfo'
-- argument, is declared in all Discipline Directories it
-- should be declared in.
--
-- The return type is a (monadic) list containing 2-tuples.
-- The first entry of each tuple is the base name of the
-- Discipline Directory, ranging through all Discipline
-- Directories with dependency access on the provided file.
-- The the second entry is a sum type. This sum type 
-- can either be:  
--
--   * A 'Left'-wrapped 'UIOE.IOException', signaling that some
--     IO exception was thrown (and caught) while trying to
--     read the project file in the Discipline Directory with
--     the name in the first entry of the tuple, or  
--   * A 'Right'-wrapped 'Bool'. If 'True', then this
--     'PathInfo' is declared in the project file of the
--     Discipline Directory with that name. If 'False', 
--     it is not.
isDeclared
  :: PathInfo
  -- ^
  -- The 'PathInfo' value associated with the file we need
  -- to check for declaration.
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      [(String, Either UIOE.IOException Bool)]
  -- ^
  -- The list containing the Discipline Directory's base name
  -- and the result of the declaration test (as given by the
  -- 'isDeclaredIn' function).
isDeclared info =
  do
    -- Get the list of all Discipline Directories which should
    -- declare the given file.
    discDirs <- allowedDiscDirs info

    -- This 'discDirs' is a '[String]', containing the
    -- base names of the Discipline Directories.
    mapM (\d -> (,) d <$> isDeclaredIn info d) discDirs

-- |
-- Adds the declarations of a proof file, associated with the
-- given 'PathInfo' argument, to the project files of all
-- Discipline Directories which have dependency access on
-- that proof file.
--
-- The return type is a (monadic) list containing 2-tuples.
-- The first entry of each Tuple is the base name of the
-- Discipline Directory, ranging through all Discipline
-- Directories with dependency access on the provided file.
-- The second entry is a sum type. This sum type 
-- can either be:  
-- 
-- * A 'Left'-wrapped 'UIOE.IOException', signaling that some
--   IO exception was thrown (and caught) while trying to
--   read the project file (to avoid duplicate declarations)
--   or append the project file in the Discipline Directory
--   with the name in the first entry of the tuple; or  
-- * A 'Right'-wrapped 'Bool'. If 'True', then the 
--   corresponding project file has been edited, and the
--   declarative path was appended to its end. If 'False',
--   then the declarative path was already there, and the
--   project file did not need to be edited. Either way, a
--   'Right'-wrapped result means that the declaration is
--   in the project file.
addDeclarations
  :: PathInfo
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      [(String, Either UIOE.IOException Bool)]
addDeclarations info =
  do
    -- Get the list of all Discipline Directories which should
    -- declare the given file.
    discDirs <- allowedDiscDirs info

    -- This 'discDirs' is a '[String]', containing the
    -- base names of the Discipline Directories.
    mapM (\d -> (,) d <$> addDeclarationTo info d) discDirs

-- | 
-- Removes the declarations of a proof file, associated with 
-- the given 'PathInfo' argument, from the project files of
-- __all__ Discipline Directories, regardless of dependency
-- access (as a countermeasure to manually adding the
-- dependency on project files of non-permitted Discipline
-- Directories).
-- 
-- The return type is a (monadic) list containing 2-tuples.
-- The first entry of each tuple is the base name of the
-- Discipline Directory, ranging through all Discipline
-- Directories, and the second entry is a sum-type.
-- This sum-type can either be:  
--
-- * A 'Left'-wrapped 'UIOE.Exception', signaling that some
--   IO exception was thrown (and caught) somewhere in the
--   process of:    
--
--     * Opening and reading the original project file of 
--       that Discipline Directory (maybe due to insufficient 
--       permissions, or the file is non-existent).  
--     * Creating a new, empty, temporary file in the 
--       system's temporary directory (maybe due to  
--       insufficient permissions or resources, or the 
--       system does not have a temporary directory).  
--     * Writing the contents of the original project file to
--       the temporary file (maybe due to insufficient 
--       resources. If the file could not be created, this is
--       not even attempted).  
--     * Trying to remove the temporary file (if no 
--       references to the argument file were found) or 
--       replace the original project file with the 
--       temporary file (if references to the argument file 
--       were found, so the reconstructed temporary file is 
--       different from the original project file) (maybe 
--       due to insufficient permissions or resources).  
--
-- * A 'Right'-wrapped 'Bool'. If 'True', then the 
--   declarative path of the argument file was found in 
--   the project file of the Discipline Directory whose 
--   base name is in the first entry of the associated 
--   2-tuple, and therefore the project file had to be 
--   replaced. If 'False', then the declarative path of the 
--   argument file was not found in the original project 
--   file, so the original project file did not need to 
--   be replaced.
removeDeclarations
  :: PathInfo
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      [(String, Either UIOE.IOException Bool)]
removeDeclarations info =
  do
    -- Gets a list of __all__ Discipline Directories,
    -- regardless of their dependency permissions.
    discDirs <-
      CTR.asks $ map discDirBaseName . disciplineDirectories

    -- This 'discDirs' is a '[String]', containing the base
    -- names of all Discipline Directories.
    mapM
      (\d -> (,) d <$> removeDeclarationFrom info d)
      discDirs

-- |
-- This function removes from all Discipline Directories
-- the declaration of all subpaths of a given path.
--
-- The intended purpose of this function is to remove from
-- project files all declarations from proof files located in
-- subpaths of a provided path, __regardless the existence__
-- __of those files__. This is because, in order to use
-- the function 'removeDeclarations', for example, one needs a
-- 'PathInfo' value, which can only be produced by the
-- functions 'pathInfo' and 'pathInfos'. However, those
-- functions produce 'PathInfo' values by scanning the
-- files in disk, so, if no file exists in disk, its
-- respective 'PathInfo' will not be generated, and it will
-- not be able to be deleted through 'removeDeclarations'.
-- This is the case if, for example, the user deleted a file
-- from the disk before asking for its declarations to be
-- removed from the project files.
--
-- If, on the other hand, the files whose declarations should
-- be removed still exist, the use of 'removeDeclarations'
-- is prefered.
--
-- This function performs the following steps:  
-- 
-- = Step 1
--   Starting from the provided path, this function tries
--   to generate a canonical version of this path, which
--   is compared to the 'ProofSystemConfig' environment's
--   'psRootPath' field, containing the canonical path of
--   the root of the proof system.  
--   Using both canonical paths, is the provided path is
--   a subpath of the proof system's root, we obtain
--   the relative path elements. That is, if the canonical
--   version of the provided path is (using '/' as the
--   path separator as an example),  
--
--   > PATHTOROOT/some/deeper/path
--
--   then the relative path elements are the list  
--
--   > ["some", "deeper", "path"] :: [String]
--   
--   However, if the provided path is not a subpath of the
--   root, the function ceases here.
--   
-- = Step 2
--   Using the relative path elements, we construct a
--   'String' which acts as a prefix to the declarative
--   path of all proof files within that path. That is,
--   for example, if we have the files @f0.v@, @f1.v@ and
--   @f2.v@, with paths  
--   
--   > PATHTOROOT/some/deeper/path/f0.v
--   > PATHTOROOT/some/deeper/path/dir/f1.v
--   > PATHTOROOT/some/deeper/path/dir/f2.v
--   
--   then their declarative paths should be (see the
--   documentation for the 'pathInfo' function for more
--   information on declarative paths),  
--
--   > ../some/deeper/path/f0.v
--   > ../some/deeper/path/dir/f1.v
--   > ../some/deeper/path/dir/f2.v
--
--   which all share the same prefix
--
--   > ../some/deeper/path/  
--
--   One caveat is that, if the provided path is the path
--   to a proof file (that is, it has a file extension
--   matching the ones in the 'proofFileExtensions' field
--   of the 'ProofSystemConfig' environment) then the
--   trailing '/' will not be added.
--   
--   Note for developers: Since this function produces
--   'String's that must be preffixes to the declarative
--   paths produces by 'pathInfo', any change in how those
--   are formed in the 'pathInfo' function will require a
--   change in this function as well.  
--
-- = Step 3
--   Using this shared prefix, we go through the project files
--   in all Discipline Directories and remove all lines
--   with that prefix. As a consequence, we end up removing
--   declarations for every proof file located in a subpath of
--   the provided path __regardless if that file still__
--   __exists or not__.
-- 
-- = Return type
-- The return type of this function, putting aside the
-- 'CTR.ReaderT ProofSystemConfig IO' monadic context,
-- can either be:  
--
-- * A 'Left'-wrapped 'UIOE.IOException': This indicates
--   that there was a problem in canonicalizing the provided
--   path, per use of the 'relativePathElem' function. Any
--   exceptions thrown are then caught and returned this way.
-- * A 'Right'-wrapped list of tuples of type
--   '(String, Either UIOE.IOException [String])': The
--   first entry of those tuples is the base name of a
--   Discipline Directory whose project is being edited.
--   The second entry is the result of that editing process.  
--
--     * If this result is a 'Left'-wrapped 
--       'UIOE.IOException', then some exception was thrown
--       (and caught) while trying to edit its project file.
--       This might have many causes, such as no read/write
--       permission on the project file, no notion of a
--       temporary directory (can be checked using the
--       'UIOD.getTemporaryDirectory' function. This is
--       a problem because, in order to edit the project file,
--       a temporary file is generated on the temporary
--       directory, which replaces the original file if
--       some modification was required), or insufficient
--       system resources.  
--     * If this result is a 'Right'-wrapped '[String]',
--       then this list contains all the lines of the
--       project file which were deleted.
removeSubpaths
  :: FilePath
  -- ^
  -- The path (to directory or proof file) from which all
  -- proof files in subpaths should have their declarations
  -- removed from all project files.
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      ( Either
          UIOE.IOException
          [(String, Either UIOE.IOException [String])]
      )
removeSubpaths path =
  do
    -- Later we will need the list of proof file extensions.
    proofFileExts <- CTR.asks proofFileExtensions

    -- Gets a list of the base names of every known
    -- Discipline Directory.
    discDirs <-
      CTR.asks $ map discDirBaseName . disciplineDirectories

    -- Try to get the relative path elements.
    eitPathElems <- relativePathElem path

    -- Check the if this process succeeded.
    case eitPathElems of
      Left e ->
        -- Canonicalizing the path yielded an exception.
        -- Just return this exception and finish the function.
        return $ Left e
      Right mbPathElems ->
        -- We must check if the path elements were actually
        -- generated. If it is a 'Nothing', that means the
        -- provided path is not a subpath of the proof
        -- system, and so we have nothing to do.
        --
        -- In this case, we return an empty list, meaning
        -- "we checked no Discipline Directories at all".
        case mbPathElems of
          Nothing -> return $ Right []
          Just pathElems ->
            let
              -- This is the part that needs to be
              -- consistent with 'pathInfo'.
              -- First, we combine the path elements
              -- adding '/' between them, regardless of
              -- the platform's path separator.
              prefix' :: String
              prefix' =
                foldl (\x y -> x ++ "/" ++ y) ".." pathElems

              -- Then, if the prefix has the extension of
              -- a proof file, we don't add the trailing
              -- '/'. If it does not have, we add.
              prefix :: String
              prefix =
                if
                  SF.takeExtension prefix'
                  `elem`
                  proofFileExts
                then prefix'
                else prefix' ++ "/"
            in
              Right <$>
                mapM
                  ( \d ->
                    (,) d <$>
                      removeLine (DL.isPrefixOf prefix) d
                  )
                  discDirs

-- |
-- This function is meant as a preview of the effects done
-- by the 'removeSubpaths' function.
--
-- Its type signature is the same as the return type of the
-- 'removeSubpaths' function, and it also executes very
-- similar procedures, with the exception that it __does__
-- __not__ produce any temporary files, nor modifies the
-- original project files in any way. Check the documentation
-- of the 'removeSubpaths' function to understand what 
-- each possible return value of this function means.
previewRemoveSubpaths
  :: FilePath
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      ( Either
          UIOE.IOException
          [(String, Either UIOE.IOException [String])]
      )
previewRemoveSubpaths path =
  do
    -- Later we will need the list of proof file extensions.
    proofFileExts <- CTR.asks proofFileExtensions

    -- Gets a list of the base names of every known
    -- Discipline Directory.
    discDirs <-
      CTR.asks $ map discDirBaseName . disciplineDirectories

    -- Try to get the relative path elements.
    eitPathElems <- relativePathElem path

    -- Check the if this process succeeded.
    case eitPathElems of
      Left e ->
        -- Canonicalizing the path yielded an exception.
        -- Just return this exception and finish the function.
        return $ Left e
      Right mbPathElems ->
        -- We must check if the path elements were actually
        -- generated. If it is a 'Nothing', that means the
        -- provided path is not a subpath of the proof
        -- system, and so we have nothing to do.
        --
        -- In this case, we return an empty list, meaning
        -- "we checked no Discipline Directories at all".
        case mbPathElems of
          Nothing -> return $ Right []
          Just pathElems ->
            let
              -- This is the part that needs to be
              -- consistent with 'pathInfo'.
              -- First, we combine the path elements
              -- adding '/' between them, regardless of
              -- the platform's path separator.
              prefix' :: String
              prefix' =
                foldl (\x y -> x ++ "/" ++ y) ".." pathElems

              -- Then, if the prefix has the extension of
              -- a proof file, we don't add the trailing
              -- '/'. If it does not have, we add.
              prefix :: String
              prefix =
                if
                  SF.takeExtension prefix'
                  `elem`
                  proofFileExts
                then prefix'
                else prefix' ++ "/"
            in
              Right <$>
                mapM
                  ( \d ->
                    (,) d <$>
                      previewDel (DL.isPrefixOf prefix) d
                  )
                  discDirs
  where
    -- This is the auxiliar function which does the
    -- analogous job of the 'removeLine' function in the
    -- 'removeSubpaths' function.
    -- Here, this function will only read a project file
    -- and return all the files that match a given
    -- predicate, but does not do anything else, like
    -- creating temporary files or moving files around.
    previewDel
      :: (String -> Bool) -- The deletion predicate.
      -> String -- Base name of the Discipline Directory.
      -> CTR.ReaderT
          ProofSystemConfig
          IO
          (Either UIOE.IOException [String])
    previewDel predicate discDir =
      do
        -- Get the canonical path of the root of the
        -- proof system from the environment.
        root <- CTR.asks psRootPath

        -- Get the base name of all project files.
        projFileBaseName <- CTR.asks projectFileName

        -- We produce the path of the project file of the
        -- provided Discipline Directory.
        let
          projFilePath :: FilePath
          projFilePath =
            root SF.</> discDir SF.</> projFileBaseName

        -- Now, we simply wrap everything in a 'handleIO'
        -- to catch possible exceptions.
        CTC.lift $ UIOE.handleIO (return . Left) $
          Right <$> UIOE.bracket
            (SIO.openFile projFilePath SIO.ReadMode)
            SIO.hClose
            ( \hdl -> do
              cnt <- filter predicate . lines <$>
                SIO.hGetContents hdl
              UIOE.evaluateDeep cnt
            )

-- |
-- This function goes through the project file of some
-- Discipline Directory, and for every path line (identified
-- by starting with a ".."), this function checks if the
-- file with that declarative path actually exists.
--
-- The return type of this function is similar to the return
-- type of functions like 'removeSubpaths', in that it can
-- be (ignoring the @'CTR.ReaderT' 'ProofSystemConfig' 'IO'@ 
-- monadic context): 
--
-- * A 'Left'-wrapped 'UIOE.IOException': This indicates that
--   the project file itself could not be read at all, and
--   so no file existence checks were even possible.
-- * A 'Right'-wrapped list of tuples of 'FilePath's. These
--   are the paths that do not exist or do not correspond
--   to files. Notice that internally, this uses the
--   'UIOD.doesFileExist' function, which does not fail,
--   and instead returns a 'False' if it cannot even reach
--   the path (due to, for example, insufficient resources),
--   so it is not necessary to return any
--   'UIOE.IOException' for possible failures.
nonExistentPaths
  :: String
  -- ^
  -- The base name of the Discipline Directory whose
  -- project file we must check.
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      (Either UIOE.IOException [FilePath])
nonExistentPaths discDir =
  let
    -- This is a test predicate to test lines that are 
    -- declarative paths, instead of lines that are
    -- other compiler flags. This function has to be
    -- consistent with how 'pathInfo' constructs 
    -- declarative paths.
    isPath :: String -> Bool
    isPath = DL.isPrefixOf ".."
  in
    do
      -- Get the canonical path of the root.
      root <- CTR.asks psRootPath

      -- Get the base name of project files.
      projFileBaseName <- CTR.asks projectFileName

      -- Build the project file's full path.
      let
        projFilePath :: FilePath
        projFilePath =
          root SF.</> discDir SF.</> projFileBaseName

      -- Since reading the file can cause an 
      -- 'UIOE.IOException' we wrap everything in 
      -- a 'handleIO'.
      UIOE.handleIO (return . Left) $
        -- First, we get only the lines in the file which
        -- correspond to paths.
        let
          paths
            :: CTR.ReaderT ProofSystemConfig IO [FilePath]
          paths =
            CTC.lift $ UIOE.bracket
              (SIO.openFile projFilePath SIO.ReadMode)
              SIO.hClose
              ( \hdl -> do
                cnt <- filter isPath . lines <$>
                  SIO.hGetContents hdl
                UIOE.evaluateDeep cnt
              )

          -- Then we make all those paths (which are
          -- declarative paths) into the absolute paths of
          -- the files they declare.
          paths'
            :: CTR.ReaderT ProofSystemConfig IO [FilePath]
          paths' = paths >>= mapM declarativeToAbsPath

          -- Finally, we filter out the paths that exist,
          -- leaving only those that do not.
          fileNotExist :: FilePath -> IO Bool
          fileNotExist f = not <$> UIOD.doesFileExist f
          paths''
            :: CTR.ReaderT ProofSystemConfig IO [FilePath]
          paths'' =
            paths' >>= CTC.lift . CM.filterM fileNotExist
        in
          -- The only thing left is to wrap up the filtered
          -- absolute paths in the 'Right' constructor.
          Right <$> paths''


-- AUXILIAR FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This function takes the 'PathInfo' value corresponding to
-- a proof file, and returns a (monadic) list of all the
-- base names from all Discipline Directories with dependency
-- access on that proof file.
allowedDiscDirs
  :: Monad m
  => PathInfo
  -- ^
  -- The 'PathInfo' value corresponding to a proof file.
  -> CTR.ReaderT ProofSystemConfig m [String]
  -- ^
  -- A (monadic) list of 'String's corresponding to the base
  -- names of all Discipline Directories which have dependency
  -- access on the file whose corresponding 'PathInfo' was
  -- provided as first argument. The project files of these
  -- Discipline Directories are the ones which should have
  -- a declaration for the given file.
  --
  -- The list of all known Discipline Directories is set by
  -- the 'disciplineDirectories' field in the environment.
allowedDiscDirs info =
  do
    -- We ask for the list of all known Discipline Directories.
    discDirs <- CTR.asks disciplineDirectories

    -- We get the Discipline Directory our current file is
    -- housed in.
    let housingDiscDir = housingDiscDirBaseName info

    -- Then, we filter the list of all Discipline Directories,
    -- leaving in only those which have dependency permission 
    -- on the one housing our current file.
    let
      filteredDiscDirs =
        filter
          (elem housingDiscDir . accessibleDiscDirs)
          discDirs

    -- And, finally we return the names of those Discipline
    -- Directories (and also prepend the name of the housing
    -- Discipline Directory, since self-dependency is always
    -- implied by default).
    return $
      housingDiscDir : map discDirBaseName filteredDiscDirs

-- |
-- This function checks if the given 'PathInfo' argument's
-- declarative path appears on the project file of the
-- Discipline Directory whose base name is given as argument.
isDeclaredIn
  :: PathInfo
  -- ^
  -- The 'PathInfo' value corresponding to the file we need
  -- to check for declaration in the Discipline Directory.
  -> String
  -- ^
  -- The __base name__ of the Discipline Directory whose
  -- project file we need to check.
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      (Either UIOE.IOException Bool)
  -- ^
  -- A (monadic) sum-type, since the internal functions for
  -- checking the file can throw exceptions. These exceptions
  -- are caught and reported back within a (monadic) 'Left'
  -- constructor. This is the case, for example, if the
  -- project file could not be read due to not existing, or
  -- insufficient permissions, for example.
  --
  -- If the file is successfully readen (no exception thrown),
  -- then the result is a (monadic) @Right True@, if the
  -- declarative path is found in the corresponding file, or
  -- a @Right False@, if it is not.
  --
  -- The environment type 'ProofSystemConfig' provides
  -- information such as the base name of the project files.
isDeclaredIn info discDir =
  let
    -- Notice how the only information we need about the
    -- 'PathInfo' value is its declarative path, which is the
    -- thing we will be looking for inside the project file.
    decPath = declarativePath info
  in
    do
      -- First, we get the canonical path of the root of the
      -- Proof System from the environment.
      root <- CTR.asks psRootPath

      -- We also ask the environment for the base name of
      -- the project files.
      projFileBaseName <- CTR.asks projectFileName

      -- We can then combine the absolute path of the root,
      -- the base name of the Discipline Directory, and the
      -- base name of the project file into the absolute path
      -- of the project file,
      let
        projFilePath :: FilePath
        projFilePath =
          root SF.</> discDir SF.</> projFileBaseName

      -- We can now check that file.
      -- We wrap everything in a 'handleIO' (just a 'catchIO'
      -- with the handler and action arguments flipped), since,
      -- if reading the file generates some kind of exception
      -- (such as the ones for inexistent file, or missing 
      -- permissions) we will just return the exception.
      CTC.lift $
        UIOE.handleIO
          (return . Left)
          ( Right <$>
              UIOE.bracket
                (SIO.openFile projFilePath SIO.ReadMode)
                SIO.hClose
                ( \hdl -> do
                  x <-
                    (decPath `elem`) . lines
                      <$> SIO.hGetContents hdl
                  UIOE.evaluateDeep x
                  -- return $! x
                )
          )

-- |
-- This function checks if the file associated with the given
-- 'PathInfo' value is declared within the project file of
-- the Discipline Directory with the given base name, and, if 
-- not, tries to add it in the end.
--
-- Notice that this function __does not check__ if the
-- Discipline Directory whose project file we are trying to
-- append to really has dependency access on our file.
--
-- For this reason, this function is __not__ exported.
-- Instead, use the 'addDeclarations' function, which does not
-- allow you to choose a Discipline Directory, but and
-- tries to add in all that depend on it.
addDeclarationTo
  :: PathInfo
  -- ^
  -- The 'PathInfo' value associated with the file we want to
  -- add the declaration.
  -> String
  -- ^
  -- The base name of the Discipline Directory whose project
  -- file should be appened, if it does not already declare
  -- the file whose 'PathInfo' is given as first argument.
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      (Either UIOE.IOException Bool)
  -- ^
  -- A (monadic) sum type. 
  --
  -- If the result is a (monadic) 'Left',
  -- then some exception was thrown, either in reading the
  -- project file or writting it, maybe for reasons like
  -- insufficient permissions or inexistent file. This 
  -- exception is caught and reported back.
  --
  -- If the result is a (monadic) 'Right True', then the
  -- declaration was not already in the file, and was
  -- properly added by this function call. Finally, a
  -- (monadic) 'Right False' means that the declaration was
  -- already in the file, and so it did not need to be edited.
addDeclarationTo info discDir =
  let
    -- Notice how the only information we need about the
    -- 'PathInfo' value is its declarative path, which is the
    -- thing we will be looking for inside the project file.
    decPath = declarativePath info
  in
    do
      -- First, we get the canonical path of the root of the
      -- Proof System from the environment.
      root <- CTR.asks psRootPath

      -- We also ask the environment for the base name of
      -- the project files.
      projFileBaseName <- CTR.asks projectFileName

      -- We can then combine the absolute path of the root,
      -- the base name of the Discipline Directory, and the
      -- base name of the project file into the absolute path
      -- of the project file,
      let
        projFilePath :: FilePath
        projFilePath =
          root SF.</> discDir SF.</> projFileBaseName

      -- We can now check that file.
      -- We wrap everything in a 'handleIO' (just a 'catchIO'
      -- with the handler and action arguments flipped), since,
      -- if reading the file generates some kind of exception
      -- (such as the ones for inexistent file, or missing 
      -- permissions) we will just return the exception.
      CTC.lift $
        UIOE.handleIO
          (return . Left)
          ( fmap Right $ do
              UIOE.bracket
                (SIO.openFile projFilePath SIO.ReadMode)
                SIO.hClose
                ( \hdl -> do
                  -- This checks if the project file already
                  -- contains the declaration to our file.
                  isDecl <-
                    (decPath `elem`) . lines
                    <$> SIO.hGetContents hdl

                  if isDecl
                  then
                    -- If it does, we leave returning a 
                    -- 'False', which indicates that our 
                    -- function call did not need to edit 
                    -- the project file at all.
                    return False
                  else
                    do
                      -- If it does not, we then try to append
                      -- the file with a new line containing 
                      -- our file's declarative path, and, if
                      -- successful we return a 'True', since
                      -- our function call did modify 
                      -- the project file.
                      appendFile projFilePath
                        $ decPath ++ "\n"
                      return True
                )
          )

-- |
-- This function checks if the file associated with the given
-- 'PathInfo' value is declared within the project file of
-- the Discipline directory with the given base name, and, if
-- it is, removes it from that file.
--
-- Notice that this function __dos not check__ if the
-- Discipline Directory whose project file we are trying to
-- edit really has dependency access on our file, nor tries
-- to remove the declaration consistently across all the
-- Discipline Directories which have dependency acess on the
-- argument file.
--
-- For these reasons, this function is __not__ exported.
-- Instead, use the 'removeDeclarations' function, which does
-- not allow you to choose a specific Discipline Directory,
-- but instead tries to remove the declaration from all
-- Discipline Directories (regardless of their dependency
-- permission on the file or not, as a countermeasure to
-- manually editing the project files to break dependency
-- access permission.).
removeDeclarationFrom
  :: PathInfo
  -- ^
  -- The 'PathInfo' value associated with the file whose
  -- declaration we want to remove.
  -> String
  -- ^
  -- The base name of the Discipline Directory whose project
  -- file should be edited to remove any declaration of the
  -- declarative path of the 'PathInfo' argument, if there
  -- is any.
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      (Either UIOE.IOException Bool)
  -- ^
  -- A (monadic) sum type.
  --
  -- If the result is a (monadic) 'Left', then some
  -- exception was thrown, either in reading the
  -- project file, writing a temporary file where we stream
  -- the contents of the project file, or replacing the
  -- initial project file with the temporary file, maybe
  -- for reasons like insufficient permissions or inexistent
  -- files. This exception is caught, encapsulated in the
  -- 'Left' constructor, and reported back.
  --
  -- If the result is a (monadic) 'Right True', then the
  -- some declaration was found in the original project
  -- file, and was removed from it. If the result is a
  -- (monadic) 'Right False', then the project file did not
  -- have a declaration in the first place, and did not need
  -- to be replaced.
removeDeclarationFrom info discDir =
  -- The only important field of the 'PathInfo' value
  -- is the declarative path, which is what we must
  -- compare within the files.
  --
  -- The function 'removeLine' returns an 'Either' value,
  -- containing either a 'Left'-wrapped 'IOException' or
  -- a 'Right'-wrapped list of predicate matches.
  --
  -- However, our predicate is just equality, those matches
  -- are only copies of the declarative path. What matters
  -- to us, is wheter some copy was found or not.
  let
    onRight :: Either a [b] -> Either a Bool
    onRight (Right l) = Right $ not $ null l
    onRight (Left x)  = Left x
  in
    onRight <$> removeLine (== declarativePath info) discDir

-- |
-- This function attempts to remove all lines matching a
-- given predicate from a project file.
--
-- This function is used internally by 'removeDeclarations'.
--
-- This function is exposed in order to provide a way to
-- remove the declaration from project files from files which
-- may have already been deleted, so that we can no longer
-- build a 'PathInfo' value from them using 'pathInfo', and
-- apply 'removeDeclarations'.
--
-- However, if the file still exists, you should instead
-- use the 'removeDeclarations' function with its associated
-- 'PathInfo' value.
--
-- To do this, this function starts reading the project file,
-- creates a new temporary file in the systems temporary file
-- directory, and copies, line by line, the original project
-- file into this temporary file, except for the lines which
-- match the deletion predicate. After the entire project 
-- file has been read, it is replaced by the old one, if
-- any modifications were needed.
--
-- This function returns a (monadic) 'Either' value, which
-- can either be:   
--
-- * A 'Left'-wrapped 'UIOE.IOException', for any exception
--   thrown (and caught) by the any step of the process.  
-- * A 'Right'-wrapped list of all the predicate matches
--   found in the file.
removeLine
  :: (String -> Bool)
  -- ^ 
  -- The predicate which, should the line match, we will
  -- not copy it to the temporary file, and so, in the end,
  -- it will be deleted from the project file.
  -> String
  -- ^
  -- The name of the Discipline Directory whose project
  -- file we must edit.
  -> CTR.ReaderT
      ProofSystemConfig
      IO
      (Either UIOE.IOException [String])
removeLine predicate discDir =
  let
    -- We will define an auxiliar function
    -- which will test each line of the original
    -- file to see if it satisfies the predicate 'pred'.
    -- Using the handle of the file where we want to write
    -- (so that file should be in 'Read' or 'ReadWrite' mode),
    -- we will:
    -- * Check if the current satisfies the predicate,
    --   * If it does, we do not write that line, but return
    --     that line prepended to the result of this
    --     function on the rest of the lines.
    --   * If it does not, we write that line and call the
    --     function on the rest of the lines.
    writeTest
      :: SIO.Handle
      -> [String] -- The list to be read.
      -> IO [String] -- The (monadic) list of matches.
    -- For an empty file, do nothing, and return an
    -- empty list.
    writeTest _ [] = return []
    -- We have to break this case as well to
    -- avoid writting more and more empty lines
    -- in the end of the file every time we
    -- process it.
    writeTest handle [l] =
      if predicate l
      then
        return [l]
      else
        -- Notice we are using 'hPutStr', and not
        -- 'hPutStrLn', because the later would 
        -- add an empty line at the end of the
        -- file. The next time this file would
        -- be processed, 'l' would be @""@
        -- (empty string), and another empty
        -- line would be added, and so on.
        SIO.hPutStr handle l >> return []
    -- The remaning case, for lists with more
    -- than 1 element. Here, 'l' is not the
    -- last line of the file.
    writeTest handle (l : ls) =
      if predicate l
      then
        -- Here, we do not write the current line
        -- to the file. Instead, we monadically prepend
        -- the line to the top of the returning list, and
        -- call the function on the rest.
        (:) l <$> writeTest handle ls
      else
        -- Here, the current line does not satisfy the
        -- deletion predicate. So, we must write it on
        -- the temporary file, and call the function
        -- on the rest of the provided list.
        SIO.hPutStrLn handle l >> writeTest handle ls
  in
    do
      -- Gets the absolute path of the root of the Proof
      -- System. This is used to construct the path of the
      -- project file.
      root <- CTR.asks psRootPath

      -- Also asks for the general base name of project files.
      projFileBaseName <- CTR.asks projectFileName

      let
        projFilePath :: FilePath
        projFilePath =
          root SF.</> discDir SF.</> projFileBaseName

      -- We will wrap the entire operation in a 'handleIO'
      -- (simply a 'catchIO' where the exception handling
      -- function comes before the operation, for readability)
      -- since, if at any point, an 'UIOE.IOException' is
      -- thown, we want to just be able to report back the
      -- encapsulated exception. Since we need to also return
      -- a 'FilePath', corresponding to the path of the
      -- temporary file, in the failure case we also return
      -- an empty 'String'.
      --
      -- At this point, we will have constructed a
      -- temporary file with all the lines in the original
      -- project files, except the ones identical to the
      -- 'line' argument.
      --
      -- So, the 'writeResult' value, can be one of two things:
      -- - A 'Left'-wrapped 'IOException'. Here, we will
      --   just report this further, since any number of
      --   things could have happened (no temporary directory,
      --   no permissions, unavailabe resources, insufficient
      --   memory, etc...).
      -- - A 'Right'-wrapped '[String]'. This list contains
      --   all the lines that were deleted. If this list
      --   is empty, however, then no matches were found,
      --   and so the reconstructed temporary file
      --   is the same as the original file, meaning we
      --   do not have to replace the original file, only
      --   delete the temporary one to free resources.
      (tempFilePath, writeResult) <-
        CTC.lift $ UIOE.handleIO
          (\e -> return ("", Left e))
          (
            -- Within the 'IO' monad.
            do
              -- We try to get the temporary file directory,
              -- where we will be creating the temporary file.
              tempDir <- UIOD.getTemporaryDirectory

              -- Here, we are acquiring a resource that
              -- needs to be closed regardless in the end:
              -- the temporary file. So, we must wrap
              -- everything in a 'UIOE.bracket'.
              -- The structure of the 'bracket' is:
              -- > bracket (ACQUIRE) (RELEASE) (COMPUTATION)
              -- and, if exceptions are thrown, they are
              -- caught, @RELEASE@ is called, and then
              -- they are rethown.
              --
              -- Ultimately, if no exceptions are thrown,
              -- we will end up with an 'IO [String]', which
              -- is the list of deleted lines.
              UIOE.bracket
                ( -- Acquire resource.
                  -- Produces an 'IO (FilePath, Handle)',
                  -- with 'FilePath' being the path of the
                  -- temporary file, and 'Handle' being
                  -- a 'ReadWrite' handle to that file.
                  --
                  -- The temporary file's base name is
                  -- > projectFileName ++ "XXX"
                  -- where "XXX" is a random number.
                  SIO.openTempFile tempDir projFileBaseName
                )
                ( -- Release resource.
                  -- Here we will simply close the handle.
                  -- We will test later if the writting
                  -- was ultimately successful, and then
                  -- see if we need to replace files.
                  \(_, tempHandle) ->
                  UIOIO.hClose tempHandle
                )
                ( -- Computation.
                  \(tempPath, tempHandle) ->
                  -- We read the file. This is read lazily.
                  -- We also break it into lines, since
                  -- the lines must contain the declarative
                  -- paths, one per line.
                  --
                  -- Then, we process the file using our
                  -- auxiliar function 'writeTest'.
                  -- We get its end type (an 'IO Bool'),
                  -- which signs whether or not we have
                  -- found the declarative path.
                  --
                  -- We also pass back the path of this
                  -- temporary file, so that we can
                  -- either delete it or use it to
                  -- replace the original file.
                  (,) tempPath . Right <$>
                  UIOE.bracket
                    (SIO.openFile projFilePath SIO.ReadMode)
                    SIO.hClose
                    ( \hdl -> do
                    cnt <- lines <$> SIO.hGetContents hdl
                    writeTest tempHandle cnt
                    -- TEST: Maybe $! cnt?
                    )
                )
          )

      -- We look at the result of our attempted writting
      -- the new project file.
      case writeResult of
        Left _ ->
          -- Here, an exception was caught.
          -- This exception might have happened even
          -- before the temporary file was created, so
          -- we just pass it on and finish this function.
          return writeResult
        Right deletions ->
          -- Here, we have a list containing the deleted
          -- lines. If none were deleted, we just have to
          -- delete the temporary file. If there were
          -- deletions however, we must replace the files.
          --
          -- Both operations can cause exceptions to be
          -- thrown, so, again, we wrap everything in a
          -- 'handleIO', and report back the exception.
          CTC.lift $
            UIOE.handleIO
            (return . Left)
            (
              (
                case deletions of
                  [] ->
                    -- Here, we just delete the 
                    -- temporary file.
                    UIOD.removeFile tempFilePath
                    :: IO ()
                  _ ->
                    -- Here, we must __replace__ the files.
                    UIOD.renameFile tempFilePath projFilePath
                    :: IO ()
              )
              -- Either way, we just return back whatever the
              -- list of matches.
              >> return (Right deletions)
            )

