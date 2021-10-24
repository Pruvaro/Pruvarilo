-- |
-- The goal of the "PathInfo" module is to create a tool which
-- is able to take a path within the system and extract the
-- relevant information about the item in the path, creating
-- a general data structure which is easier for the rest of
-- the program to parse and deal with, with the information
-- readily-available.
--
-- For example, if one gives the path of proof file, the
-- generated data structure should contain fields like:
-- The canonicized path of the file, its associated
-- Discipline Directory, whether its base name is valid
-- (according to the environment's base name validity
-- predicate), and so on.
--
-- For directory paths, the returned structure should be
-- a list of the structures corresponding to contained
-- (directly or indirectly) proof files.
module PathInfo
  ( -- * 'PathInfo' data type
    PathInfo
    -- ** 'PathInfo' getter functions
    -- |
    -- Associated with each field of the 'PathInfo' data
    -- type, we have a getter function. This way, the
    -- fields of 'PathInfo' can still be accessed, but
    -- no 'PathInfo' can be created using means other than
    -- the 'pathInfo' and 'pathInfos' functions, which 
    -- attempt to make those values in a safe way.
  , canonicalPath
  , housingDiscDirBaseName
  , declarativePath
  , buildName
  , validName
  , noNameConflict

    -- * Building @PathInfo@ values
  , pathInfo
  , pathInfos

    -- * Auxiliar functions
  , dependentDiscDirs
  , relativePathElem
  , declarativeToAbsPath
  ) where


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Config
  ( ProofSystemConfig
  , DisciplineDirectory
    ( accessibleDiscDirs
    , discDirBaseName
    )
  , psRootPath
  , disciplineDirectories
  , proofFileExtensions
  , nameValidityPredicate
  , declarativeToBuild
  , hiddenPaths
  , hiddenBaseNames
  )

import qualified Control.Monad as CM
  ( guard
  )

import qualified Control.Monad.IO.Unlift as CMIOU
  ( MonadUnliftIO
  )

import qualified Control.Monad.Trans.Class as CTC
  ( lift
  )

import qualified Control.Monad.Trans.Maybe as CTM
  ( MaybeT
    ( MaybeT
    , runMaybeT
    )
  )

import qualified Control.Monad.Trans.Reader as CTR
  ( ReaderT
    ( runReaderT
    )
  , asks
  )

import qualified Data.Char as DC
  ( toLower
  )

import qualified Data.List as DL
  ( stripPrefix
  )

import qualified Safe as S
  ( tailSafe
  )

import qualified System.FilePath as SF
  ( (</>)
  , (<.>)
  , dropExtension
  , joinPath
  , splitDirectories
  , takeBaseName
  , takeDirectory
  , takeExtension
  , takeFileName
  )

import qualified UnliftIO.Directory as UIOD
  ( canonicalizePath
  , listDirectory
  )

import qualified UnliftIO.Exception as UIOE
  ( IOException
  , catchIO
  , handleIO
  )

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- DATA TYPES
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- The data structure which contains the relevant information,
-- in readily-available format, of a proof file.
--
-- Notice that, by design, the constructors of this type
-- __is not exported__. Instead, for each field, an
-- associated getter function is exported instead.
--
-- The idea is that, the only way of creating a structure of 
-- this type is by using the functions 'pathInfo' and 
-- 'pathInfos', which try to build 'PathInfo' values from 
-- actual proof files, and do so making sure that all 
-- fields follow the correct structure.
data PathInfo =
  PathInfo
  { _canonicalPath :: FilePath
  , _housingDiscDirBaseName :: String
  , _declarativePath :: FilePath
  , _buildName :: FilePath
  , _validName :: Bool
  , _noNameConflict :: Bool
  } deriving (Show, Read, Eq)

-- |
-- The canonical version of the file's path, as per the
-- 'UnliftIO.Directory.canonicalizePath' function (which
-- is itself a lift of the 
-- 'System.Directory.canonicalizePath' function). It is
-- described as:
-- 
-- * Make a path absolute, normalize the path, and 
--   remove as many indirections from it as possible. 
--   Any trailing path separators are discarded via 
--   @dropTrailingPathSeparator@. Additionally, on Windows
--   the letter case of the path is canonicalized.
canonicalPath :: PathInfo -> FilePath
canonicalPath = _canonicalPath

-- |
-- The base name of the Discipline Directory this file
-- is housed in.
housingDiscDirBaseName :: PathInfo -> String
housingDiscDirBaseName = _housingDiscDirBaseName

-- |
-- The \"canonical declarative path\" of the file. In order
-- to understand this field, consider a structure like the
-- one below (directories end with a trailing @/@ for
-- clarity, and indentation indicates containance):  
--
-- > Root/
-- >   Mathematics/
-- >     CoqProject
-- >     M1/
-- >       Proof.v
-- >     M2/
-- >   Physics
-- >     CoqProject
-- >     P1/
-- >     P2/
-- 
-- If one looks at the file @Proof.v@, it would have a
-- canonical path similar to:  
--
-- > /path/to/root/Physics/M1/Proof.v
-- 
-- However, in order for this file to be properly compiled
-- and visible to every other proof in every Discipline
-- Directory which can access it, this file needs to be
-- declared within the \"CoqProject\" files of every
-- Discipline Directory which has dependency access on the
-- \"Mathematics\" Discipline Directory (the Discipline
-- Directory where @Proof.v@ is housed).
--
-- Both the \"Mathematics\" and the \"Physics\" Discipline
-- Directories have dependency access on the \"Mathematics\"
-- Discipline Directory, so, the declaration for the
-- @Proof.v@ file needs to be present on both  
--
-- > Mathematics/CoqProject
-- as well as  
--
-- > Physics/CoqProject
--
-- One approach would be to just put the entire canonical
-- path into those files. However, this leaves
-- user-specific information (the in-disk absolute path of
-- the root) within the the Proof System, which leads to
-- problems in the syncronization of those files. A better
-- approach uses relative paths. So, in the CoqProject
-- files, the "Proof.v" file would instead be declared as  
--
-- > "../Mathematics/M1/Proof.v"
-- thus avoiding leaking the absolute path of the root.
-- Notice that the declarative path uses @/@ as the path
-- separator _regardless of the system__. This is so that
-- the project files can be read and written equally 
-- across all systems.
--
-- In order to avoid issues with comparing relative paths
-- and links, this declarative path is derived from the
-- resolved, canonical path. 
declarativePath :: PathInfo -> FilePath
declarativePath = _declarativePath

-- | 
-- The 'buildName' of a proof file is the name of the
-- recipe used for compiling that proof file specifically.
--
-- By default, considering the recipe generated
-- by the @coq_makefile@ utility applied on the project
-- file (which contains a list of declarative paths), this
-- field is the path of the object file (i.e. the @.vo@
-- file) associated with the proof file itself. In
-- practical terms, it is the @declarativePath@ itself,
-- however, with @.vo@ as the file format instead of @.v@.
--
-- More specifically, the build name associated with a
-- file is one of the arguments in the command to build
-- that proof file. This command, set as the 
-- 'ProofSystemConfig' environment's 'compilationCommand',
-- is, by default:  
--
-- > make --directory=%0 %1 
--
-- where, according to the syntax of the 'subst' function
-- in the "SubstitutionParser" module,  @%0@ gets subsituted 
-- with the absolute path of the Discipline
-- Directory (obtained by prepending the 'psRootPath'
-- field of the configuration environment to the
-- 'housingDiscDirBaseName' field of the 'PathInfo')
-- and @%1@ gets subsituted with  the 'buildName' field of 
-- the same 'PathInfo'.
--
-- As described in the "Config" module, the 
-- 'compilationCommand' field of the 'ProofSystemConfig'
-- environment can be overwritten using the configuration
-- file. Regardless, be it a user-defined or the default
-- command, that command has to build the object file
-- associated with the proof file that originated the
-- 'PathInfo' in the first place.
buildName :: PathInfo -> FilePath
buildName = _buildName

-- |
-- A boolean flag which tests if the base name of this
-- file is valid, according to the name validity testing
-- predicate.
--
-- Notice that a failure to pass that test does not
-- result in and automatic rejection of the file property
-- parsing, but is flagged in order to, for example, warn the
-- user and require explicit input to proceed.
validName :: PathInfo -> Bool
validName = _validName

-- |
-- A boolean flag which tests if the name of the file
-- conflicts with the name of any other file in the
-- same directory __in a case-insentive test__.
-- 
-- As with the name validity, this does not reject the
-- creation of the 'PathInfo' value, but can be used
-- for example to require explicit user input to proceed,
-- as it will cause cross-system conflict.
noNameConflict :: PathInfo -> Bool
noNameConflict = _noNameConflict

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- MAIN FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This function provides the a way of (possibly)
-- constructing a 'PathInfo' structure from a path.
--
-- The word "possibly" is used for several reasons:  
--
--   * The path might fail to canonicalize through the
--     'UnliftIO.Directory.canonicalizePath' function, for
--     several reasons. Those cause 'UIOD.canonicalizePath'
--     to throw an exception. Those exceptions are caught,
--     and result in the 'PathInfo' structure to be refused
--     (i.e., it becomes a monadic 'Nothing' value).  
--     According to documentation, some possible cases for 
--     failure are  
--
--       *  'HardwareFault' A physical I/O error has 
--          occurred. [EIO]  
--       *  'isDoesNotExistError' There is no path referring 
--          to the working directory. [EPERM, ENOENT, 
--          ESTALE...]  
--       *  'isPermissionError' The process has insufficient 
--          privileges to perform the operation. [EACCES]  
--       *  'isFullError' Insufficient resources are 
--          available to perform the operation.  
--       *  'UnsupportedOperation' The operating system has no 
--          notion of current working directory.  
--
--
--   * The path in question is not the path of a proof file,
--     as set by the result of 'System.FilePath.takeExtension'
--     on the canonical path, which must be an element of the
--     configuration environment's 'proofFileExtensions' field.
--     As before, if it is not, then the construction of the
--     'PathInfo' data value is refused (becomes a monadic
--     equivalent to 'Nothing').  
--
--   * The path is not a descendant (not necessarily directly)
--     of any Discipline Directories (as set by the 
--     configuration environment's 'disciplineDirectories'
--     field). The comparison is done with the canonicalized
--     path, produced by success on the first step.  
--
-- Failure in any of the above steps results in a monadic
-- 'Nothing' value.
pathInfo
  :: CMIOU.MonadUnliftIO m
  => FilePath
  -- ^
  -- The initial file path which will be analyzed, and
  -- possibly be extracted into the relevant 'PathInfo'
  -- data structure.
  -> CTR.ReaderT ProofSystemConfig m (Maybe PathInfo)
  -- ^
  -- A reader structure expecting a configuration environment,
  -- and returning a possible (monadic) 'PathInfo' value.
  -- Notice that this value is isomorphic to  
  --
  -- > ProofSystemConfig -> m (Maybe PathInfo)
  -- where @m@ is any 'MonadUnliftIO' type (generally 
  -- 'IO' itself).
pathInfo path =
  CTM.runMaybeT $
  -- By wrapping everything in a 'runMaybeT', now we need
  -- to produce a value of type
  -- > MaybeT (ReaderT ProofSystemConfig m) PathInfo
    do
    -- First, we will try to get a canonical version of the
    -- provided path. We also need to get the same monadic
    -- wrapper, so we are aiming to get something of type
    -- > MaybeT (ReaderT ProofSystemConfig m) FilePath
    -- But notice that the monad here is
    -- > MaybeT (ReaderT ProofSystemConfig m) FilePath
    -- So, by using '<-', we can unwrap a 'FilePath' value,
    -- all the while shortcircuiting if we fail to find one,
    -- as per the 'MaybeT' monadic behaviour.

    -- This first one is a bit tricky, so we will look at
    -- it by parts.
    -- The final value 'catchIO' has to be a type
    -- > MonadUnliftIO m => m a
    -- However, 'MonadUnliftIO', differently from 'MonadIO',
    -- is not inherited throught 'MaybeT'. So, we must push
    -- the 'Maybe' back into the 'a' type, and fix it to
    -- a 'MaybeT' at the top of the stack with the
    -- 'MaybeT' constructor.
    canonPath <-
      CTM.MaybeT $ UIOE.catchIO
        ( Just <$> (UIOD.canonicalizePath path) )
        ( \_ -> return Nothing )

    -- We can now proceed, assuming that the canonical path
    -- has been correctly generated. If not, and an exception
    -- was thrown, things will just short-circuit.
    --
    -- The things to get now from the environment are the
    -- canonical path of the root,
    rootPath <- CTC.lift $ CTR.asks psRootPath
    
    -- And the base names of the Discipline Directories,
    discDirNames <-
      CTC.lift $
        CTR.asks
          ( \env -> 
            map discDirBaseName (disciplineDirectories env)
          )

    -- And the file extensions for Proof Files,
    proofExtensions <- CTC.lift $ CTR.asks proofFileExtensions
      
    -- We use the environment's 'declarativeToBuild'
    -- to provide us the build name from the declarative
    -- path. So, here, we ask for that function.
    makeBuildName <- CTC.lift $ CTR.asks declarativeToBuild

    -- And finally, the name validity predicates
    isNameValid <- CTC.lift $ CTR.asks nameValidityPredicate

    -- Now we do the remaning tests which might discard the
    -- 'PathInfo' creation.
    -- 
    -- Starting with the "is it a Proof File?" test.
    CTM.MaybeT $ return $ CM.guard $
      (SF.takeExtension canonPath) `elem` proofExtensions

    -- Now, we test if the current path descends from some
    -- Discipline Directory.
    let
      -- To do it, it is convenient to decompose the canonical
      -- paths of the root and of the argument in terms of
      -- its path elements, that is,
      -- > path/to/root
      -- becomes
      -- > ["path", "to", "root"].
      rootPathElements :: [String]
      rootPathElements = SF.splitDirectories rootPath

      -- 
      argPathElements :: [String]
      argPathElements = SF.splitDirectories canonPath

      -- We now define an auxiliar function which takes
      -- a directory base name and tests if the argument path
      -- is within a directory of that name, inside the path
      -- of the root. For example, if the canonical path of
      -- the root is
      -- > path/to/root
      -- and the given base name is
      -- > "Discdir"
      -- we will test if the argument path is a subpath of
      -- > path/to/root/Discdir
      -- If it is, we return
      -- > Just ("Discdir", remaining path elements)
      -- and @Nothing@ otherwise, where 
      -- @remaining path elements@ are the remaining path
      -- elements of the argument path, after you remove
      -- the root's path elements and the "Discdir" itself.
      -- That is, for this to be successful, the argument path
      -- will have to be something like this
      -- > path/to/root/Discdir/some/other/stuff
      -- and the return will be
      -- > Just ("Discdir", ["some", "other", "stuff"])
      pathSuffix 
        :: String
        -> Maybe (String, [String])
      pathSuffix baseName =
        DL.stripPrefix 
          (rootPathElements ++ [baseName]) 
          argPathElements
        >>= Just . (\remElem -> (baseName, remElem))
        
      -- This is another auxiliar function which returns,
      -- if any exists, the first computation of the
      -- first element resulting in a 'Just something'.
      -- It is similar to 'find', which returns the first
      -- element which tests 'True' according to some
      -- predicate, but here, instead of a predicate, we
      -- use a 'a -> Just b' function.
      --
      -- This is useful for when the testing would require
      -- producing information which will be later needed,
      -- so it would be pointless to recalculate it.
      firstJust
        :: [a]
        -> (a -> Maybe b)
        -> Maybe b
      firstJust [] _ = Nothing
      firstJust (x:xs) f =
        case (f x) of
          Nothing -> firstJust xs f
          Just y  -> Just y

    -- We can now combine everything here.
    -- Using the short-circuit behavior of the 'Maybe' monad,
    -- we can act as if we successfully find the parent
    -- Discipline Directory and the remaining path elements.
    (parentDiscDir, remainingPathElems) <-
      CTM.MaybeT $ return $ firstJust discDirNames pathSuffix

    -- The remaining tests are now warning-only tests. They
    -- do not prevent the generation of the 'PathInfo' values,
    -- but instead signal through boolean flags that not
    -- everything is okay considering the main project
    -- guidelines, but might be fine for local projects.
    let
      -- These tests require looking at the name validity
      -- of the base name, and case-insensitive conflicts
      -- with other elements in the same parent directory.
      -- So, for ease, we isolate the argument path base
      -- name and parent directory.
      --
      -- Since we are using the canonicalized formats to
      -- do this, which do not have trailing separators,
      -- this should work as expected.
      argBaseName :: String
      argBaseName = SF.takeFileName canonPath
      
      -- 
      argParentDir :: FilePath
      argParentDir = SF.takeDirectory canonPath

    -- We can then get the list of elements of the
    -- parent directory. However, if listing fails through
    -- an exception throw, we also fail the building of the
    -- 'PathInfo' value.
    parentContents <-
      CTM.MaybeT $ UIOE.catchIO
        ( Just <$> (UIOD.listDirectory argParentDir) )
        ( \_ -> return Nothing )

    -- Finally, we can check if there is more than one 
    -- lowercase match between the elements of the 
    -- parent directory and the argument base name (since
    -- the argument is an element of the parent directory,
    -- 1 conflict will happen, but ideally, it is the 
    -- only one).
    let
      noConflict :: Bool
      noConflict =
        ( length $ filter
            ( \s -> 
              (map DC.toLower s) 
              == (map DC.toLower argBaseName)
            )
            parentContents
        ) < 2
      decPath :: FilePath
      decPath =
        foldl (\x y -> x ++ "/" ++ y) ".." $ 
          parentDiscDir : remainingPathElems 
    
    -- We can then collect everything in a single 'PathInfo'
    -- value. We return it to put it into the correct
    -- monadic wrapping.
    return $
      PathInfo
      { _canonicalPath = canonPath
      , _housingDiscDirBaseName = parentDiscDir 
        -- For cross-system pattern sake, the separator
        -- between path elements is set to '/', instead
        -- of using the one defined with </>.
        --
        -- This is because the declarative path is the
        -- thing written on the project files, and compared
        -- with the project files.
      , _declarativePath = decPath
      , _buildName = makeBuildName decPath
      , _validName = isNameValid argBaseName
      , _noNameConflict = noConflict
      }

-- |
-- When pointed to a file, this function tries to produce the
-- 'PathInfo' value corresponding to that file, with all the
-- possible refusals detailed in the 'pathInfo' function.
--
-- If pointed to a directory, this function goes down the
-- directory structure recursively (but ignoring the
-- directories hidden through the 'hiddenPaths' and
-- 'hiddenBaseNames' fields of the given configuration
-- environment), collecting all the 'PathInfo' objects
-- from every file which successfully yields one along the
-- way in a list.
--
-- Internally, this function uses the 'unliftio' package's
-- function 'UnliftIO.Directory.listDirectory', which is
-- itself a wrapping of the 'directory' package's function
-- 'System.Directory.listDirectory', but for
-- 'CMIOU.MonadUnliftIO' monads instead of just 'IO'. This
-- function may throw exceptions in various cases, but any
-- thrown 'IOException' is just caught (using the 'unliftio'
-- function 'UnliftIO.Exception.catchIO') and interpreted as
-- an empty list.
pathInfos
  :: CMIOU.MonadUnliftIO m
  => FilePath
  -- ^
  -- The path of the file or directory whose information
  -- should be collected.
  -> CTR.ReaderT ProofSystemConfig m [PathInfo]
pathInfos path =
  do
    -- First, we try to canonicalize the path. This is so
    -- that hidden paths can be compared properly.
    mbCanonPath <-
      CTC.lift $
      UIOE.catchIO
        ( Just <$> (UIOD.canonicalizePath path) )
        ( \_ -> return Nothing )

    -- If we failed to canonicalize the path, we will just
    -- return an empty list and quit. Else, we can continue.
    case mbCanonPath of
      Nothing -> return []
      Just canonPath ->
        do
          -- First, we will ask the environment
          -- for the canonical path of the root of the 
          -- Proof System,
          root <- CTR.asks psRootPath
          -- And for the list of hidden paths (all given
          -- relative to the root path),
          hidePaths <- CTR.asks hiddenPaths
          -- And finally, for the list of hidden base names,
          hideBase <- CTR.asks hiddenBaseNames

          -- If our current path is a hidden path, our its
          -- base name is also hidden, we will just leave with
          -- an empty list. Otherwise, we can continue,
          if
            ( (elem canonPath $ map (root SF.</>) hidePaths)
              ||
              (elem (SF.takeBaseName canonPath) hideBase)
            )
          then
            return []
          else
            do
              -- First, we treat the path as a file, and try to 
              -- generate its 'PathInfo'.
              mbInfo <- pathInfo canonPath

              -- Then, we treat it is a directory, and try to 
              -- read its contents as a list.
              -- We will wrap everything in 'catchIO'. 
              -- This way, if it is not a directory, or if we 
              -- fail due to, for example, insufficient 
              -- permissions, we will just get back an 
              -- empty list, and the next steps are trivial.
              contents <-
                CTC.lift $ 
                  UIOE.catchIO
                    (UIOD.listDirectory path)
                    (\_ -> return [])

              -- We then map this same function on all the
              -- contents of this path (if there are none, the
              -- list will be empty, and this halts). We then
              -- concat their own lists into a big one.
              rec <-
                fmap concat $
                  mapM pathInfos $
                    map (canonPath SF.</>) contents

              -- And finally, we see if we were able to
              -- generate a 'pathInfo' for the current file or
              -- not. If yes, return it, otherwise, return the
              -- list of content recursion.
              case mbInfo of
                Nothing   -> return rec
                Just info -> return [info]


-- AUXILIAR FUNCTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- This function takes the base name of a Discipline Directory
-- and produces a (monadic) list of the base names of all
-- Discipline Directories which have dependency access on the
-- Discipline Directory with the given base name.
--
-- If the given base name corresponds to the base name of
-- a known Discipline Directory (as set by the 
-- 'ProofSystemConfig' environment), then the returning list
-- will contain the name of that directory. However, if
-- it does not correspond to the base name of a Discipline
-- Directory, this function will just return an empty list
-- (in the 'CTR.ReaderT' monadic context).
dependentDiscDirs
  :: Monad m
  => String
  -- ^
  -- The __base name__ (and not full path) of a
  -- Discipline Directory.
  -> CTR.ReaderT ProofSystemConfig m [String]
dependentDiscDirs ddBaseName =
  do
    -- Retrieves from the environment the list of known
    -- Discipline Directories.
    discDirs <- CTR.asks disciplineDirectories

    -- We then check if the given base name argument 
    -- corresponds to the base name of some Discipline
    -- Directory. If not, we just return an empty list.
    if (elem ddBaseName $ map discDirBaseName discDirs)
    then
      let
        -- Here we filter the Discipline Directory list,
        -- getting back only those which have the
        -- provided base name in its list of accessible
        -- Discipline Directories.
        accDiscDirs :: [DisciplineDirectory]
        accDiscDirs =
          filter 
            (\d -> elem ddBaseName $ accessibleDiscDirs d)
            discDirs
      in
        -- In the end, return only their base names.
        -- Since self-dependency is implied by default,
        -- the 'accDiscDirs' field does not list the
        -- actual directory, so we include it in the head
        -- of the list.
        return $ ddBaseName : map discDirBaseName accDiscDirs
    else
      return []

-- |
-- This function takes a path, tries to canonicalize it,
-- and returns the path elements (the things separated by
-- the path separator) relative to the root of the proof
-- system, __if able to__.
--
-- More precisely, the result of this function is
-- (putting aside the @'CTR.ReaderT' 'ProofSystemConfig' m@
-- monadic context) one of three things:  
--
-- * A 'Left'-wrapped 'UIOE.IOException', since the process
--   of canonicalizing a path is an 'IO' action, which
--   may throw exceptions. Any 'UIOE.IOException's thrown 
--   that way are caught, and returned back with the
--   'Left' constructor.  
-- * A 'Right'-wrapped 'Nothing'. In this case, the provided
--   path was successfully canonicalized, but it is not
--   a subpath of (or the path itself) the root of the
--   proof system, as set by the 'psRootPath' field of the
--   'ProofSystemConfig' environment.  
-- * A 'Right'-wrapped, 'Just'-wrapped, '[String]'. In this
--   case, the path was successfully canonicalized, and
--   its first path elements coincide with the first path
--   elements of the root of the proof system. Therefore,
--   this path descends from (or is itself) the path of the
--   root of the proof system (as set by the 'psRootPath'
--   field of the 'ProofSystemConfig environment).
relativePathElem
  :: CMIOU.MonadUnliftIO m
  => FilePath
  -- ^
  -- The path whose path elements, relative to the root of
  -- the proof system (as set by the 'psRootPath' field of
  -- the 'ProofSystemConfig' environment), we must attempt
  -- to get.
  -> CTR.ReaderT
      ProofSystemConfig
      m
      (Either UIOE.IOException (Maybe [String]))
relativePathElem path =
  do
    -- Get the canonical path of the root of the proof system.
    root <- CTR.asks psRootPath

    -- Decompose the root into its path elements.
    let
      rootElems :: [String]
      rootElems = SF.splitDirectories root

    -- We will wrap everything in a 'handleIO' due to
    -- the possible exceptions thrown by 'canonicalizePath'.
    UIOE.handleIO (\e -> return $ Left e) $
      Right <$> do
        -- Make the provided path canonical.
        path' <- UIOD.canonicalizePath path

        -- Break it into its elements.
        let
          path'Elems :: [String]
          path'Elems = SF.splitDirectories path'

        -- If the provided path is a subpath of the root,
        -- then 'rootElems' is a prefix of 'path'Elems'.
        -- Using 'stripPrefix', we eliminate the part of the
        -- path elements which equal the root path elements.
        -- If the root is not a prefix, then we get 'Nothing'.
        return $ DL.stripPrefix rootElems path'Elems

-- |
-- This function produces the absolute path of a file
-- given its declarative path.
--
-- Note for developers: This function has to be consistent
-- with the way 'pathInfo' generates 'PathInfo' value's
-- 'declarativePath' fields. So, any change in that function
-- requires a change here as well.
declarativeToAbsPath
  :: Monad m
  => FilePath
  -- ^
  -- The declarative path of the file, as appears on the
  -- project files of the Discipline Directories.
  -> CTR.ReaderT ProofSystemConfig m FilePath
  -- ^
  -- The absolute path (in a monadic context) associated
  -- with the file whose declarative path is provided.
  --
  -- Notice that this does not check if this file exists,
  -- allowing us to reverse the declarative path in order
  -- to check for the existence of those files.
declarativeToAbsPath decPath =
  let
    -- First we declare an auxiliar function, which
    -- will split the declarative path at '/', since
    -- we use '/' as a separator regardless of the
    -- operating system in declarative paths.
    splitOn :: Eq a => a -> [a] -> [[a]]
    splitOn c s =
      let
        -- Break the provided list on the specified
        -- element. We then get the chunk before the
        -- element, and the chunk after it.
        (bfr, aft) = break (== c) s
      in
        case aft of
          -- If the chunk after the break element is empty,
          -- then there are no more characters left in
          -- the list. This finishes the splitting.
          [] -> [bfr]
 
          -- If, instead, there are elements after,
          -- then the first element of 'aft' is the
          -- breaking element. We exclude the breaking
          -- element, add 'bfr' to the list and recurse.
          _:aft' -> bfr : splitOn c aft'

    -- We split the declarative path on the '/' character.
    -- 'tailSafe' returns '[]', instead of throwing an
    -- exception, in the '[]' case. However, the '[]'
    -- case should normally never happen.
    --
    -- We use the tail because the first element of the
    -- split list should be '..', which we want to replace
    -- by the actual canonical root path.
    pathElem :: [String]
    pathElem = S.tailSafe $ splitOn '/' decPath

    -- Now we combine the path elements, obtaining the
    -- relative (to the root of the proof system) of the
    -- proof file. We also must replace the @.vo@ with a
    -- @.v@ file extension.
    relPath :: FilePath
    relPath = 
      (SF.<.> "v") $ SF.dropExtension $ SF.joinPath pathElem
  in
    -- Append the relative path to the canonical path of
    -- the root of the proof system, as set in the 
    -- environment's 'psRootPath' field.
    (SF.</> relPath) <$> CTR.asks psRootPath
