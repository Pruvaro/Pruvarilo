-- |
-- This module is meant to define the general data types
-- used to represent the options taken by each mode of
-- operation, as well as define some common functions
-- that might be helpful across several modes.
--
-- The program consists of five operation modes:  
--
-- * @add@ mode: Used to add mentions of proof files to the
--   correct project files automatically.  
-- * @build@ mode: Used to build/compile proof files.  
-- * @doc@ mode: Used to build the documentation.  
-- * @remove@ mode: Used to remove the mentions of proof 
--   files (existing or not) from the project files.  
--
-- Each mode can be activated by calling the program as  
--
-- > pruvarilo MODE
--
-- where @MODE@ is one of the five keywords @add@, @build@,
-- @doc@, or @remove@.
module Modes
  ( -- * Modes
    Mode
    ( AddMode
    , BuildMode
    , DocMode
    , RemoveMode
    )
  
    -- * Common mode options
    -- $COMMONMODEOPTIONS
  , CommonModeOptions
    ( CommonModeOptions
    , cmo_actionLocation
    , cmo_assumeYes
    )
  , ActionLocation
    ( ActLocPath
    , ActLocDiscDir
    )

    -- * Mode-specific options
    -- $MODESPECIFICOPTIONS
  , AddModeOptions
    ( AddModeOptions
    )
  , BuildModeOptions
    ( BuildModeOptions
    )
  , DocModeOptions
    ( DocModeOptions
    )
  , RemoveModeOptions
    ( RemoveModeOptions
    )
  ) where

-- IMPORTS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- MODES
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |
-- The 'Mode' data type is a sum-type include all
-- mode-exclusive configurations under the same type.
data Mode =
    AddMode    AddModeOptions
  | BuildMode  BuildModeOptions
  | DocMode    DocModeOptions
  | RemoveMode RemoveModeOptions
  deriving (Show, Read, Eq)

-- COMMON MODE OPTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- $COMMONMODEOPTIONS
-- Some options are shared across all modes. Those options
-- are represented in the @CommonModeOptions@ data type.

-- |
-- Options shared across all modes.
data CommonModeOptions =
  CommonModeOptions
  {
    -- |
    -- Defines where the mode should act.
    --
    -- This location can either be an specfic path, or
    -- some specific Discipline Directory, determined by
    -- its base name. In general, the actions will then
    -- be applied recursively to all proof files within
    -- that path.
    cmo_actionLocation :: ActionLocation

    -- |
    -- A flag as to whether ask for any user confirmation
    -- before taking the actions.
    --
    -- For example, in the
    -- @remove@ mode, if set to 'True', this flag will
    -- already assume that the user wants all the files
    -- in the provided path to have their mentions removed
    -- from the project files, and so will not ask them
    -- for confirmation before doing so.
  , cmo_assumeYes :: Bool
  }  
  deriving Show

-- |
-- An auxiliar sum data type which encodes the location
-- of an action: either a specific path, or the base name of
-- a Discipline Directory (notice __base name__, not its
-- full path).
data ActionLocation =
    ActLocPath    FilePath
  | ActLocDiscDir String
  deriving Show

-- MODE-SPECIFIC OPTIONS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- $MODESPECIFICOPTIONS
-- Some options are specific to certain modes, not being
-- available for other operation modes. These data types
-- encode
-- 
-- /NOTE/: Currently, those options are all isomorphic to
-- @()@. The reason for defining them is to future-proof the
-- code, since it is likely that new functionalities might
-- be implemented for some modes. If those functionalities
-- are mode-specific, then they should be implemented in
-- those types.

-- |
-- Options specific for the @add@ mode.
data AddModeOptions = 
  AddModeOptions 
  deriving (Show, Read, Eq)

-- |
-- Options specific for the @build@ mode.
data BuildModeOptions = 
  BuildModeOptions
  deriving (Show, Read, Eq)

-- |
-- Options specific for the @doc@ mode.
data DocModeOptions = 
  DocModeOptions 
  deriving (Show, Read, Eq)

-- |
-- Options specific for the @remove@ mode.
data RemoveModeOptions = 
  RemoveModeOptions 
  deriving (Show, Read, Eq)
