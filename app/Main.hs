-- |
-- Main module.
module Main where

import CommandLineArguments
  ( CommandLineOptions
  , cloParser
  , modeExecCaller
  )

import Control.Applicative
  ( (<**>)
  )

import qualified Options.Applicative as OA
  ( ParserInfo
  , execParser
  , fullDesc
  , header
  , helper
  , info
  , progDesc
  )

-- |
-- Main function.
main :: IO ()
main =
  let
    parserInfo :: OA.ParserInfo CommandLineOptions
    parserInfo =
      OA.info (cloParser <**> OA.helper) $
        OA.fullDesc
        <> OA.progDesc mainProgDesc
        <> OA.header mainHeader

    -- Program description.
    mainProgDesc :: String
    mainProgDesc = 
      "Pruvarilo is the executable used for automatically\
      \ managing the project files and constraints of the\
      \ Pruvaro proof system."

    -- Program header.
    mainHeader :: String
    mainHeader = 
      "Pruvarilo - Automatic management of the Pruvaro \
      \ proof system."
  in do
    clo <- OA.execParser parserInfo
    modeExecCaller clo

