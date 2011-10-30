-- | This is a very simple project containing a few code style checks for use
-- in git hooks for the Snap Framework. Hopefully we'll get some more
-- sophisticated checks and automatic fixes implemented eventually.
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import System.Exit (ExitCode (..), exitWith)

import System.Console.CmdArgs

import HStyle

-- | CmdArgs-enabled data-type
data HStyle = HStyle
    { fix   :: Bool
    , quiet :: Bool
    , files :: [FilePath]
    } deriving (Show, Data, Typeable)

-- | CmdArgs configuration
hstyle :: HStyle
hstyle = HStyle
    { fix   = def &= help "Automatically fix (some) problems"
    , quiet = def &= help "Print less output"
    , files = def &= args 
    }

-- | Simple main that takes one command-line parameter of "check" or "fix" and
-- a list of files to be checked.
main :: IO ()
main = do
    config <- cmdArgs hstyle
    ok <- and <$> mapM (checkStyle $ quiet config) (files config)
    exitWith $ if ok then ExitSuccess else ExitFailure 1
