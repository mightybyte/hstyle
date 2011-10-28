-- | This is a very simple project containing a few code style checks for use
-- in git hooks for the Snap Framework. Hopefully we'll get some more
-- sophisticated checks and automatic fixes implemented eventually.
module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

import HStyle

-- | Simple main that takes one command-line parameter of "check" or "fix" and
-- a list of files to be checked.
main :: IO ()
main = do
    args <- getArgs
    case args of
        "check" : files -> do
            ok <- and <$> mapM checkStyle files
            exitWith $ if ok then ExitSuccess else ExitFailure 1
        "fix"   : files -> mapM_ fixStyle   files
        _               -> error "Must specify 'check' or 'fix'"
