{-|

This is a very simple project containing a few code style checks for use in
git hooks for the Snap Framework.  Hopefully we'll get some more sophisticated
checks and automatic fixes implemented eventually.

-}
module Main where

import           System

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import           Data.Text (Text)


------------------------------------------------------------------------------
-- | A check is a function that processes a Text and returns a Just String if
-- there is a problem.
type Check = Text -> Maybe String


------------------------------------------------------------------------------
-- | A problem consists of a line number and a string description.
data Problem = Problem {
    probLine :: Int,
    probDescription :: String
}


------------------------------------------------------------------------------
-- Style checks
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Checks for trailing spaces on a line.
trailingSpaces :: Check
trailingSpaces line = if T.length (T.takeWhile (==' ') $ T.reverse line) == 0
    then Nothing
    else Just "Line has trailing spaces"


------------------------------------------------------------------------------
-- | Checks for tab charcters anywhere on a line.
noTabs :: Check
noTabs line = fmap (const "Line contains tab(s).") $ T.find (=='\t') line


------------------------------------------------------------------------------
-- | Ensures line length is 78 characters or less.
lineLength :: Int -> Check
lineLength n line =
    if T.length line <= n
      then Nothing
      else Just "Line too long"


------------------------------------------------------------------------------
-- | Static list of line-oriented checks to use.
lineChecks :: [Check]
lineChecks = [lineLength 78, noTabs, trailingSpaces]


------------------------------------------------------------------------------
-- Infrastructure
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Applies a list of checks to a list of lines.  Returns a list of
-- 'Problem's.
onLines :: [Check] -> [Text] -> [Problem]
onLines fs thelines = concat $ map (go 1 thelines) fs
  where
    go _ [] _ = []
    go num (l:ls) f = case f l of
        Nothing -> go (num+1) ls f
        Just er -> (Problem num er) : go (num+1) ls f


------------------------------------------------------------------------------
-- | Breaks a file into lines and runs all the checks on them.
checkContents :: Text -> [Problem]
checkContents c = lineChecks `onLines` (T.lines c)


------------------------------------------------------------------------------
-- | Runs checks on the specified file.
checkFile :: FilePath -> IO (FilePath, [Problem])
checkFile f = do
    problems <- return . checkContents =<< TI.readFile f    
    return (f,problems)


------------------------------------------------------------------------------
-- | Checks the style of a list of files, prints the problems, and exits with
-- a return value appropriate for use in a git hook.
checkStyle :: [FilePath] -> IO ()
checkStyle fs = do
    results <- mapM checkFile fs
    mapM_ printResults results
    case results of
        [] -> exitWith ExitSuccess
        _  -> exitWith (ExitFailure 1)
  where
    printResults (f,ps) = mapM_ (printRes f) ps
    printRes f p = putStrLn $ f ++ " line " ++ (show $ probLine p) ++
                              ": " ++ (probDescription p)


------------------------------------------------------------------------------
-- | Placeholder for fixing style problems.
fixStyle :: t -> a
fixStyle _ = do
    error "Not implemented"


------------------------------------------------------------------------------
-- | Simple main that takes one command-line parameter of "check" or "fix" and
-- a list of files to be checked.
main :: IO ()
main = do
    args <- getArgs
    case args of
        "check":files -> checkStyle files
        "fix":files -> fixStyle files
        _ -> error "Must specify 'check' or 'fix'"

