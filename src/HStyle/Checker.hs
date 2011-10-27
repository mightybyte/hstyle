module HStyle.Checker
    ( Checker
    , checkLines
    ) where

import Data.Text (Text)

import HStyle.Block

-- | Takes a number of lines, and notifies of problems on each line. Indices in
-- the result are 1-based.
type Checker = Block -> [(Int, Text)]

-- | Check every line of the block, possibly returning a problem description
checkLines :: (Text -> Maybe Text) -> Checker
checkLines checker block = do
    (ln, text) <- zip [1 ..] (toLines block)
    case checker text of
        Nothing -> []
        Just p  -> [(ln , p)]
