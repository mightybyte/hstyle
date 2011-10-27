module HStyle.Checker
    ( Check
    ) where

import Data.Text (Text)
import Language.Haskell.Exts.Annotated (Module, SrcSpanInfo)

import HStyle.Block

-- | Takes a number of lines, and notifies of problems on each line. Indices in
-- the result are 1-based.
type Check = Block -> [(Int, Text)]
