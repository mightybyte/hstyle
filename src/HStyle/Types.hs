module HStyle.Types
    ( Selector
    , Check
    , Rule
    ) where

import Data.Text (Text)
import Language.Haskell.Exts.Annotated (Module, SrcSpanInfo)

import HStyle.Block

-- | Selects a portion from a haskell module
type Selector = Module SrcSpanInfo -> Block -> [Block]

-- | Takes a number of lines, and notifies of problems on each line. Indices in
-- the result are 1-based.
type Check = Block -> [(Int, Text)]

-- | A selector and a check...
type Rule = (Selector, Check)
