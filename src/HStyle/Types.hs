module HStyle.Types
    ( Selector
    , Check
    , Rule
    ) where

import Data.Text (Text)
import Language.Haskell.Exts.Annotated (Module, SrcSpanInfo)

-- | Selects a portion from a haskell module
type Selector = Module SrcSpanInfo -> [SrcSpanInfo]

-- | Takes a number of lines, and notifies of problems on each line. Indices in
-- the result are zero-based.
type Check = [Text] -> [(Int, Text)]

-- | A selector and a check...
type Rule = (Selector, Check)
