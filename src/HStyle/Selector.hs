module HStyle.Selector
    ( Selector
    , selectAll
    , selectLines
    ) where

import Language.Haskell.Exts.Annotated (Module, SrcSpanInfo)

import HStyle.Block

-- | Selects a portion from a haskell module
type Selector = Module SrcSpanInfo -> Block -> [Block]

selectAll :: Selector
selectAll _ = return

selectLines :: Selector
selectLines _ = perLine
