module HStyle.Fixer
    ( Fixer
    , fixNothing
    , fixLines
    ) where

import Data.Text (Text)

import HStyle.Block

-- | Takes a block and fixes it, if possible
type Fixer = Block -> Maybe Block

fixNothing :: Fixer
fixNothing = const Nothing

fixLines :: (Text -> Text) -> Fixer
fixLines f = Just . mapLines f
