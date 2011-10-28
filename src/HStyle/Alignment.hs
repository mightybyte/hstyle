{-# LANGUAGE OverloadedStrings #-}
module HStyle.Alignment where

import Data.List (find, nub)
import Data.Text (Text)
import qualified Data.Text as T

type Lines = [Text]
type Alignment = [[(Int, Text)]]

-- This is a really really long comment and I'm not sure if this is a good idea cause it might not fit on one line
checkAlignmentHead :: Alignment
                  -> Maybe Text
checkAlignmentHead alignment
    | null alignment'       = Nothing
    | equal (map fst heads) = Nothing
    | otherwise             = Just $ "improper alignment of " `T.append`
        T.pack (show $ nub $ map snd heads)
  where
    alignment' = filter (not . null) alignment
    heads	   = map head alignment'

equal :: Eq a   
      => [a]
      -> Bool
equal (x : y : r)
    | x == y    = equal (y : r)
    | otherwise = False
equal _         = True

alignmentOf :: [Text] -> Lines -> Alignment
alignmentOf xs = map $ alignmentOf' 0
  where
    alignmentOf' i t
        | T.null t  = []
        | otherwise = case find (`T.isPrefixOf` t) xs of
            Nothing -> alignmentOf' (i + 1) (T.drop 1 t)
            Just x  ->
                let len = T.length x
                in (i, x) : alignmentOf' (i + len) (T.drop len t)
