{-# LANGUAGE OverloadedStrings #-}
module HStyle
    ( runRule
    , main
    ) where

import Control.Monad (forM_, unless)
import Data.Char (isSpace)
import System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Block
import HStyle.Selector
import HStyle.Checker

-- | A selector and a check...
type Rule = (Selector, Checker)

runRule :: FilePath -> Block -> H.Module H.SrcSpanInfo -> Rule -> IO ()
runRule file block md (selector, check) =
    forM_ (selector md block) $ \block' -> do
        let problems = check block'
        forM_ problems $ \(i, problem) -> do
            let line = absoluteLineNumber i block'
            T.putStrLn $ T.pack file `T.append` ":" `T.append`
                T.pack (show line) `T.append` ": " `T.append` problem
            T.putStrLn ""
            T.putStr   $ prettyBlock 4 block'
            T.putStrLn ""

fromSrcSpanInfo :: H.SrcSpanInfo -> Block -> Block
fromSrcSpanInfo ssi = subBlock start end
  where
    span' = H.srcInfoSpan ssi
    start = H.srcSpanStartLine span'
    end   = H.srcSpanEndLine span'

typeSigSelector :: Selector
typeSigSelector md block = map (flip fromSrcSpanInfo block) $ tss md
  where
    tss (H.Module _ _ _ _ decls) = [ssi | H.TypeSig ssi _ _ <- decls]
    tss _                        = []

typeSigCheck :: Checker
typeSigCheck block = case checkAlignment alignment of
    Just _  -> [(1, "Bad alignment")]
    Nothing -> []
  where
    alignment = alignmentOf ["::", "=>", "->"] $ toLines block

tabsCheck :: Checker
tabsCheck = checkLines $ \line -> case T.findIndex (== '\t') line of
    Nothing -> Nothing
    Just i  -> Just $ "\\t at column " `T.append` T.pack (show $ i + 1)

lineLengthCheck :: Int -> Checker
lineLengthCheck max' = checkLines $ \line -> if T.length line > max'
    then Just $ "exceeds max line length of " `T.append` T.pack (show max')
    else Nothing

trailingWhiteSpace :: Checker
trailingWhiteSpace = checkLines $ \line ->
    if not (T.null line) && isSpace (T.last line)
        then Just "trailing whitespace"
        else Nothing

main :: IO ()
main = do
    [file] <- getArgs
    contents <- readFile file
    let block = fromText $ T.pack contents
    case H.parseModule contents of
        H.ParseOk md -> mapM_ (runRule file block md)
            [ (typeSigSelector, typeSigCheck)
            , (selectLines, tabsCheck)
            , (selectLines, lineLengthCheck 80)
            , (selectLines, trailingWhiteSpace)
            ]
        err         -> putStrLn $ show err
