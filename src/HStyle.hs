{-# LANGUAGE OverloadedStrings #-}
module HStyle
    ( runRule
    , main
    ) where

import Control.Monad (guard, forM_, unless)
import System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Block
import HStyle.Types

runRule :: Block -> H.Module H.SrcSpanInfo -> Rule -> IO ()
runRule block md (selector, check) = forM_ (selector md block) $ \block' -> do
    let problems = check block'
    unless (null problems) $ do
        T.putStrLn $ T.replicate 78 "-"
        T.putStr   $ prettyBlock block'
        T.putStrLn $ T.replicate 78 "-"
    forM_ problems $ \(i, problem) -> do
        let line = absoluteLineNumber i block'
        T.putStrLn $ T.pack (show line) `T.append` ": " `T.append` problem
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

typeSigCheck :: Check
typeSigCheck block = case checkAlignment alignment of
    Just _  -> [(1, "Bad alignment")]
    Nothing -> []
  where
    alignment = alignmentOf ["::", "=>", "->"] $ toLines block

selectAll :: Selector
selectAll _ = return

selectLines :: Selector
selectLines _ = perLine

tabsCheck :: Check
tabsCheck block = do
    (ln, text) <- zip [1 ..] (toLines block)
    case T.findIndex (== '\t') text of
        Nothing -> []
        Just i  -> [(ln , "\\t at column " `T.append` T.pack (show $ i + 1))]

lineLengthCheck :: Int -> Check
lineLengthCheck max' block = do
    (ln, text) <- zip [1 ..] (toLines block)
    guard $ T.length text > max'
    return (ln , "exceeds max line length of " `T.append` T.pack (show max'))

main :: IO ()
main = do
    [file] <- getArgs
    contents <- readFile file
    let block = fromText $ T.pack contents
    case H.parseModule contents of
        H.ParseOk md -> do
            runRule block md (typeSigSelector, typeSigCheck)
            runRule block md (selectLines, tabsCheck)
            runRule block md (selectLines, lineLengthCheck 80)
        err         -> putStrLn $ show err
