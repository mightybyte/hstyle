{-# LANGUAGE OverloadedStrings #-}
module HStyle
    ( runRule
    , main
    ) where

import Control.Monad (forM_, unless)
import System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Block
import HStyle.Types

subBlock' :: H.SrcSpanInfo -> Block -> Block
subBlock' ssi = subBlock start end
  where
    span' = H.srcInfoSpan ssi
    start = H.srcSpanStartLine span'
    end   = H.srcSpanEndLine span'

runRule :: Block -> H.Module H.SrcSpanInfo -> Rule -> IO ()
runRule block md (selector, check) = forM_ (selector md) $ \ssi -> do
    let block'   = subBlock' ssi block
        problems = check block'
    unless (null problems) $ do
        T.putStrLn $ T.replicate 78 "-"
        T.putStr   $ prettyBlock block'
        T.putStrLn $ T.replicate 78 "-"
    forM_ problems $ \(i, problem) -> do
        let line = i + H.srcSpanStartLine (H.srcInfoSpan ssi)
        T.putStrLn $ T.pack (show line) `T.append` ": " `T.append` problem

typeSigSelector :: Selector
typeSigSelector (H.Module _ _ _ _ decls) = [ssi | H.TypeSig ssi _ _ <- decls]
typeSigSelector _                        = []

typeSigCheck :: Check
typeSigCheck block = case checkAlignment alignment of
    Just _  -> [(0, "Bad alignment")]
    Nothing -> []
  where
    alignment = alignmentOf ["::", "=>", "->"] $ toLines block

main :: IO ()
main = do
    [file] <- getArgs
    contents <- readFile file
    let block = fromText $ T.pack contents
    case H.parseModule contents of
        H.ParseOk x -> runRule block x (typeSigSelector, typeSigCheck)
        err         -> putStrLn $ show err
