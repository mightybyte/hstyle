{-# LANGUAGE OverloadedStrings #-}
module HStyle
    ( runRule
    , main
    ) where

import Control.Monad (forM_, unless)
import System.Environment (getArgs)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Types

takeLines :: H.SrcSpanInfo -> Text -> [Text]
takeLines ssi = take (end - start + 1) . drop (start - 1) . T.lines
  where
    span' = H.srcInfoSpan ssi
    start = H.srcSpanStartLine span'
    end   = H.srcSpanEndLine span'

runRule :: Text -> H.Module H.SrcSpanInfo -> Rule -> IO ()
runRule text md (selector, check) = forM_ (selector md) $ \ssi -> do
    let lines'   = takeLines ssi text
        problems = check lines'
    unless (null problems) $ do
        T.putStrLn $ T.replicate 78 "-"
        T.putStr   $ T.unlines lines'
        T.putStrLn $ T.replicate 78 "-"
    forM_ (check $ takeLines ssi text) $ \(i, problem) -> do
        let line = i + H.srcSpanStartLine (H.srcInfoSpan ssi)
        T.putStrLn $ T.pack (show line) `T.append` ": " `T.append` problem

typeSigSelector :: Selector
typeSigSelector (H.Module _ _ _ _ decls) = [ssi | H.TypeSig ssi _ _ <- decls]
typeSigSelector _                        = []

typeSigCheck :: Check
typeSigCheck lines' = case checkAlignment alignment of
    Just _  -> [(0, "Bad alignment")]
    Nothing -> []
  where
    alignment = alignmentOf ["::", "=>", "->"] lines'

main :: IO ()
main = do
    [file] <- getArgs
    contents <- readFile file
    let text = T.pack contents
    case H.parseModule contents of
        H.ParseOk x -> runRule text x (typeSigSelector, typeSigCheck)
        err         -> putStrLn $ show err
