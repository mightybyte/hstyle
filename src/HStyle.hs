{-# LANGUAGE OverloadedStrings #-}
module HStyle
    ( checkStyle
    , fixStyle
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts.Annotated as H

import HStyle.Alignment
import HStyle.Block
import HStyle.Selector
import HStyle.Checker

-- | A selector and a check...
type Rule = (Selector, Checker)

runRule :: FilePath -> Block -> H.Module H.SrcSpanInfo -> Rule -> IO Bool
runRule file block md (selector, check) = fmap and $
    forM (selector md block) $ \block' -> do
        let problems = check block'
        forM_ problems $ \(i, problem) -> do
            let line = absoluteLineNumber i block'
            T.putStrLn $ T.pack file `T.append` ":" `T.append`
                T.pack (show line) `T.append` ": " `T.append` problem
            T.putStrLn ""
            T.putStr   $ prettyBlock 4 block'
            T.putStrLn ""
        return $ null problems

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
typeSigCheck block = case checkAlignmentHead alignment of
    Just t  -> [(1, t)]
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

-- | Filter out lines which use CPP macros
unCPP :: String -> String
unCPP = unlines . map unCpp' . lines
  where
    unCpp' x
        | "#" `isPrefixOf` x = ""
        | otherwise          = x

checkStyle :: FilePath -> IO Bool
checkStyle file = do
    contents <- readFile file
    let block      = fromText $ T.pack contents
        -- Determine the extensions used in the file, and update the parsing
        -- mode based upon those
        exts      = fromMaybe [] $ H.readExtensions contents
        mode      = H.defaultParseMode {H.extensions = exts}
        -- Special handling for CPP, haskell-src-exts can't deal with it
        contents' = if H.CPP `elem` exts then unCPP contents else contents
    case H.parseModuleWithMode mode contents' of
        H.ParseOk md -> and <$> mapM (runRule file block md)
            [ (typeSigSelector, typeSigCheck)
            , (selectLines, tabsCheck)
            , (selectLines, lineLengthCheck 78)
            , (selectLines, trailingWhiteSpace)
            ]
        err         -> do
            putStrLn $ "HStyle.checkStyle: could not parse " ++
                file ++ ": " ++ show err
            return False

fixStyle :: FilePath -> IO ()
fixStyle = error "HStyle.fixStyle: Not implemented"
