{-# LANGUAGE OverloadedStrings #-}
module HStyle
    ( FileState (..)
    , checkStyle
    , fixStyle
    ) where

import Control.Monad (foldM, forM_, unless, when)
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
import HStyle.Fixer

-- | A selector and a check...
type Rule = (Selector, Checker, Fixer)

data FileState = FileState
    { fileBlock   :: Block
    , fileUpdated :: Bool
    , fileOk      :: Bool
    } deriving (Show)

runRule :: Bool -> FilePath -> H.Module H.SrcSpanInfo
        -> FileState -> Rule -> IO FileState
runRule quiet file md fileState (selector, checker, fixer) =
    foldM checkBlock fileState (selector md $ fileBlock fileState)
  where
    checkBlock :: FileState -> Block -> IO FileState
    checkBlock fs block = do
        let problems        = checker block
        let (fixed, block') = case fixer block of
                Nothing -> (False, block)
                Just b  -> (True, b)
        forM_ problems $ \(i, problem) -> do
            let line = absoluteLineNumber i (fileBlock fs)
            T.putStrLn $ T.pack file `T.append` ":" `T.append`
                T.pack (show line) `T.append` ": " `T.append` problem
            unless quiet $ do
                T.putStrLn "    Found:"
                T.putStr   $ prettyBlock 4 block
                if not fixed
                    then T.putStrLn "    (Couldn't automatically fix)"
                    else do
                        T.putStrLn "    Fixed to:"
                        T.putStr $ prettyBlock 4 block'
                T.putStrLn ""
        return fs
            { fileBlock   = updateSubBlock block block' (fileBlock fs)
            , fileUpdated = fileUpdated fs || fixed
            , fileOk      = fileOk fs      && null problems
            }

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
    then Just $ "Exceeds max line length of " `T.append` T.pack (show max')
    else Nothing

trailingWhiteSpace :: Checker
trailingWhiteSpace = checkLines $ \line ->
    if not (T.null line) && isSpace (T.last line)
        then Just "Trailing whitespace"
        else Nothing

trailingWhiteSpaceFixer :: Fixer
trailingWhiteSpaceFixer = fixLines T.stripEnd

-- | Filter out lines which use CPP macros
unCPP :: String -> String
unCPP = unlines . map unCpp' . lines
  where
    unCpp' x
        | "#" `isPrefixOf` x = ""
        | otherwise          = x

checkStyle :: Bool -> FilePath -> IO FileState
checkStyle quiet file = do
    contents <- readFile file
    let block     = fromText $ T.pack contents
        -- Determine the extensions used in the file, and update the parsing
        -- mode based upon those
        exts      = fromMaybe [] $ H.readExtensions contents
        mode      = H.defaultParseMode {H.extensions = exts}
        -- Special handling for CPP, haskell-src-exts can't deal with it
        contents' = if H.CPP `elem` exts then unCPP contents else contents
        fs        = FileState block False True
    case H.parseModuleWithMode mode contents' of
        H.ParseOk md -> do
            fs' <- foldM (runRule quiet file md) fs
                [ (typeSigSelector, typeSigCheck,       fixNothing)
                , (selectLines,     tabsCheck,          fixNothing)
                , (selectLines,     lineLengthCheck 78, fixNothing)
                , (selectLines,     trailingWhiteSpace, trailingWhiteSpaceFixer)
                ]
            when (fileUpdated fs') $ T.writeFile file $ toText $ fileBlock fs'
            return fs'
        err         -> do
            putStrLn $ "HStyle.checkStyle: could not parse " ++
                file ++ ": " ++ show err
            return fs

fixStyle :: FilePath -> IO ()
fixStyle = error "HStyle.fixStyle: Not implemented"
