{-# LANGUAGE OverloadedStrings #-}
module HStyle.Block.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))
import qualified Data.Text as T

import HStyle.Block

tests :: Test
tests = testGroup "HStyle.Block.Tests"
    [ testCase "subBlock_01"       subBlock_01
    , testCase "updateSubBlock_01" updateSubBlock_01
    ]

subBlock_01 :: Assertion
subBlock_01 = toLines (subBlock 2 3 poem) @?=
    ["A little man who wasn't there", "He wasn't there again today"]

updateSubBlock_01 :: Assertion
updateSubBlock_01 = toLines (updateSubBlock old new poem) @?=
    [ "Last night I saw upon the stair"
    , "A little man who wasn't there..."
    , "He wasn't there again today..."
    , "Oh, how I wish he'd go away"
    ]
  where
    old = subBlock 2 3 poem
    new = mapLines (`T.append` "...") old

poem :: Block
poem = fromText
    "Last night I saw upon the stair\n\
    \A little man who wasn't there\n\
    \He wasn't there again today\n\
    \Oh, how I wish he'd go away"
