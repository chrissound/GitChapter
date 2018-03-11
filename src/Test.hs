{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# Language OverloadedStrings #-}
module Test where

import Render
import Test.HUnit
import Test.QuickCheck
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Data.List
import System.IO.Unsafe
import Text.Pretty.Simple (pPrint)
import Data.String.Conversions
import Data.Text (Text)

main :: IO ()
main = do
  _ <- runTestTT hUnitTests
  print "Tests completed!"

hUnitTests :: Test
hUnitTests = test [
    "testParseSectionHeader"    ~: True ~=? testParseSectionHeader
  , "testParseFileAndLimits"    ~: True ~=? testParseFileAndLimits
  , "testParseFileReference"    ~: True ~=? testParseFileReference
  , "testParseGhci"             ~: True ~=? testParseGhci
  , "testMultiLineXyz"          ~: True ~=? testMultiLineXyz
  , "testMultiLineXyz2"         ~: True ~=? testMultiLineXyz2
  , "testRealWorldSectionBlock" ~: True ~=? testRealWorldSectionBlock
  ]

testParseSectionHeader :: Bool
testParseSectionHeader = do
  let pfx = " test {} "
  let sfx =  " test {{test}}  "
  case (parse parseSectionHeader "secitonHeaderTest" (pfx ++ "{{sectionHeader}}" ++ sfx)) of
    Right (SectionHeaderReference s s') -> (s == pfx) && (s' == sfx)
    Left e -> error $ show e


testParseFileAndLimits :: Bool
testParseFileAndLimits = do
  let fp = "abc/xyz.123"
  let t1 = fp ++ " "
  let t2 = fp ++ " 123 456 "
  let t3 = fp ++ " 123 456"
  let c1 = case (parse parseFileAndLimits "fileAndLimitsTest" t1) of
        Right (x, Nothing, Nothing) -> x == fp
        Left e  -> error $ show e
        _ -> error "?"
  let c2 = case (parse parseFileAndLimits "fileAndLimitsTest" t2) of
        Right (_, Just y, Just z) -> and [y == 123, z == 456]
        Left e  -> error $ show e
        _ -> error "?"
  let c3 = case (parse parseFileAndLimits "fileAndLimitsTest" t3) of
        Right (_, Just y, Just z) -> and [y == 123, z == 456]
        Left e  -> error $ show e
        _ -> error "?"
  and [c1, c2, c3]


testParseFileReference :: Bool
testParseFileReference = do
  let fp = "abc/xyz.123"
  let t1 =  "{{ file "++ fp ++" }}"
  let t2 =  "{{ file "++ fp ++" 10 20 }}"
  let c1 = case (parse parseFileReference "fileRefTest" t1) of
        Right (FileReference x Nothing) -> x == fp
        Left e -> error $ show e
        _ -> error "?"
  let c2 = case (parse parseFileReference "fileRefTest" t2) of
        Right (FileReference x (Just (y',y''))) -> and [x == fp, y' == 10, y'' == 20]
        Left e -> error $ show e
        _ -> error "?"
  and [c1, c2]

testParseGhci :: Bool
testParseGhci = do
  let t1 = concat $ intersperse "\\n" [
            "{{{ghci"
          , "hmmm"
          , "hmmm2"
          , "abcxyz}}}"
          ]
  case (parse parseGhciTag "" t1) of
        Right x -> x == "\\nhmmm\\nhmmm2\\nabcxyz"
        Left e -> error $ show e

testMultiLineXyz :: Bool
testMultiLineXyz = do
  let t2 = concat $ intersperse "\\n" [
            "{{{ghci"
          , "hmmm"
          , "}}}"
          ]
  case (parse parseSection "testMultiLineXyz" t2) of
        (Right (SectionGHCi x:[])) -> unsafePerformIO $ do
          let expe = "\\nhmmm\\n"
          return $ x == expe
        (Left e) -> error $ show e
        (_) -> unsafePerformIO $ do
          return False


testMultiLineXyz2 :: Bool
testMultiLineXyz2 = do
  let t2 = concat $ intersperse "\\n" [
            "abcxyz"
          , "{{{ghci"
          , "hmm"
          , "hmm"
          , "}}}"
          , "abcxyz"
          ]
  case (parse parseSection "testMultiLineXyz" t2) of
        (Right (SectionRaw x:SectionGHCi y:SectionRaw x':[])) -> unsafePerformIO $ do
          return $
               (x == "abcxyz\\n")
            && (x' == "\\nabcxyz")
            && (y == "\\nhmm\\nhmm\\n")
        (Left e) -> error $ show e
        _ -> unsafePerformIO $ do
          return False

testRealWorldSectionBlock :: Bool
testRealWorldSectionBlock = do
   let t2 = concat $ intersperse "\\n" [
             "Testing"
           , "==========================================="
           , "```haskell"
           , "{-# LANGUAGE OverloadedStrings #-}"
           , "```"
           , "Or, abc xyz"
           , "-----------"
           ]
   case (parse parseSection "testMultiLineXyz" $ cs t2) of
        (Right (SectionRaw x:[])) -> unsafePerformIO $ do
          let expe = cs $ t2
          return $ x == expe
        (Left e) -> error $ show e
        (e) -> unsafePerformIO $ do
          print e
          return False
