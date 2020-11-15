{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Test where

import Render
import Test.HUnit
import Test.QuickCheck
import Data.Maybe
import qualified Text.Parsec as TPar
import qualified Text.Parsec.String
import qualified Text.Parsec
import Text.Mustache.Parser (emptyState, initState, defaultConf)
import Data.List
import System.IO.Unsafe
import Text.Pretty.Simple (pPrint)
import Data.String.Conversions
import Data.Text (Text)
import QuasiText
import GHCi
import Debug.Trace
import Operations 
import Control.Monad.Trans
import Data.Either
import Data.Bool
import Operations.ParserV2
import Operations.Types

main :: IO ()
main = do
  -- let fp = "/home/chris/Haskell/Harticles/GentleIntroductionToMonadTransformers/chapters/1_Introducing side effects.md"
  -- fp' <- readFile fp
  -- xyz fp $ cs fp'
  _ <- runTestTT hUnitTests
  print "Tests completed!"

hUnitTests :: Test
hUnitTests = test [
    -- "testParseSectionHeader"    ~: True ~=? testParseSectionHeader
    -- "testParseFileReference"    ~: True ~=? testParseFileReference
  -- , "testParseGhci"             ~: testParseGhci
  -- , "testParseFileSection"      ~: testParseFileSection
   "testParseGitDiff"          ~: testGitDiff
   -- "testParseShell"            ~: testShell
  -- , "testParseShellOutput"      ~: testShellOutput
  -- , "testMultiLineXyz"          ~: testMultiLineXyz
  -- , "testMultiLineXyz2"         ~: testMultiLineXyz2
   -- , "testRealWorldSectionBlock" ~: True ~=? testRealWorldSectionBlock
  , "testRunGhci"               ~: testGhciRun
  , "testRunGhci2"              ~: testGhciRun2
  -- , "testRenderTemplate"        ~: testRenderTemplate
  -- , "testEscape"                ~: testEscape
  , "testChris"                ~: testChris
  -- , "myHmm"                     ~: testMyHmm
  -- -- , "myHmm2"                    ~: testMyHmm2
                  ]
  -- ]

-- testMyHmm :: Test 
-- testMyHmm = do
  -- let v =
        -- [
            -- (""     , Right [])
          -- , ("123"  , Right [Left 123])
          -- , (" 123"  , Right [Left 123])
          -- , (" 123 abc"  , Right [Left 123, Right "abc"])
          -- , (" 123 abc456"  , Right [Left 123, Right "abc", Left 456])
          -- ]
  -- test $ (\v' -> (TPar.parse parseNumberString "testMyHmm" (fst v')) ~=? (snd v')) <$> v

-- testMyHmm2 :: Test 
-- testMyHmm2 = do
  -- let v =
        -- [
            -- (""     , Right [])
          -- , ("123 456"  , Right ["123", "456"])
          -- , ("123 abc"  , Right ["123", "abc"])
          -- , ("xyz 123 abc"  , Right ["xyz", "123", "abc"])
          -- , (" 123"  , Right ["123"])
          -- ]
  -- test $ (\v' -> (TPar.parse parseNumberString' "testMyHmm2" (fst v')) ~?= (snd v')) <$> v

-- testParseSectionHeader :: Bool
-- testParseSectionHeader = do
  -- case (TPar.parse parseSectionHeader "secitonHeaderTest" ("{{sectionHeader}}")) of
    -- Right (SectionHeaderReference) -> True
    -- Left e -> error $ show e


testParseFileReference :: Bool
testParseFileReference = do
  let fp = "abc/xyz.123"
  let t1 =  "{{file "++ fp ++"}}"
  let t2 =  "{{ file "++ fp ++" 10 20 }}"
  let c1 = case (TPar.parse  parse "fileRefTest" t1) of
        Right (FileReference x (FileLineRange Nothing)) -> x == fp
        Left e -> error $ show e
        _ -> error "?"
  let c2 = case (TPar.parse  parse "fileRefTest" t2) of
        Right (FileReference x (FileLineRange (Just (y',y'')))) -> and [x == fp, y' == 10, y'' == 20]
        Left e -> error $ show e
        _ -> error "?"
  and [c1, c2]

testParseGhci :: Test
testParseGhci = test
  [ do
      let t1 = concat $ intersperse "\n" [
                "ghci"
              , "hmmm"
              , "hmmm2"
              , "abcxyz"
              ]
      case (TPar.parse parse "" t1) of
            Right (GHCiReference x Nothing) -> x ~=? "hmmm\nhmmm2\nabcxyz"
            Right (GHCiReference _ n) -> Nothing ~=? n
            Left e -> error $ show e
  -- , do
      -- let t1 = [str|ghci eitherLeftOrRight
-- :t head
-- 4 + 4

-- testing123|]
      -- case (TPar.parse parse "" t1) of
            -- Right (GHCiReference x (Just "eitherLeftOrRight")) -> x ~=? ":t head\n4 + 4\n"
            -- Right (GHCiReference _ n) -> Nothing ~=? n
            -- Left e -> error $ show e
  ]

testParseGhci2 :: Bool
testParseGhci2 = do
  let t1 = concat $ intersperse "\\n" [
            "{{{ghci"
          , "hmmm"
          , "hmmm2"
          , "abcxyz}}}"
          ]
  case (TPar.parse parse "" t1) of
        Right (GHCiReference x _ ) -> x == "\\nhmmm\\nhmmm2\\nabcxyz"
        Left e -> error $ show e

testMultiLineXyz :: Test
testMultiLineXyz = do
  let t2 = concat $ intersperse "\n" $ [
            "{{ghci hello"
          , "abc"
          , "xyz"
          , "}}"
          ]
  case (runMuParser "testMultiLineXyz1287" $ cs t2) of
        (Right (x:[])) -> (SectionGHCi "abc\nxyz" (Just "hello")) ~=? x
        (Right e) -> error $ show e
        (Left e) -> error $ show e


testMultiLineXyz2 :: Test
testMultiLineXyz2 = do
  let t2 = concat $ intersperse "\n" [
            "abcxyz"
          , "{{ghci"
          , "hmm"
          , "hmm"
          , "}}"
          , "abcxyz"
          ]
  case (runMuParser "testMultiLineXyz" $ cs t2) of
        (Right (x:y:z:[])) -> 
          TestList [
               SectionRaw "abcxyz\n" ~=? x
             , SectionGHCi "hmm\nhmm" Nothing ~=? y 
             , SectionRaw "\nabcxyz" ~=? z 
             ]
        (Left e) -> hunitFailErr e
        _ -> unsafePerformIO $ do
          assertFailure "derp"

hunitFailErr :: Show a => a -> Test
hunitFailErr e = TestCase $ liftIO ( Test.HUnit.assertFailure $ show e)

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
   case (runMuParser "testMultiLineXyz" $ cs t2) of
        (Right (SectionRaw x:[])) -> unsafePerformIO $ do
          let expe = cs $ t2
          return $ x == expe
        (Left e) -> error $ show e
        (_) -> unsafePerformIO $ do
          return False

testGhciRun :: Test
testGhciRun = ("ghci> :t head\n      head :: [a] -> a") ~=? (unsafePerformIO $ runGhci ":t head")

testGhciRun2 :: Test
testGhciRun2 = "ghci> :t head\n      head :: [a] -> a\nghci> :t tail\n      tail :: [a] -> [a]"
  ~=? (unsafePerformIO $ runGhci ":t head\n:t tail")

testRenderTemplate :: Test
testRenderTemplate = do

  let input = [str|### Introducing Side-Effects

{{ghci eitherLeftOrRight
:t head
4 + 4
}}

testing123|] :: Text
  let sectionExpected = [
          SectionRaw "### Introducing Side-Effects\n\n"
        , SectionGHCi ":t head\n4 + 4" (Just "eitherLeftOrRight")
        , SectionRaw "\n\ntesting123"
        ]
  let x = runMuParser "parseSection" $ cs input
  test [ x ~=? (Right sectionExpected)
       ]
        -- let textExpected = [
                -- "### Introducing Side-Effects"
              -- , ""
              -- , "{{ghci eitherLeftOrRight\n:t head\n4 + 4\n}}"
              -- , ""
              -- , ""
              -- , "testing123"]
        -- return $ textExpected ~=? transformInnerSection sections

testParseFileSection :: Test
testParseFileSection = do
  let input = "{{fileSection src/abc xyz}}"
  case (TPar.parse parse "fileRefTest" input) of
        Right fs -> fs ~=? (FileSection "src/abc" "xyz")
        Left e -> error $ show e

testGitDiff :: Test
testGitDiff = do
  let input = "{{gitDiff src/abc}}"
  case (runMuParser "???" input) of
        Right fs -> fs ~=? [(SectionAST $ GCASTGitDiffReference $ GitDiffReference "src/abc")]
        Left e -> error $ show e

testShell :: Test
testShell = do
  let input = "{{shell src/abc}}"
  case (TPar.parse parse "fileRefTest" input) of
        Right fs -> fs ~=? (Shell ShellSuccessVoid ShellOutputVoid "src/abc")
        Left e -> error $ show e

testShellOutput :: Test
testShellOutput = do
  let input = "{{shellOut' src/abc}}"
  case (TPar.parse parse "fileRefTest" input) of
        Right fs -> fs ~=? (Shell ShellSuccessRequired ShellOutput' "src/abc")
        Left e -> error $ show e

testEscape :: Test
testEscape = do
  let input = "{{={{{ }}}>=}}{{gitDiff src/abc}}"
  case (runMuParser "???" input) of
        Right fs -> ([SectionRaw "{{gitDiff src/abc}}"]) ~=? fs
        Left e -> error $ show e

testChris :: Test
testChris = do
  let input = "# test\n"
  case (runMuParser "???" input) of
        Right fs -> ([SectionRaw "# test\n"]) ~=? fs
        Left e -> error $ show e
