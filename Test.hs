{-# OPTIONS -Wno-unused-imports #-}
module Test where

import Render
import Test.HUnit
import Test.QuickCheck
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  _ <- runTestTT hUnitTests
  print "Tests completed!"

hUnitTests :: Test
hUnitTests = test [
  "" ~: "" ~: True ~=? testParseSectionHeader
  ]

testParseSectionHeader :: Bool
testParseSectionHeader = do
  let pfx = " test {} "
  let sfx =  " test {{test}}  "
  case (parse parseSectionHeader "secitonHeaderTest" (pfx ++ "{{sectionHeader}}" ++ sfx)) of
    Right (SectionHeaderReference s s') -> (s == pfx) && (s' == sfx)
    Left e -> error $ show e

