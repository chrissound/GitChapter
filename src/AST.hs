{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wno-unused-imports #-}
module AST where

import Hart
import qualified Text.Parsec as TPar


-- import Text.Parsec.String
-- import Data.Either.Extra
-- import Safe
-- import Data.Bool
-- import qualified Data.Text as Text
import Operations
import Operations.Types

import Text.Parsec hiding (parserTrace)

import Text.Mustache.Parser (parseText, MustacheState)
import Control.Monad.Identity

import Text.Mustache.Internal.Types (Node(..), DataIdentifier(..))
import Data.Foldable
import Data.Either.Extra

parseSection :: ParsecT Text MustacheState Identity [SectionBlock] 
parseSection = fmap (fmap xyz) parseText

xyz :: Node Text -> SectionBlock 
xyz (TextBlock x) = SectionRaw x
xyz (Variable _ (NamedData x)) =
  case x of
    (x':[]) -> do
      let v = asum $ fmap (eitherToMaybe)
              [
                  (SectionAST . GCASTFileReference)            <$> (TPar.parse Operations.Types.parse  "ghci" $ cs x')
                , (SectionAST . GCASTFileSection)              <$> (TPar.parse Operations.Types.parse  "ghci" $ cs x')
                , (SectionAST . GCASTGitDiffReference)         <$> (TPar.parse Operations.Types.parse  "ghci" $ cs x')
                , (SectionAST . GCASTGitCommitOffestReference) <$> (TPar.parse Operations.Types.parse  "ghci" $ cs x')
                , (SectionAST . GCASTShell)                    <$> (TPar.parse Operations.Types.parse  "ghci" $ cs x')
                , (SectionAST . GCASTSectionHeaderReference)   <$> (TPar.parse Operations.Types.parse  "ghci" $ cs x')
                , (SectionAST . GCASTGHCiReference)            <$> (TPar.parse Operations.Types.parse  "ghci" $ cs x')
                , (SectionAST . GCASTGHCiReference)            <$> (TPar.parse Operations.Types.parse  "ghci" $ cs x')
                -- [ case (TPar.parse Operations.Types.parse  "ghci" $ cs x') of
                  -- Right x'' -> Just $ SectionAST $ GCASTGHCiReference x''
                  -- Left _ -> Nothing
                -- , case (TPar.parse Operations.Types.parse "sectionHeader" $ cs x') of
                  -- Right v -> Just $ SectionAST $ GCASTGitCommitOffestReference v
                  -- Left _ -> Nothing
                -- , case (TPar.parse (void $ string "gitCommitOffset" :: Parser ())  "gitCommitOffset" $ cs x') of
                  -- Right () -> Just $ SectionAST $ GCASTGitCommitOffestReference
                  -- Left _ -> Nothing
                -- , case (TPar.parse (parseeDi)  "gitCommitOffset" $ cs x') of
                  -- Right () -> Just $ SectionAST $ GCASTGitCommitOffestReference
                  -- Left _ -> Nothing
                ]
      case v of
        Just v' -> v'
        Nothing -> SectionError $ "Unrecognized function: " <> cs x'
    _ -> error "fuck123"
xyz _ = error "Fuck123"
