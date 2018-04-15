--{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Render where

import Prelude hiding (readFile)
import Data.Text (lines)
import Data.String.Conversions
import Turtle (ExitCode(..))
import Text.Printf
import Operations hiding (parse)
import qualified Operations as Ops

import qualified Text.Parsec
import Data.Foldable

import Git
import Hart

data Reference =
    FileRef FileReference
  | GitRef GitDiffReference
  | GitCommitOffestRef GitCommitOffestReference
  | PossibleRef PossibleTag
  | ShellOutputRef ShellOutput
  | SectionHeaderRef SectionHeaderReference
  | GHCiRef GHCiReference deriving (Show, Eq)
data PreLineOutput = Raw Text | RefOutput Reference' deriving (Show) 

compilePreOutput :: PreLineOutput -> Hart (Either String Text)
compilePreOutput (Raw x) = return $ Right x
compilePreOutput (RefOutput (Reference' r)) = render r

shellOutput :: Text -> IO Text
shellOutput x = runSh x >>= \case
  (ExitSuccess,t,_) -> return t
  (ExitFailure n,t,e) -> return $ cs ("The command failed with exit code (" ++ show n ++ "). ") <> t <> e

isPossibleRefPresent :: (ConvertibleStrings a Text, ConvertibleStrings a String, Functor t, Foldable t) => t a -> Maybe PossibleTag
isPossibleRefPresent lns = asum $
  (\x -> (parse' parsePossibleTag "possibleTag" . cs) x
      >>= (case parseLine $ cs x of
            Just _ -> const Nothing
            Nothing -> Just)
  ) <$> lns

renderTemplate :: Text -> Either String [PreLineOutput]
renderTemplate x =
  case transformInnerSection <$> Text.Parsec.parse parseSection "parseSection" (cs x) of
    Right lns -> do
      case isPossibleRefPresent lns of
        Nothing -> Right $ (\l -> maybe (Raw l) (RefOutput) (parseLine l)) <$> lns
        Just ( PossibleTag t ) -> Left $ "Possible tag found but not recognized: " ++ t
    Left e -> Left $ cs ("An internal error has occurred (unable to parse sub sections), please report this bug. Unable to parse inner sections within section: " ++ show e)

transformInnerSection :: [SectionBlock] -> [Text]
transformInnerSection ([]) = []
transformInnerSection (x:xs) = case x of
        SectionRaw b -> Data.Text.lines b ++ (transformInnerSection xs)
        SectionGHCi b _ -> "{{{ghci\n" <> b <> "}}}" : (transformInnerSection xs)

parseLine :: Text -> Maybe Reference'
parseLine x = do
  let xStr = convertString x :: String
  case asum [
          Reference' <$> (parse' Ops.parse "file reference" xStr :: Maybe FileReference)
        , Reference' <$> (parse' Ops.parse "file reference" xStr :: Maybe FileSection)
        , Reference' <$> (parse' Ops.parse "git diff tag" xStr :: Maybe GitDiffReference)
        , Reference' <$> (parse' Ops.parse "git commit offset" xStr :: Maybe GitCommitOffestReference)
        , Reference' <$> (parse' Ops.parse "shellOutput tag" xStr :: Maybe ShellOutput)
        , Reference' <$> (parse' Ops.parse "section header" xStr :: Maybe SectionHeaderReference)
        , Reference' <$> (parse' Ops.parse "ghci reference" $ xStr :: Maybe GHCiReference)
        ]
    of Just z -> Just z
       Nothing -> Nothing

gitDiff' :: GitDiffReference -> Hart (Either String Text)
gitDiff' (GitDiffReference z) = do
  gitDiff z >>= \case
    Right x -> return $ Right $ x
    Left x -> return $ Left (printf "Unable to retrieve git diff (%s) -  %s" z (show x))


surroundBackTicks :: Text -> Text
surroundBackTicks v = "```\n" <> v <> "```"

gitCommitRefence :: GitCommitOffestReference -> Hart (Either String Text)
gitCommitRefence (GitCommitOffestReference) = do
  hc <- ask
  return $ Right $ cs $ "```\n"
    <> "Git From Commit: \n"
    <> hartConfigFromHash hc <> "\n\n"
    <> "Git Until Commit: \n"
    <> hartConfigUntilHash hc <> "\n"
    <> "```"

printString :: String -> IO ()
printString = print

return2x :: (Monad m, Monad m2) => a -> m (m2 a)
return2x = return . return
