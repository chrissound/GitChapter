{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Render where

import Prelude hiding (readFile)
import Text.Mustache (compileTemplate, substituteValue)
import Data.Text (Text, lines, unlines)
import Text.Mustache.Types (Value)
import Data.String.Conversions
import Data.Text.Lazy.IO
import Data.Text.Lazy (toStrict)
import Control.Monad.Trans

import Text.Parsec
import Text.Parsec.String
import Text.Parsec ((<?>))
--import Control.Applicative
import Control.Monad.Identity (Identity, guard)
import Control.Exception as Excp
import System.IO.Error
import Data.Either.Extra
import Data.Foldable
import Git
import Hart

type FileLineRange = Maybe(Int, Int)
data FileReference = FileReference String FileLineRange deriving Show
data GitDiffReference = GitDiffReference Text deriving Show
data PossibleTag = PossibleTag String deriving Show

data Reference = FileRef FileReference | GitRef GitDiffReference | PossibleRef PossibleTag

data PreLineOutput = Raw Text | RefOutput Reference

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

fileReferenceContent :: FileReference -> IO (Maybe Text)
fileReferenceContent (FileReference p flRange) =
  (Excp.tryJust (guard . isDoesNotExistError) $ readFile p) >>= \case
    Right v -> do
      case flRange of
        Just (s, e) -> return $ Just (Data.Text.unlines $ slice s e $ Data.Text.lines $ toStrict v)
        Nothing -> return $ Just $ toStrict v
    Left (_) -> return $ Nothing

gitDiffReferenceContent :: GitDiffReference  -> Hart (Maybe Text)
gitDiffReferenceContent (GitDiffReference p) = do
    (lift . Excp.tryJust (guard . isDoesNotExistError) $ readFile $ cs p) >>= \case
      Right _ -> gitDiff $ cs p
      Left (_) -> return $ Nothing

prepareLineOutput :: Text -> PreLineOutput
prepareLineOutput l = case parseLine l of
  Nothing -> Raw l
  Just x -> RefOutput x

compilePreOutput :: PreLineOutput -> IO (Either String Text)
compilePreOutput (Raw x) = return $ Right x
compilePreOutput (RefOutput (FileRef x)) = maybeToEither "" <$> fileReferenceContent x
compilePreOutput (RefOutput (GitRef x)) = gitDiff' x
compilePreOutput (RefOutput (PossibleRef _)) = return $ Left "derp"

renderTemplate :: Text -> Either String [PreLineOutput]
renderTemplate x = do
  let lns = Data.Text.lines x
  let possibleRefs = filter (\z -> case z of; Just (PossibleRef _) -> True; _ -> False) $ parseLine <$> lns
  case possibleRefs of
    [] -> Right $ prepareLineOutput <$> lns
    (_:_) -> Left "derp"
  -- turn nothing into normal line

parseLine :: Text -> Maybe Reference
parseLine x = do
  let xStr = convertString x :: String
  asum [
          FileRef <$> parse' parseFileReference "file reference" xStr
        , GitRef <$> parse' parseGitDiffReference "git diff tag" xStr
        , PossibleRef <$> (parse' (parsePossibleTag) "possible tag" $ xStr)
        ]

gitDiff' :: GitDiffReference -> IO (Either String Text)
gitDiff' (GitDiffReference z) = do
  let  hc = HartConfig "test" "test" 0
  v' <- (runReaderT . gitDiff $ z) hc :: IO (Maybe Text)
  return . maybeToEither "Unable to retrieve git diff" $ surroundBackTicks <$> v'

fileRef :: FileReference -> IO (Either String Text)
fileRef z = do
  value <- maybeToEither "Unable to parse" <$> fileReferenceContent z
  return $ surroundBackTicks <$> value

surroundBackTicks :: Text -> Text
surroundBackTicks v = "```\n" <> v <> "```"

printString :: String -> IO ()
printString = print

parseFileReference :: Parser FileReference
parseFileReference = do
  z <- string "{{" >> optional space >> string "file" >> space >> many (noneOf " ")
  l <- optionMaybe $ string " " >> many (noneOf " ")
  l' <- optionMaybe $ string " " >> many (noneOf "}}")
  case (l, l') of
    (Just lStart, Just lEnd) -> return $ FileReference z (Just (read lStart, read lEnd))
    _ -> return $ FileReference z Nothing

parseGitDiffReference :: Parser GitDiffReference
parseGitDiffReference = do
  z <- string "{{" >> optional space >> string "gitDiff" >> space >> many (noneOf " ")
  _ <- many (noneOf "}}")
  return $ GitDiffReference $ cs z

parsePossibleTag :: Parser PossibleTag
parsePossibleTag = do
  z <- string "{{" >> manyTill anyChar (Text.Parsec.try $ string "}}")
  return $ PossibleTag z

return2x :: (Monad m, Monad m2) => a -> m (m2 a)
return2x = return . return

parse' :: Parser a -> String -> String -> Maybe a -- where Foo0 and Bar are the correct types
parse' foo s0 s1 = eitherToMaybe $ parse foo s0 s1
