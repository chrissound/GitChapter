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

import Text.Parsec
import Text.Parsec.String
import Text.Parsec ((<?>))
--import Control.Applicative
import Control.Monad.Identity (Identity, guard)
import Control.Exception as Excp
import System.IO.Error
import Data.Either.Extra
import Data.Foldable

type FileLineRange = Maybe(Int, Int)
data FileReference = FileReference String FileLineRange deriving Show
data GitDiffReference = GitDiffReference String deriving Show
data PossibleTag = PossibleTag String deriving Show

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

gitDiffReferenceContent :: GitDiffReference  -> IO (Maybe Text)
gitDiffReferenceContent (GitDiffReference p) =
  (Excp.tryJust (guard . isDoesNotExistError) $ readFile p) >>= \case
    Right v -> do
      return $ Just $ toStrict v
    Left (_) -> return $ Nothing


renderTemplate :: Text -> IO (Either String Text)
renderTemplate x = do
  let l = Data.Text.lines x
  parsedLines <- mapM parseLine l
  case sequence parsedLines of
    Right v -> return $ Right $ Data.Text.unlines $ v
    Left v -> return $ Left v

parseLine :: Text -> IO (Either String Text)
parseLine x = do
  let xStr = convertString x :: String
  let result = asum [
          fileRef <$> parse' parseFileReference "file reference" xStr
        , gitDiff <$> parse' parseGitDiffReference "git diff tag" xStr
        , possibleTag xStr <$ (parse' (parsePossibleTag) "possible tag" $ xStr)
        ] :: Maybe (IO (Either String Text))
  maybe (return2x x) id result
  where
    fileRef z = do
      value <- maybeToEither "Unable to parse" <$> fileReferenceContent z
      return $ (\v' -> "```\n" <> v' <> "```") <$> value
    gitDiff z = do
      print z
      return2x ""
    possibleTag xStr = return (Left $ "Tag that failed to match: " ++ xStr)

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
  return $ GitDiffReference z

parsePossibleTag :: Parser PossibleTag
parsePossibleTag = do
  z <- string "{{" >> manyTill anyChar (Text.Parsec.try $ string "}}")
  return $ PossibleTag z


return2x :: (Monad m, Monad m2) => a -> m (m2 a)
return2x = return . return

parse' :: Parser a -> String -> String -> Maybe a -- where Foo0 and Bar are the correct types
parse' foo s0 s1 = eitherToMaybe $ parse foo s0 s1
