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

type FileLineRange = Maybe(Int, Int)
data FileReference = FileReference String FileLineRange deriving Show
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

renderMustacheTemplate :: Text -> Value -> Maybe Text
renderMustacheTemplate name binding = case (compileTemplate "" (convertString name)) of
  Right template -> Just $ substituteValue template binding
  Left _ -> Nothing

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
  case parse (parseFileReference) "file reference" xStr of
    Right z -> do
      print z
      fileReferenceContent z >>= \case
        Just v' -> return2x $ "```\n" <> v' <> "````"
        Nothing -> return $ Left "Unable to parse"
    Left _ -> case parse (parsePossibleTag) "possible tag" xStr of
        Right _ -> return $ Left $ "Tag that failed to match: " ++ xStr
        Left _ -> return2x x

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

parsePossibleTag :: Parser PossibleTag
parsePossibleTag = do
  z <- string "{{" >> manyTill anyChar (Text.Parsec.try $ string "}}")
  return $ PossibleTag z


return2x :: (Monad m, Monad m2) => a -> m (m2 a)
return2x = return . return
