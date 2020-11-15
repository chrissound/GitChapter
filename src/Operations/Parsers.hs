{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Operations.Parsers where

import Hart
import Text.Parsec.String
import Data.Either.Extra
import Safe
import Data.Bool
import qualified Data.Text as Text
-- import Operations.Types

import Text.Parsec hiding (parserTrace)

-- import Control.Monad.Identity

stringParser :: (Monad m) => ParsecT Text u m a -> ParsecT String u m a
stringParser p = mkPT $ (\st -> (fmap . fmap . fmap) outReply $ runParsecT p (inState st))
  where inState :: State String u -> State Text u
        inState  (State i pos u) = State (Text.pack i) pos u
        outReply :: Reply Text u a -> Reply String u a
        outReply (Ok a (State i pos u) e) = Ok a (State (Text.unpack i) pos u) e
        outReply (Error e) = Error e



eofString :: Parser String
eofString = do
  eof
  return ""

parserTrace :: (Stream s m t, Show t) => String -> ParsecT s u m ()
parserTrace _ = return ()

parseGhciTag :: Parser (String, (Maybe String))
parseGhciTag = do
    parserTrace "DEBUG INIT parseGhciTag"
    _ <- string "ghci" >> optional (char ' ')
    s <- optionMaybe $ try $ manyTill anyToken (endOfLine)
    z <- many (anyToken)
    parserTrace "DEBUG SUCCESS parseGhciTag"
    pure (z :: String, bool (s) Nothing (s == Just ""))

parseFileAndLimits :: Parser (String, Maybe Int, Maybe Int)
parseFileAndLimits = do
      l <- many (noneOf " }")
      _ <- optional space
      l' <- optionMaybe $ many1 digit
      _ <- optional space
      l'' <- optionMaybe $ many1 digit
      _ <- optional space
      return (l, l' >>= readMay, l'' >>= readMay)


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


parse' :: Parser a -> String -> String -> Maybe a -- where Foo0 and Bar are the correct types
parse' foo s0 s1 = eitherToMaybe $ Text.Parsec.parse foo s0 s1

-- parseSectionHeader :: Parser SectionHeaderReference
-- parseSectionHeader = do
  -- void $ string "sectionHeader" <|> string "chapterHeader"
  -- pure $ SectionHeaderReference

-- parsePossibleTag :: Parser PossibleTag
-- parsePossibleTag = do
  -- z <- string "{{" >> manyTill anyChar (try $ string "}}")
  -- return $ PossibleTag z

