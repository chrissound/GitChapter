{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Operations.Parsers where

import Hart

import Text.Parsec.String
import Data.Either.Extra
import Safe
import Data.Bool

import Text.Parsec hiding (parserTrace)

data SectionBlock = SectionRaw Text | SectionGHCi Text (Maybe Text) deriving (Show, Eq)
data FileLineRange = FileLineRange (Maybe(Int, Int)) deriving (Show, Eq)
data FileReference = FileReference String FileLineRange deriving (Show, Eq)
data FileSection = FileSection String String deriving (Show, Eq)
data GitDiffReference = GitDiffReference Text deriving (Show, Eq)
data GitCommitOffestReference = GitCommitOffestReference deriving (Show, Eq)
data Shell = Shell (ShellSuccess) (ShellOutput') Text deriving (Show, Eq)
data ShellOutput' = ShellOutputVoid | ShellOutput' deriving (Show, Eq)
data ShellSuccess = ShellSuccessVoid | ShellSuccessRequired deriving (Show, Eq)
data PossibleTag = PossibleTag String deriving (Show, Eq)
data SectionHeaderReference = SectionHeaderReference String String deriving (Show, Eq)
data GHCiReference = GHCiReference Text (Maybe Text) deriving (Show, Eq)

parseSection :: Parser [SectionBlock]
parseSection =
  optionMaybe eof >>= \case
    Just () -> return []
    Nothing -> do
      isGhci <- optionMaybe parseGhciTag
      block <- case isGhci of
            Just ghci' -> return $ SectionGHCi ((cs . fst) $ ghci') (((cs <$>) . snd) $ ghci')
            Nothing -> do
              manyTill anyToken (lookAhead $ try $ choice [const () <$> parseGhciTag, const () <$> eofString])
              >>= return . SectionRaw . cs
      optionMaybe parseSection >>= \case
        Just sndBlock -> return ( block : sndBlock)
        Nothing -> return [block]

eofString :: Parser String
eofString = do
  eof
  return ""

parserTrace :: (Stream s m t, Show t) => String -> ParsecT s u m ()
parserTrace _ = return ()

parseGhciTag :: Parser (String, (Maybe String))
parseGhciTag = do
    parserTrace "DEBUG INIT parseGhciTag"
    _ <- string "{{{" >> optional space >> string "ghci" >> optional (char ' ')
    s <- optionMaybe $ try $ manyTill anyToken (endOfLine)
    z <- many (noneOf "}}}")
    _ <- string "}}}"
    parserTrace "DEBUG SUCCESS parseGhciTag"
    return (z :: String, bool (s) Nothing (s == Just ""))

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

parseSectionHeader :: Parser SectionHeaderReference
parseSectionHeader = do
  let sectionHeaderTag = string "{{" >> optional space >> string "sectionHeader" >> optional space >> string "}}"
  s <- manyTill anyChar (try sectionHeaderTag)
  s'' <- many anyChar
  return $ SectionHeaderReference s s''

parsePossibleTag :: Parser PossibleTag
parsePossibleTag = do
  z <- string "{{" >> manyTill anyChar (try $ string "}}")
  return $ PossibleTag z
