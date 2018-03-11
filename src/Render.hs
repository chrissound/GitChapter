--{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Render where

import Prelude hiding (readFile)
import Data.Text (Text, lines, unlines)
import Data.String.Conversions
import Data.Text.Lazy.IO
import Data.Text.Lazy (toStrict)
import Control.Monad.Trans
import Turtle (ExitCode(..))
import Text.Printf
import Safe
--import System.IO.Unsafe

import Text.Parsec hiding (parserTrace)
import Text.Parsec.String
--import Control.Applicative
import Control.Monad.Identity (guard)
import Control.Exception as Excp
import System.IO.Error
import Data.Either.Extra
import Data.Foldable
import Git
import Hart

type FileLineRange = Maybe(Int, Int)
data FileReference = FileReference String FileLineRange deriving Show
data GitDiffReference = GitDiffReference Text deriving Show
data GitCommitOffestReference = GitCommitOffestReference deriving Show
data ShellOutput = ShellOutput Text deriving Show
data PossibleTag = PossibleTag String deriving Show
data SectionHeaderReference = SectionHeaderReference String String deriving Show

data Reference =
    FileRef FileReference
  | GitRef GitDiffReference
  | GitCommitOffestRef GitCommitOffestReference
  | PossibleRef PossibleTag
  | ShellOutputRef ShellOutput
  | SectionHeaderRef SectionHeaderReference
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

prepareLineOutput :: Text -> PreLineOutput
prepareLineOutput l = case parseLine l of
  Nothing -> Raw l
  Just x -> RefOutput x

compilePreOutput :: PreLineOutput -> Hart (Either String Text)
compilePreOutput (Raw x) = return $ Right x
compilePreOutput (RefOutput (FileRef x)) = lift $ maybeToEither "" <$> fileReferenceContent x
compilePreOutput (RefOutput (GitRef x)) = gitDiff' x
compilePreOutput (RefOutput (GitCommitOffestRef x)) = gitCommitRefence x
compilePreOutput (RefOutput (ShellOutputRef (ShellOutput x))) = lift $ shellOutput x >>= return . Right
compilePreOutput (RefOutput (PossibleRef (PossibleTag x))) = return $ Left $ ("PossibleRef found of:" ++ x)
compilePreOutput (RefOutput (SectionHeaderRef (SectionHeaderReference prefix suffix))) = do
  (HartConfig _ _ section) <- ask
  return $ Right $ cs $ prefix ++ "Section " ++ show section ++ suffix

shellOutput :: Text -> IO Text
shellOutput x = runSh x >>= \case
  (ExitSuccess,t,_) -> return t
  (ExitFailure n,t,e) -> return $ cs ("The command failed with exit code (" ++ show n ++ "). ") <> t <> e

renderTemplate :: Text -> Either String [PreLineOutput]
renderTemplate x = do
  case (transformInnerSection <$> (parse parseSection "parseSection" $ cs x)) of
    Right lns -> do
      case filter (\z -> case z of; Just (PossibleRef _) -> True; _ -> False) $ parseLine <$> lns of
        [] -> Right $ prepareLineOutput <$> lns
        ((Just (PossibleRef r)):_) -> Left $ show (r)
        _ -> error "This should not be possible..."
    Left e -> do
      -- return $! unsafePerformIO $! do
      --   print $ (Data.Text.lines x) !! 0
      --   print $ (Data.Text.lines x) !! 1
      --   print $ parse parseSection "parseSection" $ cs $ Data.Text.unlines $ take 61 $ (Data.Text.lines x)
      --   _ <- error "???"
      --   print $ parse parseSection "parseSection" $ cs x
      Left $ cs ("An internal error has occurred, please report this bug. Unable to parse inner sections within section: " ++ show e)

transformInnerSection :: [SectionBlock] -> [Text]
transformInnerSection ([]) = []
transformInnerSection (x:xs) = case x of
        SectionRaw b -> Data.Text.lines b ++ (transformInnerSection xs)
        SectionGHCi b -> b                 : (transformInnerSection xs)

parseLine :: Text -> Maybe Reference
parseLine x = do
  let xStr = convertString x :: String
  asum [
          FileRef <$> parse' parseFileReference "file reference" xStr
        , GitRef <$> parse' parseGitDiffReference "git diff tag" xStr
        , GitCommitOffestRef <$> parse' parseGitCommitOffest "git commit offset" xStr
        , ShellOutputRef <$> parse' parseShellOutputTag "shellOutput tag" xStr
        , SectionHeaderRef <$> (parse' (parseSectionHeader) "section header" $ xStr)
        , PossibleRef <$> (parse' (parsePossibleTag) "possible tag" $ xStr)
        , PossibleRef <$> (parse' (parsePossibleTag) "possible tag" $ xStr)
        ]

gitDiff' :: GitDiffReference -> Hart (Either String Text)
gitDiff' (GitDiffReference z) = do
  gitDiff z >>= \case
    Right x -> return $ Right $ x
    Left x -> return $ Left (printf "Unable to retrieve git diff (%s) -  %s" z (show x))

fileRef :: FileReference -> IO (Either String Text)
fileRef z@(FileReference fr fr') = do
  value <- maybeToEither (printf "Unable to retrieve file (%s %s)" fr (show fr')) <$> fileReferenceContent z
  return $ value

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

parseGitCommitOffest :: Parser GitCommitOffestReference
parseGitCommitOffest = do
  _ <- string "{{" >> optional space >> string "gitCommitOffset" >> optional space >> string "}}"
  return GitCommitOffestReference

parseSectionHeader :: Parser SectionHeaderReference
parseSectionHeader = do
  let sectionHeaderTag = string "{{" >> optional space >> string "sectionHeader" >> optional space >> string "}}"
  s <- manyTill anyChar (Text.Parsec.try sectionHeaderTag)
  s'' <- many anyChar
  return $ SectionHeaderReference s s''

parseFileAndLimits :: Parser (String, Maybe Int, Maybe Int)
parseFileAndLimits = do
      l <- many (noneOf " ")
      _ <- optional space
      l' <- optionMaybe $ many1 digit
      _ <- optional space
      l'' <- optionMaybe $ many1 digit
      _ <- optional space
      return (l, l' >>= readMay, l'' >>= readMay)

parseFileReference :: Parser FileReference
parseFileReference = do
  z <- between (string "{{" >> optional space >> string "file" >> space) (string "}}") parseFileAndLimits
  case (z) of
    (fPath, Just lStart, Just lEnd) -> return $ FileReference fPath (Just (lStart, lEnd))
    (fPath, Nothing, Nothing) -> return $ FileReference fPath Nothing
    _ -> fail "Unable to read start and end file reference"

parseGitDiffReference :: Parser GitDiffReference
parseGitDiffReference = do
  z <- string "{{" >> optional space >> string "gitDiff" >> space >> many (noneOf " ")
  _ <- many (noneOf "}}")
  return $ GitDiffReference $ cs z

parsePossibleTag :: Parser PossibleTag
parsePossibleTag = do
  z <- string "{{" >> manyTill anyChar (Text.Parsec.try $ string "}}")
  return $ PossibleTag z

parseShellOutputTag :: Parser ShellOutput
parseShellOutputTag = do
  z <- string "{{{{" >> optional space >> string "shellOutput" >> space >> many (noneOf "}}}}}")
  _ <- string "}}}"
  return $ ShellOutput $ cs z

parseGhciTag :: Parser String
parseGhciTag = do
  parserTrace "DEBUG INIT parseGhciTag"
  z <- string "{{{" >> optional space >> string "ghci" >> optional space >> many (noneOf "}}}")
  _ <- string "}}}"
  parserTrace "DEBUG SUCCESS parseGhciTag"
  return $ cs z

parserTrace :: (Stream s m t, Show t) => String -> ParsecT s u m ()
parserTrace _ = return ()

data SectionBlock = SectionRaw Text | SectionGHCi Text deriving Show

eofString :: Parser String
eofString = do
  eof
  return ""

parseSection :: Parser [SectionBlock]
parseSection =
  optionMaybe eof >>= \case
    Just () -> return []
    Nothing -> do
      isGhci <- optionMaybe parseGhciTag
      block <- case isGhci of
            Just ghci' -> return $ SectionGHCi $ cs ghci'
            Nothing -> do
              manyTill anyToken (lookAhead $ Text.Parsec.try $ choice [parseGhciTag, eofString])
              >>= return . SectionRaw . cs
      optionMaybe parseSection >>= \case
        Just sndBlock -> return ( block : sndBlock)
        Nothing -> return [block]

return2x :: (Monad m, Monad m2) => a -> m (m2 a)
return2x = return . return

parse' :: Parser a -> String -> String -> Maybe a -- where Foo0 and Bar are the correct types
parse' foo s0 s1 = eitherToMaybe $ parse foo s0 s1

-- return $! unsafePerformIO $ do
--       print ("hmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm" :: String)
-- () <- unsafePerformIO $! do
--       error ("yolo" :: String)
      -- _ <- many anyChar >>= fail . ("Remainng: "++)
