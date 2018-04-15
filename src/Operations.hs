{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
module Operations (
  module Operations,
  module Operations.Parsers,
  )where

import Operations.Parsers
import FileSection
import Hart
import Git
import GHCi
import Text.Parsec.String
import Data.Either.Extra
import Data.Text (lines, unlines)
import Turtle (ExitCode(..))
import Text.Printf
import Control.Arrow

import Text.Parsec hiding (parserTrace)
import Control.Monad.Identity (guard)
import Control.Exception as Excp
import System.IO.Error

class Operation a where
  parse :: Parser a
  render :: a -> Hart (Either String Text)

data Reference' = forall a. (Operation a, Show a) => Reference' a

instance Show Reference' where
  show (Reference' a) = show a


instance Operation FileReference where
  parse = do
    z <- between (string "{{" >> optional space >> string "file" >> space) (string "}}") parseFileAndLimits
    case (z) of
      (fPath, Just lStart, Just lEnd) -> return $ FileReference fPath (FileLineRange $ Just (lStart, lEnd))
      (fPath, Nothing, Nothing) -> return $ FileReference fPath (FileLineRange Nothing)
      _ -> fail "Unable to read start and end file reference"
  render x = lift $ maybeToEither "" <$> fileReferenceContent x

fileReferenceContent :: FileReference -> IO (Maybe Text)
fileReferenceContent (FileReference p flRange) =
  (Excp.tryJust (guard . isDoesNotExistError) $ readFile p) >>= \case
    Right v -> do
      case flRange of
        FileLineRange (Just (s, e)) -> return $ Just (Data.Text.unlines $ slice s e $ Data.Text.lines $ cs v)
        FileLineRange Nothing -> return $ Just $ cs v
    Left (_) -> return $ Nothing

fileRef :: FileReference -> IO (Either String Text)
fileRef z@(FileReference fr fr') = do
  value <- maybeToEither (printf "Unable to retrieve file (%s %s)" fr (show fr')) <$> fileReferenceContent z
  return $ value

instance Operation GitDiffReference where
  parse = do
    z <- string "{{" >> optional space >> string "gitDiff" >> space >> many (noneOf " ")
    _ <- many (noneOf "}}")
    return $ GitDiffReference $ cs z
  render (GitDiffReference z) = gitDiff z >>= \case
      Right x -> return $ Right $ x
      Left x -> return $ Left (printf "Unable to retrieve git diff (%s) -  %s" z (show x))

instance Operation ShellOutput where
  parse = do
    z <- string "{{{{" >> optional space >> string "shellOutput" >> space >> many (noneOf "}}}}}")
    _ <- string "}}}"
    return $ ShellOutput $ cs z
  render (ShellOutput x) = lift (runSh x) >>= pure <$> \case
    (ExitSuccess,t,_) -> Right $ cs t
    (ExitFailure n,t,e) -> Left $ cs $ cs ("The command failed with exit code (" ++ show n ++ "). ") <> t <> e


instance Operation GitCommitOffestReference where
  parse = do
    _ <- string "{{" >> optional space >> string "gitCommitOffset" >> optional space >> string "}}"
    return GitCommitOffestReference
  render (GitCommitOffestReference) = do
    hc <- ask
    return $ Right $ cs $ "```\n"
      <> "Git From Commit: \n"
      <> hartConfigFromHash hc <> "\n\n"
      <> "Git Until Commit: \n"
      <> hartConfigUntilHash hc <> "\n"
      <> "```"

instance Operation SectionHeaderReference where
  parse = do
    let sectionHeaderTag = string "{{" >> optional space >> string "sectionHeader" >> optional space >> string "}}"
    s <- manyTill anyChar (Text.Parsec.try sectionHeaderTag)
    s'' <- many anyChar
    return $ SectionHeaderReference s s''
  render (SectionHeaderReference prefix suffix) = do
   (HartConfig _ _ section) <- ask
   return $ Right $ cs $ prefix ++ "Section " ++ show section ++ suffix

instance Operation GHCiReference where
  parse = (\(x, y) -> GHCiReference x y) <$> (first cs . second (cs <$>)) <$> parseGhciTag
  render (GHCiReference x _) = lift $
    (Right . (<> "\n````Haskell") . ((<>) "````\n") )  <$> runGhci x

instance Operation FileSection where
  parse = do
    (f,s) <- between (string "{{" >> optional space >> string "fileSection" >> space) (string "}}")
      (do
        f <- many (noneOf " }")
        _ <- space
        s <- many (noneOf " }")
        _ <- optional space
        return $ (f, s)
      )
    return $ FileSection f s
  render fs@(FileSection f s) = do
    lift $ fileRef (FileReference f $ FileLineRange Nothing) >>= \case
      Right lns -> do
        section <- getSection (cs s) (Data.Text.lines lns)
        case length $ Data.Text.lines section of
          0 -> return $ Left $ show fs ++ " - FileSection returned no text"
          _ -> return $ Right section
      Left e -> return $ Left e
