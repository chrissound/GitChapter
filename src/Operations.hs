{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.Text (lines, unlines, intercalate)
import Turtle (ExitCode(..))
import Text.Printf
import Control.Arrow
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.HashMap.Strict
import Data.Foldable

import Text.Parsec hiding (parserTrace)
import Control.Monad.Identity (guard)
import Control.Exception as Excp
import System.IO.Error

class Operation a where
  parse :: Parser a
  render :: a -> Hart (Either String (Maybe Text))

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
  render x = liftIO $ fmap (fmap Just) $ maybeToEither ( "Unable to read file: " ++ show x) <$> fileReferenceContent x

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
    z <- string "{{" >> optional space >> string "gitDiff" >> space >> manyTill anyChar (string " }}" <|> string "}}")
    return $ GitDiffReference $ cs z
  render (GitDiffReference z) = gitDiff z >>= \case
      Right x -> return $ Right $ Just x
      Left x -> return $ Left (printf "Unable to retrieve git diff (%s) -  %s" z (show x))

instance Operation Shell where
  parse = do
    let f x = (
          do
            z <- string "{{{{" >> optional space >> string x >> space >> many (noneOf "}}}}}")
            _ <- string "}}}"
            pure z)
    asum [
        Text.Parsec.try $
        f "shell"        >>= pure . Shell ShellSuccessVoid ShellOutputVoid . cs
      , Text.Parsec.try $
        f "shellOutput" >>= pure . Shell ShellSuccessRequired ShellOutput' . cs
      , f "shellSuccess" >>= pure . Shell ShellSuccessRequired ShellOutput' . cs
      ]
  render (Shell required output x) =
    liftIO (runSh x) >>= pure <$> \case
      (ExitSuccess,v,_) -> Right $
        case output of
          ShellOutputVoid -> Nothing
          ShellOutput' -> Just v
      (ExitFailure n,t,e) -> do
        let et = (("The command failed with exit code (" ++ show n ++ "). ") <> cs t <> cs e)
        case required of
          ShellSuccessRequired -> Left $ cs et
          ShellSuccessVoid -> Right $ Just $ cs et

instance Operation GitCommitOffestReference where
  parse = do
    _ <- string "{{" >> optional space >> string "gitCommitOffset" >> optional space >> string "}}"
    return GitCommitOffestReference
  render (GitCommitOffestReference) = do
    hc <- ask
    let sct = intercalate "," (fmap cs $ tagsRef $ fromCommitRef hc) :: Text
    let ect = intercalate "," (fmap cs $ tagsRef $ toCommitRef hc) :: Text
    return $ Right $ Just $ cs $ "```\n"
      <> "Chapter offset\n\n"
      <> "Start Commit: \n"
      <> "SHA: "
      <> (cs $ commitRef $ fromCommitRef hc) <> "\n"
      <> "Tag(s): "
      <> sct <> "\n"
      <> "-------\n"
      <> "End commit: \n"
      <> "SHA: "
      <> (cs $ commitRef $ toCommitRef hc) <> "\n"
      <> "Tag(s): "
      <> ect <> "\n"
      <> "```"

instance Operation SectionHeaderReference where
  parse = do
    let sectionHeaderTag = string "{{" >> optional space >> string "sectionHeader" >> optional space >> string "}}"
    s <- manyTill anyChar (Text.Parsec.try sectionHeaderTag)
    s'' <- many anyChar
    return $ SectionHeaderReference s s''
  render (SectionHeaderReference prefix suffix) = do
   (HartConfig _ _ section) <- ask
   return $ Right $ Just $ cs $ prefix ++ "Section " ++ show section ++ suffix

instance Operation GHCiReference where
  parse = (\(x, y) -> GHCiReference x y) <$> (first cs . second (cs <$>)) <$> parseGhciTag
  render (GHCiReference x s) =
    (Right . Just . ((<>) "```Haskell\n") . (<> "\n```") )
    <$>
    ( do
      liftIO $ putStrLn $ "Using GHCI session of: " ++ show s
      case s of
        Just s' -> (do
          (GitChapterState gcs) <- lift $ get
          ghci <- case Data.HashMap.Strict.lookup (cs  s') gcs of
            Just s'' -> pure $ s''
            Nothing -> liftIO $ initializeGHCiSession
          lift $ put (GitChapterState (insert (cs s') ghci gcs))
          liftIO $ runGhciSession ghci x
          )
        Nothing -> liftIO $ runGhci x
    )

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
    liftIO $ fileRef (FileReference f $ FileLineRange Nothing) >>= \case
      Right lns -> do
        section <- getSection (cs s) (Data.Text.lines lns)
        case length $ Data.Text.lines section of
          0 -> return $ Left $ show fs ++ " - FileSection returned no text"
          _ -> return $ Right $ Just section
      Left e -> return $ Left e
