{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-orphans #-}

module Operations
  ( module Operations
  , module Operations.Parsers
  )
where

import           Operations.Parsers
import           FileSection
import           Hart
import           Git
import           GHCi
-- import Text.Parsec.String
import           Data.Either.Extra
import           Data.Text                      ( lines
                                                , unlines
                                                , intercalate
                                                )
import           Turtle                         ( ExitCode(..) )
import           Text.Printf
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Lazy
import           Data.HashMap.Strict
import           Data.Foldable

import           Text.Parsec             hiding ( parserTrace )
import           Control.Monad.Identity         ( guard )
import           Control.Exception             as Excp
import           System.IO.Error
import           Operations.Types


instance Operation FileReference where
  parse = do
    void $ string "file"
    z <- parseFileAndLimits
    case (z) of
      (fPath, Just lStart, Just lEnd) ->
        return $ FileReference fPath (FileLineRange $ Just (lStart, lEnd))
      (fPath, Nothing, Nothing) ->
        return $ FileReference fPath (FileLineRange Nothing)
      _ -> fail "Unable to read start and end file reference"
  render x =
    liftIO
      $   fmap (fmap Just)
      $   maybeToEither ("Unable to read file: " ++ show x)
      <$> fileReferenceContent x

fileReferenceContent :: FileReference -> IO (Maybe Text)
fileReferenceContent (FileReference p flRange) =
  (Excp.tryJust (guard . isDoesNotExistError) $ readFile p) >>= \case
    Right v -> do
      case flRange of
        FileLineRange (Just (s, e)) ->
          return $ Just (Data.Text.unlines $ slice s e $ Data.Text.lines $ cs v)
        FileLineRange Nothing -> return $ Just $ cs v
    Left (_) -> return $ Nothing

fileRef :: FileReference -> IO (Either String Text)
fileRef z@(FileReference fr fr') = do
  value <-
    maybeToEither (printf "Unable to retrieve file (%s %s)" fr (show fr'))
      <$> fileReferenceContent z
  return $ value

instance Operation GitDiffReference where
  parse = do
    z <- string "gitDiff" >> space >> many anyChar
    pure $ GitDiffReference $ cs z
  render (GitDiffReference z) = gitDiff z >>= \case
    Right x -> return $ Right $ Just x
    Left x ->
      return $ Left (printf "Unable to retrieve git diff (%s) -  %s" z (show x))

instance Operation Shell where
  parse = do
    let f x = string x >> space >> many anyChar
        shelly t v = Text.Parsec.try $ f t >>= pure . v . cs
    asum
      [ 
        shelly "shell" $ Shell ShellSuccessVoid ShellOutputVoid
      , shelly "shell'" $ Shell ShellSuccessRequired ShellOutput'
      , shelly "shellOut" $ Shell ShellSuccessVoid ShellOutput'
      , shelly "shellOut'" $ Shell ShellSuccessRequired ShellOutput'
      ]
  render (Shell required output x) = liftIO (runSh x) >>= pure <$> \case
    (ExitSuccess, v, _) -> Right $ case output of
      ShellOutputVoid -> Nothing
      ShellOutput'    -> Just v
    (ExitFailure n, t, e) -> do
      let et =
            (  ("The command failed with exit code (" ++ show n ++ "). ")
            <> cs t
            <> cs e
            )
      case required of
        ShellSuccessRequired -> Left $ cs et
        ShellSuccessVoid     -> Right $ Just $ cs et

instance Operation GitCommitOffestReference where
  parse = string "gitCommitOffset" >> pure GitCommitOffestReference
  render (GitCommitOffestReference) = do
    hc <- ask
    let sct = intercalate "," (fmap cs $ tagsRef $ fromCommitRef hc) :: Text
    let ect = intercalate "," (fmap cs $ tagsRef $ toCommitRef hc) :: Text
    return
      $  Right
      $  Just
      $  cs
      $  "```\n"
      <> "Chapter offset\n\n"
      <> "Start Commit: \n"
      <> "SHA: "
      <> (cs $ commitRef $ fromCommitRef hc)
      <> "\n"
      <> "Tag(s): "
      <> sct
      <> "\n"
      <> "-------\n"
      <> "End commit: \n"
      <> "SHA: "
      <> (cs $ commitRef $ toCommitRef hc)
      <> "\n"
      <> "Tag(s): "
      <> ect
      <> "\n"
      <> "```"

instance Operation SectionHeaderReference where
  parse = do
    void $ string "sectionHeader" <|> string "chapterHeader"
    pure $ SectionHeaderReference
  render (SectionHeaderReference) = do
    (HartConfig _ _ section) <- ask
    return $ Right $ Just $ cs $ "Chapter " ++ show section

instance Operation GHCiReference where
  parse =
    (\(x, y) -> GHCiReference x y)
      <$> (first cs . second (cs <$>))
      <$> parseGhciTag
  render (GHCiReference x s) =
    (Right . Just . ((<>) "```Haskell\n") . (<> "\n```"))
      <$> (do
            liftIO $ putStrLn $ "Using GHCI session of: " ++ show s
            case s of
              Just s' ->
                (do
                  (GitChapterState gcs) <- lift $ get
                  ghci <- case Data.HashMap.Strict.lookup (cs s') gcs of
                    Just s'' -> pure $ s''
                    Nothing  -> liftIO $ initializeGHCiSession
                  lift $ put (GitChapterState (insert (cs s') ghci gcs))
                  liftIO $ runGhciSession ghci x
                )
              Nothing -> liftIO $ runGhci x
          )

instance Operation FileSection where
  parse = do
    (f, s) <- between
      (string "{{" >> optional space >> string "fileSection" >> space)
      (string "}}")
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

renderAst :: GitChapterAST -> Hart (Either String (Maybe Text))
renderAst (GCASTFileReference x) = render x
renderAst (GCASTFileSection x) = render x
renderAst (GCASTGitDiffReference x) = render x
renderAst (GCASTGitCommitOffestReference x) = render x
renderAst (GCASTShell x) = render x
renderAst (GCASTSectionHeaderReference x) = render x
renderAst (GCASTGHCiReference x) = render x
renderAst _ = undefined
