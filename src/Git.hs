{-# LANGUAGE OverloadedStrings #-}
--{-# OPTIONS -Wno-unused-imports #-}
--{-# OPTIONS -Wno-unused-matches #-}
{-# LANGUAGE LambdaCase #-}

module Git
  ( module Git
  , module GitTextPartial
  ) where

import Turtle hiding (f, e, x, o, s)
--import Filesystem.Path.CurrentOS ( FilePath(FilePath) )
import Data.Text (Text, lines, intercalate)
import Data.Monoid ((<>))
import Prelude hiding (lines)
import Safe
import Data.String.Conversions
import Hart
import GitTextPartial

gitRevList :: Text -> IO (Either RunShReturn [String])
gitRevList b = do
  x@(r, o, _) <- runSh $ "git rev-list --reverse " <> b
  case (r) of
    ExitSuccess -> pure $ pure $ cs <$> lines o
    _ -> pure $ Left x

gitCheckout :: Text -> IO (ExitCode)
gitCheckout c = do
  (r, _, _) <- runSh $ "git checkout " <> c
  pure r

gitPathCommitHash :: Text -> Text -> IO (Maybe Text)
gitPathCommitHash branch x = do
  (r, o, _) <- runSh $ "git rev-list " <> branch <> " -- \"" <> x <> "\""
  case (r) of
    ExitSuccess -> case (headMay $ lines o) of
      Nothing -> pure $ Nothing
      v' -> pure $ v'
    _ -> pure Nothing

gitLsFiles :: Text -> Text -> IO (Maybe [Prelude.FilePath])
gitLsFiles branch x = do
  runSh ( "git ls-tree -r --name-only " <> branch <> " -- " <> x) >>= \case
    (ExitSuccess, o, _) -> return . (fmap . fmap) convertString $ return $ lines o
    _ -> return $ Nothing

gitRmTag :: Text -> IO (Either (RunShReturn) ())
gitRmTag t = do
  let cmd = intercalate " " ["git tag -d", t]
  runSh cmd >>= \case
    (ExitSuccess, _, _) -> pure $ pure ()
    x -> pure $ Left x

gitTagCommit :: Text -> Text -> IO (Either (RunShReturn) ())
gitTagCommit t c = do
  _ <- gitRmTag t
  let cmd = intercalate " " ["git tag", t, c]
  runSh cmd >>= \case
    (ExitSuccess, _, _) -> pure $ pure ()
    x -> pure $ Left x

gitTagPointsAt :: Text -> IO (Either (RunShReturn) [Text])
gitTagPointsAt c =
  runSh ( "git tag --points-at " <> c) >>= \case
    (ExitSuccess, o, _) -> pure . (fmap . fmap) convertString $ pure $ lines o
    x -> pure $ Left x
