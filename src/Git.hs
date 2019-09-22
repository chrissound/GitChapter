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
import Data.Text (Text, lines)
import Data.Monoid ((<>))
import Prelude hiding (lines)
import Safe
import Data.String.Conversions
import Hart
import GitTextPartial

gitCheckout :: Text -> IO (ExitCode)
gitCheckout c = do
  (r, _, _) <- runSh $ "git checkout " <> c
  return r

gitPathCommitHash :: Text -> Text -> IO (Maybe Text)
gitPathCommitHash branch x = do
  (r, o, _) <- runSh $ "git rev-list " <> branch <> " -- \"" <> x <> "\""
  case (r) of
    ExitSuccess -> case (headMay $ lines o) of
      Nothing -> return $ Nothing
      v' -> return $ v'
    _ -> return Nothing

gitLsFiles :: Text -> Text -> IO (Maybe [Prelude.FilePath])
gitLsFiles branch x = do
  runSh ( "git ls-tree -r --name-only " <> branch <> " -- " <> x) >>= \case
    (ExitSuccess, o, _) -> return . (fmap . fmap) convertString $ return $ lines o
    _ -> return $ Nothing

