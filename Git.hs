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

gitCheckout :: Text -> Text -> IO (ExitCode)
gitCheckout c p = do
  (r, _, _) <- runSh $ "git checkout " <> c <> " -- " <> p
  return r

gitPathCommitHash :: Text -> IO (Maybe Text)
gitPathCommitHash x = do
  (r, o, _) <- runSh $ "git rev-list HEAD -- \"" <> x <> "\""
  case (r) of
    ExitSuccess -> case (headMay $ lines o) of
      Nothing -> return $ Nothing
      v' -> return $ v'
    _ -> return Nothing

gitLsFiles :: Text -> IO (Maybe [Prelude.FilePath])
gitLsFiles x = do
  runSh ( "git ls-files -- " <> x) >>= \case
    (ExitSuccess, o, _) -> return . (fmap . fmap) convertString $ return $ lines o
    _ -> return $ Nothing

