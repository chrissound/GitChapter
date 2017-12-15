{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# LANGUAGE LambdaCase #-}

module Git where

import Turtle hiding (f, e, x, o, s)
--import Filesystem.Path.CurrentOS ( FilePath(FilePath) )
import Data.Text (Text, lines)
import Data.Monoid ((<>))
import Prelude hiding (lines)
import Safe
import System.Posix.Directory
import qualified Control.Foldl as Fold
import Data.String.Conversions
import Filesystem.Path.CurrentOS (encodeString, fromText)
import Render
import Text.Mustache
import Data.Bool
import System.Directory
import Data.Text (splitOn)

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

runSh :: Text -> IO (ExitCode, Text, Text)
runSh x = do
  putStrLn "Running command: "
  putStrLn $ "  " ++ convertString x
  (r,o,e) <- shellStrictWithErr x empty
  print r
  print o
  print e
  return (r,o,e)

gitLsFiles :: Text -> IO (Maybe [Prelude.FilePath])
gitLsFiles x = do
  runSh ( "git ls-files -- " <> x) >>= \case
    (ExitSuccess, o, _) -> return . (fmap . fmap) convertString $ return $ lines o
    _ -> return $ Nothing

gitDiff :: Text -> IO (Maybe Text)
gitDiff x = do
  (r, o, _) <- runSh $ "git diff HEAD~1..HEAD-- \"" <> x <> "\""
  case (r) of
    ExitSuccess -> case (headMay $ lines o) of
      Nothing -> return $ Nothing
      v' -> return $ v'
    _ -> return Nothing
