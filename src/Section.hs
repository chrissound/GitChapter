{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
module Section where

import Text.Printf
import Turtle hiding (f, e, x, o, s, printf)
--import Filesystem.Path.CurrentOS ( FilePath(FilePath) )
import Data.Text as T (Text, unlines, lines)
import Data.Monoid ((<>))
import Prelude hiding (lines)
import qualified Control.Foldl as Fold
import Data.String.Conversions
import Filesystem.Path.CurrentOS (encodeString)
import Text.Regex.Posix
import Control.Monad.Trans.State.Lazy
import Data.HashMap.Strict as HM (empty)

import Render
import Git
import Hart
import Safe


getSectionHashOffsets :: Integer -> IO (Either String (Text, Turtle.FilePath) )
getSectionHashOffsets (-1) = do
  (r, o, _) <- runSh "git rev-list --max-parents=0 HEAD"
  case (r) of
    ExitSuccess -> case (headMay $ lines o) of
      Nothing -> return $ Left "derp"
      Just v' -> return $ Right (v', "")
    _ -> return $ Left "derp"
getSectionHashOffsets filePrefix = do
  let v = find (prefix $ fromString $ cs articleDir ++ (show filePrefix ++ "_")) (fromString $ cs articleDir)
  fP <- fold (v) Fold.head
  case fP of
    Just fP' -> do
      let fP'' = convertString $ encodeString $ fP'
      gitPathCommitHash fP'' >>= \case
        Just cHash' -> return $ Right (cHash', fP')
        Nothing -> return $ Left $ convertString $ "Unable to retrieve commit for " <> fP''
    Nothing -> return $ Left $ "No chater file found for chapter " ++ show filePrefix

compileChapter :: Text -> IO (Either String Text)
compileChapter filePrefix = do
  case readMay ( (cs filePrefix :: String) =~ ("([0-9]+)" :: String) ) :: Maybe Integer of
    Just sectionKey -> do
      cHashPrevious <- getSectionHashOffsets $ sectionKey - 1
      cHash <- getSectionHashOffsets sectionKey
      case (cHash, cHashPrevious) of
        (Right (cHash', fP'), Right (cHashPrevious', _)) ->
          gitCheckout cHash' "src/" >>= \case
            ExitSuccess -> renderChapterTemplate fP' cHashPrevious' cHash' sectionKey filePrefix where
            _ -> return $ Left ":("
        (Left e, _) -> pure $ Left $ "Unable to determine `from git commit` hash: " ++ e
        (_, Left e) -> pure $ Left $ "Unable to determine `until git commit` hash: " ++ e
    Nothing -> do
      print filePrefix
      error "could not read section key index"

renderChapterTemplate :: (ConvertibleStrings t2 String, ConvertibleStrings t3 String, PrintfArg t2, PrintfArg t3, IsString b, Monoid b)
  => Turtle.FilePath -> t3 -> t2 -> Integer -> b -> IO (Either a b)
renderChapterTemplate fP' cHashPrevious' cHash' sectionKey filePrefix =
  renderTemplate <$> readTextFile fP' >>= \case
    Right rendered -> do
      let  hc = HartConfig (cs cHashPrevious') (cs cHash') sectionKey
      evalStateT
        (runReaderT ((fmap . fmap) T.unlines $ sequence <$> traverse compilePreOutput rendered) hc)
        (GitChapterState HM.empty)
      >>= \case
        Right x -> do
          appendFile compiledOutput $ cs x
          pure $ Right $ "Successful compilation for section " <> filePrefix <> "\n"
        Left e -> sectionError sectionKey cHash' cHashPrevious' ("Render error: " ++ e)
    Left e -> sectionError sectionKey cHash' cHashPrevious' ("Template error: " ++ e)

sectionError :: (PrintfArg t3, PrintfArg t2, PrintfArg t1) => t1 -> t2 -> t3 -> [Char] -> IO b
sectionError sk hashPrev hash e = do
    putStrLnError ("Hart compilation error!" :: String)
    putStrLnError (printf "Occurred within section: %d" sk :: String)
    putStrLnError ( printf "Occurred within commit range of (%s - %s)" hashPrev hash :: String)
    error $ show e

compiledOutput :: Prelude.FilePath
compiledOutput = "compiledArticle.md"
