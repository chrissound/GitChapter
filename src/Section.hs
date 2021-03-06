{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# OPTIONS -Wno-unused-imports #-}
module Section where

import Text.Printf
-- import Turtle hiding (f, e, x, o, s, printf, fp)
--import Filesystem.Path.CurrentOS ( FilePath(FilePath) )
import System.Exit
import Data.String (IsString)
import Data.Text as T (Text, unlines, lines)
import Data.Monoid ((<>))
import Prelude hiding (lines)
import Data.String.Conversions
import Text.Regex.Posix
import Control.Monad.Trans.State.Lazy
import Data.HashMap.Strict as HM (empty)
import Control.Monad.Reader
import Data.Maybe
import Data.List (isInfixOf, elemIndex)
import Data.Bool

import Render
import Git
import Hart
import Safe

-- returns the inclusive END commit
getSectionHashOffsets :: Integer -> IO (Either String (CommitRef, Prelude.FilePath) )
getSectionHashOffsets (-1) = do
  (r, o, _) <- runSh "git rev-list --max-parents=0 HEAD"
  case (r, headMay $ lines o) of
    (ExitSuccess, Just v') -> do
      t <- gitTagPointsAt v' >>= \case
        Right x -> pure x
        Left _ -> error "derp"
      pure $ Right (CommitRef (cs v') (fmap cs t), "")
    _                      -> pure $ Left "derp"
getSectionHashOffsets chapterIndex = do
  (r, o, _) <- runSh "git ls-tree -r --name-only master"
  case (r) of
    ExitSuccess -> do
      case (headMay $ filter (isInfixOf $ "chapters/" ++ show chapterIndex ++ "_") (cs <$> T.lines o)) of
        Just fp -> do
          gitPathCommitHash "master" (cs fp) >>= \case
            Just cHash' -> do
              t <- gitTagPointsAt (cHash') >>= \case
                Right x -> pure x
                Left _ -> error "derp"
              pure $ Right (CommitRef (cs cHash') (fmap cs t), fp)
            Nothing     -> pure $ Left $ convertString $ "Unable to retrieve commit for " <> fp
        Nothing -> pure $ Left $ "No chapter file found for chapter " ++ show chapterIndex
    _ -> pure $ Left "derp"

tagChapterOffsets :: (Show a, Eq a, Num a) => a -> CommitRef -> CommitRef -> IO ()
tagChapterOffsets realChapterIndex cHashPrevious' cHash' = do
  allCommits <- gitRevList "master" >>= \case
    Right x -> pure x
    Left _ -> error "derp"
  let commitChild x = case (elemIndex x allCommits) of
        Just x' -> allCommits !! (x' + 1)
        Nothing -> error "derp"
  _ <- gitTagCommit
    ("gch-begin-" <> (cs $ show (realChapterIndex)))
    (cs $ bool
      (commitChild $ cs (commitRef cHashPrevious'))
      (cs $ commitRef cHashPrevious')
      (realChapterIndex == 1)
    )
  _ <- gitTagCommit
    ("gch-end-" <> (cs $ show (realChapterIndex)))
    (cs (commitRef cHash'))
  pure ()


compileChapter :: Text -> IO (Either String Text)
compileChapter filePrefix = do
  _ <- gitCheckout "master"
  case readMay ( (cs filePrefix :: String) =~ ("([0-9]+)" :: String) ) :: Maybe Integer of
    Just sectionKey -> do
      cHashPrevious''' <- getSectionHashOffsets $ sectionKey - 1
      cHash''' <- getSectionHashOffsets sectionKey
      case (cHash''', cHashPrevious''') of
        (Right (cHash', _), Right (cHashPrevious', _)) -> do
          tagChapterOffsets (sectionKey + 1) cHashPrevious' cHash'
        _ -> error "Unable to tag chapter offset commits"
      -- Do this again to get the commits with tags...
      cHashPrevious <- getSectionHashOffsets $ sectionKey - 1
      cHash <- getSectionHashOffsets sectionKey
      case (cHash, cHashPrevious) of
        (Right (cHash', fP'), Right (cHashPrevious', _)) -> do
          print cHash'
          print cHashPrevious'
          tagChapterOffsets (sectionKey + 1) cHashPrevious' cHash'
          gitCheckout (cs $ commitRef cHash') >>= \case
            ExitSuccess -> renderChapterTemplate fP' (cHashPrevious') (cHash') sectionKey filePrefix
            _           -> return $ Left "Git checkout failed"
        (Left e, _) -> pure $ Left $ "Unable to determine `from git commit` hash: " ++ e
        (_, Left e) -> pure $ Left $ "Unable to determine `until git commit` hash: \n" ++ e
    Nothing -> do
      print filePrefix
      error "could not read section key index"

renderChapterTemplate :: (IsString b, Monoid b)
  => Prelude.FilePath -> CommitRef -> CommitRef -> Integer -> b -> IO (Either a b)
renderChapterTemplate fP' cHashPrevious' cHash' sectionKey filePrefix =
  (renderTemplate . cs) <$> (readFile fP') >>= \case
    Right rendered -> do
      let  hc = HartConfig cHashPrevious' cHash' sectionKey
      let hmmm = traverse compilePreOutput rendered :: Hart [Either String (Maybe Text)]
      let fffff = (fmap (T.unlines . catMaybes) . sequence)
      fffff <$> evalStateT (runReaderT hmmm hc)
        (GitChapterState HM.empty)
      >>= \case
        Right (x) -> do
          appendFile compiledOutput $ cs x
          pure $ Right $ "Successful compilation for section " <> filePrefix <> "\n"
        Left e -> fError ("Render error: " ++ e)
    Left e -> fError ("Template error: " ++ e)
  where
    fError vv = sectionError sectionKey (commitRef cHash') (commitRef cHashPrevious') vv

sectionError :: (PrintfArg t3, PrintfArg t2, PrintfArg t1) => t1 -> t2 -> t3 -> [Char] -> IO b
sectionError sk hashPrev hash e = do
    putStrLnError ("Hart compilation error!" :: String)
    putStrLnError (printf "Occurred within section: %d" sk :: String)
    putStrLnError ( printf "Occurred within commit range of (%s - %s)" hashPrev hash :: String)
    error $ e

compiledOutput :: Prelude.FilePath
compiledOutput = "compiledArticle.md"
