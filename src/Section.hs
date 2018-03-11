{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
import Render
import Text.Regex.Posix
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
  let v = find (prefix $ fromString $ dir ++ (show filePrefix ++ "_")) (fromString dir)
        where
          dir = "sections/"
  fP <- fold (v) Fold.head
  case fP of
    Just fP' -> do
      let fP'' = convertString $ encodeString $ fP'
      gitPathCommitHash fP'' >>= \case
        Just cHash' -> return $ Right (cHash', fP')
        Nothing -> return $ Left $ convertString $ "Unable to retrieve commit for " <> fP''
    Nothing -> return $ Left "Nope"

compileSection :: Text -> IO (Either String Text)
compileSection filePrefix = do
  case readMay ( (cs filePrefix :: String) =~ ("([0-9]+)" :: String) ) :: Maybe Integer of
    Just sectionKey -> do
      cHashPrevious <- getSectionHashOffsets $ sectionKey - 1
      cHash <- getSectionHashOffsets sectionKey
      case (cHash, cHashPrevious) of
        (Right (cHash', fP'), Right (cHashPrevious', _)) ->
          gitCheckout cHash' "src/" >>= \case
            ExitSuccess ->
              renderTemplate <$> readTextFile fP' >>= \case
                Right rendered -> do
                  let  hc = HartConfig (cs cHashPrevious') (cs cHash') sectionKey
                  runReaderT ((fmap . fmap) T.unlines $ sequence <$> traverse compilePreOutput rendered) hc >>= \case
                    Right x -> do
                      appendFile compiledOutput $ cs x
                      pure $ Right $ "Successful compilation for section " <> filePrefix
                    Left x -> sectionError sectionKey cHash' cHashPrevious' x
                Left e -> sectionError sectionKey cHash' cHashPrevious' e
            _ -> return $ Left ":("
        (Left e, _) -> pure $ Left $ "Unable to determine `from git commit` hash: " ++ e
        (_, Left e) -> pure $ Left $ "Unable to determine `until git commit` hash: " ++ e
    Nothing -> do
      print filePrefix
      error "could not read section key index"

sectionError :: (PrintfArg t3, PrintfArg t2, PrintfArg t1) => t1 -> t2 -> t3 -> [Char] -> IO b
sectionError sk hashPrev hash e = do
    putStrLn ("Hart compilation error!" :: String)
    putStrLn (printf "Occurred within section: %d" sk :: String)
    putStrLn ( printf "Occurred within commit range of (%s - %s)" hashPrev hash :: String)
    error e

compiledOutput :: Prelude.FilePath
compiledOutput = "compiledArticle.md"
