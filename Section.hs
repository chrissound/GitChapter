{-# LANGUAGE OverloadedStrings #-}
--{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
module Section where

import Turtle hiding (f, e, x, o, s)
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
      cHash <- gitPathCommitHash fP''
      case cHash of
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
        (Right (cHash', fP'), Right (cHashPrevious', _)) -> do
          let  hc = HartConfig (cs cHashPrevious') (cs cHash') sectionKey
          z <- gitCheckout cHash' "src/"
          case z of
            ExitSuccess -> do
              sContent <- readTextFile fP'
              let preRendered = renderTemplate sContent
              case preRendered of
                Right rendered -> do
                  r <- runReaderT ((fmap . fmap) T.unlines (sequence <$> traverse compilePreOutput rendered)) hc
                  case r of
                    Right x -> do
                      appendFile compiledOutput $ cs x
                      return $ Right "Success compilation"
                    Left x -> error x
                Left e -> return $ Left e
            _ -> return $ Left ":("
        (Left e, _) -> return $ Left e
        (_, Left e) -> return $ Left e
    Nothing -> do
      print filePrefix
      error "could not read section key index"

compiledOutput :: Prelude.FilePath
compiledOutput = "compiledArticle.md"
