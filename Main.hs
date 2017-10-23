{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}

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

exitCodeToEither :: String -> ExitCode  -> Either String ()
exitCodeToEither m e = case e of
  ExitSuccess -> Right ()
  _ -> Left m

runSh :: Text -> IO (ExitCode, Text, Text)
runSh x = shellStrictWithErr x empty

gitCheckout :: Text -> Text -> IO (ExitCode)
gitCheckout c p = do
  (r, _, _) <- runSh $ "git checkout " <> c <> " -- " <> p
  return r

gitPathCommitHash :: Text -> IO (Maybe Text)
gitPathCommitHash x = do
  (r, o, _) <- runSh $ "git rev-list HEAD -- " <> x
  case (r) of
    ExitSuccess -> case (headMay $ lines o) of
      Nothing -> return $ Nothing
      v' -> return $ v'
    _ -> return Nothing

compileSection :: Text -> IO (Either String Text)
compileSection filePrefix = do
  let v = find (prefix $ fromString $ dir ++ convertString filePrefix) (fromString dir)
        where
          dir = "sections/"
  fP <- fold (v) Fold.head
  case fP of
    Just fP' -> do
      return $ Right $ convertString $ encodeString $ fP'
    Nothing -> return $ Left "Nope"

main :: IO ()
main = do
  changeWorkingDirectory "/home/chris/Projects/Haskell/Articles/Rainbox"
  let counter = "0"
  compileSection (counter <> "_") >>= (\z -> case z of
    Left e -> print e
    Right r -> putStrLn $ convertString r
    )
