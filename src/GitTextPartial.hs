{-# LANGUAGE LambdaCase #-}
module GitTextPartial where

import Turtle hiding (f, e, x, o, s)
import Hart
import Data.Text (Text, lines)
import Prelude hiding (lines)
import Prelude hiding (lines)
import Control.Exception as Excp
import System.IO.Error
import Operations.Types

data GitDiffError = EmptyDiff | Failed | FileDoesNotExist deriving (Show)

gitDiff :: Text -> Hart (Either GitDiffError Text)
gitDiff p = do
  (liftIO . Excp.tryJust (guard . isDoesNotExistError) $ readFile $ cs p)
  >>= (\case
    Right _ -> do
        (HartConfig commitStart commitStop _) <- ask
        (r, o, _) <- liftIO . runSh
          $
          "git diff " ++<> commitRef commitStart ++<> ".." ++<> commitRef commitStop ++<> " -- \"" ++<> p ++<> "\""
        case (r) of
          ExitSuccess -> case (length $ lines o) > 0 of
            True -> return $ Right o
            False -> return $ Left EmptyDiff
          _ -> return $ Left Failed
    Left (_) -> return $ Left FileDoesNotExist
    :: Either () String -> Hart (Either GitDiffError Text))
