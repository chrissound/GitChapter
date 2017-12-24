module GitTextPartial where

import Turtle hiding (f, e, x, o, s)
import Hart
import Data.Text (Text, lines)
import Prelude hiding (lines)
import Prelude hiding (lines)
import Safe

gitDiff :: Text -> Hart (Maybe Text)
gitDiff x = do
  (HartConfig commitStart commitStop _) <- ask
  (r, o, _) <- lift . runSh $
    "git diff " ++<> commitStart ++<> ".." ++<> commitStop ++<> " -- \"" ++<> x ++<> "\""
  case (r) of
    ExitSuccess -> case (headMay $ lines o) of
      Nothing -> return $ Nothing
      _ -> return $ Just o
    _ -> return Nothing
