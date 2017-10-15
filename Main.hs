{-# LANGUAGE OverloadedStrings #-}

import Turtle (empty, shellStrictWithErr, ExitCode)
import Data.Text (Text)
import Data.Text.IO as Text.IO (putStrLn)
import Data.Monoid ((<>))

runSh :: Text -> IO (ExitCode, Text, Text)
runSh x' = shellStrictWithErr ("git rev-list -- " <> x') empty

main :: IO ()
main = do
  (e, v, v') <- runSh "Main.hs"
  print e
  Text.IO.putStrLn v
  Text.IO.putStrLn v'
