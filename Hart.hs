{-# language FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hart
  ( module Hart, ask, lift, runReaderT
  ) where

import Data.Text hiding (empty)
import Data.String.Conversions
import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import Control.Monad.Trans (lift)
import Turtle (ExitCode, shellStrictWithErr, empty)

data HartConfig = HartConfig String String Integer

type Hart = ReaderT HartConfig IO

(++<>) :: (
  ConvertibleStrings a Text,
  ConvertibleStrings b Text
  ) => a -> b -> Text
a ++<> b = cs a <> cs b :: Text

runSh :: Text -> IO (ExitCode, Text, Text)
runSh x = do
  putStrLn "Running command: "
  putStrLn $ "  " ++ convertString x
  (r,o,e) <- shellStrictWithErr x empty
  print r
  print o
  print e
  return (r,o,e)

