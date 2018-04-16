{-# language FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hart
  ( module Hart
  , ask, lift, runReaderT
  , module Data.String.Conversions
  , Text
  , module Data.Monoid
  , module GHCiSession
  , module Data.HashMap.Strict
  ) where

import Data.Monoid
import Data.Text hiding (empty)
import Data.String.Conversions
import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import Control.Monad.Trans (lift)
import Turtle (ExitCode(..), shellStrictWithErr, empty)
import Control.Monad.Trans.State.Lazy
import Data.HashMap.Strict (HashMap, lookup)

import GHCiSession

data HartConfig = HartConfig String String Integer
data GitChapterState = GitChapterState (HashMap String GHCiSession)

hartConfigFromHash, hartConfigUntilHash :: HartConfig -> String
hartConfigFromHash (HartConfig h _ _) = h
hartConfigUntilHash (HartConfig _ h _) = h

type Hart = ReaderT HartConfig (StateT GitChapterState IO)

(++<>) :: (
  ConvertibleStrings a Text,
  ConvertibleStrings b Text
  ) => a -> b -> Text
a ++<> b = cs a <> cs b :: Text

runSh :: Text -> IO (ExitCode, Text, Text)
runSh x = do
  putStrLn "Running external command: "
  putStrLn $ "  " ++ convertString x
  (r,o,e) <- shellStrictWithErr x empty
  putStrLn $ "  Return: " ++ show r
  case o of
    "" -> pure ()
    o' -> putStrLn $ "  Output: " ++ show o'
  case e of
    "" -> pure ()
    e' -> putStrLn $ "  Error: " ++ show e'
  putStrLn $ ""
  return (r,o,e)

