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
import Rainbow 

import GHCiSession

data CommitRef = CommitRef {
    commitRef :: String
  , tagsRef :: [String]
  }
data HartConfig = HartConfig {
  fromCommitRef :: CommitRef,
  toCommitRef :: CommitRef,
  fuck :: Integer
  }
data GitChapterState = GitChapterState (HashMap String GHCiSession)

hartConfigFromHash, hartConfigUntilHash :: HartConfig -> String
hartConfigFromHash (HartConfig h _ _) = commitRef h
hartConfigUntilHash (HartConfig _ h _) = commitRef h

type Hart = ReaderT HartConfig (StateT GitChapterState IO)

articleDir :: Text
articleDir = "./chapters/"

(++<>) :: (
  ConvertibleStrings a Text,
  ConvertibleStrings b Text
  ) => a -> b -> Text
a ++<> b = cs a <> cs b :: Text

runSh :: Text -> IO (ExitCode, Text, Text)
runSh x = do
  putStrLnInfo ( "Running external command: " :: String)
  putStrLnInfo $ "  " ++ convertString x
  (r,o,e) <- shellStrictWithErr x empty
  case r of
    ExitSuccess -> putStrLnInfo $ "  Return: " ++ show r
    (ExitFailure _) -> putStrLnError $ "  Return: " ++ show r
  case o of
    "" -> pure ()
    o' -> putStrLn $ "  Output: " ++ cs o'
  case e of
    "" -> pure ()
    e' -> putStrLnError $ "  Error: " ++ cs e'
  putStrLn $ ""
  return (r,o,e)


putStrLnSuccess, putStrLnError, putStrLnInfo :: Renderable a => a -> IO ()
putStrLnSuccess x = putChunkLn $ chunk (x) & fore green
putStrLnError x = putChunkLn $ chunk (x) & fore red
putStrLnInfo x = putChunkLn $ chunk (x) & fore blue
