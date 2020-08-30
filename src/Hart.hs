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
import Control.Monad.Trans.Reader (runReaderT, ask)
import Control.Monad.Trans (lift)
import Turtle (ExitCode(..), shellStrictWithErr, empty)
import Data.HashMap.Strict (HashMap, lookup)
import Rainbow

import GHCiSession
import Operations.Types

hartConfigFromHash, hartConfigUntilHash :: HartConfig -> String
hartConfigFromHash (HartConfig h _ _) = commitRef h
hartConfigUntilHash (HartConfig _ h _) = commitRef h


articleDir :: Text
articleDir = "./chapters/"

(++<>) :: (
  ConvertibleStrings a Text,
  ConvertibleStrings b Text
  ) => a -> b -> Text
a ++<> b = cs a <> cs b :: Text

type RunShReturn = (ExitCode, Text, Text)

runSh :: Text -> IO (RunShReturn)
runSh x = do
  putStrLn ( "Running external command: " :: String)
  putStrLnInfo $ "  " ++ cs x
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
