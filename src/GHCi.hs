{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# OPTIONS -Wno-type-defaults #-}
module GHCi where

import qualified Data.Text as T
import Data.Monoid
import Control.Monad.Identity
import System.Process
import GHC.IO.Handle
import BlogLiterately
import Hart
import Debug.Trace
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, modifyMVar_, newMVar)
import System.Timeout (timeout)
import Data.Maybe

runGhci :: Text -> IO Text
runGhci expr =  do
  let inputLines = filter (/= "") (T.lines expr)
  createProcess ((proc "ghci" ["-v0", "-ignore-dot-ghci"]) {std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe}) >>= \case
    (Just pin, Just pout, Just perr, ph) -> do
      output <- do
        forM inputLines
          (\i -> do
              let script = "putStrLn " ++ show magic ++ "\n"
                            ++ cs i ++ "\n"
                            ++ "putStrLn " ++ show magic ++ "\n"
              do
                stdoutMVar <- newEmptyMVar
                stderrMVar <- newMVar ""
                hPutStr pin script
                hFlush pin
                tOutId <- forkIO $ extract' pout >>= putMVar stdoutMVar
                tErrId <- forkIO $ do
                  let f' = hGetLine perr >>= (\l -> modifyMVar_ stderrMVar (return . (++ (l ++ "\n"))))
                  forever f'
                x <- timeout (1 * (10^6)) (takeMVar stdoutMVar) >>= return . fromMaybe "***ghci timed out"
                y <- timeout (1 * (10^6)) (takeMVar stderrMVar) >>= return . fromMaybe "***ghci timed out"
                killThread tOutId
                killThread tErrId
                return $ cs $! x ++ y
          )
      let final = T.concat ( zipWith f (inputLines :: [Text]) (output :: [Text]) :: [Text])
      terminateProcess ph
      pure $ T.strip $ cs $ final
    _ -> error "Invaild GHCI process"

isEof :: Char -> Bool
isEof = (== '\n')

trimPreEndLines :: Text -> Text
trimPreEndLines = T.dropWhile isEof

trimEndLines :: Text -> Text
trimEndLines = T.dropWhileEnd isEof

f :: Text -> Text -> Text
f i "\n" = f i ""
f i "" = "ghci> " <> i
         <> "\n"
f i o = "ghci> " <> i
        <> "\n      " <> (trimEndLines . trimPreEndLines $ o) <> "\n"
