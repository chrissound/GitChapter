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

initializeGHCiSession :: IO GHCiSession
initializeGHCiSession = do
  createProcess ((proc "ghci" ["-v0", "-ignore-dot-ghci"]) {std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe})
  >>= \case
    (Just pin, Just pout, Just perr, ph) -> return (GHCiSession (pin, pout, perr, ph))
    _ -> error "Invaild GHCI process"

runGhciSession :: GHCiSession -> Text -> IO Text 
runGhciSession (GHCiSession ghci) expr = do
  let (pin, pout, perr, _) = ghci
  let inputLines = filter (/= "") (T.lines expr)
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
  let final = T.concat ( zipWith ghciPrompt (inputLines :: [Text]) (output :: [Text]) :: [Text])
  pure $ T.strip $ cs $ final

runGhci :: Text -> IO Text
runGhci expr =  do
  g@(GHCiSession (_,_,_,ph)) <- initializeGHCiSession
  t <- runGhciSession g expr
  terminateProcess ph
  return t

isEof :: Char -> Bool
isEof = (== '\n')

trimPreEndLines :: Text -> Text
trimPreEndLines = T.dropWhile isEof

trimEndLines :: Text -> Text
trimEndLines = T.dropWhileEnd isEof

ghciPrompt :: Text -> Text -> Text
ghciPrompt i "\n" = ghciPrompt i ""
ghciPrompt i "" = "ghci> " <> i
         <> "\n"
ghciPrompt i o = "ghci> " <> i
        <> "\n      " <> (trimEndLines . trimPreEndLines $ o) <> "\n"
