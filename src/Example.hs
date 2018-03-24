{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Example where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Control.Monad.Identity
import System.Process
import GHC.IO.Handle
import Debug.Trace
import Data.String.Conversions

runGhci :: Text -> IO Text
runGhci _ =  do
  let expr = "print \"test\""
  let inputLines = (<> "\n") <$> T.lines expr :: [Text]
  print inputLines
  createProcess ((proc "ghci" ["-v0", "-ignore-dot-ghci"]) {std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe}) >>= \case
    (Just pin, Just pout, Just perr, ph) -> do
      output <-
        forM inputLines (\i -> do
          let script = i <> "\n"
          do
            hPutStr pin $ cs $ script
            hFlush pin
            x <- hIsEOF pout >>= \case
              True -> return ""
              False -> hGetLine pout
            y <- hIsEOF perr >>= \case
              True -> return ""
              False -> hGetLine perr
            let output = cs $! x ++ y
            return $ trace "OUTPUT" $ output
        )
      let f i o = "ghci>" <> i <> o
      let final = T.concat ( zipWith f (inputLines :: [Text]) (output :: [Text]) :: [Text])
      print final
      terminateProcess ph
      pure $ T.strip $  final
    _ -> error "Invaild GHCI process"

