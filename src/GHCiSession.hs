module GHCiSession where

import System.Process
import GHC.IO.Handle


data GHCiSession = GHCiSession (Handle, Handle, Handle, ProcessHandle)
