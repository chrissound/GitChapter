module Operations.Types where

import Data.Text
import Data.HashMap.Strict (HashMap)
import GHCiSession
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Lazy
import Text.Parsec.String

data SectionBlock = SectionRaw Text | SectionGHCi Text (Maybe Text) | SectionError Text deriving (Show, Eq)
data FileLineRange = FileLineRange (Maybe(Int, Int)) deriving (Show, Eq)
data FileReference = FileReference String FileLineRange deriving (Show, Eq)
data FileSection = FileSection String String deriving (Show, Eq)
data GitDiffReference = GitDiffReference Text deriving (Show, Eq)
data GitCommitOffestReference = GitCommitOffestReference deriving (Show, Eq)
data Shell = Shell (ShellSuccess) (ShellOutput') Text deriving (Show, Eq)
data ShellOutput' = ShellOutputVoid | ShellOutput' deriving (Show, Eq)
data ShellSuccess = ShellSuccessVoid | ShellSuccessRequired deriving (Show, Eq)
data PossibleTag = PossibleTag String deriving (Show, Eq)
data SectionHeaderReference = SectionHeaderReference String String deriving (Show, Eq)
data GHCiReference = GHCiReference Text (Maybe Text) deriving (Show, Eq)

data CommitRef = CommitRef {
    commitRef :: String
  , tagsRef :: [String]
  } deriving Show
data HartConfig = HartConfig {
  fromCommitRef :: CommitRef,
  toCommitRef :: CommitRef,
  fuck :: Integer
  }
data GitChapterState = GitChapterState (HashMap String GHCiSession)
type Hart = ReaderT HartConfig (StateT GitChapterState IO)

class Operation a where
  parse :: Parser a
  render :: a -> Hart (Either String (Maybe Text))
