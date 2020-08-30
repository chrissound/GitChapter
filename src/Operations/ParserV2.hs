module Operations.ParserV2 where

import Text.Mustache.Parser
import Data.Text
-- import Text.Pretty.Simple
import qualified Text.Parsec
import Operations.Types
import Operations.Parsers
--import Text.Parsec.Error
--import Text.Mustache.Types

-- xyz :: FilePath -> Text -> IO ()
-- xyz fp x = do
  -- pPrint $ parse fp x
  -- pure ()

runMuParser :: Text.Parsec.SourceName -> Text -> Either Text.Parsec.ParseError [Operations.Types.SectionBlock]
runMuParser = Text.Parsec.runParser parseSection (initState defaultConf) 
