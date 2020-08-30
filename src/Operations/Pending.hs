{-# Language OverloadedStrings #-}
module Operations.Pending where

import Text.Mustache.Internal.Types
import Data.Text
import Operations.Types
import qualified Text.Parsec as TPar
import qualified Text.Parsec
import qualified Text.Parsec.String

-- xyz :: Node Text -> SectionBlock 
-- xyz (TextBlock x) = SectionRaw x
-- xyz (Variable _ (NamedData x)) =
  -- case x of
    -- (x':[]) ->
      -- case (TPar.parse parseGhciTag "ghci" x') of
            -- Right (s,s') -> x == fp
            -- Left e -> SectionError e
            -- _ -> error "?"
    -- _ -> error "fuck"
-- xyz _ = error "Fuck"


