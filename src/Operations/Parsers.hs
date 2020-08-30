{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Operations.Parsers where

import Hart
import qualified Text.Parsec as TPar


import Text.Parsec.String
import Data.Either.Extra
import Safe
import Data.Bool
import qualified Data.Text as Text
import Operations.Types

import Text.Parsec hiding (parserTrace)

import Text.Mustache.Parser (parseText, MustacheState)
-- import Operations.Pending
import Control.Monad.Identity

import Text.Mustache.Internal.Types (Node(..), DataIdentifier(..))

stringParser :: (Monad m) => ParsecT Text u m a -> ParsecT String u m a
stringParser p = mkPT $ (\st -> (fmap . fmap . fmap) outReply $ runParsecT p (inState st))
  where inState :: State String u -> State Text u
        inState  (State i pos u) = State (Text.pack i) pos u
        outReply :: Reply Text u a -> Reply String u a
        outReply (Ok a (State i pos u) e) = Ok a (State (Text.unpack i) pos u) e
        outReply (Error e) = Error e

-- stringParser' :: (Monad m) => (u -> u2) -> ParsecT s u m a -> ParsecT s u2 m a
-- stringParser' f p = mkPT $ \st -> (fmap . fmap . fmap) outReply $ runParsecT p (inState st)
  -- where inState :: State s u -> State s u
        -- inState  (State i pos u) = State i pos u
        -- outReply :: Reply s u a -> Reply s (u) a
        -- outReply (Ok a (State i pos u) e) = Ok a (State i pos (u)) e
        -- outReply (Error e) = Error e

parseNumberString :: Parser [Either Int String]
parseNumberString = do
  z <- optionMaybe eof 
  case z of
    Just () -> pure []
    Nothing -> do
      _ <- optional space
      x <- optionMaybe $ many1 (digit)
      case (x) of
        (Just x') -> do
          y <- parseNumberString
          pure $ [Left (read x')] ++ y
        Nothing -> do
          xx <- many1 letter
          y <- parseNumberString
          pure $ [Right xx] ++ y

parseNumberString' :: Parser [String]
parseNumberString' = do
  z <- optionMaybe eof 
  case z of
    Just () -> pure []
    Nothing -> do
      _ <- optional space
      x <- many1 (digit <|> letter)
      y <- parseNumberString'
      pure $ [x] ++ y

-- abc :: Parsec Text b c -> Parsec String b c
-- abc = undefined

parseSection :: ParsecT Text MustacheState Identity [SectionBlock] 
parseSection = fmap (fmap xyz) parseText


-- ParsecT s u Identity
-- parseSection' :: Monad m => ParsecT Text b m [SectionBlock] 
-- parseSection' = do
  -- -- pure []
  -- -- let s = initState defaultConf
  -- -- case (runParser parseText s "filepath" "content") of
    -- -- Right x -> pure undefined

  -- where
    -- xyz (TextBlock x:_) = [SectionRaw x]

  -- optionMaybe eof >>= \case
  --   Just () -> return []
  --   Nothing -> do
  --     isGhci <- optionMaybe parseGhciTag
  --     block <- case isGhci of
  --           Just ghci' -> return $ SectionGHCi ((cs . fst) $ ghci') (((cs <$>) . snd) $ ghci')
  --           Nothing -> do
  --             manyTill anyToken (lookAhead $ try $ choice [const () <$> parseGhciTag, const () <$> eofString])
  --             >>= return . SectionRaw . cs
  --     optionMaybe parseSection >>= \case
  --       Just sndBlock -> return ( block : sndBlock)
  --       Nothing -> return [block]

eofString :: Parser String
eofString = do
  eof
  return ""

parserTrace :: (Stream s m t, Show t) => String -> ParsecT s u m ()
parserTrace _ = return ()

parseGhciTag :: Parser (String, (Maybe String))
parseGhciTag = do
    parserTrace "DEBUG INIT parseGhciTag"
    _ <- string "ghci" >> optional (char ' ')
    s <- optionMaybe $ try $ manyTill anyToken (endOfLine)
    z <- many (anyToken)
    parserTrace "DEBUG SUCCESS parseGhciTag"
    pure (z :: String, bool (s) Nothing (s == Just ""))

parseFileAndLimits :: Parser (String, Maybe Int, Maybe Int)
parseFileAndLimits = do
      l <- many (noneOf " }")
      _ <- optional space
      l' <- optionMaybe $ many1 digit
      _ <- optional space
      l'' <- optionMaybe $ many1 digit
      _ <- optional space
      return (l, l' >>= readMay, l'' >>= readMay)


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


parse' :: Parser a -> String -> String -> Maybe a -- where Foo0 and Bar are the correct types
parse' foo s0 s1 = eitherToMaybe $ Text.Parsec.parse foo s0 s1

parseSectionHeader :: Parser SectionHeaderReference
parseSectionHeader = do
  let sectionHeaderTag = string "{{" >> optional space >> string "sectionHeader" >> optional space >> string "}}"
  s <- manyTill anyChar (try sectionHeaderTag)
  s'' <- many anyChar
  return $ SectionHeaderReference s s''

parsePossibleTag :: Parser PossibleTag
parsePossibleTag = do
  z <- string "{{" >> manyTill anyChar (try $ string "}}")
  return $ PossibleTag z

xyz :: Node Text -> SectionBlock 
xyz (TextBlock x) = SectionRaw x
xyz (Variable _ (NamedData x)) =
  case x of
    (x':[]) ->
      case (TPar.parse parseGhciTag "ghci" $ cs x') of
            Right (s,s') -> SectionGHCi (cs s) (fmap cs s')
            Left e -> SectionError $ cs $ show e
            -- _ -> error "?"
    _ -> error "fuck"
xyz _ = error "Fuck"
