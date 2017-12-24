{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE LambdaCase #-}

import Turtle hiding (f, e, x, o, s, header)
--import Filesystem.Path.CurrentOS ( FilePath(FilePath) )
import System.Posix.Directory
import Data.String.Conversions
import Data.Bool
import System.Directory
import Data.Text (splitOn)
import Section
import Git
import Text.Regex.Posix
import Options.Applicative


work :: String -> IO ()
work x = do
  cd $ fromString x
  gitLsFiles "sections/" >>= \case
    Nothing -> error "Unable to find files in sections/"
    Just sections -> do
      case traverse (\v -> if (length (getSectionFilenameParts . convertString $ v) >= 2) then Just v else Nothing) sections of
        Nothing -> error "Not all files in sections/ are valid filenames"
        Just sectionFiles -> do
          putStrLn "Section files found:"
          print sectionFiles
          let sectionFileIndexs = read . getIndexFromSectionFilepath <$> sectionFiles :: [Int]
          rmIfExists $ fromString compiledOutput
          mapM_
            (\counter -> compileSection ((convertString $ show counter) <> "_") >>= (\z -> case z of
              Left e -> print e
              Right r -> do
                putStrLn $ "Compiled section : " ++ show counter
                putStrLn $ convertString r
              )) sectionFileIndexs


rmIfExists :: Turtle.FilePath -> IO ()
rmIfExists f = (testfile $ f) >>= boolFlip (rm $ f) (return ())

getSectionFilenameParts :: Text -> [Text]
getSectionFilenameParts = splitOn "_" . convertString

getIndexFromSectionFilepath :: String -> String
getIndexFromSectionFilepath x = do
  let (_,_,_,[v]) = x =~ ("sections/(.*)_" :: String) :: (String, String, String, [String])
  v

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Hart compiler"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> argument str
            (  metavar "STRING"
            <> help "the directory of the article project"
            )


boolFlip :: a -> a -> Bool -> a
boolFlip = flip bool
