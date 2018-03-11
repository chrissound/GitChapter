{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE LambdaCase #-}

import Turtle hiding (e, f, header, o, s, x)

import Control.Monad
import Data.Bool
import Data.String.Conversions
import Data.Text (splitOn)
import Git
import Options.Applicative
import Safe
import Section
import System.Directory
--import Filesystem.Path.CurrentOS ( FilePath(FilePath) )
import System.Posix.Directory
import Text.Regex.Posix

work :: String -> IO ()
work x = do
  cd $ fromString x
  gitLsFiles "sections/" >>= \case
    Nothing ->
      error
        "Found no relevant files within sections/ within the repository"
    Just sections -> do
      let sectionIndexes = getIndexFromSectionFilepath <$> sections
      case (sequence $ readMay <$> sectionIndexes :: Maybe [Int]) of
        Nothing -> do
          error $
            "Unable to determine indexes for all section files which are of: " ++
            show sections
        Just sectionFiles -> do
          putStrLn "Section files found:"
          putStrLn $ "  " ++ show sections
          putStrLn ""
          rmIfExists $ fromString compiledOutput
          forM_
            sectionFiles
            (\counter ->
               compileSection (cs $ show counter) >>= \case
                 Left e -> do
                   putStrLn $ "Error compiling section: " ++ show counter
                   error e
                 Right r -> do
                   putStrLn $ "Compiled section : " ++ show counter
                   putStrLn $ convertString r)

rmIfExists :: Turtle.FilePath -> IO ()
rmIfExists f = (testfile $ f) >>= boolFlip (rm $ f) (return ())

getIndexFromSectionFilepath :: String -> String
getIndexFromSectionFilepath x = do
  let (_, _, _, [v]) =
        x =~ ("sections/(.[^_]*)_" :: String) :: ( String
                                                 , String
                                                 , String
                                                 , [String])
  v

main :: IO ()
main =
  join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser) (fullDesc <> header "Hart compiler")
  where
    parser :: Parser (IO ())
    parser =
      work <$>
      argument
        str
        (metavar "STRING" <> help "the directory of the article project")

boolFlip :: a -> a -> Bool -> a
boolFlip = flip bool
