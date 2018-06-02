{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE LambdaCase #-}

module Main where

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
  gitLsFiles "chapters/" >>= \case
    Nothing -> error "git ls-files failure"
    Just chapters -> do
      let chapterIndexes = getIndexFromChapterFilepath <$> chapters
      case (sequence $ readMay <$> chapterIndexes :: Maybe [Int]) of
        Nothing -> do
          error $
            "Unable to determine indexes for all chapter files which are of: " ++
            show chapters
        Just [] -> error "Found no relevant files within chapters/ within the repository"
        Just chapterFiles -> do
          putStrLn "Chapter files found:"
          putStrLn $ "  " ++ show chapters
          putStrLn ""
          rmIfExists $ fromString compiledOutput
          forM_
            chapterFiles
            (\counter ->
               compileChapter (cs $ show counter) >>= \case
                 Left e -> do
                   putStrLn $ "Error compiling chapter: " ++ show counter
                   error e
                 Right r -> do
                   putStrLn $ "Compiled chapter : " ++ show counter
                   putStrLn $ convertString r)

rmIfExists :: Turtle.FilePath -> IO ()
rmIfExists f = (testfile $ f) >>= boolFlip (rm $ f) (return ())

getIndexFromChapterFilepath :: String -> String
getIndexFromChapterFilepath x = do
  let (_, _, _, [v]) =
        x =~ ("chapters/(.[^_]*)_" :: String) :: ( String
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
