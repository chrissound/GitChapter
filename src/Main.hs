{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Turtle                  hiding ( e
                                                , f
                                                , header
                                                , o
                                                , s
                                                , x
                                                )
import           Hart

import           Control.Monad
import           Data.Bool
import           Data.String.Conversions
import           Data.Text                      ( splitOn )
import           Git
import           Options.Applicative
import           Safe
import           Section
import           System.Directory
--import Filesystem.Path.CurrentOS ( FilePath(FilePath) )
import           System.Posix.Directory
import           Text.Regex.Posix

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

work :: String -> [String] -> IO ()
work x explitChapters = do
  cd $ fromString x
  chaptersToProcess <-
    case explitChapters of
      [] -> gitLsFiles "master" "chapters/"
      v -> pure $ pure v
  case chaptersToProcess of
    Nothing       -> error "See above ^"
    Just chapters -> do
      let chapterIndexes = getIndexFromChapterFilepath <$> chapters
      case (sequence $ readMay <$> chapterIndexes :: Maybe [Int]) of
        Nothing -> do
          error
            $ "Unable to determine indexes for all chapter files which are of: "
            ++ show chapters
        Just [] ->
          error "Found no relevant files within chapters/ within the repository"
        Just chapterFiles -> do
          putStrLnInfo $ ("Chapter files found:" :: Text)
          mapM_ putStrLn chapters
          putStrLnInfo $ ("Removing git tags with `gch` prefix:" :: Text)
          _ <- runSh "git tag | grep \"^gch\" | xargs git tag -d"
          putStrLn "-------------------------"
          rmIfExists $ fromString compiledOutput
          forM_
            chapterFiles
            (\counter -> compileChapter (cs $ show counter) >>= \case
              Left e -> do
                putStrLnError ("Error compiling chapter: \n" :: Text)
                putStrLnError $ show counter
                error e
              Right r -> do
                putStrLnSuccess ("Compiled chapter : " ++ show counter)
                putStrLnSuccess (cs r :: Text)
            )
          gitCheckout "master" >> pure ()

rmIfExists :: Turtle.FilePath -> IO ()
rmIfExists f = (testfile $ f) >>= boolFlip (rm $ f) (return ())

getIndexFromChapterFilepath :: String -> String
getIndexFromChapterFilepath x = do
  case x =~ ("chapters/(.[^_]*)_" :: String) :: ( String
          , String
          , String
          , [String]
          ) of
    (_, _, _, [v]) -> v
    _ -> error $ "Chapter does not seem to match regex of chapters/(.[^_]*)_ :" ++ x

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $ info
  (helper <*> parser)
  (fullDesc <> header "GitChapter")
 where
  parser :: Parser (IO ())
  parser = work
    <$> argument
    str
    (metavar "STRING" <> help "the directory of the project")
    <*>
      many
          (strOption
            (long "chapter" <> metavar "STRING" <> help
              "path to chapter file"
            )
          )

boolFlip :: a -> a -> Bool -> a
boolFlip = flip bool
