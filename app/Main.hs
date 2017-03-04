{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Protolude.Lifted hiding ((<>), catch, find, try)

import Cache.Header
import Inspector
import Inspector.Repl
import Inspector.Repl.Prelude
import Options.Applicative hiding (header)

data Options =
  Options Command
  deriving (Show)

data Command
  = Inspect Text
            Bool
  | Find Text
         Text
  | Repl Text
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
  command "inspect" (parseInspect `withInfo` "Inspect a cache file") <>
  command "find" (parseFind `withInfo` "Find cache files matching a key") <>
  command "repl" (parseRepl `withInfo` "REPL") <>
  mempty

parseInspect :: Parser Command
parseInspect =
  Inspect <$> (toS <$> argument str (metavar "FILE")) <*>
  switch
    (long "dump-body" <> help "Optionally dump the body of the cache file.")

parseFind :: Parser Command
parseFind =
  Find <$>
  (toS <$> argument str (metavar "PATH" <> help "Path to the cache folder")) <*>
  (toS <$>
   argument
     str
     (metavar "NEEDLE" <> help "String to search for within the  key"))

parseRepl :: Parser Command
parseRepl =
  Repl <$>
  (toS <$> argument str (metavar "PATH" <> help "Path to the cache folder"))

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)

main :: IO ()
main = do
  Options cmd <- execParser (parseOptions `withInfo` "parse nginx cache files")
  case cmd of
    Inspect path dumpBody -> inspect path dumpBody
    Find path needle -> find path ((needle `matches`) . cacheHeaderKey)
    Repl _path -> runRepl
