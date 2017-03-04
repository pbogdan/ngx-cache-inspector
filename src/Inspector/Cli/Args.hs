{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Inspector.Cli.Args
  ( Options(..)
  , Command(..)
  , Inspect(..)
  , Operand(..)
  , parseOptions
  , allFlagsPredicate
  , anyFlagsPredicate
  ) where

import           Protolude.Lifted hiding ((<>), catch, find, pred, try)

import           Cache.Header
import qualified Data.ByteString.Short as ShortBytes
import           Data.String (String)
import           Inspector
import           Options.Applicative hiding (header)

data Options =
  Options Command
  deriving (Show)

data Command
  = Inspect Text
            Bool
  | Find Text
         [FindFlag]
         (Maybe Operand)
         Inspect
  deriving (Eq, Show)

data FindFlag
  = Key Text
  | ValidUntil Text
  | Created Text
  | Etag Text
  | Vary Text
  deriving (Eq, Show)

data Inspect
  = Yes
  | No
  deriving (Eq, Show)

data Operand
  = And
  | Or
  deriving (Eq, Show)

parseFindFlags :: Parser [FindFlag]
parseFindFlags =
  some
    (parseKey <|> parseValidUntil <|> parseCreated <|> parseEtag <|> parseVary)

parseKey :: Parser FindFlag
parseKey =
  Key <$>
  (toS <$>
   strOption
     (long "key" <> metavar "needle" <>
      help "Search for needle within the cache key."))

parseValidUntil :: Parser FindFlag
parseValidUntil =
  ValidUntil <$>
  (toS <$>
   strOption
     (long "valid-until" <> metavar "[+-]date" <>
      help "Compare valid until date against the argument."))

parseCreated :: Parser FindFlag
parseCreated =
  Created <$>
  (toS <$>
   strOption
     (long "created" <> metavar "[+-]date" <>
      help "Compare created date against the argument."))

parseEtag :: Parser FindFlag
parseEtag =
  Key <$>
  (toS <$>
   strOption
     (long "etag" <> metavar "needle" <>
      help "Search for needle within the etag."))

parseVary :: Parser FindFlag
parseVary =
  Key <$>
  (toS <$>
   strOption
     (long "vary" <> metavar "needle" <> help "Search for needle within vary."))

findFlagPredicate :: FindFlag -> CacheHeader -> Bool
findFlagPredicate (Key key) header = key `matches` cacheHeaderKey header
findFlagPredicate (ValidUntil time) header =
  let (comparator, val) = flagValueComparator . toS $ time
  in cacheHeaderValidSec header `comparator` (fromIntegral . date . toS $ val)
findFlagPredicate (Created time) header =
  let (comparator, val) = flagValueComparator . toS $ time
  in cacheHeaderDate header `comparator` (fromIntegral . date . toS $ val)
findFlagPredicate (Etag key) header =
  key `matches` (ShortBytes.fromShort . cacheHeaderEtag $ header)
findFlagPredicate (Vary key) header =
  key `matches` (ShortBytes.fromShort . cacheHeaderEtag $ header)

flagValueComparator
  :: Ord b
  => String -> (b -> b -> Bool, String)
flagValueComparator s =
  case s of
    ('+':xs) -> ((>), xs)
    ('-':xs) -> ((<), xs)
    _ -> ((==), s)

allFlagsPredicate :: [FindFlag] -> CacheHeader -> Bool
allFlagsPredicate preds header = all ($ header) (map findFlagPredicate preds)

anyFlagsPredicate :: [FindFlag] -> CacheHeader -> Bool
anyFlagsPredicate preds header = any ($ header) (map findFlagPredicate preds)

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
  command
    "inspect"
    (parseInspect `withInfo` "Inspect the header of a single cache file.") <>
  command "find" (parseFind `withInfo` "Find cache files matching the flags.") <>
  mempty

parseInspect :: Parser Command
parseInspect =
  Inspect <$> (toS <$> argument str (metavar "file")) <*>
  switch
    (long "dump-body" <> help "Optionally dump the body of the cache file.")

parseFind :: Parser Command
parseFind =
  Find <$>
  (toS <$> argument str (metavar "path" <> help "Location of the cache directory.")) <*>
  parseFindFlags <*> parseOperand <*> parseInspectFlag

parseOperand :: Parser (Maybe Operand)
parseOperand =
  optional
    (flag' And (long "and" <> help "Combine the flags with \"and\".") <|>
     flag' Or (long "or" <> help "Combine the flags with \"or\"."))

parseInspectFlag :: Parser Inspect
parseInspectFlag =
  flag
    No
    Yes
    (long "inspect" <>
     help "Optionally dump the header of each matched cache file.")

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)
