{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Protolude hiding ((<>), catch, find, try)

import           Cache.Header
import qualified Codec.Compression.GZip as GZip
import           Control.Exception.Safe
import qualified Data.ByteString as Bytes
import           Data.ByteString.Search
import           Data.String (String)
import           Options.Applicative hiding (header)
import           Pipes
import qualified Pipes.Files as PF
import qualified Pipes.Prelude as P
import           Pipes.Safe (runSafeT)

data Options =
  Options Command
  deriving (Show)

data Command
  = Inspect Text
            Bool
  | Find Text
         Text
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
  command "inspect" (parseInspect `withInfo` "Inspect a cache file") <>
  command "find" (parseFind `withInfo` "Find cache files matching a key") <>
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

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)

liftEither
  :: MonadError e m
  => Either e a -> m a
liftEither = either throwError return

main :: IO ()
main = do
  Options cmd <- execParser (parseOptions `withInfo` "parse nginx cache files")
  case cmd of
    Inspect path dumpBody -> inspect path dumpBody
    Find path needle -> find path needle

inspect
  :: (MonadIO m, MonadCatch m)
  => Text -> Bool -> m ()
inspect path dumpBody = do
  ret <-
    runExceptT $ do
      content <-
        liftEither =<<
        first (("Unable to read file: " <>) . toS . displayException) <$>
        tryAny (liftIO . Bytes.readFile . toS $ path)
      header <-
        liftEither .
        first ("Unable to parse the header: " <>) . parseCacheHeader . toS $
        content
      putText . displayHeader $ header
      when dumpBody $ do
        putText ""
        let body =
              toS . Bytes.drop (fromIntegral . cacheHeaderBodyStart $ header) $
              content
        decoded <-
          catchAnyDeep (pure . GZip.decompress $ body) (const (pure body))
        liftIO $ putStrLn (toS decoded :: String)
  case ret of
    Right _ -> pure ()
    Left err -> putText $ "An error occurred: " <> err

find
  :: (MonadMask m, MonadIO m)
  => Text -> Text -> m ()
find p needle =
  runSafeT $
  runEffect $
  PF.find (toS p) PF.regular >->
  forever
    (do path <- await
        header <- liftIO $ parseCacheFileHeader path
        either (const (pure ())) (yield . (path, )) header) >->
  forever
    (do (path, header) <- await
        unless (null $ indices (toS needle) (cacheHeaderKey header)) $
          yield path) >->
  P.stdoutLn
