{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Protolude hiding ((<>), catch, try)

import           Cache.Header
import qualified Codec.Compression.GZip as GZip
import           Control.Exception.Safe
import qualified Data.ByteString as Bytes
import           Data.String (String)
import           Options.Applicative hiding (header)

data Options =
  Options Text
          Bool
  deriving (Show)

parseOptions :: Parser Options
parseOptions =
  Options <$> (toS <$> argument str (metavar "FILE")) <*>
  switch
    (long "dump-body" <> help "Optionally dump the body of the cache file.")

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)

liftEither
  :: MonadError e m
  => Either e a -> m a
liftEither = either throwError return

main :: IO ()
main = do
  Options path dumpBody <-
    execParser (parseOptions `withInfo` "parse nginx cache files")
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
