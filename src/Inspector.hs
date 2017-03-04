{-# LANGUAGE TupleSections #-}

module Inspector
  ( inspect
  , find
  ) where

import           Protolude.Lifted hiding ((<>), catch, find, pred, try, yield)

import           Cache.Header
import qualified Codec.Compression.GZip as GZip
import           Control.Exception.Safe
import qualified Data.ByteString as Bytes
import           Data.String (String)
import           Options.Applicative hiding (header)
import           Pipes
import qualified Pipes.Files as PF
import qualified Pipes.Prelude as P
import           Pipes.Safe (runSafeT)
import           System.FilePath.Posix

liftEither
  :: MonadError e m
  => Either e a -> m a
liftEither = either throwError return

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
  => Text -> (CacheHeader -> Bool) -> m ()
find p pred =
  runSafeT $
  runEffect $
  PF.find (toS p) PF.regular >->
  forever
    (do path <- await
        header <- liftIO $ parseCacheFileHeader path
        either (const (pure ())) (yield . (path, )) header) >->
  forever
    (do (path, header) <- await
        when (pred header) $ yield (normalise path)) >->
  P.stdoutLn
