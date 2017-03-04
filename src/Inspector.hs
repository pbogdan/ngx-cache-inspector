{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Inspector
  ( date
  , matches
  , inspect
  , find
  , findInspect
  , findCount
  ) where

import           Protolude.Lifted hiding ((<>), catch, find, pred, try, yield)

import           Cache.Header
import qualified Codec.Compression.GZip as GZip
import           Control.Exception.Safe
import qualified Data.ByteString as Bytes
import           Data.ByteString.Search
import           Data.String (String)
import           Data.Time.Git
import           Options.Applicative hiding (header)
import           Pipes
import qualified Pipes.Files as PF
import qualified Pipes.Prelude as P
import           Pipes.Safe
import           System.FilePath.Posix

liftEither
  :: MonadError e m
  => Either e a -> m a
liftEither = either throwError return


date :: String -> Integer
date s = fromMaybe (panic $ "date in unkwown format: " <> toS s) (approxidate s)

matches :: Text -> ByteString -> Bool
matches needle haystack = not . null $ indices (toS needle) haystack

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

scanFolder
  :: (MonadSafe m, StringConv a FilePath)
  => a -> Producer FilePath m ()
scanFolder p = PF.find (toS p) PF.regular

parseHeader
  :: (MonadIO m, MonadSafe m)
  => Pipe FilePath (FilePath, CacheHeader) m b
parseHeader =
  forever $ do
    path <- await
    header <- liftIO $ parseCacheFileHeader path
    either (const (pure ())) (yield . (path, )) header

filterPaths
  :: Monad m
  => (CacheHeader -> Bool) -> Pipe (FilePath, CacheHeader) FilePath m b
filterPaths pred =
  forever $ do
    (path, header) <- await
    when (pred header) $ yield (normalise path)

find
  :: (MonadMask m, MonadIO m)
  => Text -> (CacheHeader -> Bool) -> m ()
find p pred =
  runSafeT $
  runEffect $ scanFolder p >-> parseHeader >-> filterPaths pred >-> P.stdoutLn

findMap
  :: (MonadMask m, MonadIO m, StringConv FilePath b)
  => Text -> (CacheHeader -> Bool) -> (b -> Pipes.Safe.SafeT m ()) -> m ()
findMap p pred mapper =
  runSafeT $
  runEffect $
  scanFolder p >-> parseHeader >-> filterPaths pred >->
  P.mapM_ (mapper . toS) >->
  P.stdoutLn

findInspect
  :: (MonadMask m, MonadIO m)
  => Text -> (CacheHeader -> Bool) -> m ()
findInspect p pred =
  findMap p pred (\path -> putStrLn path >> inspect path False)

findCount
  :: (MonadMask m, MonadIO m)
  => Text -> (CacheHeader -> Bool) -> m Int
findCount p pred =
  runSafeT $
  runEffect $ P.length (scanFolder p >-> parseHeader >-> filterPaths pred)
