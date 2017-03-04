{-# LANGUAGE FlexibleContexts #-}

module Inspector.Repl.Prelude
  ( date
  , matches
  ) where

import Protolude.Lifted

import Data.ByteString.Search
import Data.String
import Data.Time
import Data.Time.Git
import Inspector

date :: String -> UTCTime
date s =
  case approxidate s of
    Just d -> posixToUTC d
    Nothing -> panic $ "unknown date format for " <> toS s

matches
  :: (StringConv b ByteString, StringConv a ByteString)
  => a -> b -> Bool
matches needle haystack = not . null $ indices (toS needle) (toS haystack)
