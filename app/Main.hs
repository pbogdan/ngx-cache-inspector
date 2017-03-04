{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Protolude.Lifted hiding ((<>), catch, find, pred, try)

import Inspector
import Inspector.Cli.Args
import Options.Applicative hiding (header)

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)

main :: IO ()
main = do
  Options cmd <- execParser (parseOptions `withInfo` "Parse nginx cache files.")
  case cmd of
    Inspect path dumpBody -> inspect path dumpBody
    Find path flags op shouldInspect -> do
      let operand = fromMaybe And op
          combine =
            if operand == And
              then allFlagsPredicate
              else anyFlagsPredicate
          pred = combine flags
          finder =
            if shouldInspect == Yes
              then findInspect
              else find
      finder path pred
