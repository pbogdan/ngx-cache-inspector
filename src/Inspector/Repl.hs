{-# LANGUAGE ScopedTypeVariables #-}

module Inspector.Repl
  ( runRepl
  ) where

import Protolude.Lifted hiding (pred)

import Cache.Header
import Language.Haskell.Interpreter
import System.Console.Repline

evalAndPrint :: Text -> IO ()
evalAndPrint expr = runInterpreter (testHint expr) >>= either print return

runRepl :: IO ()
runRepl =
  evalRepl
    "nci > "
    (liftIO . evalAndPrint . toS)
    []
    (Word (pure . pure []))
    (pure ())

testHint :: Text -> Interpreter ()
testHint expr = do
  setImportsQ
    [("Protolude.Lifted", Nothing), ("Inspector.Repl.Prelude", Nothing)]
  a <- eval (toS expr)
  liftIO . print $ a
