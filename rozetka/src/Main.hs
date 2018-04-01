module Main where

import RIO
import Options.Applicative.Simple

data App = App
  { appLogFunc :: !LogFunc
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

app :: RIO App ()
app = logDebug $ display @Text "hello world"

main :: IO ()
main = do
  (verbose,()) <- simpleOptions "0.1.0.0" "rozetka reviews" "rozetka review extractor" (flag False True (long "verbose")) empty
  lo <- logOptionsHandle stdout verbose
  withLogFunc lo $ \appLogFunc ->
    runRIO App {..} $ do app
