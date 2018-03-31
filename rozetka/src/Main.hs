module Main where

import RIO
import Options.Applicative.Simple
import qualified Prelude as Prelude

data App = App
  { appLogFunc :: !LogFunc
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

main :: IO ()
main = do
  (verbose,()) <- simpleOptions "0.1.0.0" "rozetka domashka" "rozetka review extractor" (flag () () (long "verbose")) empty
  Prelude.print verbose
  -- lo <- logOptionsHandle stdout (verbose::Bool)
  -- withLogFunc lo $ \appLogFunc ->
  --   runRIO App {..} $ do logDebug $ display "hello world"
