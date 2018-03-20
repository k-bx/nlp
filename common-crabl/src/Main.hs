{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main where

import Control.Arrow (second)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import Data.HashMap.Monoidal (MonoidalHashMap)
import qualified Data.HashMap.Monoidal as MH
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List (sortBy)
import Data.Semigroup
import qualified Data.String.Class as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Warc
import Network.URI
import qualified Pipes as P
import Pipes.ByteString (fromHandle)
import qualified Pipes.GZip as PGZip
import System.IO
import Text.Groom

data Env = Env
  { domains :: IORef (MonoidalHashMap Text (Sum Int))
  }

-- |
-- > extractHostRoot "www.haskell.org"
-- "org"
extractHostRoot :: Text -> Text
extractHostRoot = last . T.splitOn "."

iterFunc :: Env -> Record IO b -> IO b
iterFunc Env {..} Record {..} = do
  domainsVal <- readIORef domains
  -- I'm bored
  let mroot =
        recHeader ^. recHeaders . at "WARC-Target-URI" <&> S.toString <&>
        parseURI &
        join <&>
        uriAuthority &
        join <&>
        uriRegName <&>
        S.toText <&>
        extractHostRoot
  case mroot of
    Nothing -> return ()
    Just root -> modifyIORef domains (MH.modify (+ 1) root)
  r <- liftIO $ P.runEffect $ P.for recContent (\x -> x `seq` return ())
  return r

-- | See https://github.com/k0001/pipes-zlib/issues/29
decompressAll ::
     MonadIO m => P.Producer ByteString m r -> P.Producer ByteString m r
decompressAll p = do
  er <- PGZip.decompress' p
  case er of
    Left leftover -> decompressAll leftover
    Right r -> return r

main :: IO ()
main = do
  let crawlFile =
        "/home/kb/Downloads/commoncrawl/CC-MAIN-20180221222354-20180222002354-00249.warc.gz"
  -- let crawlFile =
  --       "/home/kb/Downloads/commoncrawl/CC-MAIN-20180221222354-20180222002354-00249.warc"
  withFile crawlFile ReadMode $ \h -> do
    env <- Env <$> newIORef mempty
    _ <- iterRecords (iterFunc env) (parseWarc (decompressAll (fromHandle h)))
    -- _ <- iterRecords (iterFunc env) (parseWarc (fromHandle h))
    doms <- readIORef (domains env)
    -- putStrLn $ groom $ doms
    putStrLn "Top 10 domains:"
    putStrLn $ groom $
      take
        10
        (sortBy (flip compare `on` snd) (map (second getSum) (MH.toList doms)))
    return ()
