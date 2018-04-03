module Main where

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Network.Wreq as W
import Options.Applicative.Simple
import RIO

jsonOpts :: J.Options
jsonOpts = J.defaultOptions {J.fieldLabelModifier = J.camelTo2 '_'}

data App = App
  { appLogFunc :: !LogFunc
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

type CategoryId = Int

type ProductId = Int

type PageId = Int

data PageData = PageData
  { ids :: [ProductId]
  , totalPages :: Int
  } deriving (Eq, Show, Generic)

instance J.FromJSON PageData where
  parseJSON = J.genericParseJSON jsonOpts

getPageData :: CategoryId -> PageId -> RIO App PageData
getPageData catId pageId = do
  let url =
        "https://fashion-api.rozetka.com.ua/goods/get?sort=popularity&category_id=" <>
        show catId <>
        "&page=" <>
        show pageId <>
        "&lang=ua"
  r <- liftIO $ W.asJSON =<< W.get url
  threadDelay 1000000
  return (r ^. W.responseBody)

getProductIds :: CategoryId -> RIO App [ProductId]
getProductIds catId = do
  p1data <- getPageData catId 1
  restIds <-
    forM [2 .. totalPages p1data] $ \page -> do
      pdata <- getPageData catId page
      return $ ids pdata
  return (ids p1data ++ concat restIds)

getComments :: ProductId -> RIO App ()
getComments productId = do
  let url = "https://rozetka.com.ua/ua/" <> show productId <> "/p" <> show productId <> "/tab=comments/"
  undefined

app :: RIO App ()
app = do
  let catId = 4637799 :: CategoryId
  logDebug $ display $ "Grabbing the category id: " <> tshow catId
  productIds <- getProductIds catId
  prodWithComms <- forM productIds $ \pId -> do
    cs <- getComments pId
    return (pId, cs)
  return ()

main :: IO ()
main = do
  (verbose, ()) <-
    simpleOptions
      "0.1.0.0"
      "rozetka reviews"
      "rozetka review extractor"
      (flag False True (long "verbose"))
      empty
  lo <- logOptionsHandle stdout verbose
  withLogFunc lo $ \appLogFunc -> runRIO App {..} $ do app
