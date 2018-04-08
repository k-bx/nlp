module Main where

import qualified Data.Aeson as J
import Data.Maybe (fromJust)
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.Wreq as W
import Options.Applicative.Simple
import qualified Prelude
import RIO
import qualified RIO.HashMap as Hash
import Safe
import Text.CLD2 (Language(Cld2Language_UKRAINIAN), detectLanguageSimple)
import Text.HTML.Scalpel.Core

jsonOpts :: J.Options
jsonOpts = J.defaultOptions {J.fieldLabelModifier = J.camelTo2 '_'}

data App = App
  { appLogFunc :: !LogFunc
  , appWreqOpts :: W.Options
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
  parseJSON =
    J.withObject "PageData" $ \hm ->
      let Just d = Hash.lookup "data" hm
      in J.genericParseJSON jsonOpts d

data Comment = Comment
  { cScore :: Int
  , cText :: Text
  } deriving (Show, Eq, Generic)

instance J.ToJSON Comment where
  toJSON = J.genericToJSON jsonOpts

instance J.FromJSON Comment where
  parseJSON = J.genericParseJSON jsonOpts

getPageData :: CategoryId -> PageId -> RIO App PageData
getPageData catId pageId = do
  let url =
        "https://fashion-api.rozetka.com.ua/goods/get?sort=popularity&category_id=" <>
        show catId <>
        "&page=" <>
        show pageId <>
        "&lang=ua"
  logDebug $ display $ "Downloading: " <> tshow url
  wreqOpts <- asks appWreqOpts
  r <- liftIO $ W.asJSON =<< W.getWith wreqOpts url
  threadDelay 500000
  return (r ^. W.responseBody)

getProductIds :: CategoryId -> RIO App [ProductId]
getProductIds catId = do
  p1data <- getPageData catId 1
  restIds <-
    forM [2 .. totalPages p1data] $ \page -> do
      pdata <- getPageData catId page
      return $ ids pdata
  return (ids p1data ++ concat restIds)

parseComments :: Scraper Text [Comment]
parseComments =
  chroots ("article" @: [hasClass "pp-review-i"]) $ do
    cScoreMay1 <-
      fmap
        (readDef (fail "couldn't parse rating") . T.unpack)
        (attr "content" ("span" @: [hasClass "g-rating-stars-i"]))
    cScoreMay2 <-
      fmap fromStyle (attr "style" ("span" @: [hasClass "g-rating-stars-i"]))
    cTextRaw <- text ("div" @: [hasClass "pp-review-text-i"])
    let cText = T.strip cTextRaw
    let cScore = fromMaybe 0 (cScoreMay1 <|> cScoreMay2)
    return Comment {..}
  where
    fromStyle t =
      fmap
        (`div` 20)
        (readMay (S.toString (T.dropEnd 1 (T.drop (T.length "width:") t))))

testParseComments01 :: IO ()
testParseComments01 = do
  t <- readFileUtf8 "data/comments_sprite_score.html"
  -- t <- readFileUtf8 "data/comments_multi_page.html"
  let (Just coms) = scrapeStringLike t parseComments
  forM_ coms $ \Comment {..} -> do
    Prelude.putStrLn $ "Rating: " <> show cScore
    T.putStrLn $ "Comment: \n" <> cText

parseNumCommentPages :: Scraper Text Int
parseNumCommentPages = do
  pageNumTexts <-
    chroots ("li" @: [hasClass "paginator-catalog-l-i"]) (text "span")
  let pageNums = map (readDef 1 . S.toString) pageNumTexts
  case pageNums of
    [] -> return 1
    _ -> return (Prelude.last pageNums)

testParseNumCommentPages01 :: IO ()
testParseNumCommentPages01 = do
  t <- readFileUtf8 "data/comments_multi_page.html"
  let (Just numPages) = scrapeStringLike t parseNumCommentPages
  Prelude.print numPages

getComments :: ProductId -> PageId -> RIO App [Comment]
getComments productId page = do
  let url =
        "https://rozetka.com.ua/ua/" <> show productId <> "/p" <> show productId <>
        "/tab=comments;page=" <>
        show page <>
        "/"
  logDebug $ display $ "Downloading: " <> tshow url
  wreqOpts <- asks appWreqOpts
  r <- liftIO $ W.getWith wreqOpts url
  threadDelay 500000
  let comPages =
        fromMaybe 1 $
        scrapeStringLike
          (r ^. W.responseBody . to S.toText)
          parseNumCommentPages
  let comsCurrPage =
        fromMaybe [] $
        scrapeStringLike (r ^. W.responseBody . to S.toText) parseComments
  if comPages <= page
    then return comsCurrPage
    else do
      comsRest <- getComments productId (page + 1)
      return (comsCurrPage ++ comsRest)

downloadCategory :: RIO App ()
downloadCategory = do
  let catId = 4634865 :: CategoryId -- men's sneakers
    -- let catId = 2349092 :: CategoryId -- men's underwear
    -- let catId = 4637799 :: CategoryId -- jeans
  logDebug $ display $ "Grabbing the category id: " <> tshow catId
  productIds <- getProductIds catId
  prodWithComms <-
    forM productIds $ \pId -> do
      cs <- getComments pId 1
      return (pId, cs)
  writeFileUtf8 "data/comments.json" (S.toText (J.encode prodWithComms))
  logDebug $ "Wrote data in data/comments.json"
  return ()

getUkrainian :: [(ProductId, [Comment])] -> [Comment]
getUkrainian pwc = concatMap f pwc
  where
    f (_, coms) = mapMaybe f2 coms
    f2 com =
      if detectLanguageSimple (cText com) == Cld2Language_UKRAINIAN
        then Just com
        else Nothing

processComments :: RIO App ()
processComments = do
  commentsFile <- readFileUtf8 "data/comments_sneakers.json"
  let prodWithComms :: [(ProductId, [Comment])]
      prodWithComms = fromJust (J.decode (S.fromText commentsFile))
      ukrainian = getUkrainian prodWithComms
      withScore = filter (\c -> cScore c > 0) ukrainian
  logDebug $ display $ "Number of ukrainian comments with score: " <>
    tshow (length withScore)
  return ()

app :: RIO App ()
app
  -- downloadCategory
 = do
  processComments

main :: IO ()
main = do
  (verbose, ()) <-
    simpleOptions
      "0.1.0.0"
      "rozetka reviews"
      "rozetka review extractor"
      (flag True False (long "no-verbose"))
      empty
  lo <- logOptionsHandle stdout verbose
  W.withManager $ \appWreqOpts ->
    withLogFunc lo $ \appLogFunc -> runRIO App {..} $ do app
