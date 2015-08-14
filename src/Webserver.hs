{-# LANGUAGE OverloadedStrings #-}
module Webserver where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import           Snap.Util.FileUploads
import qualified Data.Enumerator.List as EL
import Control.Monad (liftM)
import Control.Applicative ((<$>))


import qualified Data.ByteString.UTF8 as BS8 (toString, fromString)
import qualified Data.ByteString                as BS  (readFile, ByteString(..), pack, writeFile, concat)
import Control.Monad.IO.Class (liftIO)


import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T (pack)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.FilePath.Posix ((</>))

import RawRGB


main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (method GET homeHandler)                <|> -- /
    dir "static" (serveDirectory ".")             <|> -- /static/FILE
    route [ ("picCount", picInfoHandler)
          , ("upload"  , uploadHandler)
          , ("convert", convertHandler)
         -- ("instagram/:user", instagramHandler)
          ]

homeHandler :: Snap ()
homeHandler = writeBS "#This is my first Snap Server!"

picInfoHandler :: Snap ()
picInfoHandler = do
  content <- liftIO $ getCurrentDirectory >>= \fp -> getDirectoryContents (fp </> "raw")
  let elements  = filter (`notElem` [".", ".."]) content
  let elementsf = map (\x -> '#' : x ++ "\n") elements
  writeBS $ encodeUtf8 $ T.pack $ '#' : show ((length elements) - 1) ++ '\n' : concat elementsf


convertHandler :: Snap ()
convertHandler = do
  elements <- liftIO $ do
    convertJpgDirToBMP "jpg" "raw"
    convertBmpDirToTxt "raw" "raw"
    removeWithExtentionAt "raw" ".bmp"
    filter (`notElem` [".", ".."]) <$> getDirectoryContents "raw"
  writeBS $ encodeUtf8 $ T.pack $ '#' : show (length elements)









uploadHandler :: Snap ()
uploadHandler = method POST uploadPost <|> error405

uploadPost :: Snap ()
uploadPost = do
  --files <- handleMultipart defaultUploadPolicy $ \part -> do
  --  content <- liftM BS.concat EL.consume
  --  return (part, content)
  --saveFiles files
  mcount <- getPostParam "count"
  mbs    <- getPostParam "bytestring"
  case (mcount, mbs) of
    (Just count, Just bs) -> do
      liftIO $ putStrLn $ BS8.toString count
  --    let name = BS8.toString count
  --    saveFile ((undefined, bs), name)
  --    liftIO $ jpgToBMP (name ++ ".jpg")
    (_)                   -> return ()

-- saveFiles :: [(PartInfo, ByteString)] -> Snap ()
-- saveFiles fs = mapM_ saveFile (zip fs [0..])

saveFile :: ((PartInfo, BS.ByteString), String) -> Snap ()
saveFile ((_, bs), count) = liftIO $ BS.writeFile (count ++ ".jpg") bs

error405 = genericError 405 "Method Not Allowed"

genericError :: Int -> String -> Snap ()
genericError c s = do
  modifyResponse $ setResponseStatus c $ BS8.fromString s
  writeText $ T.pack ((show c) ++ " - " ++ s)
  r <- getResponse
  finishWith r



-- instagramHandler :: Snap ()
-- instagramHandler = do
--     user <- getParam "user"
--     let url = "http://iconosquare.com/feed/" ++ (BS8.toString (fromJust user))
--     urls <- liftIO (getUserPics url)
--     mapM_ writeText urls

    --maybe (writeBS "must specify echo/param in URL")
    --       writeBS
    --       param
