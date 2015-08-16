{-# LANGUAGE TypeSynonymInstances #-}
module Converter (pictureToRaw, pictureToC) where

import qualified Data.ByteString as BS (readFile)
import Data.ByteString.Lazy            (toStrict)
import System.FilePath.Posix           (takeBaseName, (</>), takeExtension)
import Data.Time                       (getCurrentTime, UTCTime())

import Codec.Picture.Types
import Codec.Picture.Saving (imageToBitmap)
import Codec.Picture.Jpg    (decodeJpeg)
import Codec.Picture.Bitmap (decodeBitmap)

import RGB565               (toRGB565Hex, to4Hex)
import C                    (toCFile)
import Raw                  (toRawFile)


pictureToRaw :: FilePath -> FilePath -> IO ()
pictureToRaw saveTo fp = do
  case takeExtension fp of
    (".jpg") -> jpgToDynImg fp >>= dynimgtoRaw saveTo fp
    (".bmp") -> bmpToDynImg fp >>= dynimgtoRaw saveTo fp
    (_)      -> error "Argument filter let through some unsupportet types"

pictureToC :: FilePath -> FilePath -> IO ()
pictureToC saveTo fp = do
  case takeExtension fp of
    (".jpg") -> jpgToDynImg fp >>= dynimgtoC saveTo fp
    (".bmp") -> bmpToDynImg fp >>= dynimgtoC saveTo fp
    (_)      -> error "Argument filter let through some unsupportet types"


jpgToDynImg :: FilePath -> IO (Maybe DynamicImage)
jpgToDynImg fp = do
  bs <- BS.readFile fp
  case decodeJpeg bs of
    Left err     -> putStrLn ("Error happend while decoding the jpg: " ++ err) >> return Nothing
    Right dynimg ->
      case decodeBitmap (toStrict (imageToBitmap dynimg)) of
        Left err'     -> putStrLn ("Error happend while decoding the bmp: " ++ err') >> return Nothing
        Right dynimg' -> return $ Just dynimg'


bmpToDynImg :: FilePath -> IO (Maybe DynamicImage)
bmpToDynImg fp = do
  bs <- BS.readFile fp
  case decodeBitmap bs of
    Left err     -> putStrLn ("Error happend while decoding the bmp: " ++ err) >> return Nothing
    Right dynimg -> return $ Just dynimg



dynimgtoRaw :: FilePath -> FilePath -> Maybe DynamicImage -> IO ()
dynimgtoRaw      _  _      Nothing  = return ()
dynimgtoRaw saveTo fp (Just dynimg) = do
  time <- getCurrentTime
  let img  = fromDynamicImage dynimg
      name = takeBaseName fp
      content = toRawFile (encodePixels img)
  writeFile (saveTo </> name ++ ".raw") content
  putStrLn $ "Converted " ++ name ++ ".raw"

dynimgtoC :: FilePath -> FilePath -> Maybe DynamicImage -> IO ()
dynimgtoC _       _      Nothing  = putStrLn "Something happened..."
dynimgToC saveTo fp (Just dynimg) = do
  time <- getCurrentTime
  let img@(Image w h _) = fromDynamicImage dynimg
      name              = takeBaseName fp
      content           = toCFile (encodePixels img) name (w, h) time
  writeFile (saveTo </> name ++ ".c") content
  putStrLn $ "Converted " ++ name ++ ".c"







encodePixels :: Image PixelRGBA8 -> [String]
encodePixels img@(Image w h _) = [ format (pixelAt img y x) | x <- [0..(h-1)], y <- [0..(w-1)]]
  where format (PixelRGBA8 r g b _) = toRGB565Hex (r, g, b)

---- Copied from
---- See http://hackage.haskell.org/package/JuicyPixels-util-0.2/docs/Codec-Picture-RGBA8.html

class ToPixelRGBA8 a where
    toRGBA8 :: a -> PixelRGBA8

instance ToPixelRGBA8 Pixel8 where
    toRGBA8 b = PixelRGBA8 b b b 255

instance ToPixelRGBA8 PixelYA8 where
    toRGBA8 (PixelYA8 l a) = PixelRGBA8 l l l a

instance ToPixelRGBA8 PixelRGB8 where
    toRGBA8 (PixelRGB8 r g b) = PixelRGBA8 r g b 255

instance ToPixelRGBA8 PixelRGBA8 where
    toRGBA8 = id

fromDynamicImage :: DynamicImage -> Image PixelRGBA8
fromDynamicImage (ImageY8 img)    = pixelMap toRGBA8 img
fromDynamicImage (ImageYA8 img)   = pixelMap toRGBA8 img
fromDynamicImage (ImageRGB8 img)  = pixelMap toRGBA8 img
fromDynamicImage (ImageRGBA8 img) = img

---- end of Codec.Picture.RGBA8