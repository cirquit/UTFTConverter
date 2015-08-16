{-# LANGUAGE TypeSynonymInstances #-}
module Converter (pictureToRaw, pictureToC) where

import qualified Data.ByteString as BS (readFile)
import Data.ByteString.Lazy            (toStrict)
import System.FilePath.Posix           (takeBaseName, (</>), takeExtension)
import Data.Time                       (getCurrentTime)

import Codec.Picture.Types
import Codec.Picture.Saving (imageToBitmap)
import Codec.Picture.Jpg    (decodeJpeg)
import Codec.Picture.Bitmap (decodeBitmap)
import Codec.Picture.Png    (decodePng)

import RGB565               (toRGB565Hex)
import C                    (toCFile)
import Raw                  (toRawFile)


pictureToRaw :: FilePath -> FilePath -> IO ()
pictureToRaw saveTo fp = do
  case takeExtension fp of
    (".jpg") -> jpgToDynImg fp >>= dynimgToRaw saveTo fp
    (".bmp") -> bmpToDynImg fp >>= dynimgToRaw saveTo fp
    (".png") -> pngToDynImg fp >>= dynimgToRaw saveTo fp
    (_)      -> error "Argument filter let through some unsupported types"

pictureToC :: FilePath -> FilePath -> IO ()
pictureToC saveTo fp = do
  case takeExtension fp of
    (".jpg") -> jpgToDynImg fp >>= dynimgToC saveTo fp
    (".bmp") -> bmpToDynImg fp >>= dynimgToC saveTo fp
    (".png") -> pngToDynImg fp >>= dynimgToC saveTo fp
    (_)      -> error "Argument filter let through some unsupported types"

jpgToDynImg :: FilePath -> IO (Maybe DynamicImage)
jpgToDynImg fp = do
  bs <- BS.readFile fp
  case decodeJpeg bs of
    Left err     -> putStrLn ("Error happend while decoding the jpg: " ++ err) >> return Nothing
    Right dynimg ->
      case decodeBitmap (toStrict (imageToBitmap dynimg)) of
        Left err'     -> putStrLn ("Error happend while decoding the converted bmp: " ++ err') >> return Nothing
        Right dynimg' -> return $ Just dynimg'

pngToDynImg :: FilePath -> IO (Maybe DynamicImage)
pngToDynImg fp = do
  bs <- BS.readFile fp
  case decodePng bs of
    Left err     -> putStrLn ("Error happend while decoding the png: " ++ err) >> return Nothing
    Right dynimg -> return $ Just dynimg

bmpToDynImg :: FilePath -> IO (Maybe DynamicImage)
bmpToDynImg fp = do
  bs <- BS.readFile fp
  case decodeBitmap bs of
    Left err     -> putStrLn ("Error happend while decoding the bmp: " ++ err) >> return Nothing
    Right dynimg -> return $ Just dynimg

dynimgToRaw :: FilePath -> FilePath -> Maybe DynamicImage -> IO ()
dynimgToRaw      _  _      Nothing  = return ()
dynimgToRaw saveTo fp (Just dynimg) = do
  let img  = fromDynamicImage dynimg
      name = takeBaseName fp
      content = toRawFile (encodePixels img)
  writeFile (saveTo </> name ++ ".raw") content
  putStrLn $ "Converted " ++ name ++ ".raw"

dynimgToC :: FilePath -> FilePath -> Maybe DynamicImage -> IO ()
dynimgToC _       _      Nothing  = return ()
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
fromDynamicImage _                = error "fromDynamicImage in Converter got an image type it doesn't know..."

---- end of Codec.Picture.RGBA8