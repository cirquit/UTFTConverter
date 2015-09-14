{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Format.Converter
-- License     :  MIT
-- Maintainer  :  Alexander Isenko <alex.isenko@googlemail.com>
--
-- @Format.Converter@ exports the functions that create the @.c@ or @.raw@ files when the @FilePath@ is given
--
-- Both @pictureToRaw@ and @pictureToC@ accept these picture formats
--
-- * @.gif@
-- * @.png@
-- * @.jpg \/\ .jpe \/\ .jpeg@
-- * @.bmp@
-- * @.tga@
--
-- If it's not any of those formats, an error gets thrown and if there is an error while decoding the pictures it only gets printed out (it gets skipped)

-----------------------------------------------------------------------------

module Format.Converter (pictureToRaw, pictureToC) where

import qualified Data.ByteString as BS (readFile)
import Data.ByteString.Lazy            (toStrict)
import System.FilePath.Posix           (takeBaseName, (</>),
                                        takeExtension, takeFileName)
import Data.Time                       (getCurrentTime)

import Codec.Picture.Types
import Codec.Picture.Saving            (imageToBitmap)
import Codec.Picture.Jpg               (decodeJpeg)
import Codec.Picture.Bitmap            (decodeBitmap)
import Codec.Picture.Png               (decodePng)
import Codec.Picture.Gif               (decodeGif)
import Codec.Picture.Tga               (decodeTga)

import Format.RGB565                   (toRGB565Hex)
import Format.C                        (toCFile, Platform())
import Format.Raw                      (toRawFile)

-- | pictureToRaw takes a picture, decodes it, parses every pixel to a 4 digit RGB565 hex and saves it to
--   a file with the same name and a @.raw@ extention in the specified directory
--
-- This function takes two arguments
--
-- * first @FilePath@ is the directory to save the file to
-- * second @FilePath@ is the filepath to the picture
--
-- __Possible errors:__
--
-- * Can throw an error if the picture format is not supported
-- * If there is an error while decoding the pictures the file gets skipped and an error message is printed out
--
-- __Results:__
--
-- If the conversion was successful, a message gets printed
--
-- @
-- cat_01_bmp_120x120.bmp --> cat_01_bmp_120x120.raw
-- @
--
-- __Example usage:__ (assumed that the picture is in the directory where ghci was started)
--
-- @
-- λ> dir <- getCurrentDirectory
-- λ> pictureToRaw dir "cat_01_bmp_120x120.bmp"
-- cat_01_bmp_120x120.bmp --> cat_01_bmp_120x120.raw
-- @

pictureToRaw :: FilePath -> FilePath -> IO ()
pictureToRaw saveTo fp = do
  case takeExtension fp of
    (".jpg")  -> jpgToDynImg fp >>= dynimgToRaw saveTo fp
    (".jpeg") -> jpgToDynImg fp >>= dynimgToRaw saveTo fp
    (".jpe")  -> jpgToDynImg fp >>= dynimgToRaw saveTo fp
    (".bmp")  -> bmpToDynImg fp >>= dynimgToRaw saveTo fp
    (".png")  -> pngToDynImg fp >>= dynimgToRaw saveTo fp
    (".gif")  -> gifToDynImg fp >>= dynimgToRaw saveTo fp
    (".tga")  -> tgaToDynImg fp >>= dynimgToRaw saveTo fp
    (_)       -> error "Argument filter let through some unsupported types"


-- | pictureToC takes a picture, decodes it, parses every pixel to a 4 digit RGB565 hex, adds the header
--   based on the desired platform and saves it to a file with the same name and a @.c@ extention in the specified
--   directory
--
-- This function takes three arguments
--
-- * @Platform@ is the desired platform to convert to
-- * first @FilePath@ is the directory to save the file to
-- * second @FilePath@ is the filepath to the picture
--
-- __Possible errors:__
--
-- * Can throw an error if the picture format is not supported
-- * If there is an error while decoding the pictures the file gets skipped and an error message is printed out
--
-- __Results:__
--
-- If the conversion was successful, a message gets printed
--
-- @
-- cat_01_bmp_120x120.bmp --> cat_01_bmp_120x120.c
-- @
--
-- __Example usage:__ (assumed that the picture is in the directory where ghci was started)
--
-- @
-- λ> dir <- getCurrentDirectory
-- λ> pictureToC AVR dir "cat_01_bmp_120x120.bmp"
-- cat_01_bmp_120x120.bmp --> cat_01_bmp_120x120.c
-- @

pictureToC :: Platform -> FilePath -> FilePath -> IO ()
pictureToC platform saveTo fp = do
  case takeExtension fp of
    (".jpg")  -> jpgToDynImg fp >>= dynimgToC platform saveTo fp
    (".jpeg") -> jpgToDynImg fp >>= dynimgToC platform saveTo fp
    (".jpe")  -> jpgToDynImg fp >>= dynimgToC platform saveTo fp
    (".bmp")  -> bmpToDynImg fp >>= dynimgToC platform saveTo fp
    (".png")  -> pngToDynImg fp >>= dynimgToC platform saveTo fp
    (".gif")  -> gifToDynImg fp >>= dynimgToC platform saveTo fp
    (".tga")  -> tgaToDynImg fp >>= dynimgToC platform saveTo fp
    (_)       -> error "Argument filter let through some unsupported types"

jpgToDynImg :: FilePath -> IO (Maybe DynamicImage)
jpgToDynImg fp = do
  bs <- BS.readFile fp
  case decodeJpeg bs of
    Left err     -> putStrLn ("Error happend while decoding the jpg: " ++ err) >> return Nothing
    Right dynimg ->
      case decodeBitmap (toStrict (imageToBitmap dynimg)) of
        Left err'     -> putStrLn ("Error happend while decoding the converted bmp: " ++ err') >> return Nothing
        Right dynimg' -> return $ Just dynimg'

bmpToDynImg :: FilePath -> IO (Maybe DynamicImage)
bmpToDynImg fp = do
  bs <- BS.readFile fp
  case decodeBitmap bs of
    Left err     -> putStrLn ("Error happend while decoding the bmp: " ++ err) >> return Nothing
    Right dynimg -> return $ Just dynimg

pngToDynImg :: FilePath -> IO (Maybe DynamicImage)
pngToDynImg fp = do
  bs <- BS.readFile fp
  case decodePng bs of
    Left err     -> putStrLn ("Error happend while decoding the png: " ++ err) >> return Nothing
    Right dynimg -> return $ Just dynimg

gifToDynImg :: FilePath -> IO (Maybe DynamicImage)
gifToDynImg fp = do
  bs <- BS.readFile fp
  case decodeGif bs of
    Left err     -> putStrLn ("Error happend while decoding the gif: " ++ err) >> return Nothing
    Right dynimg -> return $ Just dynimg

tgaToDynImg :: FilePath -> IO (Maybe DynamicImage)
tgaToDynImg fp = do
  bs <- BS.readFile fp
  case decodeTga bs of
    Left err     -> putStrLn ("Error happend while decoding the tga: " ++ err) >> return Nothing
    Right dynimg -> return $ Just dynimg


dynimgToRaw :: FilePath -> FilePath -> Maybe DynamicImage -> IO ()
dynimgToRaw      _  _      Nothing  = return ()
dynimgToRaw saveTo fp (Just dynimg) = do
  let img     = fromDynamicImage dynimg
      name    = takeBaseName fp
      fname   = takeFileName fp
      content = toRawFile (encodePixels img)
  writeFile (saveTo </> name ++ ".raw") content
  putStrLn $ fname ++ " --> " ++ name ++ ".raw"

dynimgToC :: Platform -> FilePath -> FilePath -> Maybe DynamicImage -> IO ()
dynimgToC        _      _  _      Nothing  = return ()
dynimgToC platform saveTo fp (Just dynimg) = do
  time <- getCurrentTime
  let img@(Image w h _) = fromDynamicImage dynimg
      name              = takeBaseName fp
      ext               = takeExtension fp
      fname             = takeFileName fp
      content           = toCFile (encodePixels img) (name, ext) (w, h) time platform
  writeFile (saveTo </> name ++ ".c") content
  putStrLn $ fname ++ " --> " ++ name ++ ".c"

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
fromDynamicImage _                = error "fromDynamicImage in Converter got a not supported DynamicImage format!"

---- end of Codec.Picture.RGBA8