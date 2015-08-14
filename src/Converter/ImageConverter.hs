{-# LANGUAGE TypeSynonymInstances #-}

module RawRGB where

import qualified Data.ByteString.UTF8  as BSU8 (toString, fromString)
import qualified Data.ByteString       as BS   (readFile, ByteString(..), pack, writeFile, concat)
import qualified Data.ByteString.Lazy  as BSL  (writeFile)
import Data.Maybe             (fromJust)
import Codec.Picture.Jpg      (decodeJpeg)
import Codec.Picture          (readImage, pixelAt, PixelRGB8(..), decodeBitmap)
import Codec.Picture.Saving   (imageToBitmap)
import qualified Data.ByteString.Builder as BB
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T (pack)
import Codec.Picture.Types
import System.Directory (getDirectoryContents, getCurrentDirectory, doesFileExist, removeFile)
import System.FilePath.Posix (takeBaseName, takeFileName, (</>),
                              splitExtension, takeExtension)
import Data.Function (on)
import Data.List (sortBy)

convertMyDirs :: IO ()
convertMyDirs = do
    convertJpgDirToBMP "jpg" "raw"
    convertBmpDirToTxt "raw" "raw"
    removeWithExtentionAt "raw" ".bmp"

convertJpgtoRaw :: FilePath -> IO()
convertJpgtoRaw path = do
  cur <- getCurrentDirectory
  convertJpgToBMP cur path
  let name = takeBaseName path
  convertBmpDirToTxt cur (name ++ ".bmp")

--  ________________________
-- |                        |
-- | BMP to RAW  Conversion |
-- |________________________|

convertBmpDirToTxt :: FilePath -- directory to get the .bmp from
                   -> FilePath -- directory to save them to as .txt (as many as possible)
                   -> IO ()
convertBmpDirToTxt bmpdir txtdir = do
  content <- getDirectoryContents bmpdir
  let elements = filter (`notElem` [".", ".."]) content
  mapM_ (bmpToRaw txtdir . (bmpdir </>)) elements


bmpToRaw :: FilePath ->  -- directory to save to
            FilePath ->  -- filepath to the bmp
            IO ()
bmpToRaw dir fp = do
    image <- readImage fp
    case image of
      Left _ -> putStrLn $ "Sorry, not a supported codec for " ++ fp
      Right dynimg -> do
        let imgrgba8 = fromDynamicImage dynimg
            name     = takeBaseName fp
            pixels   = encodePixels imgrgba8
        writeFile (dir </> name ++ ".txt") (concat pixels)

encodePixels img@(Image w h _) = [ format (pixelAt img x y) x y | x <- [0..(w-1)], y <- [0..(h-1)]]

  where format (PixelRGBA8 r g b _) j k = "#" ++ show r ++ "$"
                                              ++ show g ++ "$"
                                              ++ show b ++ "$"
                                              ++ show j ++ "$"
                                              ++ show k ++ "*&\n"


--  ________________________
-- |                        |
-- | JPEG to BMP Conversion |
-- |________________________|


convertJpgDirToBMP :: FilePath -- directory to get the .jpgs from
                    -> FilePath -- directory to save them to as .bmp (as many as possible)
                    -> IO ()
convertJpgDirToBMP jpegdir bmpdir = do
  content <- getDirectoryContents jpegdir
  let elements = filter (`notElem` [".", ".."]) content
  mapM_ (convertJpgToBMP bmpdir . (jpegdir </>)) elements


-- | tries to check if a .jpg-file exists and saves it
--   if possible as .bmp in the specified directory

convertJpgToBMP :: FilePath -- directory to save to
         -> FilePath -- filepath to the .jpg
         -> IO ()
convertJpgToBMP dir fp = do
  exists <- doesFileExist fp
  case splitExtension (takeFileName fp) of
    (name, ".jpg") -> BS.readFile fp >>= \bs -> toBMP dir (bs, name)
    (_)            -> putStrLn $ "Sorry, " ++ fp ++ "is not a .jpg!"


-- | tries to decode a .jpg-file and saves it as a .bmp

toBMP :: FilePath               -- directory to save to
     -> (BS.ByteString, String) -- jpg-bytestring and its name
     -> IO ()
toBMP dir (bs, name) = do
  case decodeJpeg bs of
    (Right dynIm) -> BSL.writeFile (dir </> name ++ ".bmp") (imageToBitmap dynIm)
    (Left  _)     -> putStrLn $ "Sorry, I couldn't decode " ++ name ++ ".jpg..."


-- | removes every file in the directory with the specified extention

removeWithExtentionAt :: FilePath   -- file directory
                      -> String     -- extention
                      -> IO ()
removeWithExtentionAt dir extention = do
  contents <- getDirectoryContents dir
  let contents' = filter (`notElem` [".", ".."]) contents
  let files      = filter (\x -> takeExtension x == extention) contents'
  mapM_ (removeFile . (dir </>)) files






-- Copied from
-- See http://hackage.haskell.org/package/JuicyPixels-util-0.2/docs/Codec-Picture-RGBA8.html

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
fromDynamicImage (ImageY8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageYA8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGB8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGBA8 img) = img

-- end of Codec.Picture.RGBA8