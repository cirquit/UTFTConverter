module Main where

import Format.RGB565                   (toHex, toNHex, toRGB565, toRGB565Hex)
import Format.Converter                (pictureToRaw, toMaybeFormat, Format())
import Test.Hspec
import System.Directory                (getCurrentDirectory, doesDirectoryExist,
                                        createDirectory, getDirectoryContents,
                                        removeDirectoryRecursive)
import System.FilePath.Posix           ((</>))
import Data.Maybe                      (fromJust)
import Data.List                       (foldl')

main :: IO ()
main = do
  dir <- getCurrentDirectory
  exist <- doesDirectoryExist "tmp"
  if exist
    then removeDirectoryRecursive "tmp"
    else return ()
  hspec $ do
    describe "UTFTConverter Library" $ do
      describe "Format.RGB565"    $ do
        describe "toHex" $ do
          it "toHex should be 0" $ do
            toHex 0     `shouldBe` "0"
          it "toHex should be C" $ do
            toHex 12    `shouldBe` "C"
          it "toHex should be C6" $ do
            toHex 198   `shouldBe` "C6"
          it "toHex should be 7A" $ do
            toHex 122   `shouldBe` "7A"
          it "toHex should be FF" $ do
            toHex 255   `shouldBe` "FF"
          it "toHex should be 10000" $ do
            toHex 65536 `shouldBe` "10000"

        describe "toNHex" $ do
          it "toNHex 4 0 should be 0000" $ do
            toNHex 4 0     `shouldBe` "0000"
          it "toNHex 4 12 should be 000C" $ do
            toNHex 4 12    `shouldBe` "000C"
          it "toNHex 4 198 should be 00C6" $ do
            toNHex 4 198   `shouldBe` "00C6"
          it "toNHex 4 122 should be 007A" $ do
            toNHex 4 122   `shouldBe` "007A"
          it "toNHex 4 255 should be 00FF" $ do
            toNHex 4 255   `shouldBe` "00FF"
          it "toNHex 4 65535 should be FFFF" $ do
            toNHex 4 65535 `shouldBe` "FFFF"

        describe "toRGB565" $ do
          it "toRGB565 (255, 255, 255) should be 65535" $ do
            toRGB565 (255, 255, 255) `shouldBe` 65535
          it "toRGB565 (123, 123, 123) should be 31695" $ do
            toRGB565 (123, 123, 123) `shouldBe` 31695
          it "toRGB565 (90, 255, 0) should be 24544" $ do
            toRGB565 ( 90, 255,   0) `shouldBe` 24544
          it "toRGB565 (0, 0, 0) should be 0" $ do
            toRGB565 (  0,   0,   0) `shouldBe` 0

        describe "toRGB565Hex" $ do
          it "toRGB565Hex (255, 255, 255) should be FFFF" $ do
            toRGB565Hex (255, 255, 255) `shouldBe` "FFFF"
          it "toRGB565Hex (123, 123, 123) should be 7BCF" $ do
            toRGB565Hex (123, 123, 123) `shouldBe` "7BCF"
          it "toRGB565Hex (90, 255, 0) should be 5FE0" $ do
            toRGB565Hex ( 90, 255,   0) `shouldBe` "5FE0"
          it "toRGB565Hex (0, 0, 0) should be 0" $ do
            toRGB565Hex (  0,   0,   0) `shouldBe` "0000"

      describe "Format.Converter" $ do
        it "tests/examples/cat_01 exists" $ do
          exists <- doesDirectoryExist (dir </> "tests" </> "examples" </> "cat_01")
          exists `shouldBe` True
        it "tests/examples/cat_02 exists" $ do
          exists <- doesDirectoryExist (dir </> "tests" </> "examples" </> "cat_02")
          exists `shouldBe` True
        it "tests/examples/cat_03 exists" $ do
          exists <- doesDirectoryExist (dir </> "tests" </> "examples" </> "cat_03")
          exists `shouldBe` True
        it "15 example files should be in tests/examples/cat_0?" $ do
          dir1 <- getExamplePicsPath dir ("tests" </> "examples" </> "cat_01")
          dir2 <- getExamplePicsPath dir ("tests" </> "examples" </> "cat_02")
          dir3 <- getExamplePicsPath dir ("tests" </> "examples" </> "cat_03")
          length (dir1 ++ dir2 ++ dir3) `shouldBe` 15
        it "cat_01 pics should be all converted to almost the same .raw-files (>99% similarity)" $ do
          createDirectory "tmp"
          pics <- getExampleFormatPicsPath dir ("tests" </> "examples" </> "cat_01")
          mapM_ (pictureToRaw (dir </> "tmp")) pics
          rawfps <- getExamplePicsPath dir "tmp"
          [p1, p2, p3, p4, p5] <- mapM readFile rawfps
          let r1 = picSimilarity p1 p2
              r2 = picSimilarity p1 p3
              r3 = picSimilarity p1 p4
              r4 = picSimilarity p1 p5
          putStrLn $ "Pic similarity for p1 ~ p2: " ++ show r1
          putStrLn $ "Pic similarity for p1 ~ p3: " ++ show r2
          putStrLn $ "Pic similarity for p1 ~ p4: " ++ show r3
          putStrLn $ "Pic similarity for p1 ~ p5: " ++ show r4
          removeDirectoryRecursive (dir </> "tmp")
          ((r1 + r2 + r3 + r4) / 4) > 98 `shouldBe` True
        it "cat_02 pics should be all converted to almost the same .raw-files (>99% similarity)" $ do
          createDirectory "tmp"
          pics <- getExampleFormatPicsPath dir ("tests" </> "examples" </> "cat_02")
          mapM_ (pictureToRaw (dir </> "tmp")) pics
          rawfps <- getExamplePicsPath dir "tmp"
          [p1, p2, p3, p4, p5] <- mapM readFile rawfps
          let r1 = picSimilarity p1 p2
              r2 = picSimilarity p1 p3
              r3 = picSimilarity p1 p4
              r4 = picSimilarity p1 p5
          putStrLn $ "Pic similarity for p1 ~ p2: " ++ show r1
          putStrLn $ "Pic similarity for p1 ~ p3: " ++ show r2
          putStrLn $ "Pic similarity for p1 ~ p4: " ++ show r3
          putStrLn $ "Pic similarity for p1 ~ p5: " ++ show r4
          removeDirectoryRecursive (dir </> "tmp")
          ((r1 + r2 + r3 + r4) / 4) > 98 `shouldBe` True
        it "cat_03 pics should be all converted to almost the same .raw-files (>99% similarity)" $ do
          createDirectory "tmp"
          pics <- getExampleFormatPicsPath dir ("tests" </> "examples" </> "cat_03")
          mapM_ (pictureToRaw (dir </> "tmp")) pics
          rawfps <- getExamplePicsPath dir "tmp"
          [p1, p2, p3, p4, p5] <- mapM readFile rawfps
          let r1 = picSimilarity p1 p2
              r2 = picSimilarity p1 p3
              r3 = picSimilarity p1 p4
              r4 = picSimilarity p1 p5
          putStrLn $ "Pic similarity for p1 ~ p2: " ++ show r1
          putStrLn $ "Pic similarity for p1 ~ p3: " ++ show r2
          putStrLn $ "Pic similarity for p1 ~ p4: " ++ show r3
          putStrLn $ "Pic similarity for p1 ~ p5: " ++ show r4
          removeDirectoryRecursive (dir </> "tmp")
          ((r1 + r2 + r3 + r4) / 4) > 98 `shouldBe` True

getExamplePicsPath :: FilePath -> FilePath -> IO [FilePath]
getExamplePicsPath curdir picdir = do
  content <- getDirectoryContents (curdir </> picdir)
  let picnames = filter (`notElem` [".", ".."]) content
  return $ map (\x -> curdir </> picdir </> x) picnames

getExampleFormatPicsPath :: FilePath -> FilePath -> IO [(Format, FilePath)]
getExampleFormatPicsPath curdir picdir = do
  content <- getDirectoryContents (curdir </> picdir)
  let picnames = filter (`notElem` [".", ".."]) content
      picpaths = map (\x -> curdir </> picdir </> x) picnames
  flip zip picpaths . map fromJust <$> mapM toMaybeFormat picpaths

picSimilarity :: String -> String -> Double
picSimilarity p1 p2 =
  let f1 = words p1
      f2 = words p2
      (fcount, count) = foldl' (\(fc, c) (a, b) -> if a == b
                                                     then (fc    , c + 1)
                                                     else (fc + 1, c + 1)) (0,0) (zip f1 f2)
  in (100 - (fcount / count))
