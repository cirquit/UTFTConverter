module Main where

import Converter (jpgToC, jpgToRaw)

import System.FilePath.Posix (takeBaseName, takeExtension, takeDirectory, (</>))
import System.Directory      (doesDirectoryExist, getCurrentDirectory,
                              getDirectoryContents)
import System.Environment    (getArgs, getProgName)

main :: IO()
main = do
  args <- getArgs
  case args of
    (fpath:"/c":spath) -> do
      savepath <- getSavePath spath
      files    <- getFiles    fpath
      mapM_ (jpgToC savepath) files
    (fpath:"/r":spath) -> do
      savepath <- getSavePath spath
      files    <- getFiles    fpath
      mapM_ (jpgToRaw savepath) files
    (_) -> getProgName >>= help

getFiles :: FilePath -> IO [FilePath]
getFiles fp = do
  let name = takeBaseName  fp
      dir  = takeDirectory fp
      ext  = takeExtension fp
  exists <- doesDirectoryExist dir
  case (exists, name, ext) of
    (False,   _,      _) -> putStrLn "This directory doesn't exist" >> return []
    (_    , "*", ".jpg") -> do
      contents <- getDirectoryContents dir
      let files = filter (`notElem` [".", ".."]) contents
          jpgs  = filter (\x -> takeExtension x == ".jpg") files
      return $ map (dir </>) jpgs
    (_    ,   _, ".jpg") -> return [fp]

getSavePath :: [String] -> IO FilePath
getSavePath ("/o":dir:[]) = do
  exists <- doesDirectoryExist dir
  if exists then return dir
            else do
              putStrLn "This directory doesn't exists, using the current instead"
              getCurrentDirectory
getSavePath _                 = getCurrentDirectory

help :: String -> IO ()
help name = do
  putStrLn   "Usage:                                           "
  putStrLn $ "      " ++ name ++ " <filespec> /c|r [/o <path]\n"
  putStrLn   "<filespec>:  File(s) to convert"
  putStrLn   "parameters: /c        - Create output as .c array files"
  putStrLn   "            /r        - Create output as .raw files"
  putStrLn   "            /o <path> - Set the output directory to <path>\n"
  putStrLn   "You must specify either /c or /r. All other parameters are optional."
  putStrLn   "If /o is ommited the current directory will be used for output."

