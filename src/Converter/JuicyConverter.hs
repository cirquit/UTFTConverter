module Main where

import Converter (jpgToC, jpgToRaw)

import System.FilePath.Posix (takeBaseName, takeExtension, takeDirectory, (</>))
import System.Directory      (doesDirectoryExist, getCurrentDirectory,
                              getDirectoryContents, createDirectoryIfMissing)
import System.Environment    (getArgs, getProgName)

type Args = [String]

main :: IO()
main = do
  args <- getArgs
  print args
  case args of
    (_)
      | "/c" `elem` args, "/r" `notElem` args -> do
        files    <- args `getFilesFor` "/c"
        savepath <- getSavePath args
        putStrLn $ "Got this savepath in /c: " ++ show savepath
        mapM_ (jpgToC savepath) files
      | "/r" `elem` args, "/c" `notElem` args -> do
        files    <- args `getFilesFor` "/r"
        savepath <- getSavePath args
        putStrLn $ "Got this savepath in /r: " ++ show savepath
        mapM_ (jpgToRaw savepath) files
    (_) -> getProgName >>= help

getFilesFor :: Args -> String -> IO [FilePath]
getFilesFor l delim = go (takeWhile (delim /=) l)
  where go :: [String] -> IO [FilePath]
        go []     = return []
        go (fp:xs) = do
          case takeExtension fp of
            (".jpg") -> go xs >>= \x -> return (fp : x)
            (_)      -> putStrLn "This format is not supported!" >> go xs

getSavePath :: Args -> IO FilePath
getSavePath l = do
  let savepath = dropWhile ("/o" /=) l
  if length savepath == 2
    then  do
      let path = savepath !! 1
      createDirectoryIfMissing True path
      return path
    else getCurrentDirectory

help :: String -> IO ()
help name = do
  putStrLn   "Usage:                                           "
  putStrLn $ "      " ++ name ++ " <filespec> /c|r [/o <path>]\n"
  putStrLn   "<filespec>:  File(s) to convert"
  putStrLn   "parameters: /c        - Create output as .c array files"
  putStrLn   "            /r        - Create output as .raw files"
  putStrLn   "            /o <path> - Set the output directory to <path>\n"
  putStrLn   "You must specify either /c or /r. All other parameters are optional."
  putStrLn   "If /o is ommited the current directory will be used for output."

