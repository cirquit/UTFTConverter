module Main where

import System.FilePath.Posix (takeExtension)
import System.Directory      (getCurrentDirectory, createDirectoryIfMissing)
import System.Environment    (getArgs, getProgName)

import Control.Applicative   (pure, (<$>), (<*>))

import Format.Converter      (pictureToC, pictureToRaw)
import Format.C              (Platform(..))

type Args = [String]

data FileType = Raw
              | C

instance Show FileType where
  show C   = ".c array file(s)"
  show Raw = ".raw file(s)"

data ProcessingInfo = PInfo { mplatform :: Maybe Platform
                            , outdir    :: FilePath
                            , filetype  :: FileType
                            }

main :: IO()
main = do
  args <- getArgs
  case args of
    (_)
      | "/c" `elem` args, "/r" `notElem` args -> do
        files            <- args `getFilesFor` "/c"
        (savepath, rest) <- getSavePath args
        pltform          <- getPlatform rest
        printProcessingInfo $ PInfo { mplatform = (Just pltform), outdir = savepath, filetype = C }
        mapM_ (pictureToC pltform savepath) files
      | "/r" `elem` args, "/c" `notElem` args -> do
        files            <- args `getFilesFor` "/r"
        (savepath, rest) <- getSavePath args
        _                <- getPlatform rest
        printProcessingInfo $ PInfo { mplatform = Nothing, outdir = savepath, filetype = Raw }
        mapM_ (pictureToRaw savepath) files
    (_) -> getProgName >>= help

getFilesFor :: Args -> String -> IO [FilePath]
getFilesFor l delim = go (takeWhile (delim /=) l)
  where go :: [String] -> IO [FilePath]
        go []      = return []
        go (fp:xs) = do
          case takeExtension fp of
            (".jpg")  -> go xs >>= \x -> return (fp : x)
            (".jpeg") -> go xs >>= \x -> return (fp : x)
            (".jpe")  -> go xs >>= \x -> return (fp : x)
            (".bmp")  -> go xs >>= \x -> return (fp : x)
            (".png")  -> go xs >>= \x -> return (fp : x)
            (".gif")  -> go xs >>= \x -> return (fp : x)
            (".tga")  -> go xs >>= \x -> return (fp : x)
            (_)       -> putStrLn ("This format is not supported ~ " ++ fp) >> go xs

getSavePath :: Args -> IO (FilePath, Args)
getSavePath l = do
  case dropWhile ("/o" /=) l of
    ("/o" : "/t" : rest)        -> putStrLn "WARNING: Output directory missing. Using default output directory."
                                >> getCurrentDirectory      >>= \dir -> return (dir, rest)
    ("/o" : dir  : "/t" : rest) -> createDirectoryIfMissing True dir >> return (dir, rest)
    ("/o" : dir  : rest)        -> putStrLn "WARNING: More than one output directory specified, using the first one."
                                >> createDirectoryIfMissing True dir >> return (dir, rest)
    _                           -> (,) <$> getCurrentDirectory <*> pure l

getPlatform :: Args -> IO Platform
getPlatform l =
    case dropWhile ("/t" /=) l of
      ("/t" : "ARM"   : rest) -> warning rest >> return ARM
      ("/t" : "AVR"   : rest) -> warning rest >> return AVR
      ("/t" : "PIC32" : rest) -> warning rest >> return PIC32
      [""]                    -> return AVR
      ("/t": rest   )         -> do
        putStrLn $ "WARNING: Unknown target platform " ++ unwords rest ++ " will be ignored. Using default target platform.\n"
        warning rest
        return AVR
      rest            -> warning rest >> return AVR
  where warning = mapM_ (\x -> putStrLn $ "WARNING: Unknown parameter " ++ x)

printProcessingInfo :: ProcessingInfo -> IO ()
printProcessingInfo (PInfo        Nothing savepath fltype) = do
  putStrLn $ "Converting to     : " ++ show fltype ++ "\n"
  putStrLn $ "Output directory  : " ++ savepath      ++ "\n"
  putStrLn   "Processing file(s):"
printProcessingInfo (PInfo (Just platform) savepath fltype) = do
  putStrLn $ "Converting to     : " ++ show fltype
  putStrLn $ "Target platform   : " ++ show platform ++ "\n"
  putStrLn $ "Output directory  : " ++ savepath      ++ "\n"
  putStrLn   "Processing file(s):"


help :: String -> IO ()
help name = do
  putStrLn   "Usage:                                           "
  putStrLn $ "      " ++ name ++ " <filespec> /c|r [/o <path>] [/t AVR|ARM|PIC32]\n"
  putStrLn   "<filespec>:  File(s) to convert"
  putStrLn   "parameters: /c            - Create output as .c array files"
  putStrLn   "            /r            - Create output as .raw files"
  putStrLn   "            /o <path>     - Set the output directory to <path>\n"
  putStrLn   "            /t <platform> - Select target plaform"
  putStrLn   "                            AVR   : Most Arduinos, Bobuion"
  putStrLn   "                            ARM   : Arduino Due, Teensy, TI CC3200 LaunchPad"
  putStrLn   "                            PIC32 : All chipKit boards\n"
  putStrLn   "You must specify either /c or /r. All other parameters are optional."
  putStrLn   "If /o is ommited the current directory will be used for output."
  putStrLn   "If /t is ommited the target platform will be set to AVR."

