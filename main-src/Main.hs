module Main where

import System.FilePath.Posix (takeExtension, (</>))
import System.Directory      (getCurrentDirectory, createDirectoryIfMissing,
                              doesFileExist)
import System.Environment    (getArgs, getProgName)
import System.Exit           (exitFailure)

import Format.Converter      (pictureToC, pictureToRaw)
import Format.C              (Platform(..))

type Args = [String]

data FileType = C | Raw

data Option = Option
 { outdir   :: FilePath
 , platform :: Platform
 , filetype :: Maybe FileType
 , files    :: [FilePath]
 }


main :: IO ()
main = do
  args    <- getArgs
  defopts <- defaultOptions
  opt     <- argsParser args defopts
  case opt of
    Option _       _         _               [] -> putStrLn "ERROR: No valid files found to convert."  >> help >> exitFailure
    Option outdir' platform' (Just C)    files' -> printOpt (outdir', platform', C  ) >> mapM_ (pictureToC   platform' outdir') files'
    Option outdir' platform' (Just Raw)  files' -> printOpt (outdir', platform', Raw) >> mapM_ (pictureToRaw           outdir') files'
    Option _       _         Nothing     _      -> putStrLn "ERROR: No filetype defined."              >> help >> exitFailure

defaultOptions :: IO Option
defaultOptions = do
  dir <- getCurrentDirectory
  return $ Option dir AVR Nothing []

argsParser :: Args -> Option -> IO Option
argsParser [] opt          = return opt

-- filetype parser
argsParser ("/c"        :xs) opt
  | Just _ <- filetype opt = putStrLn "ERROR: You can only specifiy one target platform" >> exitFailure
  | otherwise              = argsParser xs (opt { filetype = Just C   } )
argsParser ("/r"        :xs) opt
  | Just _ <- filetype opt = putStrLn "ERROR: You can only specifiy one target platform" >> exitFailure
  | otherwise              = argsParser xs (opt { filetype = Just Raw } )

-- platform parser
argsParser ("/t":"AVR"  :xs) opt = argsParser xs (opt { platform = AVR   } )
argsParser ("/t":"PIC32":xs) opt = argsParser xs (opt { platform = PIC32 } )
argsParser ("/t":"ARM"  :xs) opt = argsParser xs (opt { platform = ARM   } )
argsParser ("/t"        :xs) opt = do
  putStrLn "WARNING: Platform not specified or unknown. Using the default platform"
  argsParser xs opt

-- output directory parser

argsParser ("/o":"/t"   :xs) opt = do
  putStrLn     "WARNING: Output directory is missing. Using default output directory"
  argsParser ("/t":xs) opt
argsParser ("/o":fp     :xs) opt = do
  case takeExtension fp of
    ""   -> do
      createDirectoryIfMissing True fp
      dir <- getCurrentDirectory
      argsParser xs (opt { outdir = dir </> fp } )
    _    -> do
      putStrLn "WARNING: Output directory has an extention. Using default output directory"
      argsParser xs opt
argsParser ("/o"        :xs) opt = do
  putStrLn     "WARNING: Output directory is missing. Using default output directory"
  argsParser xs opt


-- files parser
argsParser (fp           :xs) opt = do
  exists <- doesFileExist fp
  case (exists, takeExtension fp) of
    (True, ".jpg")  -> argsParser xs (opt { files = fp : files opt})
    (True, ".jpeg") -> argsParser xs (opt { files = fp : files opt})
    (True, ".jpe")  -> argsParser xs (opt { files = fp : files opt})
    (True, ".bmp")  -> argsParser xs (opt { files = fp : files opt})
    (True, ".png")  -> argsParser xs (opt { files = fp : files opt})
    (True, ".gif")  -> argsParser xs (opt { files = fp : files opt})
    (True, ".tga")  -> argsParser xs (opt { files = fp : files opt})
    (True,      _)  -> putStrLn ("WARNING: This format is not supported ~ " ++ fp) >> argsParser xs opt
    (False,     _)  -> putStrLn ("WARNING: Unreconized flag ~ "    ++ fp) >> argsParser xs opt

printOpt :: (FilePath, Platform, FileType) -> IO ()
printOpt (outdir', platform', C)   = do
  putStrLn $ "Converting to     : .c array file(s)"
  putStrLn $ "Target platform   : " ++ show platform'
  putStrLn $ "Output directory  : " ++ outdir'        ++ "\n"
  putStrLn   "Processing file(s):"
printOpt (outdir',        _, Raw) = do
  putStrLn $ "Converting to     : .raw file(s)"
  putStrLn $ "Output directory  : " ++ outdir'        ++ "\n"
  putStrLn   "Processing file(s):"

help :: IO ()
help = do
  name <- getProgName
  putStrLn   "\nUsage:                                           "
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