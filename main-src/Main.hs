module Main where

import System.FilePath.Posix (takeExtension, (</>))
import System.Directory      (getCurrentDirectory, createDirectoryIfMissing,
                              doesFileExist)
import System.Environment    (getArgs, getProgName)
import System.Exit           (exitFailure)

import Format.Converter      (pictureToC, pictureToRaw, toMaybeFormat, Format(..))
import Format.C              (Platform(..))

type Args = [String]

data FileType = C | Raw

data Option = Option
  { outdir   :: FilePath
  , platform :: Platform
  , filetype :: Maybe FileType
  , files    :: [(Format, FilePath)]
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
  mext   <- toMaybeFormat fp
  case (exists, mext) of
    (True, Just ext) -> argsParser xs (opt { files = (ext, fp) : files opt })
    (True,  Nothing) -> putStrLn ("WARNING: This format is not supported ~ " ++ fp) >> argsParser xs opt
    (False,       _) -> putStrLn ("WARNING: Unreconized flag ~ "    ++ fp) >> argsParser xs opt

printOpt :: (FilePath, Platform, FileType) -> IO ()
printOpt (outdir', platform', C)   = do
  putStrLn $ unlines [ "Converting to     : .c array file(s)"
                     , "Target platform   : " ++ show platform'
                     , "Output directory  : " ++ outdir'        ++ "\n"
                     , "Processing file(s):"
                     ]
printOpt (outdir',        _, Raw) = do
  putStrLn $ unlines [ "Converting to     : .raw file(s)"
                     , "Output directory  : " ++ outdir'        ++ "\n"
                     , "Processing file(s):"
                     ]

help :: IO ()
help = do
  name <- getProgName
  putStrLn $ unlines [ "\nUsage:                                           "
                     , "      " ++ name ++ " <filespec> /c|r [/o <path>] [/t AVR|ARM|PIC32]\n"
                     , "<filespec>:  File(s) to convert"
                     , "parameters: /c            - Create output as .c array files"
                     , "            /r            - Create output as .raw files"
                     , "            /o <path>     - Set the output directory to <path>\n"
                     , "            /t <platform> - Select target plaform"
                     , "                            AVR   : Most Arduinos, Bobuion"
                     , "                            ARM   : Arduino Due, Teensy, TI CC3200 LaunchPad"
                     , "                            PIC32 : All chipKit boards\n"
                     , "You must specify either /c or /r. All other parameters are optional."
                     , "If /o is ommited the current directory will be used for output."
                     , "If /t is ommited the target platform will be set to AVR."
                     ]