# UTFTConverter

This is my take at an offline converter from the below defined formats to `.c` or `.raw` format.

I will try to make the tool as identical as possible to the corresponding tool at [RinkyDink](http://www.rinkydinkelectronics.com/library.php?id=51).

## Usage:

  ```
  ./UTFTConverter <filespec> /c|r [/o <path>] [/t AVR|ARM|PIC32]

  <filespec>:  File(s) to convert
  parameters: /c            - Create output as .c array files
              /r            - Create output as .raw files
              /o <path>     - Set the output directory to <path>\n
              /t <platform> - Select target plaform
                              AVR   : Most Arduinos, Bobuion
                              ARM   : Arduino Due, Teensy, TI CC3200 LaunchPad
                              PIC32 : All chipKit boards\n

  You must specify either /c or /r. All other parameters are optional.
  If /o is ommited the current directory will be used for output.
  If /t is ommited the target platform will be set to AVR.
  ```

## Differences to the Windows tool:

  * You can specifiy as many files as you want, not only a directory (`mydir/*.jpg` still works, the shell does all the work for you)
  * If you specify a target platform while converting to `.raw` the platform will not be printed
  * If you specify a non-existing directory as target, it will be created
  * The length of the array is NOT preceded by `0x`
  * You can use the flags in any order you want

## Binaries

  * precompiled binary for Ubuntu distributions in `bin/UTFTConverter_u` (compiled on Linux Mint 17)
  * precompiled binary for Windows in `bin/UTFTConverter_w.exe` (compiled on Windows 7) (TODO)

## Installation:

**Step 1**: Install the Glasgow Haskell Compiler and `cabal` OR the Haskell packaging tool

#### For Ubuntu distributions:
  * `sudo apt-get install ghc`
  * download the `cabal-install.tar.gz` from [cabal](http://hackage.haskell.org/package/cabal-install)
  * unpack the `.tar.gz file`, the cabal-install folder should include a `bootstrap.sh`
  * run `./bootstrap.sh`

#### For any operation system:
  * Follow the instructions for your operation system on [Haskell Platform](http://haskell.org/platform)


**Step 2**: When you have `ghc` and `cabal` OR the Haskell Platform installed:

#### If you want the library AND the binary (the easier way):
  * run `cabal install UTFTConverter`
  * the executable is now at `~/.cabal/bin/UTFTConverter`

#### If you want to build it by hand:
  * download the JuicyPixels library - `cabal install JuicyPixels`
  * download the git repository - `git clone http://github.com/cirquit/UTFTConverter`
  * run `make` and the binary is in the same directory OR
  * run `cabal install` and the binary is in `/dist/build/` and in your home directory under `~/.cabal/bin/UTFTConverter`, you can also add `~/.cabal/bin` to your `PATH` variable, so you can start it from anywhere

#### To do:

  * maybe add resizing with a basic linear algorithm
  * I'm planning to make a simple API that you can start locally which accepts any below defined formats and responses with the parsed `.raw` file. (another project)
  * add `stack` support

#### About:

While working on a streaming project on the Arduino, I needed to convert `.jpg's` to the `.c` or `.raw` format on the fly and send them to the Arduino. Unfortunately, the tools that were included in the corresponding UTFT library were compiled for Windows and there was no public API to do so. That's why I thought it would be a nice first project, that some people could benefit from.
