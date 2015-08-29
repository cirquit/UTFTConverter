## UTFTConverter

This is my take on an offline converter from the below defined formats to `.c` or `.raw` format.

I will try to make the offline tool as identical as possible to the corresponding tool at [RinkyDink](http://www.rinkydinkelectronics.com/library.php?id=51).

#### Working

  * AVR / PIC32 / ARM compatibility (tested offline with the output from the corresponding tool)
  * converting `.jpg` to `.raw` + `.c`
  * converting `.bmp` to `.raw` + `.c`
  * converting `.png` to `.raw` + `.c`
  * converting `.gif` to `.raw` + `.c`
  * converting `.tga` to `.raw` + `.c`

#### In testing:

  * cmd parsing

#### Differences to the Windows tool:

  * You can specifiy as many files as you want, not only a directory (`mydir/*.jpg` still works, the shell does all the work for you)
  * If you specify a target platform while converting to `.raw` the platform will not be printed
  * If you specify a non-existing directory as target, it will be created ()
  * The length of the array is NOT preceded by `0x`
  * The output for the different platforms is exactly the same, but it's not the one you get if you use the online converter at [RinkyDink](http://www.rinkydinkelectronics.com/t_imageconverter565.php). I still have to understand the differences in the header

#### TODO:

  * maybe add resizing with a basic linear algorithm
  * I'm planning to make a simple API that you can start locally which accepts any below defined formats and responses with the parsed `.raw` file. (another project)


##### About:

While working on a streaming project on the Arduino, I needed to convert `.jpg's` to the `.c` or `.raw` format on the fly and send them to the Arduino. Unfortunately, the tools that were included in the corresponding UTFT library were compiled for Windows and there was no public API to do so. That's why I thought it would be a nice first project, that some people could benefit from.
