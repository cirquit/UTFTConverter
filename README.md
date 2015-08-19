## UTFTConverter

This is my take on an offline converter from the below defined formats to `.c` or `.raw` format and a simple server that you can start locally which accepts any below defined formats and responses with the parsed `.raw` file.

I will try to make the offline tool as identical as possible to the corresponding tool at [RinkyDink](http://www.rinkydinkelectronics.com/library.php?id=51).

#### In testing:

  * converting `.jpg` to `.raw` + `.c`
  * converting `.bmp` to `.raw` + `.c`
  * converting `.png` to `.raw` + `.c`
  * converting `.gif` to `.raw` + `.c`
  * converting `.tga` to `.raw` + `.c`
  * cmd parsing

#### TODO:

  * add functionality to convert for AVR or ARM/PIC32
  * test basic functionality with the Snap server (maybe create a testing environment)
  * learn about cabal packaging and how to deploy a simple project

##### About:

While working on a streaming project on the Arduino, I needed to convert `.jpg's` to the `.c` or `.raw` format on the fly and send them to the Arduino. Unfortunately, the tools that were included in the corresponding UTFT library were compiled for Windows and there was no public API to do so. That's why I thought it would be a nice first project, that some people could benefit from.
