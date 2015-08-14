# UTFTConverter

While working on a streaming project on the Arduino, I needed to convert .jpg's to the .c or .raw format on the fly and send them to the Arduino. Unfortunately, the tools that were included in the corresponding UTFT library were compiled for Windows and there was no public API to do so.

This is my take on an offline converter from .jpg to .c or .raw format and a simple server that you can start locally which accepts any .jpg's and responses with the parsed .raw file.

I will try to make the offline tool as identical as possible to the corresponding tool at [RinkyDink](http://www.rinkydinkelectronics.com/library.php?id=51).
