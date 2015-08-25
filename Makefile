CFLAGS= -O2 -Wall
OUT= -o UTFTConverter
CC= ghc


all: converter clean


converter: src/Main.hs\
					 src/Format/Converter.hs\
					 src/Format/Raw.hs\
					 src/Format/C.hs\
					 src/Format/RGB565.hs

	$(CC) $(CFLAGS) $(OUT) \
		src/Main.hs\
		src/Format/Converter.hs\
		src/Format/Raw.hs\
		src/Format/C.hs\
		src/Format/RGB565.hs


clean:
	rm -f src/*.hi
	rm -f src/*.o
	rm -f src/Format/*.hi
	rm -f src/Format/*.o