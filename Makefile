CFLAGS= -O2 -Wall
OUT= -o UTFTConverter
CC= ghc


all: converter clean


converter: src/Main.hs\
					 src/Converter.hs\
					 src/Raw.hs\
					 src/C.hs\
					 src/RGB565.hs

	$(CC) $(CFLAGS) $(OUT) \
		src/Main.hs\
		src/Converter.hs\
		src/Raw.hs\
		src/C.hs\
		src/RGB565.hs


clean:
	rm -f src/converter/*.hi
	rm -f src/converter/*.o