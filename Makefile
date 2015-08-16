CFLAGS= -O2
OUT= -o bin/converter
CC= ghc


all: converter clean


converter: src/converter/Main.hs\
					 src/converter/Converter.hs\
					 src/converter/Raw.hs\
					 src/converter/C.hs\
					 src/converter/RGB565.hs

	$(CC) $(CFLAGS) $(OUT) \
		src/converter/Main.hs\
		src/converter/Converter.hs\
		src/converter/Raw.hs\
		src/converter/C.hs\
		src/converter/RGB565.hs


clean:
	rm -f src/converter/*.hi
	rm -f src/converter/*.o