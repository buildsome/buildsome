all: fs_override.so master

fs_override.so: fs_override.c Makefile
	gcc -o $@ -g -Wall -Wextra -Winit-self -shared -fPIC $< -ldl -lbsd

master: *.hs Lib/*.hs Makefile
	ghc -threaded $@

clean:
	-rm fs_override.so
