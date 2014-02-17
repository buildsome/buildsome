all: fs_override.so master

fs_override.so: fs_override.c
	gcc -o $@ -g -Wall -Wextra -Winit-self -shared -fPIC $< -ldl

master: *.hs Lib/*.hs
	ghc -threaded $@

clean:
	-rm fs_override.so
