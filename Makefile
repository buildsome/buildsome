all: fs_override.so buildsome

fs_override.so: fs_override.c Makefile
	gcc -o $@ -g -Wall -Wextra -Winit-self -shared -fPIC $< -ldl -lbsd

buildsome:
	cabal build
	cp dist/build/buildsome/buildsome $@

clean:
	-rm fs_override.so

.PHONY: clean buildsome all
