all: fs_override.so buildsome

PKG_LISTING=${HOME}/.cabal/packages/hackage.haskell.org/00-index.tar

fs_override.so: fs_override.c Makefile
	gcc -o $@ -g -Wall -Wextra -Winit-self -shared -fPIC $< -ldl -lbsd

buildsome: *.cabal src/*.hs src/Lib/*.hs dist/
	cabal build
	cp dist/build/buildsome/buildsome $@

${PKG_LISTING}:
	cabal update

dist/: ${PKG_LISTING}
	cabal install --only-dependencies
	cabal configure

clean:
	-rm fs_override.so

.PHONY: clean all
