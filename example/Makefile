all: example/out/a example/out/subdir.list other

include "example/other.Makefile"

CFLAGS=-g -Wall

# Linkage
example/out/a: example/out/a.o example/out/b.o | example/a.h
	gcc -o example/out/a $^ ${CFLAGS}

example/out/a.o: example/a.c
	# Compilation
	gcc -c -o $@ $< ${CFLAGS}

example/out/b.o: example/b.c
	gcc -c -o $@ $< ${CFLAGS}

example/out/auto.h:
	python example/generate.py

example/out/subdir.list:
	ls -l example/subdir > $@

example/subdir/a:
	echo hi > example/subdir/a

%.o: %.c
	gcc -o $@ $< ${CFLAGS}

.PHONY: all