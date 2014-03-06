all: out/a out/subdir.list other

include "inc/other.Makefile"

CFLAGS=-g -Wall

# Linkage
out/a: out/a.o out/b.o | a.h
	gcc -o out/a $^ ${CFLAGS}

out/auto.h:
	python generate.py

out/subdir.list:
	ls -l subdir > $@

subdir/a:
	echo hi > subdir/a

out/%.o: %.c
	gcc -c -o $@ $< ${CFLAGS}

.PHONY: all