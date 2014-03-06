all: out/a out/subdir.list needsm other

include "inc/other.Makefile"

CFLAGS=-g -Wall -Wextra
LDFLAGS=-g -Wall
CC=gcc

COMPILE=${CC} -c -o $@ $< ${CFLAGS}
LINK=${CC} -o $@ $^ ${LDFLAGS}

out/%.o: %.c
	${COMPILE}

out/a: out/a.o out/b.o | a.h
	${LINK}

needsm.o: needsm.c
	${COMPILE}

LDFLAGS=-g -Wall -lm
needsm: needsm.o
	${LINK}

out/auto.h:
	python generate.py

out/subdir.list:
	ls -l subdir > $@

subdir/a:
	echo hi > subdir/a

.PHONY: all