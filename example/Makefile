default: all

all: out/a out/subdir.list needsm other out/par_result

INCLUDE_DIR=inc

include "${INCLUDE_DIR}/other.Makefile"
include /inc/empty

.=.

CFLAGS=-g -Wall -Wextra -Wno-init-self
LDFLAGS=-g -Wall
CC=gcc

COMPILE=${CC} -c -o $@ $< ${CFLAGS}
LINK=${CC} -o $@ $^ ${LDFLAGS}

echo $./out/%.o

$./out/%.o: $./%.c
	${COMPILE}

OBJS=out/a.o out/b.o

local {
LDFLAGS=-g -Wall -lm
needsm: needsm.o
	${LINK}
local }

out:
	mkdir $@

out/a: ${OBJS} | $./a.h
	${LINK}

needsm.o: needsm.c
	${COMPILE}

out/auto.h:
	python generate.py

out/subdir.list:
	for a in 1 2
	do
		ls -l subdir > $@
	done

out/par_result:
	./par_exec > $@

par_exec: par_exec.hs
	ghc -o $@ $<
	rm par_exec.{o,hi}

out/after_sleep:
	sleep 0.5
	echo "after_sleep" > $@

subdir/a:
	echo hi > subdir/a

.PHONY: all
.PHONY: default
