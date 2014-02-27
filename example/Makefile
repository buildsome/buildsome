all: example/out/a example/out/subdir.list other

include "example/other.Makefile"

# Linkage
example/out/a: example/out/a.o example/out/b.o | example/a.h
	gcc -o example/out/a $^ -g -Wall

example/out/a.o: example/a.c
	# Compilation
	gcc -c -o $@ $< -g -Wall

example/out/b.o: example/b.c
	gcc -c -o $@ $< -g -Wall

example/out/auto.h:
	python example/generate.py

example/out/subdir.list:
	ls -l example/subdir > $@

example/subdir/a:
	echo hi > example/subdir/a

.PHONY: all