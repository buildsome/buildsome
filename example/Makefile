all: example/out/a example/out/subdir.list

example/out/a: example/out/a.o
	gcc -o example/out/a example/out/a.o -g -Wall

example/out/a.o:
	gcc -c -o example/out/a.o example/a.c -g -Wall

example/out/auto.h:
	python example/generate.py

example/out/subdir.list:
	ls -l example/subdir > example/out/subdir.list

example/subdir/a:
	echo hi > example/subdir/a

.PHONY: all
