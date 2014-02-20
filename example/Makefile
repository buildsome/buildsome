all: example/a example/subdir.list

example/a: example/a.o
	gcc -o example/a example/a.o -g -Wall

example/a.o:
	gcc -c -o example/a.o example/a.c -g -Wall

example/auto.h:
	python example/generate.py

example/subdir.list:
	ls -l example/subdir > example/subdir.list

example/subdir/a:
	echo hi > example/subdir/a
