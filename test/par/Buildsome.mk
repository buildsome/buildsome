.PHONY: default failure
default: foo.o failure

foo.o:
	gcc -o $@ -c foo.c

failure:
	exit 1
