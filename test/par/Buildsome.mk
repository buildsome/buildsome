.PHONY: default failure
default: foo.o failure

foo.c: ./foo.py
	python $< > $@

foo.o:
	gcc -o $@ -c foo.c

failure:
	exit 1
