.PHONY: default

COMPILE=gcc -c -o $@ $<

%.o: %.c
	${COMPILE}

default: foo.o moshe.a moshe.a.b

%.a %.a.b:
	cat foo.c > $@ ; cat foo.c >"$@.b"
