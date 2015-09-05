.PHONY: default

COMPILE=gcc -c -o $@ $<

%.o: %.c
	${COMPILE}

default: foo.o moshe.a moshe.a.b escape_input should_be_deleted_by_make

%.a %.a.b:
	cat foo.c > $@ ; cat foo.c >"$@.b"

escape(bla):
	echo hi > escape\(bla\)

escape_input:
	cat escape\(bla\) > escape_input

should_be_deleted_by_make: should_be_deleted_by_make_dependency
	echo "# bla" >> should_be_deleted_by_make
