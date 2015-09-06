.PHONY: default

COMPILE=gcc -c -o "$@" $<

%.o: %.c
	${COMPILE}

default: foo.o moshe.a moshe.a.b escape_input should_be_deleted_by_make

%.a %.a.b:
	cat foo.c > "$@" ; cat foo.c >"$@.b"

escape(bla=12):
	echo hi > "$@"

escape(bla=123,foo='123'):
	echo hi > "$@"

escape_input:
	cat escape\(bla=12\) escape\(bla=123,foo=\'123\'\) > "$@"

should_be_deleted_by_make: should_be_deleted_by_make_dependency
	echo "# bla" >> "$@"
