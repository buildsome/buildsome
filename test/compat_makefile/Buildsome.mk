.PHONY: default

COMPILE=gcc -c -o "$@" $<

out:
	mkdir out

out/%.o: %.c
	${COMPILE}

default: out/foo.o moshe.a moshe.a.b should_be_deleted_by_make # escape_input

%.a %.a.b:
	cat foo.c > "$@" ; cat foo.c >"$@.b"

# TODO: Add support for escape again?

# escape(bla=12):
# 	echo hi > "$@"

# escape(bla=123,foo='123'):
# 	echo hi > "$@"

# escape_input:
# 	cat escape\(bla=12\) escape\(bla=123,foo=\'123\'\) > "$@"

should_be_deleted_by_make: should_be_deleted_by_make_dependency
	echo "# bla" >> "$@"
