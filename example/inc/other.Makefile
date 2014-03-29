other: stdouterr
	echo "Hello from the other Makefile"
include cmds.include # some comment
	echo "And another cmd after"
include ROOT/inc/foo.include

stdouterr:
	echo "Some stdout print"
	echo "And to stderr too" >&2

.PHONY: other stdouterr
