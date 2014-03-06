other:
	echo "Hello from the other Makefile"
include cmds.include # some comment
	echo "And another cmd after"
include ROOT/inc/foo.include

.PHONY: other
