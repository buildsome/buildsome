.PHONY: default
default:
	cat foo.result 2>/dev/null
	exit 0

%.result: %.input

%.input: %.input2
	cat $<
