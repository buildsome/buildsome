.PHONY: default
default:
	cat foo.result
	exit 0

%.result: %.input
