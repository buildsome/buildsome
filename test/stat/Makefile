.PHONY: default
default: test.result

test.result: test
	./$< > $@

test: test.c
	gcc -o $@ -g -Wall $<

foo:
	echo HI > $@