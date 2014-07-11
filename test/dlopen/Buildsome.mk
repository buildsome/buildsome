.PHONY: default
default: dlopen.result
dlopen.result: dlopen
	./$< > $@
dlopen: dlopen.c
	gcc -o $@ $< -ldl
lib.so: lib.c
	gcc -o $@ $< -shared -fPIC
