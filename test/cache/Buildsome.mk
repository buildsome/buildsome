
.PHONY: default

default: copy1 copy2

copy1:
	cp origA copy1

copy2:
	cp origB copy2

origA:
	dd if=/dev/urandom of="$@" bs=1024 count=1024

origB:
	dd if=/dev/urandom of="$@" bs=1024 count=1024
