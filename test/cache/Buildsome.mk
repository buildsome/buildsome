
FLAG_random ?= enable

.PHONY: default

default: copy1 copy2

copy1:
	cp origA copy1

copy2:
	cp origB copy2


ifeq (${FLAG_random},enable)
origA:
	cp random1 "$@"

origB:
	cp random2 "$@"
else
origA:
	echo '123456' > "$@"

origB:
	echo 'ABCDEF' > "$@"
endif
