.PHONY: default
default: success failure with_illegal modify mkdir

success:
	echo >$@

failure:
	echo >$@
	sleep 1
	exit 1

with_illegal:
	echo >$@
	echo >random_output

modify:
	echo Another run >> existing

.PHONY: mkdir mkdir_out mkdir_in
mkdir: mkdir_out mkdir_in mkdir_new

mkdir_out:
	cd ../..
	mkdir .
	echo mkdir ignored?

mkdir_in:
	cd dir
	mkdir dar
	echo mkdir ignored?

mkdir_new:
	cd dir
	mkdir new
