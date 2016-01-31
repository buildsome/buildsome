.PHONY: default
default: a.before a.after

a.before a.after:
	cat a.input > a.before
	while [ ! -e a.meddling ] ; do printf '.'; done
	cat a.input > a.after
