.PHONY: default
default:
	cat foo || echo foo not found, as expected

.PHONY: foo
foo:
	echo This phony should never be referenced
	exit 1
