%.pyc: %.py
	pycompile "$<"

.PHONY: default

default: hi

hi:
	python test.py
