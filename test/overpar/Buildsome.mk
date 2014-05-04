.PHONY: default
default: par_result

par_result: par_exec
	./par_exec +RTS -N
	touch $@

par_exec: par_exec.hs
	ghc -threaded -o $@ $<
	rm par_exec.o
	rm par_exec.hi

after_sleep.%:
	sleep 0.5
	echo "after_sleep" > $@
