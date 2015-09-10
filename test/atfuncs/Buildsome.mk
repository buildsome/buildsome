default:
	./at > $@


at: at.c
	gcc -o $@ $<

x:
	echo 12345 > $@
