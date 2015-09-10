default:
	./at > $@


at: at.c
	gcc -o $@ -g -Wall -Wextra $<

x:
	echo 12345 > $@
