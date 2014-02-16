fs_override.so: fs_override.c
	gcc -o $@ -shared -fPIC $< -ldl

clean:
	-rm fs_override.so
