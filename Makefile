fs_override.so: fs_override.c
	gcc -o $@ -g -Wall -Wextra -Winit-self -shared -fPIC $< -ldl

clean:
	-rm fs_override.so
