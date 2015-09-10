#include <stdio.h>
#include <fcntl.h>
#include <assert.h>

static void verify_fd(int xfd)
{
    assert(-1 != xfd);
    char buf[5];
    assert(sizeof buf == read(xfd, buf, sizeof buf));
    assert(0 == memcmp(buf, "12345", sizeof buf));
}

static void create_file(const char *path)
{
    const int scratchfd = open(path, O_CREAT | O_WRONLY);
    assert(-1 != scratchfd);
    close(scratchfd);
}

int main(void) {
    verify_fd(openat(AT_FDCWD, "x", O_RDONLY));
    const int parfd = open("..", O_RDONLY);
    verify_fd(openat(parfd, "atfuncs/x", O_RDONLY));

    create_file("scratch");
    unlinkat(AT_FDCWD, "scratch");

    create_file("scratch2");
    unlinkat(parfd, "atfuncs/scratch2");

    return 0;
}
