#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdarg.h>
#include <dirent.h>
#include <unistd.h>

#define DEFINE_WRAPPER(ret_type, name, params)  \
    typedef ret_type name##_func params;        \
    name##_func name;                           \
    ret_type name params

#define FORWARD(ret_type, name, fmt, ...)               \
    name##_func *real = dlsym(RTLD_NEXT, #name);        \
    ret_type rc = real(__VA_ARGS__);                    \
    fprintf(stderr, #name " " fmt "\n", ##__VA_ARGS__, rc);  \
    return rc;

DEFINE_WRAPPER(int, open, (const char *pathname, int flags, ...))
{
    va_list args;
    va_start(args, flags);
    mode_t mode = va_arg(args, mode_t);
    va_end(args);

    FORWARD(int, open, "%s (0x%X, 0x%X) called: %d", pathname, flags, mode);
}

DEFINE_WRAPPER(int, creat, (const char *pathname, mode_t mode))
{
    FORWARD(int, creat, "%s (0x%X) called: %d", pathname, mode);
}

DEFINE_WRAPPER(int, stat, (const char *path, struct stat *buf))
{
    FORWARD(int, stat, "%s (%p) called: %d", path, buf);
}

DEFINE_WRAPPER(int, fstat, (int fd, struct stat *buf))
{
    FORWARD(int, fstat, "%d (%p) called: %d", fd, buf);
}

DEFINE_WRAPPER(int, lstat, (const char *path, struct stat *buf))
{
    FORWARD(int, stat, "%s (%p) called: %d", path, buf);
}

DEFINE_WRAPPER(DIR *, opendir, (const char *name))
{
    FORWARD(DIR *, opendir, "%s: %p", name);
}

DEFINE_WRAPPER(DIR *, fdopendir, (int fd))
{
    FORWARD(DIR *, fdopendir, "%d: %p", fd);
}

DEFINE_WRAPPER(int, access, (const char *pathname, int mode))
{
    FORWARD(int, access, "%s, 0x%X: %d", pathname, mode);
}

DEFINE_WRAPPER(int, truncate, (const char *path, off_t length))
{
    FORWARD(int, truncate, "%s, %lu bytes: %d", path, length);
}

DEFINE_WRAPPER(int, ftruncate, (int fd, off_t length))
{
    FORWARD(int, ftruncate, "fd=%d, %lu bytes: %d", fd, length);
}
