#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdarg.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <string.h>

#define MAX_FRAME_SIZE 8192

#define PREFIX "EFBUILD_"

#define LOG(fmt, ...) fprintf(stderr, fmt "\n", ##__VA_ARGS__);
#define ASSERT(x)  do { if (!(x)) { LOG("ASSERTION FAILED at %s:%d: " #x, __FILE__, __LINE__); abort(); } } while(0)

static int connect_master(void)
{
    int fd = socket(AF_UNIX, SOCK_SEQPACKET, 0);
    ASSERT(-1 != fd);

    char *env_sockaddr = getenv(PREFIX "MASTER_UNIX_SOCKADDR");
    ASSERT(env_sockaddr);

    char *env_slave_id = getenv(PREFIX "SLAVE_ID");
    ASSERT(env_slave_id);

    struct sockaddr_un addr = {
        .sun_family = AF_UNIX,
    };
    ASSERT(strlen(env_sockaddr) < sizeof addr.sun_path);
    strcpy(addr.sun_path, env_sockaddr);

    LOG("pid%d: connecting \"%s\"", getpid(), env_sockaddr);
    int connect_rc = connect(fd, &addr, sizeof addr);
    ASSERT(0 == connect_rc);

    #define HELLO "HELLO, I AM: "
    char hello[strlen(HELLO) + strlen(env_slave_id) + 16];
    hello[sizeof hello-1] = 0;
    int len = snprintf(hello, sizeof hello-1, HELLO "%d:%s", getpid(), env_slave_id);
    ssize_t send_rc = send(fd, hello, len, 0);
    ASSERT(send_rc == len);
    LOG("pid%d: Sent \"%.*s\"", getpid(), len, hello);
    return fd;
}

static int connection_fd = -1;

static int connection(void)
{
    if(-1 == connection_fd) {
        connection_fd = connect_master();
    }
    return connection_fd;
}

static void send_connection(const char *buf, size_t size, int flags)
{
    ssize_t rc = send(connection(), buf, size, flags);
    ASSERT(rc == (ssize_t)size);
}

#define REPORT(fmt, ...)                                                \
    do {                                                                \
        char buf[MAX_FRAME_SIZE];                                       \
        int res = snprintf(buf, MAX_FRAME_SIZE, fmt, ##__VA_ARGS__);    \
        ASSERT(res < MAX_FRAME_SIZE);                                   \
        send_connection(buf, res, 0);                                   \
    } while(0)

#define DEFINE_WRAPPER(ret_type, name, params)  \
    typedef ret_type name##_func params;        \
    name##_func name;                           \
    ret_type name params

#define FORWARD(ret_type, name, fmt, ...)               \
    name##_func *real = dlsym(RTLD_NEXT, #name);        \
    ret_type rc = real(__VA_ARGS__);                    \
    REPORT(#name " called: " fmt, ##__VA_ARGS__, rc);    \
    return rc;

DEFINE_WRAPPER(int, open, (const char *pathname, int flags, ...))
{
    va_list args;
    va_start(args, flags);
    mode_t mode = va_arg(args, mode_t);
    va_end(args);

    FORWARD(int, open, "%s (0x%X, 0x%X): %d", pathname, flags, mode);
}

DEFINE_WRAPPER(int, creat, (const char *pathname, mode_t mode))
{
    FORWARD(int, creat, "%s (0x%X): %d", pathname, mode);
}

/* No need to wrap fstat because "fd" was opened so is considered an input */

DEFINE_WRAPPER(int, stat, (const char *path, struct stat *buf))
{
    FORWARD(int, stat, "%s (%p): %d", path, buf);
}

DEFINE_WRAPPER(int, lstat, (const char *path, struct stat *buf))
{
    FORWARD(int, lstat, "%s (%p): %d", path, buf);
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

DEFINE_WRAPPER(int, unlink, (const char *pathname))
{
    FORWARD(int, unlink, "%s: %d", pathname);
}

DEFINE_WRAPPER(int, rename, (const char *oldpath, const char *newpath))
{
    FORWARD(int, rename, "%s -> %s: %d", oldpath, newpath);
}

DEFINE_WRAPPER(int, chmod, (const char *path, mode_t mode))
{
    FORWARD(int, chmod, "%s, 0x%X: %d", path, mode);
}

DEFINE_WRAPPER(ssize_t, readlink, (const char *path, char *buf, size_t bufsiz))
{
    FORWARD(ssize_t, readlink, "%s, buf=%p, bufsiz=%zd: %zd", path, buf, bufsiz);
}

DEFINE_WRAPPER(int, mknod, (const char *path, mode_t mode, dev_t dev))
{
    FORWARD(int, mknod, "%s, mode=0x%X dev=%ld: %d", path, mode, dev);
}

DEFINE_WRAPPER(int, mkdir, (const char *path, mode_t mode))
{
    FORWARD(int, mkdir, "%s, mode=0x%X: %d", path, mode);
}

DEFINE_WRAPPER(int, rmdir, (const char *path))
{
    FORWARD(int, rmdir, "%s: %d", path);
}

DEFINE_WRAPPER(int, symlink, (const char *target, const char *linkpath))
{
    FORWARD(int, symlink, "%s -> %s: %d", linkpath, target);
}

DEFINE_WRAPPER(int, link, (const char *oldpath, const char *newpath))
{
    FORWARD(int, link, "%s -> %s: %d", oldpath, newpath);
}

DEFINE_WRAPPER(int, chown, (const char *path, uid_t owner, gid_t group))
{
    FORWARD(int, chown, "%s owner=%d, group=%d: %d", path, owner, group);
}

/* TODO: Track utime? */
/* TODO: Track statfs? */
/* TODO: Track extended attributes? */
/* TODO: Track flock? */
/* TODO: Track ioctls? */
