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
#include <bsd/string.h>         /* strlcpy */
#include <stdint.h>
#include <stdbool.h>
#include <sys/syscall.h>

#define MAX_FRAME_SIZE 8192

#define PREFIX "EFBUILD_"

#define LOG(fmt, ...) fprintf(stderr, fmt "\n", ##__VA_ARGS__);
#define DEBUG(fmt, ...)
#define ASSERT(x)  do { if (!(x)) { LOG("ASSERTION FAILED at %s:%d: " #x, __FILE__, __LINE__); abort(); } } while(0)

#define PS(x)   ((char *)& (x)) , sizeof (x)

static int gettid(void)
{
    return syscall(__NR_gettid);
}

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

    DEBUG("pid%d, tid%d: connecting \"%s\"", getpid(), gettid(), env_sockaddr);
    int connect_rc = connect(fd, &addr, sizeof addr);
    ASSERT(0 == connect_rc);

    #define HELLO "HELLO, I AM: "
    char hello[strlen(HELLO) + strlen(env_slave_id) + 16];
    hello[sizeof hello-1] = 0;
    int len = snprintf(hello, sizeof hello-1, HELLO "%d:%d:%s", getpid(), gettid(), env_slave_id);
    ssize_t send_rc = send(fd, hello, len, 0);
    ASSERT(send_rc == len);
    return fd;
}

static __thread struct {
    pid_t pid;
    int connection_fd;
} thread_state = {-1, -1};

static int connection(void)
{
    pid_t pid = getpid();
    if(pid != thread_state.pid) {
        thread_state.connection_fd = connect_master();
        thread_state.pid = pid;
    }
    return thread_state.connection_fd;
}

static int assert_connection(void)
{
    ASSERT(getpid() == thread_state.pid);
    return thread_state.connection_fd;
}

static void send_connection(const char *buf, size_t size)
{
    ssize_t rc = send(connection(), buf, size, 0);
    ASSERT(rc == (ssize_t)size);
}

/* NOTE: This must be kept in sync with Protocol.hs */
enum func {
    func_open = 0x10000,
    func_creat,
    func_stat,
    func_lstat,
    func_opendir,
    func_fdopendir,
    func_access,
    func_truncate,
    func_ftruncate,
    func_unlink,
    func_rename,
    func_chmod,
    func_readlink,
    func_mknod,
    func_mkdir,
    func_rmdir,
    func_symlink,
    func_link,
    func_chown,
};

/* NOTE: This must be kept in sync with Protocol.hs */
#define MAX_PATH 256

/* func_open.flags */
#define FLAG_WRITE 1
#define FLAG_CREATE 2           /* func_open.mode is meaningful iff this flag */

/* NOTE: This must be kept in sync with Protocol.hs */
struct func_open      {char path[MAX_PATH]; uint32_t flags; uint32_t mode;};
struct func_creat     {char path[MAX_PATH]; uint32_t mode;};
struct func_stat      {char path[MAX_PATH];};
struct func_lstat     {char path[MAX_PATH];};
struct func_opendir   {char path[MAX_PATH];};
struct func_fdopendir {uint32_t fd;};
struct func_access    {char path[MAX_PATH]; uint32_t mode;};
struct func_truncate  {char path[MAX_PATH]; uint64_t length;};
struct func_ftruncate {uint32_t fd; off_t length;};
struct func_unlink    {char path[MAX_PATH];};
struct func_rename    {char oldpath[MAX_PATH]; char newpath[MAX_PATH];};
struct func_chmod     {char path[MAX_PATH]; uint32_t mode;};
struct func_readlink  {char path[MAX_PATH]; char buf[MAX_PATH]; uint32_t bufsiz;};
struct func_mknod     {char path[MAX_PATH]; uint32_t mode; uint64_t dev;};
struct func_mkdir     {char path[MAX_PATH]; uint32_t mode;};
struct func_rmdir     {char path[MAX_PATH];};
struct func_symlink   {char target[MAX_PATH]; char linkpath[MAX_PATH];};
struct func_link      {char oldpath[MAX_PATH]; char newpath[MAX_PATH];};
struct func_chown     {char path[MAX_PATH]; uint32_t owner; uint32_t group;};

#define DEFINE_WRAPPER(ret_type, name, params)  \
    typedef ret_type name##_func params;        \
    name##_func name;                           \
    ret_type name params

#define CALL_REAL(ret_type, name, ...)                \
    name##_func *real = dlsym(RTLD_NEXT, #name);    \
    ret_type rc = real(__VA_ARGS__);                \
    return rc;

#define DEFINE_MSG(name)                        \
    struct {                                    \
        enum func func;                         \
        struct func_##name args;                \
    } msg = { .func = func_##name };

#define CREATION_FLAGS (O_CREAT | O_EXCL)

void open_readwrite(void)
{
    LOG("What can we do with read-write files?");
    /* TODO: Allow them, but only if they do not already exist? */
    abort();
}

static void await_go(void)
{
    char buf[16];
    ssize_t rc = recv(assert_connection(), PS(buf), 0);
    ASSERT(2 == rc);        /* Expect "GO" response! */
    ASSERT(!strncmp(buf, "GO", 2));
}

static mode_t open_common(const char *path, int flags, va_list args)
{
    bool creation = flags & CREATION_FLAGS;
    mode_t mode = creation ? va_arg(args, mode_t) : 0;

    DEFINE_MSG(open);
    strlcpy(msg.args.path, path, sizeof msg.args.path);
    if(O_RDWR == (flags & (O_RDONLY | O_RDWR | O_WRONLY))) {
        open_readwrite();
    } else if(O_WRONLY == (flags & (O_RDONLY | O_RDWR | O_WRONLY))) {
        msg.args.flags |= FLAG_WRITE;
    } else if(O_RDONLY == (flags & (O_RDONLY | O_RDWR | O_WRONLY))) {
    } else {
        LOG("invalid open mode?!");
        ASSERT(0);
    }
    if(creation) {
        msg.args.flags |= FLAG_CREATE;
    }
    msg.args.mode = mode;
    send_connection(PS(msg));
    if(!(msg.args.flags & FLAG_WRITE)) {
        ASSERT(!(msg.args.flags & FLAG_CREATE));
        await_go();
    }
    return mode;
}

DEFINE_WRAPPER(int, open, (const char *path, int flags, ...))
{
    va_list args;
    va_start(args, flags);
    mode_t mode = open_common(path, flags, args);
    va_end(args);

    CALL_REAL(int, open, path, flags, mode);
}

DEFINE_WRAPPER(int, open64, (const char *path, int flags, ...))
{
    va_list args;
    va_start(args, flags);
    mode_t mode = open_common(path, flags, args);
    va_end(args);

    CALL_REAL(int, open64, path, flags, mode);
}

static void fopen_common(const char *path, const char *mode)
{
    DEFINE_MSG(open);
    strlcpy(msg.args.path, path, sizeof msg.args.path);
    switch(mode[0]) {
    case 'r':
        if(mode[1] == '+') open_readwrite();
        break;
    case 'w':
        // w+ not a problem: no reading of previous content possible
        msg.args.flags |= FLAG_WRITE;
        break;
    case 'a':
        if(mode[1] == '+') open_readwrite();
        msg.args.flags |= FLAG_WRITE;
        break;
    default:
        LOG("Invalid fopen mode?!");
        ASSERT(0);
    }
    send_connection(PS(msg));
    if(!(msg.args.flags & FLAG_WRITE)) {
        ASSERT(!(msg.args.flags & FLAG_CREATE));
        await_go();
    }
}

DEFINE_WRAPPER(FILE *, fopen, (const char *path, const char *mode))
{
    fopen_common(path, mode);
    CALL_REAL(FILE *, fopen, path, mode);
}

DEFINE_WRAPPER(FILE *, fopen64, (const char *path, const char *mode))
{
    fopen_common(path, mode);
    CALL_REAL(FILE *, fopen64, path, mode);
}

DEFINE_WRAPPER(int, creat, (const char *path, mode_t mode))
{
    DEFINE_MSG(creat);
    strlcpy(msg.args.path, path, sizeof msg.args.path);
    msg.args.mode = mode;
    send_connection(PS(msg));

    CALL_REAL(int, creat, path, mode);
}

/* No need to wrap fstat because "fd" was opened so is considered an input */

DEFINE_WRAPPER(int, stat, (const char *path, struct stat *buf))
{
    CALL_REAL(int, stat, path, buf);
}

DEFINE_WRAPPER(int, lstat, (const char *path, struct stat *buf))
{
    CALL_REAL(int, lstat, path, buf);
}

DEFINE_WRAPPER(DIR *, opendir, (const char *name))
{
    CALL_REAL(DIR *, opendir, name);
}

DEFINE_WRAPPER(DIR *, fdopendir, (int fd))
{
    CALL_REAL(DIR *, fdopendir, fd);
}

DEFINE_WRAPPER(int, access, (const char *path, int mode))
{
    DEFINE_MSG(access);
    strlcpy(msg.args.path, path, sizeof msg.args.path);
    msg.args.mode = mode;
    send_connection(PS(msg));
    await_go();
    CALL_REAL(int, access, path, mode);
}

DEFINE_WRAPPER(int, truncate, (const char *path, off_t length))
{
    CALL_REAL(int, truncate, path, length);
}

DEFINE_WRAPPER(int, ftruncate, (int fd, off_t length))
{
    CALL_REAL(int, ftruncate, fd, length);
}

DEFINE_WRAPPER(int, unlink, (const char *path))
{
    DEFINE_MSG(unlink);
    strlcpy(msg.args.path, path, sizeof msg.args.path);
    send_connection(PS(msg));
    CALL_REAL(int, unlink, path);
}

DEFINE_WRAPPER(int, rename, (const char *oldpath, const char *newpath))
{
    DEFINE_MSG(rename);
    strlcpy(msg.args.oldpath, oldpath, sizeof msg.args.oldpath);
    strlcpy(msg.args.newpath, newpath, sizeof msg.args.newpath);
    send_connection(PS(msg));

    CALL_REAL(int, rename, oldpath, newpath);
}

DEFINE_WRAPPER(int, chmod, (const char *path, mode_t mode))
{
    CALL_REAL(int, chmod, path, mode);
}

DEFINE_WRAPPER(ssize_t, readlink, (const char *path, char *buf, size_t bufsiz))
{
    CALL_REAL(ssize_t, readlink, path, buf, bufsiz);
}

DEFINE_WRAPPER(int, mknod, (const char *path, mode_t mode, dev_t dev))
{
    CALL_REAL(int, mknod, path, mode, dev);
}

DEFINE_WRAPPER(int, mkdir, (const char *path, mode_t mode))
{
    CALL_REAL(int, mkdir, path, mode);
}

DEFINE_WRAPPER(int, rmdir, (const char *path))
{
    CALL_REAL(int, rmdir, path);
}

DEFINE_WRAPPER(int, symlink, (const char *target, const char *linkpath))
{
    CALL_REAL(int, symlink, linkpath, target);
}

DEFINE_WRAPPER(int, link, (const char *oldpath, const char *newpath))
{
    CALL_REAL(int, link, oldpath, newpath);
}

DEFINE_WRAPPER(int, chown, (const char *path, uid_t owner, gid_t group))
{
    CALL_REAL(int, chown, path, owner, group);
}

/* TODO: Track utime? */
/* TODO: Track statfs? */
/* TODO: Track extended attributes? */
/* TODO: Track flock? */
/* TODO: Track ioctls? */
