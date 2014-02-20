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

#define SAFE_STRCPY(dest, src)      strlcpy(dest, src, sizeof (dest))

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

    char *env_slave_id = getenv(PREFIX "CMD_ID");
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
    func_access,
    func_truncate,
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
struct func_access    {char path[MAX_PATH]; uint32_t mode;};
struct func_truncate  {char path[MAX_PATH]; uint64_t length;};
struct func_unlink    {char path[MAX_PATH];};
struct func_rename    {char oldpath[MAX_PATH]; char newpath[MAX_PATH];};
struct func_chmod     {char path[MAX_PATH]; uint32_t mode;};
struct func_readlink  {char path[MAX_PATH]; };
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

#define SILENT_CALL_REAL(ret_type, name, ...)           \
    name##_func *real = dlsym(RTLD_NEXT, #name);        \
    ret_type rc = real(__VA_ARGS__);                    \
    return rc;

#define SEND_CALL_REAL(msg, ...)                \
    do {                                        \
        send_connection(PS(msg));               \
        SILENT_CALL_REAL(__VA_ARGS__);          \
    } while(0)

#define AWAIT_CALL_REAL(msg, ...)               \
    do {                                        \
        send_connection_await(PS(msg));         \
        SILENT_CALL_REAL(__VA_ARGS__);          \
    } while(0)

#define DEFINE_MSG(msg, name)                   \
    struct {                                    \
        enum func func;                         \
        struct func_##name args;                \
    } msg = { .func = func_##name };

#define CREATION_FLAGS (O_CREAT | O_EXCL)

static void await_go(void)
{
    char buf[16];
    ssize_t rc = recv(assert_connection(), PS(buf), 0);
    ASSERT(2 == rc);        /* Expect "GO" response! */
    ASSERT(!strncmp(buf, "GO", 2));
}

static void notify_open(const char *path, bool is_write, bool is_create, mode_t mode)
{
    DEFINE_MSG(msg, open);
    SAFE_STRCPY(msg.args.path, path);
    if(is_write) {
        msg.args.flags |= FLAG_WRITE;
        if(is_create) {
            msg.args.flags |= FLAG_CREATE;
        }
    } else {
        ASSERT(!is_create);
    }
    msg.args.mode = mode;
    send_connection(PS(msg));
    if(!is_write) {
        await_go();
    }
}

static mode_t open_common(const char *path, int flags, va_list args)
{
    bool is_write = false;
    bool is_create = flags & CREATION_FLAGS;
    mode_t mode = is_create ? va_arg(args, mode_t) : 0;

    switch(flags & (O_RDONLY | O_RDWR | O_WRONLY)) {
    case O_RDWR:
    case O_WRONLY:
        is_write = true;
        break;
    case O_RDONLY:
        break;
    default:
        LOG("invalid open mode?!");
        ASSERT(0);
    }
    notify_open(path, is_write, is_create, mode);
    return mode;
}

/* Full direct path refers to both the [in]existence of a file, its stat and
 * content */
/* Full path refers to the full direct path and any dereferences of
 * symlinks from it */

/* All outputs to the full path also output (non-exclusively) to the
 * containing directory */

/* For read: Depends on the full path */
/* For write: Outputs the full path */
DEFINE_WRAPPER(int, open, (const char *path, int flags, ...))
{
    va_list args;
    va_start(args, flags);
    mode_t mode = open_common(path, flags, args);
    va_end(args);

    /* open_common handles the reporting */
    SILENT_CALL_REAL(int, open, path, flags, mode);
}

/* Ditto open */
DEFINE_WRAPPER(int, open64, (const char *path, int flags, ...))
{
    va_list args;
    va_start(args, flags);
    mode_t mode = open_common(path, flags, args);
    va_end(args);

    /* open_common handles the reporting */
    SILENT_CALL_REAL(int, open64, path, flags, mode);
}

static void fopen_common(const char *path, const char *mode)
{
    bool is_write = false, is_create = false;
    switch(mode[0]) {
    case 'r':
        if(mode[1] == '+') is_write = true;
        break;
    case 'w':
    case 'a':
        is_create = true;
        is_write = true;
        break;
    default:
        LOG("Invalid fopen mode?!");
        ASSERT(0);
    }
    notify_open(path, is_write, is_create, 0666);
}

/* Ditto open */
DEFINE_WRAPPER(FILE *, fopen, (const char *path, const char *mode))
{
    fopen_common(path, mode);
    /* fopen_common handles the reporting */
    SILENT_CALL_REAL(FILE *, fopen, path, mode);
}

/* Ditto open */
DEFINE_WRAPPER(FILE *, fopen64, (const char *path, const char *mode))
{
    fopen_common(path, mode);
    /* fopen_common handles the reporting */
    SILENT_CALL_REAL(FILE *, fopen64, path, mode);
}

/* Ditto open(W) */
DEFINE_WRAPPER(int, creat, (const char *path, mode_t mode))
{
    DEFINE_MSG(msg, creat);
    SAFE_STRCPY(msg.args.path, path);
    msg.args.mode = mode;
    SEND_CALL_REAL(msg, int, creat, path, mode);
}

static void send_connection_await(const char *msg, size_t size)
{
    send_connection(msg, size);
    await_go();
}

/* Depends on the full path */
DEFINE_WRAPPER(int, stat, (const char *path, struct stat *buf))
{
    DEFINE_MSG(msg, stat);
    SAFE_STRCPY(msg.args.path, path);
    AWAIT_CALL_REAL(msg, int, stat, path, buf);
}

/* Depends on the full direct path */
DEFINE_WRAPPER(int, lstat, (const char *path, struct stat *buf))
{
    DEFINE_MSG(msg, lstat);
    SAFE_STRCPY(msg.args.path, path);
    AWAIT_CALL_REAL(msg, int, lstat, path, buf);
}

/* Depends on the full path */
DEFINE_WRAPPER(DIR *, opendir, (const char *path))
{
    DEFINE_MSG(msg, opendir);
    SAFE_STRCPY(msg.args.path, path);
    AWAIT_CALL_REAL(msg, DIR *, opendir, path);
}

/* Depends on the full path */
DEFINE_WRAPPER(int, access, (const char *path, int mode))
{
    DEFINE_MSG(msg, access);
    SAFE_STRCPY(msg.args.path, path);
    msg.args.mode = mode;
    AWAIT_CALL_REAL(msg, int, access, path, mode);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, truncate, (const char *path, off_t length))
{
    DEFINE_MSG(msg, truncate);
    SAFE_STRCPY(msg.args.path, path);
    SEND_CALL_REAL(msg, int, truncate, path, length);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, unlink, (const char *path))
{
    DEFINE_MSG(msg, unlink);
    SAFE_STRCPY(msg.args.path, path);
    SEND_CALL_REAL(msg, int, unlink, path);
}

/* Outputs both full paths */
DEFINE_WRAPPER(int, rename, (const char *oldpath, const char *newpath))
{
    DEFINE_MSG(msg, rename);
    SAFE_STRCPY(msg.args.oldpath, oldpath);
    SAFE_STRCPY(msg.args.newpath, newpath);
    SEND_CALL_REAL(msg, int, rename, oldpath, newpath);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, chmod, (const char *path, mode_t mode))
{
    DEFINE_MSG(msg, chmod);
    SAFE_STRCPY(msg.args.path, path);
    msg.args.mode = mode;
    SEND_CALL_REAL(msg, int, chmod, path, mode);
}

/* Depends on the full direct path */
DEFINE_WRAPPER(ssize_t, readlink, (const char *path, char *buf, size_t bufsiz))
{
    DEFINE_MSG(msg, readlink);
    SAFE_STRCPY(msg.args.path, path);
    AWAIT_CALL_REAL(msg, ssize_t, readlink, path, buf, bufsiz);
}

/* Outputs the full path, must be deleted aftewards? */
DEFINE_WRAPPER(int, mknod, (const char *path, mode_t mode, dev_t dev))
{
    DEFINE_MSG(msg, mknod);
    SAFE_STRCPY(msg.args.path, path);
    msg.args.mode = mode;
    msg.args.dev = dev;
    SEND_CALL_REAL(msg, int, mknod, path, mode, dev);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, mkdir, (const char *path, mode_t mode))
{
    DEFINE_MSG(msg, mkdir);
    SAFE_STRCPY(msg.args.path, path);
    msg.args.mode = mode;
    SEND_CALL_REAL(msg, int, mkdir, path, mode);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, rmdir, (const char *path))
{
    DEFINE_MSG(msg, rmdir);
    SAFE_STRCPY(msg.args.path, path);
    SEND_CALL_REAL(msg, int, rmdir, path);
}

/* Outputs the full linkpath, input the target (to make the symlink,
 * you don't depend on the target, but whoever made the symlink
 * depended on it so is going to use it, thus it makes sense to just
 * depend on the target too) */
DEFINE_WRAPPER(int, symlink, (const char *target, const char *linkpath))
{
    DEFINE_MSG(msg, symlink);
    SAFE_STRCPY(msg.args.target, target);
    SAFE_STRCPY(msg.args.linkpath, linkpath);
    /* TODO: Maybe not AWAIT here, and handle it properly? */
    AWAIT_CALL_REAL(msg, int, symlink, linkpath, target);
}

/* Inputs the full oldpath, outputs the full newpath (treated like a
 * read/write copy) */
DEFINE_WRAPPER(int, link, (const char *oldpath, const char *newpath))
{
    DEFINE_MSG(msg, link);
    SAFE_STRCPY(msg.args.oldpath, oldpath);
    SAFE_STRCPY(msg.args.newpath, newpath);
    AWAIT_CALL_REAL(msg, int, link, oldpath, newpath);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, chown, (const char *path, uid_t owner, gid_t group))
{
    DEFINE_MSG(msg, chown);
    SAFE_STRCPY(msg.args.path, path);
    msg.args.owner = owner;
    msg.args.group = group;
    SEND_CALL_REAL(msg, int, chown, path, owner, group);
}

/* TODO: Track utime? */
/* TODO: Track statfs? */
/* TODO: Track extended attributes? */
/* TODO: Track flock? */
/* TODO: Track ioctls? */
