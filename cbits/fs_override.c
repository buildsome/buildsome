#include "c.h"
#include "writer.h"
#include "canonize_path.h"
#include <sys/types.h>
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
#include <stdint.h>
#include <stdbool.h>
#include <sys/syscall.h>

#include <errno.h>

#define MAX_FRAME_SIZE 8192

#define ENVVARS_PREFIX "BUILDSOME_"
#define PROTOCOL_HELLO "PROTOCOL3: HELLO, I AM: "

#define PERM_ERROR(x, fmt, ...)                                 \
    ({                                                          \
        DEBUG("Returning EPERM error: " fmt, ##__VA_ARGS__);    \
        errno = EPERM;                                          \
        (x);                                                    \
    })

static int gettid(void)
{
    return syscall(__NR_gettid);
}

static int connect_master(void)
{
    int fd = socket(AF_UNIX, SOCK_SEQPACKET, 0);
    ASSERT(-1 != fd);

    char *env_sockaddr = getenv(ENVVARS_PREFIX "MASTER_UNIX_SOCKADDR");
    ASSERT(env_sockaddr);

    char *env_job_id = getenv(ENVVARS_PREFIX "JOB_ID");
    ASSERT(env_job_id);

    struct sockaddr_un addr = {
        .sun_family = AF_UNIX,
    };
    ASSERT(strlen(env_sockaddr) < sizeof addr.sun_path);
    strcpy(addr.sun_path, env_sockaddr);

    DEBUG("pid%d, tid%d: connecting \"%s\"", getpid(), gettid(), env_sockaddr);
    int connect_rc = connect(fd, &addr, sizeof addr);
    if(0 != connect_rc) {
        close(fd);
        return -1;
    }

    char hello[strlen(PROTOCOL_HELLO) + strlen(env_job_id) + 16];
    hello[sizeof hello-1] = 0;
    int len = snprintf(hello, sizeof hello-1, PROTOCOL_HELLO "%d:%d:%s", getpid(), gettid(), env_job_id);
    ssize_t send_rc = send(fd, hello, len, 0);
    if(send_rc != len) {
        close(fd);
        return -1;
    }

    return fd;
}

/* NOTE: This must be kept in sync with Protocol.hs */
#define MAX_PATH 256

static struct {
    unsigned cwd_length;
    char cwd[MAX_PATH];
    unsigned root_filter_length;
    char root_filter[MAX_PATH];
} process_state = {-1U, "", -1U, ""};

static __thread struct {
    pid_t pid;
    int connection_fd;
} thread_state = {-1, -1};

static bool await_go(void) __attribute__((warn_unused_result));

static void update_cwd(void)
{
    if(NULL == getcwd(process_state.cwd, sizeof process_state.cwd)) {
        LOG("Failed to getcwd: %s", strerror(errno));
        ASSERT(0);
    }
    process_state.cwd_length = strnlen(process_state.cwd, sizeof process_state.cwd);

    /* Append a '/' */
    process_state.cwd_length++;
    ASSERT(process_state.cwd_length < MAX_PATH);
    process_state.cwd[process_state.cwd_length-1] = '/';
    process_state.cwd[process_state.cwd_length] = 0;
}

static void initialize_process_state(void)
{
    if(-1U != process_state.cwd_length) return;
    update_cwd();

    const char *root_filter = getenv(ENVVARS_PREFIX "ROOT_FILTER");
    ASSERT(root_filter);

    unsigned len = strlen(root_filter);
    ASSERT(len < sizeof process_state.root_filter);
    memcpy(process_state.root_filter, root_filter, len);
    if(root_filter[len] == '/') {
        len--;
    }
    ASSERT(len < sizeof process_state.root_filter);
    process_state.root_filter[len] = 0;
    process_state.root_filter_length = len;
}

static int connection(void)
{
    pid_t pid = getpid();
    if(pid != thread_state.pid) {
        int fd = connect_master();
        if(-1 == fd) return -1;
        thread_state.connection_fd = fd;
        thread_state.pid = pid;
        if(!await_go()) return -1;
    }
    return thread_state.connection_fd;
}

static int assert_connection(void)
{
    ASSERT(getpid() == thread_state.pid);
    return thread_state.connection_fd;
}

static bool send_connection(const char *buf, size_t size) __attribute__((warn_unused_result));
static bool send_connection(const char *buf, size_t size)
{
    int fd = connection();
    if(-1 == fd) return false;

    ssize_t rc = send(fd, buf, size, 0);
    return rc == (ssize_t)size;
}

static bool send_connection_await(const char *buf, size_t size, bool is_delayed) __attribute__((warn_unused_result));
static bool send_connection_await(const char *buf, size_t size, bool is_delayed)
{
    if(!send_connection(buf, size)) return false;
    if(!is_delayed) return true;
    return await_go();
}

/* NOTE: This must be kept in sync with Protocol.hs */
enum func {
    func_openr     = 0x10000,
    func_openw     = 0x10001,
    func_creat     = 0x10002,
    func_stat      = 0x10003,
    func_lstat     = 0x10004,
    func_opendir   = 0x10005,
    func_access    = 0x10006,
    func_truncate  = 0x10007,
    func_unlink    = 0x10008,
    func_rename    = 0x10009,
    func_chmod     = 0x1000A,
    func_readlink  = 0x1000B,
    func_mknod     = 0x1000C,
    func_mkdir     = 0x1000D,
    func_rmdir     = 0x1000E,
    func_symlink   = 0x1000F,
    func_link      = 0x10010,
    func_chown     = 0x10011,
    func_exec      = 0x10012,
    func_execp     = 0x10013,
};

/* func_open.flags */
#define FLAG_ALSO_READ 1
#define FLAG_CREATE 2           /* func_open.mode is meaningful iff this flag */

/* NOTE: This must be kept in sync with Protocol.hs */
#define MAX_PATH_ENV_VAR_LENGTH (10*1024)
#define MAX_PATH_CONF_STR       (10*1024)
#define MAX_EXEC_FILE           (MAX_PATH)

/* NOTE: This must be kept in sync with Protocol.hs */
struct func_openr     {char path[MAX_PATH];};
struct func_openw     {char path[MAX_PATH]; uint32_t flags; uint32_t mode;};
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
struct func_exec      {char path[MAX_PATH];};
struct func_execp     {char file[MAX_EXEC_FILE]; char cwd[MAX_PATH]; char env_var_PATH[MAX_PATH_ENV_VAR_LENGTH]; char conf_str_CS_PATH[MAX_PATH_CONF_STR];};

#define DEFINE_WRAPPER(ret_type, name, params)  \
    typedef ret_type name##_func params;        \
    name##_func name;                           \
    ret_type name params

#define SILENT_CALL_REAL(ret_type, name, ...)           \
    ({                                                  \
        name##_func *real = dlsym(RTLD_NEXT, #name);    \
        real(__VA_ARGS__);                              \
    })

#define SEND_MSG_AWAIT(_is_delayed, msg)        \
    ({                                          \
        (msg).is_delayed = (_is_delayed);       \
        send_connection_await(PS(msg), _is_delayed);    \
    })

#define AWAIT_CALL_REAL(err, needs_await, msg, ...)  \
    ({                                          \
        SEND_MSG_AWAIT(needs_await, msg)        \
            ? SILENT_CALL_REAL(__VA_ARGS__)     \
            : (err);                            \
    })

#define DEFINE_MSG(msg, name)                   \
    initialize_process_state();                 \
    struct {                                    \
        uint8_t is_delayed;                     \
        enum func func;                         \
        struct func_##name args;                \
    } __attribute__ ((packed))                  \
    msg = { .func = func_##name };

#define CREATION_FLAGS (O_CREAT | O_EXCL)

static bool await_go(void)
{
    char buf[16];
    ssize_t rc = recv(assert_connection(), PS(buf), 0);
    return 2 == rc && !strncmp(buf, "GO", 2);
}

#define PATH_COPY(needs_await, dest, src)                       \
    do {                                                        \
        char temp_path[MAX_PATH];                               \
        struct writer w = { temp_path, sizeof temp_path };      \
        if(src[0] != '/') {                                     \
            writer_append_data(&w, process_state.cwd,           \
                               process_state.cwd_length);       \
        }                                                       \
        writer_append_str(&w, src);                             \
        struct writer dest_writer = { dest, sizeof dest };      \
        canonize_abs_path(&dest_writer, temp_path);             \
        bool in_root = try_chop_common_root(                    \
            process_state.root_filter_length,                   \
            process_state.root_filter, dest);                   \
        needs_await = needs_await || in_root;                   \
    } while(0)

static bool try_chop_common_root(unsigned prefix_length, char *prefix, char *canonized_path)
{
    if(0 == prefix_length) return true;

    size_t canonized_path_len = strlen(canonized_path);
    if(canonized_path_len < prefix_length ||
       strncmp(canonized_path, prefix, prefix_length))
    {
        return false;
    }

    unsigned copy_pos = prefix_length + (canonized_path[prefix_length] == '/' ? 1 : 0);
    memmove(canonized_path, canonized_path + copy_pos, canonized_path_len - copy_pos);
    canonized_path[canonized_path_len - copy_pos] = 0;
    return true;
}

static bool notify_openr_await(const char *path) __attribute__((warn_unused_result));
static bool notify_openr_await(const char *path)
{
    bool needs_await = false;
    DEFINE_MSG(msg, openr);
    PATH_COPY(needs_await, msg.args.path, path);
    return SEND_MSG_AWAIT(needs_await, msg);
}

static bool notify_openw(const char *path, bool is_also_read, bool is_create, mode_t mode) __attribute__((warn_unused_result));
static bool notify_openw(const char *path, bool is_also_read, bool is_create, mode_t mode)
{
    bool needs_await = false;
    DEFINE_MSG(msg, openw);
    PATH_COPY(needs_await, msg.args.path, path);
    if(is_also_read) msg.args.flags |= FLAG_ALSO_READ;
    if(is_create)    msg.args.flags |= FLAG_CREATE;
    msg.args.mode = mode;
    return SEND_MSG_AWAIT(needs_await, msg);
}

static bool open_common(const char *path, int flags, va_list args, mode_t *out_mode) __attribute__((warn_unused_result));
static bool open_common(const char *path, int flags, va_list args, mode_t *out_mode)
{
    bool is_also_read = false;
    bool is_create = flags & CREATION_FLAGS;
    *out_mode = is_create ? va_arg(args, mode_t) : 0;

    switch(flags & (O_RDONLY | O_RDWR | O_WRONLY)) {
    case O_RDONLY:
        return notify_openr_await(path);
    case O_RDWR:
        is_also_read = true;
    case O_WRONLY:
        return notify_openw(path, is_also_read, is_create, *out_mode);
    default:
        LOG("invalid open mode?!");
        ASSERT(0);
    }
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
    mode_t mode;
    bool res = open_common(path, flags, args, &mode);
    va_end(args);
    if(!res) return PERM_ERROR(-1, "open \"%s\"", path);

    /* open_common handles the reporting */
    return SILENT_CALL_REAL(int, open, path, flags, mode);
}

/* Ditto open */
DEFINE_WRAPPER(int, open64, (const char *path, int flags, ...))
{
    va_list args;
    va_start(args, flags);
    mode_t mode;
    bool res = open_common(path, flags, args, &mode);
    va_end(args);

    if(!res) return PERM_ERROR(-1, "open64 \"%s\"", path);

    /* open_common handles the reporting */
    return SILENT_CALL_REAL(int, open64, path, flags, mode);
}

static bool fopen_common(const char *path, const char *modestr) __attribute__ ((warn_unused_result));
static bool fopen_common(const char *path, const char *modestr)
{
    mode_t mode = 0666;
    switch(modestr[0]) {
    case 'r':
        if(modestr[1] == '+') {
            return notify_openw(path, /*is_also_read*/true, /*create*/false, 0);
        } else {
            return notify_openr_await(path);
        }
        break;
    case 'w':
        return notify_openw(path, /*is_also_read*/modestr[1] == '+',
                     /*create*/true, mode);
    case 'a':
        return notify_openw(path, true, /*create*/true, mode);
    default:
        LOG("Invalid fopen mode?!");
        ASSERT(0);
    }
}

/* Ditto open */
DEFINE_WRAPPER(FILE *, fopen, (const char *path, const char *mode))
{
    if(!fopen_common(path, mode)) return PERM_ERROR(NULL, "fopen \"%s\" \"%s\"", path, mode);
    /* fopen_common handles the reporting */
    return SILENT_CALL_REAL(FILE *, fopen, path, mode);
}

/* Ditto open */
DEFINE_WRAPPER(FILE *, fopen64, (const char *path, const char *mode))
{
    if(!fopen_common(path, mode)) return PERM_ERROR(NULL, "fopen64 \"%s\" \"%s\"", path, mode);
    /* fopen_common handles the reporting */
    return SILENT_CALL_REAL(FILE *, fopen64, path, mode);
}

/* Ditto open(W) */
DEFINE_WRAPPER(int, creat, (const char *path, mode_t mode))
{
    bool needs_await = false;
    DEFINE_MSG(msg, creat);
    PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "creat \"%s\"", path), needs_await, msg, int, creat, path, mode);
}

/* Depends on the full path */
DEFINE_WRAPPER(int, __xstat, (int vers, const char *path, struct stat *buf))
{
    bool needs_await = false;
    DEFINE_MSG(msg, stat);
    PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "xstat \"%s\"", path), needs_await, msg, int, __xstat, vers, path, buf);
}

/* Depends on the full direct path */
DEFINE_WRAPPER(int, __lxstat, (int vers, const char *path, struct stat *buf))
{
    bool needs_await = false;
    DEFINE_MSG(msg, lstat);
    PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "lstat \"%s\"", path), needs_await, msg, int, __lxstat, vers, path, buf);
}

/* Depends on the full path */
DEFINE_WRAPPER(DIR *, opendir, (const char *path))
{
    bool needs_await = false;
    DEFINE_MSG(msg, opendir);
    PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(PERM_ERROR(NULL, "opendir \"%s\"", path), needs_await, msg, DIR *, opendir, path);
}

/* Depends on the full path */
DEFINE_WRAPPER(int, access, (const char *path, int mode))
{
    bool needs_await = false;
    DEFINE_MSG(msg, access);
    PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "access \"%s\" 0x%X", path, mode), needs_await, msg, int, access, path, mode);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, truncate, (const char *path, off_t length))
{
    bool needs_await = false;
    DEFINE_MSG(msg, truncate);
    PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "truncate \"%s\" %lu", path, length), needs_await, msg, int, truncate, path, length);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, unlink, (const char *path))
{
    bool needs_await = false;
    DEFINE_MSG(msg, unlink);
    PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "unlink \"%s\"", path), needs_await, msg, int, unlink, path);
}

/* Outputs both full paths */
DEFINE_WRAPPER(int, rename, (const char *oldpath, const char *newpath))
{
    bool needs_await = false;
    DEFINE_MSG(msg, rename);
    PATH_COPY(needs_await, msg.args.oldpath, oldpath);
    PATH_COPY(needs_await, msg.args.newpath, newpath);
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "rename \"%s\" -> \"%s\"", oldpath, newpath), needs_await, msg, int, rename, oldpath, newpath);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, chmod, (const char *path, mode_t mode))
{
    bool needs_await = false;
    DEFINE_MSG(msg, chmod);
    PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "chmod \"%s\"", path), needs_await, msg, int, chmod, path, mode);
}

/* Depends on the full direct path */
DEFINE_WRAPPER(ssize_t, readlink, (const char *path, char *buf, size_t bufsiz))
{
    bool needs_await = false;
    DEFINE_MSG(msg, readlink);
    PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "readlink \"%s\"", path), needs_await, msg, ssize_t, readlink, path, buf, bufsiz);
}

/* Outputs the full path, must be deleted aftewards? */
DEFINE_WRAPPER(int, mknod, (const char *path, mode_t mode, dev_t dev))
{
    bool needs_await = false;
    DEFINE_MSG(msg, mknod);
    PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;
    msg.args.dev = dev;
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "mknod \"%s\"", path), needs_await, msg, int, mknod, path, mode, dev);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, mkdir, (const char *path, mode_t mode))
{
    bool needs_await = false;
    DEFINE_MSG(msg, mkdir);
    PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "mkdir \"%s\"", path), needs_await, msg, int, mkdir, path, mode);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, rmdir, (const char *path))
{
    bool needs_await = false;
    DEFINE_MSG(msg, rmdir);
    PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "rmdir \"%s\"", path), needs_await, msg, int, rmdir, path);
}

/* Outputs the full linkpath, input the target (to make the symlink,
 * you don't depend on the target, but whoever made the symlink
 * depended on it so is going to use it, thus it makes sense to just
 * depend on the target too) */
DEFINE_WRAPPER(int, symlink, (const char *target, const char *linkpath))
{
    bool needs_await = false;
    DEFINE_MSG(msg, symlink);
    PATH_COPY(needs_await, msg.args.target, target);
    PATH_COPY(needs_await, msg.args.linkpath, linkpath);
    /* TODO: Maybe not AWAIT here, and handle it properly? */
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "symlink \"%s\" -> \"%s\"", linkpath, target), needs_await, msg, int, symlink, linkpath, target);
}

/* Inputs the full oldpath, outputs the full newpath (treated like a
 * read/write copy) */
DEFINE_WRAPPER(int, link, (const char *oldpath, const char *newpath))
{
    bool needs_await = false;
    DEFINE_MSG(msg, link);
    PATH_COPY(needs_await, msg.args.oldpath, oldpath);
    PATH_COPY(needs_await, msg.args.newpath, newpath);
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "link \"%s\" -> \"%s\"", newpath, oldpath), needs_await, msg, int, link, oldpath, newpath);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, chown, (const char *path, uid_t owner, gid_t group))
{
    bool needs_await = false;
    DEFINE_MSG(msg, chown);
    PATH_COPY(needs_await, msg.args.path, path);
    msg.args.owner = owner;
    msg.args.group = group;
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "chown \"%s\" %d:%d", path, owner, group), needs_await, msg, int, chown, path, owner, group);
}

int fchdir(int fd)
{
    (void)fd;
    /* TODO: We need to track open of files and directories to know
     * the path here */
    LOG("fchdir is not supported!");
    ASSERT(0);
    return -1;
}

DEFINE_WRAPPER(int, chdir, (const char *path))
{
    update_cwd();
    return SILENT_CALL_REAL(int, chdir, path);
}

static unsigned count_non_null_char_ptrs(va_list args)
{
    va_list args_copy;
    va_copy(args_copy, args);
    unsigned arg_count;
    for(arg_count = 0; va_arg(args_copy, const char *); arg_count++) {
        /* No need to do anything here... */
    }
    va_end(args_copy);
    return arg_count;
}

static char **malloc_argv_from(char *arg, va_list args)
{
    unsigned arg_count = count_non_null_char_ptrs(args) + /*for first arg*/ 1;
    char **argv = malloc(arg_count * sizeof(const char *));
    argv[0] = arg;
    unsigned i;
    for(i = 1; i < arg_count; i++) {
        argv[i] = va_arg(args, char *);
    }
    return argv;
}

int execlp(const char *file, const char *arg, ...)
{
    va_list args;
    va_start(args, arg);
    /* Need to cast away the constness, because execl*'s prototypes
     * are buggy -- taking ptr to const char whereas execv* take ptr
     * to const array of ptr to NON-CONST char */
    char **argv = malloc_argv_from((char *)arg, args);
    va_end(args);
    int rc = execvp(file, argv);
    free(argv);
    return rc;
}

int execvp(const char *file, char *const argv[])
{
    return execvpe(file, argv, environ);
}

int execv(const char *path, char *const argv[])
{
    return execve(path, argv, environ);
}

DEFINE_WRAPPER(int, execvpe, (const char *file, char *const argv[], char *const envp[]))
{
    DEFINE_MSG(msg, execp);

    // char env_var_PATH[MAX_PATH_ENV_VAR_LENGTH];
    // char conf_str_CS_PATH[MAX_PATH_CONF_STR];

    {
        struct writer w = { PS(msg.args.file) };
        writer_append_str(&w, file);
    }

    {
        struct writer w = { PS(msg.args.cwd) };
        writer_append_data(&w, process_state.cwd, process_state.cwd_length);
        *writer_append(&w, 1) = 0;
    }

    {
        struct writer w = { PS(msg.args.env_var_PATH) };
        const char *PATH = getenv("PATH");
        writer_append_str(&w, PATH);
    }

    {
        errno = 0;
        size_t size = confstr(_CS_PATH, msg.args.conf_str_CS_PATH, sizeof msg.args.conf_str_CS_PATH);
        if(0 == size && 0 != errno) {
            LOG("confstr failed: %s", strerror(errno));
            ASSERT(0);
        }
        ASSERT(size <= sizeof msg.args.conf_str_CS_PATH); /* Value truncated */
    }

    return AWAIT_CALL_REAL(PERM_ERROR(-1, "execvpe \"%s\"", file), true, msg, int, execvpe, file, argv, envp);
}

/* The following exec* functions that do not have "v" in their name
 * aren't hooks, but translators to the va_list interface which is
 * hooked. If we let this translation happen within libc we won't be
 * able to hook the intra-libc calls. */

int execl(const char *path, const char *arg, ...)
{
    va_list args;
    va_start(args, arg);
    /* Need to cast away the constness, because execl*'s prototypes
     * are buggy -- taking ptr to const char whereas execv* take ptr
     * to const array of ptr to NON-CONST char */
    char **argv = malloc_argv_from((char *)arg, args);
    va_end(args);
    int rc = execv(path, argv);
    free(argv);
    return rc;
}

int execle(const char *path, const char *arg, ...)
{
    va_list args;
    va_start(args, arg);
    char **argv = malloc_argv_from((char *)arg, args);
    ASSERT(NULL == va_arg(args, const char *));
    char *const *envp = va_arg(args, char * const *);
    va_end(args);
    int rc = execve(path, argv, envp);
    free(argv);
    return rc;
}

DEFINE_WRAPPER(int, execve, (const char *filename, char *const argv[], char *const envp[]))
{
    bool needs_await = false;
    DEFINE_MSG(msg, exec);
    PATH_COPY(needs_await, msg.args.path, filename);
    return AWAIT_CALL_REAL(PERM_ERROR(-1, "execve: \"%s\"", filename), needs_await, msg, int, execve, filename, argv, envp);
}

/* TODO: Track utime? */
/* TODO: Track statfs? */
/* TODO: Track extended attributes? */
/* TODO: Track flock? */
/* TODO: Track ioctls? */

FILE *log_file(void)
{
    static FILE *f = NULL;
    if(!f) {
        FILE *(*fopen_real)(const char *path, const char *mode) =
            dlsym(RTLD_NEXT, "fopen");
        char name[256];
        snprintf(PS(name), "/tmp/fs_override.so.log.%d", getpid());
        f = fopen_real(name, "w");
        ASSERT(f);
    }
    return f;
}
