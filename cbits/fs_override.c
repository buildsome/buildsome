#include "protocol.h"
#include "writer.h"
#include "canonize_path.h"
#include "client.h"
#include <dirent.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

#include <errno.h>

static void vtrace(enum severity, const char *fmt, va_list);
static void trace(enum severity, const char *fmt, ...);

#define TRACE_DEBUG(...)   // TRACE(severity_debug, __VA_ARGS__)
#define TRACE_WARNING(...) trace(severity_warning, __VA_ARGS__)
#define TRACE_ERROR(...)   trace(severity_error, __VA_ARGS__)

#include "undef_64_symbols.h"

static struct {
    unsigned cwd_length;
    char cwd[MAX_PATH];
    unsigned root_filter_length;
    char root_filter[MAX_PATH];
} process_state = {-1U, "", -1U, ""};

static void update_cwd(void)
{
    if(NULL == getcwd(process_state.cwd, sizeof process_state.cwd)) {
        LOG(error, "Failed to getcwd: %s", strerror(errno));
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

static void send_connection_await(const char *buf, size_t size, bool is_delayed)
{
    if(!client__send_hooked(is_delayed, buf, size)) return;
    if(!is_delayed) return;
    bool res ATTR_UNUSED = await_go();
}


#define DEFINE_WRAPPER(ret_type, name, params)  \
    typedef ret_type name##_func params;        \
    name##_func name;                           \
    ret_type name params

#define SILENT_CALL_REAL(name, ...)                     \
    ({                                                  \
        name##_func *real = dlsym(RTLD_NEXT, #name);    \
        real(__VA_ARGS__);                              \
    })

#define SEND_MSG_AWAIT(_is_delayed, msg)                \
    ({                                                  \
        send_connection_await(PS(msg), _is_delayed);    \
    })

#define AWAIT_CALL_REAL(needs_await, msg, ...)     \
    ({                                                  \
        SEND_MSG_AWAIT(needs_await, msg);               \
        SILENT_CALL_REAL(__VA_ARGS__);                  \
    })

#define DEFINE_MSG(msg, name)                   \
    struct {                                    \
        enum func func;                         \
        struct func_##name args;                \
    } __attribute__ ((packed))                  \
    msg = { .func = func_##name };

#define CREATION_FLAGS (O_CREAT | O_EXCL)

#define PATH_COPY(needs_await, dest, src)                               \
    do {                                                                \
        char temp_path[MAX_PATH];                                       \
        struct writer temp_writer = { temp_path, sizeof temp_path };    \
        if(src[0] != '/') {                                             \
            writer_append_data(&temp_writer, process_state.cwd,         \
                               process_state.cwd_length);               \
        }                                                               \
        writer_append_str(&temp_writer, src);                           \
        struct writer dest_writer = { PS(dest) };                       \
        canonize_abs_path(&dest_writer, temp_path);                     \
        bool in_root = try_chop_common_root(                            \
            process_state.root_filter_length,                           \
            process_state.root_filter, dest);                           \
        needs_await = needs_await || in_root;                           \
    } while(0)

#define IN_PATH_COPY(needs_await, dest, src)    \
    PATH_COPY(needs_await, (dest).in_path, src)

#define OUT_PATH_COPY(needs_await, dest, src)           \
    do {                                                \
        PATH_COPY(needs_await, (dest).out_path, src);   \
        (dest).out_effect = OUT_EFFECT_UNKNOWN;         \
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

#define CALL_WITH_OUTPUTS(msg, _is_delayed, ret_type, args, out_report_code) \
    ({                                                                  \
        if(_is_delayed) {                                               \
            send_connection_await(PS(msg), true);                       \
        }                                                               \
        ret_type result = SILENT_CALL_REAL args;                        \
        if(!_is_delayed) {                                              \
            do out_report_code while(0);                                \
            bool ATTR_UNUSED res = client__send_hooked(false, PS(msg)); \
            /* Can't stop me now, effect already happened, so just ignore */ \
            /* server-side rejects after-the-fact. */                   \
        }                                                               \
        result;                                                         \
    })
#define OUT_EFFECT_IF_NOT_ERROR(err_val, effect)        \
            ((err_val) == result) ? OUT_EFFECT_NOTHING : (effect)

DEFINE_WRAPPER(int, creat, (const char *path, mode_t mode))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, creat);
    OUT_PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;

    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (creat, path, mode),
        {
            /* May actually truncate file, rather than create it, but
             * the new content is created now: */
            msg.args.path.out_effect =
                OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_CREATED);
        });
}

/* Depends on the full path */
#define DEFINE_STAT_WRAPPER(name, msg_type, stat_struct)                \
DEFINE_WRAPPER(int, name, (int vers, const char *path, struct stat_struct *buf)) \
{                                                                       \
    initialize_process_state();                                         \
    bool needs_await = false;                                           \
    DEFINE_MSG(msg, msg_type);                                          \
    IN_PATH_COPY(needs_await, msg.args.path, path);                     \
    return AWAIT_CALL_REAL(needs_await, msg, name, vers, path, buf);    \
}

DEFINE_STAT_WRAPPER(   __xstat,  stat, stat  )
DEFINE_STAT_WRAPPER(  __lxstat, lstat, stat  )
DEFINE_STAT_WRAPPER( __xstat64,  stat, stat64)
DEFINE_STAT_WRAPPER(__lxstat64, lstat, stat64)

/* Depends on the full path */
DEFINE_WRAPPER(DIR *, opendir, (const char *path))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, opendir);
    IN_PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(needs_await, msg, opendir, path);
}

/* Depends on the full path */
DEFINE_WRAPPER(int, access, (const char *path, int mode))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, access);
    IN_PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;
    return AWAIT_CALL_REAL(needs_await, msg, access, path, mode);
}

/* Outputs the full path */
DEFINE_WRAPPER(int, truncate, (const char *path, off_t length))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, truncate);
    msg.args.length = length;
    OUT_PATH_COPY(needs_await, msg.args.path, path);
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (truncate, path, length),
        {
            msg.args.path.out_effect =
                OUT_EFFECT_IF_NOT_ERROR(-1, length == 0 ? OUT_EFFECT_CREATED : OUT_EFFECT_CHANGED);
        });
}

/* Depends on the full direct path */
DEFINE_WRAPPER(ssize_t, readlink, (const char *path, char *buf, size_t bufsiz))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, readlink);
    IN_PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(needs_await, msg, readlink, path, buf, bufsiz);
}

static bool dereference_dir(int dirfd, char *buf, size_t buf_len) ATTR_WARN_UNUSED_RESULT;
static bool dereference_dir(int dirfd, char *buf, size_t buf_len)
{
    const pid_t pid = getpid();
    char path[MAX_PATH];
    snprintf(PS(path), "/proc/%d/fd/%d", pid, dirfd);
    const ssize_t res = SILENT_CALL_REAL(readlink, path, buf, buf_len);
    if (res < 1) return false;

    buf[MIN(buf_len-1, (size_t)res)] = 0;
    return true;
}

static bool get_fullpath_of_dirfd(char *fullpath, size_t fullpath_size, int dirfd, const char *path) ATTR_WARN_UNUSED_RESULT;
static bool get_fullpath_of_dirfd(char *fullpath, size_t fullpath_size, int dirfd, const char *path)
{
    /* path is relative to some dir... */
    char dirpath[MAX_PATH];
    if (!dereference_dir(dirfd, PS(dirpath))) {
        LOG(error, "Cannot dereference directory fd: %d", dirfd);
        return false;
    }
    const uint32_t size = snprintf(fullpath, fullpath_size, "%s/%s", dirpath, path);
    if (size >= fullpath_size) {
        TRACE_ERROR("Path too long!");
        ASSERT(0);
    }
    return true;
}

/* Outputs the full path */
DEFINE_WRAPPER(int, unlink, (const char *path))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, unlink);
    msg.args.flags = 0;
    OUT_PATH_COPY(needs_await, msg.args.path, path);
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (unlink, path),
        {
            msg.args.path.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_DELETED);
        });
}

/* Outputs the full path */
DEFINE_WRAPPER(int, unlinkat, (int dirfd, const char *path, int flags))
{
    char fullpath[MAX_PATH];
    const char *pathptr = path;
    initialize_process_state();
    if (AT_FDCWD != dirfd && path[0] != '/') {
        if (!get_fullpath_of_dirfd(PS(fullpath), dirfd, path)) return -1;
        TRACE_DEBUG("unlinkat %d, %s, %X -> %s", dirfd, path, flags, fullpath);
        pathptr = fullpath;
    }
    bool needs_await = false;
    DEFINE_MSG(msg, unlink);
    msg.args.flags = flags;
    OUT_PATH_COPY(needs_await, msg.args.path, pathptr);
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (unlinkat, dirfd, path, flags),
        {
            msg.args.path.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_DELETED);
        });
}

/* Outputs both full paths */
DEFINE_WRAPPER(int, rename, (const char *oldpath, const char *newpath))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, rename);
    OUT_PATH_COPY(needs_await, msg.args.oldpath, oldpath);
    OUT_PATH_COPY(needs_await, msg.args.newpath, newpath);
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (rename, oldpath, newpath),
        {
            msg.args.oldpath.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_DELETED);
            /* not CREATED because it has a useful existing content from old file: */
            msg.args.newpath.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_CHANGED);
        });
}

/* Outputs the full path */
DEFINE_WRAPPER(int, chmod, (const char *path, mode_t mode))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, chmod);
    OUT_PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (chmod, path, mode),
        {
            msg.args.path.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_CHANGED);
        });
}

/* Outputs the full path, must be deleted aftewards? */
DEFINE_WRAPPER(int, mknod, (const char *path, mode_t mode, dev_t dev))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, mknod);
    OUT_PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;
    msg.args.dev = dev;
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (mknod, path, mode, dev),
        {
            msg.args.path.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_CREATED);
        });
}

/* Outputs the full path */
DEFINE_WRAPPER(int, mkdir, (const char *path, mode_t mode))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, mkdir);
    OUT_PATH_COPY(needs_await, msg.args.path, path);
    msg.args.mode = mode;
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (mkdir, path, mode),
        {
            msg.args.path.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_CREATED);
        });
}

/* Outputs the full path */
DEFINE_WRAPPER(int, rmdir, (const char *path))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, rmdir);
    OUT_PATH_COPY(needs_await, msg.args.path, path);
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (rmdir, path),
        {
            msg.args.path.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_DELETED);
        });
}

/* Outputs the full linkpath, input the target (to make the symlink,
 * you don't depend on the target, but whoever made the symlink
 * depended on it so is going to use it, thus it makes sense to just
 * depend on the target too) */
DEFINE_WRAPPER(int, symlink, (const char *target, const char *linkpath))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, symlink);
    IN_PATH_COPY(needs_await, msg.args.target, target);
    OUT_PATH_COPY(needs_await, msg.args.linkpath, linkpath);
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (symlink, target, linkpath),
        {
            msg.args.linkpath.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_CREATED);
        });
}

DEFINE_WRAPPER(int, link, (const char *oldpath, const char *newpath))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, link);
    OUT_PATH_COPY(needs_await, msg.args.oldpath, oldpath);
    OUT_PATH_COPY(needs_await, msg.args.newpath, newpath);
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (link, oldpath, newpath),
        {
            msg.args.oldpath.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_CHANGED); // stat/refcount changed
            msg.args.newpath.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_CREATED);
        });
}

/* Outputs the full path */
DEFINE_WRAPPER(int, chown, (const char *path, uid_t owner, gid_t group))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, chown);
    OUT_PATH_COPY(needs_await, msg.args.path, path);
    msg.args.owner = owner;
    msg.args.group = group;
    return CALL_WITH_OUTPUTS(
        msg, needs_await,
        int, (chown, path, owner, group),
        {
            msg.args.path.out_effect = OUT_EFFECT_IF_NOT_ERROR(-1, OUT_EFFECT_CHANGED);
        });
}

DEFINE_WRAPPER(int, fchdir, (int fd))
{
    int result = SILENT_CALL_REAL(fchdir, fd);
    update_cwd();
    return result;
}

DEFINE_WRAPPER(int, chdir, (const char *path))
{
    int result = SILENT_CALL_REAL(chdir, path);
    update_cwd();
    return result;
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

int execvpe_real(const char *file, char *const argv[], char *const envp[]);

DEFINE_WRAPPER(int, execvpe, (const char *file, char *const argv[], char *const envp[]))
{
    initialize_process_state();
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
            LOG(error, "confstr failed: %s", strerror(errno));
            ASSERT(0);
        }
        ASSERT(size <= sizeof msg.args.conf_str_CS_PATH); /* Value truncated */
    }

    SEND_MSG_AWAIT(true, msg);

    return execvpe_real(file, argv, envp);
}

#ifdef __APPLE__
extern char **environ;
#endif

int execvpe_real(const char *file, char *const argv[], char *const envp[])
{
#ifdef __APPLE__
// See http://stackoverflow.com/a/7789795/40916 for reasoning about thread safety
    char **saved = environ;
    int rc;
    environ = (char**) envp;
    rc = execvp(file, argv);
    environ = saved;
    return rc;
#else
    return SILENT_CALL_REAL(execvpe, file, argv, envp);
#endif
}

int execvp(const char *file, char *const argv[])
{
    return execvpe(file, argv, environ);
}

int execv(const char *path, char *const argv[])
{
    return execve(path, argv, environ);
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
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, exec);
    IN_PATH_COPY(needs_await, msg.args.path, filename);
    return AWAIT_CALL_REAL(needs_await, msg, execve, filename, argv, envp);
}

/****************** open ********************/

#define OPEN_HANDLER(_name, _path, _flags)                              \
    do {                                                                \
        initialize_process_state();                                     \
        bool is_also_read = false;                                      \
        bool is_create = _flags & CREATION_FLAGS;                       \
        bool is_truncate = _flags & O_TRUNC;                            \
        va_list args;                                                   \
        va_start(args, _flags);                                         \
        mode_t mode = is_create ? va_arg(args, mode_t) : 0;             \
        va_end(args);                                                   \
        switch(_flags & (O_RDONLY | O_RDWR | O_WRONLY)) {               \
        case O_RDONLY: {                                                \
            /* TODO: Remove ASSERT and correctly handle O_CREAT, O_TRUNCATE */ \
            ASSERT(!(flags & CREATION_FLAGS));                          \
            bool needs_await = false;                                   \
            DEFINE_MSG(msg, openr);                                     \
            IN_PATH_COPY(needs_await, msg.args.path, _path);            \
            return AWAIT_CALL_REAL(                                     \
                needs_await, msg, _name, _path, _flags);                \
        }                                                               \
        case O_RDWR:                                                    \
            is_also_read = true;                                        \
        case O_WRONLY: {                                                \
            bool needs_await = false;                                   \
            DEFINE_MSG(msg, openw);                                     \
            OUT_PATH_COPY(needs_await, msg.args.path, _path);           \
            if(is_also_read) msg.args.flags |= FLAG_ALSO_READ;          \
            if(is_create)    msg.args.flags |= FLAG_CREATE;             \
            if(is_truncate)  msg.args.flags |= FLAG_TRUNCATE;           \
            msg.args.mode = mode;                                       \
            return CALL_WITH_OUTPUTS(                                   \
                msg, needs_await,                                       \
                int, (_name, _path, _flags, mode),                      \
                {                                                       \
                    msg.args.path.out_effect =                          \
                        OUT_EFFECT_IF_NOT_ERROR(                        \
                            -1, is_truncate ? OUT_EFFECT_CREATED : OUT_EFFECT_CHANGED); \
                });                                                     \
        }                                                               \
        }                                                               \
        LOG(error, "invalid open mode?!");                              \
        ASSERT(0);                                                      \
        return -1;                                                      \
    } while(0)

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
    OPEN_HANDLER(open, path, flags);
}

DEFINE_WRAPPER(int, openat, (int dirfd, const char *path, int flags, ...))
{
    if (AT_FDCWD == dirfd || path[0] == '/') {
        TRACE_DEBUG("openat %d %s %X degrading to normal open", dirfd, path, flags);
        OPEN_HANDLER(open, path, flags);
    } else {
        char fullpath[MAX_PATH];
        if (!get_fullpath_of_dirfd(PS(fullpath), dirfd, path)) return -1;
        TRACE_DEBUG("openat %d %s %X converted to open of %s", dirfd, path, flags, fullpath);
        OPEN_HANDLER(open, fullpath, flags);
    }
}

/* Ditto open */
DEFINE_WRAPPER(int, open64, (const char *path, int flags, ...))
{
    OPEN_HANDLER(open64, path, flags);
}

DEFINE_WRAPPER(int, __open_2, (const char *path, int flags, ...))
{
    OPEN_HANDLER(__open_2, path, flags);
}

struct fopen_mode_bools {
    bool is_create;
    bool is_write;
    bool is_read;
    bool is_truncate;
};

struct fopen_mode_bools fopen_parse_modestr(const char *modestr)
{
    struct fopen_mode_bools res = {false, false, false, false};
    switch(modestr[0]) {
    case 'r': {
        res.is_read = true;
        if(modestr[1] == '+') {
            res.is_write = true;
        }
        break;
    }
    case 'w':
        res.is_write = true;
        res.is_create = true;
        if(modestr[1] == '+') {
            res.is_read = true;
        } else {
            res.is_truncate = true;
        }
        break;
    case 'a':
        res.is_write = true;
        res.is_read = true;
        res.is_create = true;
    default:
        LOG(error, "Invalid fopen mode?!");
        ASSERT(0);
    }
    return res;
}

#define FOPEN_HANDLER(name, path, modestr, ...)                         \
    do {                                                                \
        initialize_process_state();                                     \
        struct fopen_mode_bools mode = fopen_parse_modestr(modestr);    \
        if(!mode.is_write && !mode.is_create && !mode.is_truncate) {    \
            ASSERT(mode.is_read);                                       \
            bool needs_await = false;                                   \
            DEFINE_MSG(msg, openr);                                     \
            IN_PATH_COPY(needs_await, msg.args.path, path);             \
            return AWAIT_CALL_REAL(                                     \
                needs_await, msg, name, path, modestr, ##__VA_ARGS__);  \
        }                                                               \
        bool needs_await = false;                                       \
        DEFINE_MSG(msg, openw);                                         \
        OUT_PATH_COPY(needs_await, msg.args.path, path);                \
        if(mode.is_read)     msg.args.flags |= FLAG_ALSO_READ;          \
        if(mode.is_create)   msg.args.flags |= FLAG_CREATE;             \
        if(mode.is_truncate) msg.args.flags |= FLAG_TRUNCATE;           \
        msg.args.mode = 0666;                                           \
        return CALL_WITH_OUTPUTS(                                       \
            msg, needs_await, FILE *, (name, path, modestr, ##__VA_ARGS__),               \
            {                                                           \
                msg.args.path.out_effect =                              \
                    OUT_EFFECT_IF_NOT_ERROR(                            \
                        NULL, mode.is_truncate ? OUT_EFFECT_CREATED : OUT_EFFECT_CHANGED); \
            });                                                         \
    } while(0)

DEFINE_WRAPPER(FILE *, fopen, (const char *path, const char *modestr))
{
    FOPEN_HANDLER(fopen, path, modestr);
}

DEFINE_WRAPPER(FILE *, fopen64, (const char *path, const char *modestr))
{
    FOPEN_HANDLER(fopen64, path, modestr);
}

DEFINE_WRAPPER(FILE *, freopen, (const char *path, const char *modestr, FILE *stream))
{
    FOPEN_HANDLER(freopen, path, modestr, stream);
}

DEFINE_WRAPPER(FILE *, freopen64, (const char *path, const char *modestr, FILE *stream))
{
    FOPEN_HANDLER(freopen64, path, modestr, stream);
}

/* NOTE: The real implementation of dlopen does nasty things like
 * looking up who called it, reading elf variables specifying where to
 * search for the given filename, and more.
 *
 * This all means that even a trivial wrapper hook for dlopen changes
 * its behavior observably.
 *
 * For example, the `pdwtags' program uses dlopen on a .so filename
 * that is found in a directory specified in the elf variable of
 * libdw.so (which does the dlopen call). When buildsome hooks this
 * call, the elf variable is not found, and thus the given file is not
 * found, altering the output to an incorrect one.
 *
 * Switching to fuse should resolve this issue.
 */
DEFINE_WRAPPER(void *, dlopen, (const char *filename, int flag))
{
    if(!filename) return SILENT_CALL_REAL(dlopen, filename, flag);

    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, openr);
    IN_PATH_COPY(needs_await, msg.args.path, filename);
    /* TODO: dlopen does a lot of searching/etc if the pathname does
     * not contain slash, need to handle it correctly (or switch to
     * fuse!) */
    return AWAIT_CALL_REAL(needs_await, msg, dlopen, filename, flag);
}

DEFINE_WRAPPER(char *, realpath, (const char *path, char *resolved_path))
{
    initialize_process_state();
    bool needs_await = false;
    DEFINE_MSG(msg, realpath);
    IN_PATH_COPY(needs_await, msg.args.path, path);
    return AWAIT_CALL_REAL(needs_await, msg, realpath, path, resolved_path);
}

/*************************************/

static void vtrace(enum severity sev, const char *fmt, va_list args)
{
    initialize_process_state();
    DEFINE_MSG(msg, trace);
    msg.args.severity = sev;
    vsnprintf(PS(msg.args.msg), fmt, args);
    const bool success __attribute__((unused)) =
        client__send_hooked(false, PS(msg));
}

static void trace(enum severity sev, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vtrace(sev, fmt, args);
    va_end(args);
}

/* TODO: Track utime? */
/* TODO: Track statfs? */
/* TODO: Track extended attributes? */
/* TODO: Track flock? */
/* TODO: Track ioctls? */

static FILE *log_file(void)
{
    /* TODO: This is buggy for fork() */
    static FILE *f = NULL;
    if(!f) {
        FILE *(*fopen_real)(const char *path, const char *mode) =
            dlsym(RTLD_NEXT, "fopen");
        char name[256];
        int (*mkdir_real)(const char *pathname, mode_t mode) =
            dlsym(RTLD_NEXT, "mkdir");
        mkdir_real("/tmp/fs_override.so.log", 0777);
        snprintf(name, sizeof name, "/tmp/fs_override.so.log/pid.%d", getpid());
        f = fopen_real(name, "w");
        ASSERT(f);
    }
    return f;
}

void _do_log(enum severity sev, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(log_file(), fmt, args);
    va_end(args);
    fflush(log_file());
    va_start(args, fmt);
    vtrace(sev, fmt, args);
    va_end(args);
}
