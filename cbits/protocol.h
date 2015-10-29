#ifndef ___BUILDSOME_CBITS_PROTOCOL_H_
#define ___BUILDSOME_CBITS_PROTOCOL_H_

#include "severity.h"
#include <stdint.h>

/* NOTE: This must be kept in sync with Protocol.hs */
#define MAX_PATH 1024

/* NOTE: This must be kept in sync with Protocol.hs */
enum func {
    func_openr     = 0x10000,
    func_openw     = 0x10001,
    func_creat     = 0x10002,   /* TODO: Merge creat into openw? */
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
    func_realpath  = 0x10014,

    /* Send a debug message */
    func_trace     = 0xF0000
};

/* func_openw.flags */
#define FLAG_ALSO_READ 1
#define FLAG_CREATE 2             /* func_open.mode is meaningful iff this flag */
#define FLAG_TRUNCATE 4

/* NOTE: This must be kept in sync with Protocol.hs */
#define MAX_PATH_ENV_VAR_LENGTH (10*1024)
#define MAX_PATH_CONF_STR       (10*1024)
#define MAX_EXEC_FILE           (MAX_PATH)

typedef struct {
    char in_path[MAX_PATH];
} in_path;

enum out_effect {
    OUT_EFFECT_NOTHING,         /* File did not change */
    OUT_EFFECT_CREATED,         /* File was created */
    OUT_EFFECT_DELETED,         /* File was deleted */
    OUT_EFFECT_CHANGED,         /* File or directory was changed, but had already existed */
    OUT_EFFECT_UNKNOWN,         /* File may have changed in some way */
};

typedef struct {
    char out_path[MAX_PATH];
    enum out_effect out_effect;
} out_path;

/* NOTE: This must be kept in sync with Protocol.hs */
struct func_openr     {in_path path;};
struct func_openw     {out_path path; uint32_t flags; uint32_t mode;};
struct func_creat     {out_path path; uint32_t mode;};
struct func_stat      {in_path path;};
struct func_lstat     {in_path path;};
struct func_opendir   {in_path path;};
struct func_access    {in_path path; uint32_t mode;};
struct func_truncate  {out_path path; uint64_t length;};
struct func_unlink    {out_path path; uint32_t flags;};
struct func_rename    {out_path oldpath; out_path newpath;};
struct func_chmod     {out_path path; uint32_t mode;};
struct func_readlink  {in_path path; };
struct func_mknod     {out_path path; uint32_t mode; uint64_t dev;};
struct func_mkdir     {out_path path; uint32_t mode;};
struct func_rmdir     {out_path path;};
struct func_symlink   {in_path target /* TODO: target should probably not even be here */; out_path linkpath;};
struct func_link      {out_path oldpath; out_path newpath;};
struct func_chown     {out_path path; uint32_t owner; uint32_t group;};
struct func_exec      {in_path path;};
struct func_execp     {char file[MAX_EXEC_FILE]; char cwd[MAX_PATH]; char env_var_PATH[MAX_PATH_ENV_VAR_LENGTH]; char conf_str_CS_PATH[MAX_PATH_CONF_STR];};
struct func_realpath  {in_path path;};
struct func_trace     {enum severity severity; char msg[1024];};

#endif
