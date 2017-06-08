#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "shared.h"
#include "sha1.h"

// #define FSHOOK_SHARED_DEBUG

#ifdef SHARED_TEST
#define FSHOOK_SHARED_DEBUG
#endif

#ifdef FSHOOK_SHARED_DEBUG
#define LOG(fmt, params...) do { \
    fprintf(stderr, "fshooks-shared<pid=%d>: " \
            fmt "\n", getpid(), ##params); \
    fflush(stderr); \
} while (0)
#else
#define LOG(fmt, params...) do { if (0) { printf(fmt "\n", ##params); } } while (0)
#endif

#define KEY_DATA_SIZE                 20
#define MAX_DISTANCE_FOR_RESIZE       0x10
#define INITIAL_TABLE_ORDER           9

typedef struct _key_hash {
    union {
        unsigned char data[KEY_DATA_SIZE];
        struct {
            uint64_t a;
            uint64_t b;
            uint32_t c;
        };
    };
    uint32_t blob_offset;
} key_hash;

typedef struct _shmem_header {
    size_t table_start;
    size_t table_order;
    size_t table_end;
    size_t items_used;
    size_t next_blob_offset;
    size_t mappable_size;
} shmem_header;

typedef struct _shmem_context {
    int fd;
    int ro_fd;
    size_t size;
    size_t file_size;
    size_t mapped_size;
    pthread_mutex_t mutex;
    void *root;
} shmem_context;

static void string_to_key_hash(const char *str, size_t len, key_hash *out)
{
    sha1(out->data, (unsigned char *)str, len);
}

static bool key_hash_is_empty(const key_hash *existing_key)
{
    return (existing_key->a == 0  &&
            existing_key->b == 0  &&
            existing_key->c == 0);
}

static uint64_t key_hash_to_full_idx(const key_hash *key)
{
    return key->a ^ key->b ^ key->c;
}

static void shmem_remap(shmem_context *shmem_ctx)
{
    const size_t mapped_size = (shmem_ctx->size + 0xfff) & ~0xfff;
    const bool is_readonly = shmem_ctx->fd == -1;

    if (shmem_ctx->mapped_size != mapped_size) {
        LOG("remap to size 0x%lx", mapped_size);

        if (shmem_ctx->mapped_size) {
            munmap(shmem_ctx->root, shmem_ctx->mapped_size);
        }

        const int fd = is_readonly ? shmem_ctx->ro_fd : shmem_ctx->fd;
        shmem_ctx->root = mmap(NULL, mapped_size,
                               PROT_READ | (is_readonly ? 0 : PROT_WRITE),
                               MAP_SHARED,
                               fd,
                               0);
        if (shmem_ctx->root == MAP_FAILED) {
            if (errno == EBADF) {
                /*
                 * The process may have did a for-loop to
                 * close all FDs on shutdown.
                 */
                return;
            }
        }

        shmem_ctx->mapped_size = mapped_size;

        if (!is_readonly) {
            shmem_header *header = ((shmem_header *)shmem_ctx->root);
            header->mappable_size = mapped_size;
        }

        assert(shmem_ctx->root != MAP_FAILED);
    }
}

static void shmem_resize(shmem_context *shmem_ctx)
{
    if (shmem_ctx->file_size != shmem_ctx->size) {
        LOG("resize to 0x%lx (from 0x%lx)", shmem_ctx->size,
            shmem_ctx->file_size);

        int ret = ftruncate(shmem_ctx->fd, shmem_ctx->size);
        assert(ret >= 0);

        shmem_ctx->file_size = shmem_ctx->size;
    }

    shmem_remap(shmem_ctx);
}

void shmem_dump(const shmem_context *shmem_ctx)
{
    const shmem_header *header = ((shmem_header *)shmem_ctx->root);

    if (!header) {
        LOG("no header at this point");
        return;
    }

    LOG("shmem header (0x%lx, 0x%lx, 0x%lx, 0x%lx)", header->table_start,
        header->table_order, header->table_end, header->next_blob_offset);
}

size_t shmem_add_string(shmem_context *shmem_ctx, const char *str, size_t len)
{
    shmem_header *header = ((shmem_header *)shmem_ctx->root);
    size_t offset = header->next_blob_offset;
    int n = (len + 1 + 3) & ~3;

    if (n + offset > shmem_ctx->size) {
        shmem_ctx->size = n + offset + 0x10000;
        shmem_resize(shmem_ctx);

        header = ((shmem_header *)shmem_ctx->root);
    }

    header->next_blob_offset = n + offset;

    char *buf = shmem_ctx->root + offset;
    memcpy(buf, str, len);
    buf[n] = '\0';

    LOG("added string \"%s\" at offset 0x%lx", str, offset);

    return offset;
}

static key_hash *shmem_get_item_by_idx(shmem_context *shmem_ctx, uint64_t offset, shmem_header *header)
{
    const uint64_t trunc_offset = offset & ((1ULL << header->table_order) - 1ULL);
    key_hash *const key_table = (key_hash *)(header->table_start + shmem_ctx->root);
    key_hash *const key_item = &key_table[trunc_offset];

    if ((char *)&key_item[1] > (char *)(shmem_ctx->root + shmem_ctx->mapped_size)) {
        return NULL;
    }

    return key_item;
}

static void shmem_init_table_end(shmem_context *shmem_ctx, shmem_header *header)
{
    header->table_end = header->table_start + sizeof(key_hash) * (1ULL << header->table_order);
    header->next_blob_offset = header->table_end;

    if (shmem_ctx->size < header->next_blob_offset) {
        shmem_ctx->size = header->next_blob_offset;
    }
}

static void shmem_double_capacity(shmem_context *shmem_ctx)
{
    shmem_header *header = ((shmem_header *)shmem_ctx->root);
    shmem_header new_header = *header;
    const uint64_t orig_table_size = 1ULL << header->table_order;

    new_header.table_order += 1;
    new_header.table_start = (header->next_blob_offset + 0x3fUL) & ~0x3fUL;

    const uint64_t max_tries = 1ULL << new_header.table_order;

    shmem_init_table_end(shmem_ctx, &new_header);
    shmem_resize(shmem_ctx);

    header = ((shmem_header *)shmem_ctx->root);

    uint64_t counter = 0;

    for (uint64_t idx = 0; idx < orig_table_size; idx++) {
        key_hash *const existing_key = shmem_get_item_by_idx(shmem_ctx, idx, header);

        if (key_hash_is_empty(existing_key)) {
            continue;
        }

        const uint64_t full_idx = key_hash_to_full_idx(existing_key);

        counter++;
        for (uint64_t try = 0; try < max_tries; try++) {
            key_hash *const migrated_key =
                shmem_get_item_by_idx(shmem_ctx, try + full_idx, &new_header);

            assert(memcmp(&migrated_key->data,
                          &existing_key->data, KEY_DATA_SIZE) != 0);

            if (!key_hash_is_empty(migrated_key)) {
                counter++;
                continue;
            }

            *migrated_key = *existing_key;
            break;
        }
    }

    LOG("total items migrate: %ld, total iterations: %ld",
        header->items_used, counter);

    *header = new_header;
}

key_hash *shmem_get_item_non_deterministic(shmem_context *shmem_ctx,
                                           const char *str)
{
    if (shmem_ctx->root == MAP_FAILED) {
        return NULL;
    }

    key_hash new_key;

    string_to_key_hash(str, strlen(str), &new_key);
    const uint64_t full_idx = key_hash_to_full_idx(&new_key);

    shmem_header header = *((shmem_header *)shmem_ctx->root);
    if (header.mappable_size != shmem_ctx->size) {
        shmem_ctx->size = header.mappable_size;
        shmem_remap(shmem_ctx);

        if (shmem_ctx->root == MAP_FAILED) {
            return NULL;
        }
    }

    uint64_t max_tries = 0x20; /* failures are okay */

    LOG("get_item_nondeter %p for '%s'", shmem_ctx, str);

    for (uint64_t try = 0; try < max_tries; try++) {
        key_hash *const existing_key =
            shmem_get_item_by_idx(shmem_ctx, try + full_idx, &header);

        if (!existing_key) {
            return NULL;
        }

        if (!memcmp(&existing_key->data, new_key.data, KEY_DATA_SIZE)) {
            LOG("found at %p", existing_key);

            return existing_key;
        }
    }

    return NULL;
}

key_hash *shmem_add_item_bs(shmem_context *shmem_ctx,
                            const char *str, size_t len)
{
    key_hash new_key;

    string_to_key_hash(str, len, &new_key);
    const uint64_t full_idx = key_hash_to_full_idx(&new_key);

    pthread_mutex_lock(&shmem_ctx->mutex);

    uint64_t max_tries;

    do {
        shmem_header *header = ((shmem_header *)shmem_ctx->root);
        max_tries = 1ULL << header->table_order;
        if (header->items_used >= max_tries / 2) {
            shmem_double_capacity(shmem_ctx);
            continue;
        }
        break;
    } while (1);

    shmem_header *header = ((shmem_header *)shmem_ctx->root);

    for (uint64_t try = 0; try < max_tries; try++) {
        key_hash *const existing_key = shmem_get_item_by_idx(shmem_ctx, try + full_idx, header);

        if (!memcmp(&existing_key->data, new_key.data, KEY_DATA_SIZE)) {
            pthread_mutex_unlock(&shmem_ctx->mutex);

            return existing_key;
        }

        if (key_hash_is_empty(existing_key)) {
            uint64_t existing_key_offset = (size_t)existing_key - (size_t)shmem_ctx->root;
            const size_t blob_offset = shmem_add_string(shmem_ctx, str, len);

            key_hash *const added_key = existing_key_offset + shmem_ctx->root;
            memcpy(&added_key->data, new_key.data, KEY_DATA_SIZE);
            added_key->blob_offset = blob_offset;

            header = ((shmem_header *)shmem_ctx->root);
            header->items_used += 1;

            LOG("added key offset 0x%lx", existing_key_offset);

            pthread_mutex_unlock(&shmem_ctx->mutex);

            return added_key;
        }
    }

    assert(0);
}

key_hash *shmem_add_item(shmem_context *shmem_ctx, const char *str)
{
    return shmem_add_item_bs(shmem_ctx, str, strlen(str));
}

shmem_context *new_shmem(void)
{
    char filename[0x100];

    snprintf(filename, sizeof(filename),
             "buildsome.main-shmem.%d", getpid());

    const int fd = shm_open(filename, O_CREAT | O_RDWR | O_CLOEXEC, 0664);
    assert(fd != -1);

    const int ro_fd = shm_open(filename, O_RDONLY | O_CLOEXEC, 0444);
    assert(fd != -1);

    shm_unlink(filename);

    shmem_context *shmem_ctx;
    shmem_ctx = calloc(sizeof(*shmem_ctx), 1);
    assert(shmem_ctx != NULL);
    shmem_ctx->fd = fd;
    shmem_ctx->ro_fd = ro_fd;
    shmem_ctx->size = sizeof(shmem_header);

    shmem_resize(shmem_ctx);

    shmem_header *header = ((shmem_header *)shmem_ctx->root);
    header->table_order = INITIAL_TABLE_ORDER;
    header->table_start = sizeof(*header);
    shmem_init_table_end(shmem_ctx, header);

    shmem_resize(shmem_ctx);

    return shmem_ctx;
}

void shmem_send_fd(shmem_context *shmem_ctx, int target_fd)
{
    struct msghdr hdr;
    struct iovec data;

    char cmsgbuf[CMSG_SPACE(sizeof(int))];
    char dummy = '*';
    data.iov_base = &dummy;
    data.iov_len = sizeof(dummy);

    memset(&hdr, 0, sizeof(hdr));
    hdr.msg_name = NULL;
    hdr.msg_namelen = 0;
    hdr.msg_iov = &data;
    hdr.msg_iovlen = 1;
    hdr.msg_flags = 0;

    hdr.msg_control = cmsgbuf;
    hdr.msg_controllen = CMSG_LEN(sizeof(int));

    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&hdr);
    cmsg->cmsg_len = CMSG_LEN(sizeof(int));
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type = SCM_RIGHTS;

    memcpy(CMSG_DATA(cmsg), &shmem_ctx->ro_fd, sizeof(int));

    const int n = sendmsg(target_fd, &hdr, 0);

    if (n < 0) {
        if (errno == EPIPE) {
            return;
        }
    }

    assert(n >= 0);
}

shmem_context *recv_readonly_shmem(int source_fd)
{
    char dummy;

    struct iovec data;
    data.iov_base = &dummy;
    data.iov_len = sizeof(dummy);

    char cmsgbuf[CMSG_SPACE(sizeof(int))];
    memset(cmsgbuf, 0, CMSG_SPACE(sizeof(int)));

    struct msghdr hdr;
    memset(&hdr, 0, sizeof(struct msghdr));
    hdr.msg_name = NULL;
    hdr.msg_namelen = 0;
    hdr.msg_control = cmsgbuf;
    hdr.msg_controllen = CMSG_SPACE(sizeof(int));
    hdr.msg_iov = &data;
    hdr.msg_iovlen = 1;

    int ret = recvmsg(source_fd, &hdr, 0);
    assert(ret >= 0);

    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&hdr);

    int ro_fd;
    memcpy(&ro_fd, CMSG_DATA(cmsg), sizeof(int));

    ret = fcntl(ro_fd, F_SETFD, FD_CLOEXEC);
    assert(ret >= 0);

    return new_readonly_shmem(ro_fd);
}

shmem_context *new_readonly_shmem(int ro_fd)
{
    shmem_context *shmem_ctx;
    shmem_ctx = calloc(sizeof(*shmem_ctx), 1);
    assert(shmem_ctx != NULL);
    shmem_ctx->fd = -1;
    shmem_ctx->ro_fd = ro_fd;

    pthread_mutex_init(&shmem_ctx->mutex, NULL);

    struct stat st;
    int ret = fstat(shmem_ctx->ro_fd, &st);
    assert(ret == 0);

    shmem_ctx->size = st.st_size;

    shmem_remap(shmem_ctx);

    return shmem_ctx;
}
