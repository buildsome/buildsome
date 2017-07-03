#ifndef __CBITS_SHARED__H__
#define __CBITS_SHARED__H__

#include <stddef.h>

typedef struct _shmem_context shmem_context;
typedef struct _key_hash key_hash;

shmem_context *new_shmem(void);

void shmem_send_fd(shmem_context *shmem_ctx, int target_fd);

key_hash *shmem_add_item(shmem_context *shmem_ctx,
                        const char *str);

key_hash *shmem_add_item_bs(shmem_context *shmem_ctx,
                           const char *str,
                           size_t len);

key_hash *shmem_get_item_non_deterministic(shmem_context *shmem_ctx,
                                          const char *str);

shmem_context *new_readonly_shmem(int ro_fd);

shmem_context *recv_readonly_shmem(int source_fd);

#endif
