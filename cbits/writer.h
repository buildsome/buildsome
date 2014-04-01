#ifndef __writer_h_
#define __writer_h_

#include "c.h"                  /* ASSERT */
#include <string.h>             /* memcpy */

/* Can pass/copy by value */
struct writer {
    char *buf;
    size_t bytes_left;
};

static inline char *writer_append(struct writer *w, size_t len)
{
    ASSERT(len <= w->bytes_left);
    char *res = w->buf;
    w->buf += len;
    w->bytes_left -= len;
    return res;
}

static inline void writer_append_data(struct writer *w, const char *buf, size_t len)
{
    char *dest = writer_append(w, len);
    memcpy(dest, buf, len);
}

static inline void writer_append_str(struct writer *w, const char *str)
{
    writer_append_data(w, str, strlen(str)+1);
}

#endif
