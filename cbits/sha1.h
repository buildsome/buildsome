#ifndef __SHA1_H__
#define __SHA1_H__

#include <stdint.h>
#include <stddef.h>

void sha1(void *outbuf, const void *inbuf, size_t length);

#endif

