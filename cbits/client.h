#ifndef __BUILDSOME__CBITS__CLIENT_H_
#define __BUILDSOME__CBITS__CLIENT_H_

#include "c.h"
#include <sys/types.h>
#include <stdbool.h>

#define ENVVARS_PREFIX "BUILDSOME_"

bool client__send_hooked(bool is_delayed, const char *buf, size_t size) ATTR_WARN_UNUSED_RESULT;
bool await_go(void) ATTR_WARN_UNUSED_RESULT;

enum need { HOOK, HINT };
int client_make_connection(enum need);

#endif
