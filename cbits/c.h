#ifndef __c_h_
#define __c_h_

#include "severity.h"
#include <stdio.h>              /* fprintf */
#include <stdlib.h>             /* abort */

#define BUILDSOME_TRACE_ON "BUILDSOME_TRACE_ON"

void _do_log(enum severity, const char *fmt, ...);
void _maybe_do_trace(enum severity, const char *fmt, ...);

#define LOG(sev, fmt, ...) _do_log(severity_##sev, fmt "\n", ##__VA_ARGS__)

// Traces issued if 'BUILDSOME_TRACE_ON' environment variable been set.
#define TRACE(sev, fmt, ...) _maybe_do_trace(sev, fmt "\n", ##__VA_ARGS__)

#define ASSERT(x)  do { if (!(x)) { LOG(error, "ASSERTION FAILED at %s:%d: " #x, __FILE__, __LINE__); abort(); } } while(0)

#define PS(x)   ((char *)& (x)) , sizeof (x)

#define MIN(x, y)  ((x) < (y) ? (x) : (y))

#define ATTR_UNUSED  __attribute__((unused))
#define ATTR_WARN_UNUSED_RESULT __attribute__((warn_unused_result))

#endif
