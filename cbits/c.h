#ifndef __c_h_
#define __c_h_

#include <stdio.h>              /* fprintf */
#include <stdlib.h>             /* abort */

FILE *log_file(void);

#define LOG(fmt, ...)                                                   \
    do {                                                                \
        fprintf(log_file(), __FILE__ ": " fmt "\n", ##__VA_ARGS__);     \
        fflush(log_file());                                             \
    } while(0)

#define ASSERT(x)  do { if (!(x)) { LOG("ASSERTION FAILED at %s:%d: " #x, __FILE__, __LINE__); abort(); } } while(0)

#define PS(x)   ((char *)& (x)) , sizeof (x)

#define MIN(x, y)  ((x) < (y) ? (x) : (y))

#define ATTR_UNUSED  __attribute__((unused))
#define ATTR_WARN_UNUSED_RESULT __attribute__((warn_unused_result))

#endif
