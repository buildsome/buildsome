#ifndef __c_h_
#define __c_h_

#include <bsd/string.h>         /* strlcpy */
#include <stdio.h>              /* fprintf */
#include <stdlib.h>             /* abort */

#define SAFE_STRCPY(dest, src)      strlcpy(dest, src, sizeof (dest))
#define SAFE_STRCAT(dest, src)      strlcat(dest, src, sizeof (dest))

FILE *log_file(void);

#define LOG(fmt, ...)                                                   \
    do {                                                                \
        fprintf(log_file(), __FILE__ ": " fmt "\n", ##__VA_ARGS__);     \
        fflush(log_file());                                             \
    } while(0)

#define DEBUG(fmt, ...)
#define ASSERT(x)  do { if (!(x)) { LOG("ASSERTION FAILED at %s:%d: " #x, __FILE__, __LINE__); abort(); } } while(0)

#define PS(x)   ((char *)& (x)) , sizeof (x)

#endif
