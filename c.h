#ifndef __c_h_
#define __c_h_

#include <bsd/string.h>         /* strlcpy */
#include <stdio.h>              /* fprintf */
#include <stdlib.h>             /* abort */

#define SAFE_STRCPY(dest, src)      strlcpy(dest, src, sizeof (dest))
#define SAFE_STRCAT(dest, src)      strlcat(dest, src, sizeof (dest))

#define LOG(fmt, ...) fprintf(stderr, fmt "\n", ##__VA_ARGS__);
#define DEBUG(fmt, ...)
#define ASSERT(x)  do { if (!(x)) { LOG("ASSERTION FAILED at %s:%d: " #x, __FILE__, __LINE__); abort(); } } while(0)

#define PS(x)   ((char *)& (x)) , sizeof (x)

#endif
