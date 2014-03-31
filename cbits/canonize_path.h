#ifndef __canonize_path_h_
#define __canonize_path_h_

struct writer;

void canonize_abs_path(struct writer *dest, const char *src);

#endif
