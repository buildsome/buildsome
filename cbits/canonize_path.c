#include "canonize_path.h"
#include "c.h"
#include "writer.h"

#include <stdbool.h>

/*
 * src -> dest
 * progress on both, copying path components.
 * recurse to progress, return to revert the dest state
 */
enum copy_components_result {
    BACK_TO_PARENT,
    DONE,
};

#ifdef __APPLE__
const char *strchrnul(const char *s, int c)
{
    const char *r = strchr(s, c);
    return r ? r : s + strlen(s);
}
#endif

static enum copy_components_result copy_components(struct writer *dest, const char **src, bool prepend_slash_to_dest)
{
    if(!**src) {
        *writer_append(dest, 1) = 0;
        return DONE;
    }

    /* Eat next src component: */
    const char *start = *src;
    *src = strchrnul(*src, '/');
    unsigned length = *src - start;
    if(**src == '/') ++*src;

#define COMPONENT_MATCH(literal_str)                                    \
    (sizeof (literal_str) - 1 == length && 0 == strncmp((literal_str), start, length))

    if(COMPONENT_MATCH("") || COMPONENT_MATCH(".")) {
        /* Ignore empties (e.g: foo//bar should be canonized as foo/bar) and "." (e.g: foo/./bar)*/
        return copy_components(dest, src, prepend_slash_to_dest);
    }
    if(COMPONENT_MATCH("..")) {
        return BACK_TO_PARENT;
    }

    struct writer dest_bookmark = *dest;
    char *dest_component = writer_append(dest, length + (prepend_slash_to_dest ? 1 : 0));
    switch(copy_components(dest, src, true)) {
    case BACK_TO_PARENT:
        /* Keep src progress, but go back to dest bookmark */
        *dest = dest_bookmark;
        return copy_components(dest, src, prepend_slash_to_dest);
    case DONE:
        if(prepend_slash_to_dest) {
            *dest_component = '/';
            dest_component++;
        }
        memcpy(dest_component, start, length);
        return DONE;
    }

    ASSERT(0);
#undef COMPONENT_MATCH
}

void canonize_abs_path(struct writer *dest, const char *src)
{
    ASSERT(*src == '/');        /* Don't need to support relative paths */
    /* Copy absolute marker */
    *writer_append(dest, 1) = '/';
    src++;

    /* Ignore escapes out of the root */
    copy_components(dest, &src, false);
}

#ifdef TEST

int main() 
{
    char abuf[1024];
    struct writer a = { PS(abuf) };
    canonize_abs_path(&a, "/a/../b/c/../../d/");
    ASSERT(!strcmp(abuf, "/d"));

    char bbuf[1024];
    struct writer b = { PS(bbuf) };
    canonize_abs_path(&b, "/a/./b///c/./../d");
    ASSERT(!strcmp(bbuf, "/a/b/d"));

    printf("Success!\n");

    return 0;
}
#endif
