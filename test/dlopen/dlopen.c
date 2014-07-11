#include <dlfcn.h>
#include <stdio.h>

int main()
{
    void *handle = dlopen("./lib.so", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle) {
        fprintf(stderr, "dlopen failed: %s", dlerror());
        return -1;
    }
    void (*fn)(void) = dlsym(handle, "func");
    if(!fn) {
        fprintf(stderr, "failed to load symbol 'func': %s", dlerror());
        return -1;
    }
    fn();
    return 0;
}
