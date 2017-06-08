/* Unit tests for shared.c */

#define TEST_SHARED

#include "shared.c"

int main(int argc, char *argv[])
{
    shmem_context *context = new_shmem();

    shmem_dump(context);
    for (int i = 0; i < 10; i++) {
        char buf[0x100];
        snprintf(buf, sizeof(buf), "BLA qwe qwe qweqweqw eqwe qwqweqweqee qwe%d", i);
        shmem_add_item(context, buf);
    }

    /* TODO: do more stuff here */
    
    return 0;
}
