#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <stdio.h>

static void print_envs()
{
    extern char **environ;
    unsigned i;
    for(i = 0; environ[i]; i++) {
        printf("env=%s\n", environ[i]);
    }
}

int main()
{
    print_envs();

    struct stat buf;
    stat("foo", &buf);
    int rc = lstat("foo", &buf);
    printf("stat \"foo\" = %d\n", rc);
    return rc;
}
