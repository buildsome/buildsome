#include <stdio.h>
#include "a.h"
#include "out/auto.h"

int main() {
    int unused;
    printf("0x%X\n", f());
    auto_generated_function();
    return 0;
}
