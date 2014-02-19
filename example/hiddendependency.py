import os

def generate(name):
    path = os.path.join(os.path.dirname(__file__), name)
    open(path, "wb").write(r"""
#include <stdio.h>

void auto_generated_function(void)
{
    printf("Hello from Python-generated code!\n");
}
""")
