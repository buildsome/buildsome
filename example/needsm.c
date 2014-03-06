#include <math.h>
#include <stdio.h>

#define M_PI 3.14159265358979323846

int main() 
{
    int num;
    scanf("%d", &num);
    printf("%f\n", sin(M_PI*num));
    return 0;
}
