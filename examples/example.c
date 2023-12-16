#include <stdlib.h>
#include <stdio.h>

 int main() {
int f;
int i;
int n;
int p;
p = 1;
i = 1;
f = 10;
n = f + 1;
while ((i < n)) {
p = p * i;
i = i + 1;
}

printf("p = %d\n", p);

return 0;
}
