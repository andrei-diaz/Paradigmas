#include <stdio.h>
#include <unistd.h>

int main() {
    for (int i = 0; i < 100; i++) {
        printf("iloveyou\n");
        fflush(stdout);
        sleep(1);
    }
    return 0;
}
