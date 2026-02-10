#include <stdio.h>
int main() {
    int n;
    printf("Ingresa un numero: ");
    scanf("%d", &n);
    printf("Ultimos dos digitos: %d\n", n % 100);
}