#include <stdio.h>
int main() {
    int n;
    printf("Ingresa un numero: ");
    scanf("%d", &n);
    printf("Mitad del ultimo digito: %.1f\n", n % 10 / 2.0);
}
