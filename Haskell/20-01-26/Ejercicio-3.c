#include <stdio.h>
int main() {
    int a, b;
    printf("Ingresa el primer numero: ");
    scanf("%d", &a);
    printf("Ingresa el segundo numero: ");
    scanf("%d", &b);
    printf("Suma de los ultimos digitos: %d\n", a % 10 + b % 10);
}