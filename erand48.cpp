#include<stdio.h>
#include<stdlib.h>

int main() {
    unsigned short Xi[3] = {0, 0, (unsigned short)(42*42*42)};

    FILE *f = fopen("erand48.txt", "w");
    for(int i = 1; i <= 100; ++i) {
        double d = erand48(Xi);
        fprintf(f, "%d: %.5f\n", i, d);
        // cout << i << ":" << d << "\n";
    }
    fclose(f);
}
