#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

typedef int64_t i64;

i64 input() {
    i64 result = 0;
    (void) scanf("%" SCNd64, &result);
    return result;
}

i64 print(i64 x) {
    (void) printf("%" PRId64 "\n", x);
    return 0;
}

i64 f(i64 n) {
    return 10 * (n % 10) + n / 10;
}

int main() {
    print(f(input()));
}
