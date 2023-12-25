#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


struct CacheEntry {
    bool is_full;
    int64_t key;
    int64_t value;
};

int64_t input(void) {
    int64_t result = 0;
    if (scanf("%" SCNd64, &result) < 1) exit(-1);
    return result;
}

int64_t print(int64_t x) {
    (void) printf("%" PRId64 "\n", x);
    return 0;
}

int64_t f_impl(int64_t);
int64_t f(int64_t x) {
    static struct CacheEntry __cache[1229];
    if (__cache[x % 1229].is_full && __cache[x % 1229].key == x) {
        return __cache[x % 1229].value;
    }
    struct CacheEntry value;
    value.is_full = true;
    value.key = x;
    value.value = f_impl(x);
    __cache[x % 1229] = value;
    return value.value;
}

int64_t f_impl(int64_t n) {
    return 10 * (n % 10) + n / 10;
}

int main(void) {
    print(f(input()));
}
