#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUMARGS(...) (sizeof((int64_t[]){__VA_ARGS__}) / sizeof(int64_t))
#define HASH(...) (_get_hash(NUMARGS(__VA_ARGS__), __VA_ARGS__))

int64_t _get_hash(int numargs, ...) {
    uint64_t result = 194114084445485833;
    va_list ap;

    va_start(ap, numargs);
    while (numargs--)
        result = result * 31 ^ va_arg(ap, int64_t);
    va_end(ap);

    return result;
}


int64_t input(void) {
    int64_t result = 0;
    if (scanf("%" SCNd64, &result) < 1) exit(-1);
    return result;
}

int64_t print(int64_t x) {
    (void) printf("%" PRId64 "\n", x);
    return 0;
}

int64_t fib(int64_t n);
int64_t iter(int64_t i, int64_t n);
int main(void);

int64_t fib_impl(int64_t n);
int64_t fib(int64_t n) {
    struct CacheEntry {
        int64_t n;
        int64_t _hash;
        int64_t _cached_value;
    };
    static struct CacheEntry _cache[1229];
    int64_t hash = HASH(n);
    if (_cache[hash % 1229]._hash == hash) {
        if ((int) (_cache[hash % 1229].n == n)) {
            return _cache[hash % 1229]._cached_value;
        }
    }
    struct CacheEntry entry;
    entry.n = n;
    entry._hash = hash;
    entry._cached_value = fib_impl(n);
    memcpy(&_cache[hash % 1229], &entry, sizeof(struct CacheEntry));
    return entry._cached_value;
}

int64_t fib_impl(int64_t n) {
    if (n < 2) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

int64_t iter(int64_t i, int64_t n) {
    if (i == n) {
        return 0;
    } else {
        print(fib(i));
        return iter(i + 1, n);
    }
}

int main(void) {
    int64_t _ = iter(0, input());
}
