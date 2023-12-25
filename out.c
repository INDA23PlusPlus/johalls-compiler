#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUMARGS(...) (sizeof((int64_t[]){__VA_ARGS__}) / sizeof(int64_t))
#define HASH(...) (_get_hash(NUMARGS(__VA_ARGS__), __VA_ARGS__))

#define CACHE_SIZE 65537

uint64_t _get_hash(int numargs, ...) {
    uint64_t result = 194114084445485833;
    va_list ap;

    va_start(ap, numargs);
    while (numargs--)
        result = result * 31 ^ (uint64_t) va_arg(ap, int64_t);
    va_end(ap);
    if (result == 0) return 1; // could cause bug with initialization of cache if hash was 0
    return result;
}

int64_t _dpp_input(void) {
    int64_t result = 0;
    if (scanf("%" SCNd64, &result) < 1) exit(-1);
    return result;
}

int64_t _dpp_print(int64_t x) {
    (void) printf("%" PRId64 "\n", x);
    return 0;
}

int64_t _dpp_f(int64_t _dpp_n);
int64_t _dpp_main(void);

int64_t _dpp_f_impl(int64_t _dpp_n);
int64_t _dpp_f(int64_t _dpp_n) {
    struct CacheEntry {
        int64_t n;
        uint64_t _hash;
        int64_t _cached_value;
    };
    static struct CacheEntry _cache[CACHE_SIZE];
    uint64_t hash = HASH(_dpp_n);
    if (_cache[hash % CACHE_SIZE]._hash == hash) {
        if ((int) (_cache[hash % CACHE_SIZE].n == _dpp_n)) {
            return _cache[hash % CACHE_SIZE]._cached_value;
        }
    }
    struct CacheEntry entry;
    entry.n = _dpp_n;
    entry._hash = hash;
    entry._cached_value = _dpp_f_impl(_dpp_n);
    memcpy(&_cache[hash % CACHE_SIZE], &entry, sizeof(struct CacheEntry));
    return entry._cached_value;
}

int64_t _dpp_f_impl(int64_t _dpp_n) {
    return 10 * (_dpp_n % 10) + _dpp_n / 10;
}

int64_t _dpp_main(void) {
    int64_t _dpp_n = _dpp_input();
    _dpp_print(_dpp_f(_dpp_n));
    _dpp_print(_dpp_f(_dpp_f(_dpp_n)));
    _dpp_print(_dpp_f(_dpp_f(_dpp_f(_dpp_n))));
    _dpp_print(_dpp_f(_dpp_f(_dpp_f(_dpp_f(_dpp_n)))));
    return 0;
}

int main(void) { return (int) _dpp_main(); }
