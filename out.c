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

int64_t _dpp_ack(int64_t _dpp_m, int64_t _dpp_n);
int main(void);

int64_t _dpp_ack_impl(int64_t _dpp_m, int64_t _dpp_n);
int64_t _dpp_ack(int64_t _dpp_m, int64_t _dpp_n) {
    struct CacheEntry {
        int64_t m;
        int64_t n;
        uint64_t _hash;
        int64_t _cached_value;
    };
    static struct CacheEntry _cache[CACHE_SIZE];
    uint64_t hash = HASH(_dpp_m, _dpp_n);
    if (_cache[hash % CACHE_SIZE]._hash == hash) {
        if ((int) (_cache[hash % CACHE_SIZE].m == _dpp_m) & (int) (_cache[hash % CACHE_SIZE].n == _dpp_n)) {
            return _cache[hash % CACHE_SIZE]._cached_value;
        }
    }
    struct CacheEntry entry;
    entry.m = _dpp_m;
    entry.n = _dpp_n;
    entry._hash = hash;
    entry._cached_value = _dpp_ack_impl(_dpp_m, _dpp_n);
    memcpy(&_cache[hash % CACHE_SIZE], &entry, sizeof(struct CacheEntry));
    return entry._cached_value;
}

int64_t _dpp_ack_impl(int64_t _dpp_m, int64_t _dpp_n) {
    if (_dpp_m == 0) {
        return _dpp_n + 1;
    }
    if (_dpp_n == 0) {
        return _dpp_ack(_dpp_m - 1, 1);
    }
    return _dpp_ack(_dpp_m - 1, _dpp_ack(_dpp_m, _dpp_n - 1));
}

int main(void) {
    _dpp_print(_dpp_ack(_dpp_input(), _dpp_input()));
}
