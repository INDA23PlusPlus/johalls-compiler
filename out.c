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

int64_t _dpp_fib_contrived(int64_t _dpp_n);
int64_t _dpp_iter(int64_t _dpp_i, int64_t _dpp_n);
int64_t _dpp_main(void);

int64_t _dpp_fib_contrived(int64_t _dpp_n) {
    if (_dpp_n < 2 && !(!1)) {
        return (_dpp_n & ~~3);
    } else {
        return _dpp_fib_contrived(_dpp_n - 1) + _dpp_fib_contrived(_dpp_n - 2);
    }
}

int64_t _dpp_iter(int64_t _dpp_i, int64_t _dpp_n) {
    if (_dpp_i == _dpp_n) {
        return 0;
    } else {
        if (1) {
            _dpp_print(_dpp_fib_contrived(_dpp_i));
            return _dpp_iter(_dpp_i + 1, _dpp_n);
        }
    }
    return 0;
}

int64_t _dpp_main(void) {
    return _dpp_iter(0, 10);
}

int main(void) { return (int) _dpp_main(); }
