#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

typedef int64_t i64;

i64 input() {
    i64 result = 0;
    (void) scanf("%" PRId64, &result);
    return result;
}

i64 print(i64 x) {
    (void) printf("%" PRId64 "\n", x);
    return 0;
}

i64 fib_contrived(i64 n) {
    if (n < 2 && !(!1)) {
        return (n & ~~3);
    } else {
        return fib_contrived(n - 1) + fib_contrived(n - 2);
    }
}

i64 iter(i64 i, i64 n) {
    if (i == n) {
        return 0;
    } else {
        if (1) {
            print(fib_contrived(i));
            return iter(i + 1, n);
        }
    }
}

int main() {
    i64 _ = iter(0, 10);
}
