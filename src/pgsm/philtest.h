#ifndef _PHILTEST_H_
#define _PHILTEST_H_

#include <stdio.h>
#define SUCCESS 0
#define FAILURE 1
// Cross mark ✗ : HEX 0xe29c97
// Checkmark ✓  : HEX 0xe29c93
// I could put the unicode char itself in the string but for some
// reason this feels more portable.
#define FAIL printf("\033[31m\xe2\x9c\x97\033[1;37m %s()\033[0m \n%s:%d: fail: \n", __func__, __FILE__, __LINE__)
#define FAIL_PRINT(x) FAIL;putchar('\t');printf((x));putchar('\n')
#define SUCCESS_PRINT printf("\033[32m\xe2\x9c\x93\033[1;37m %s()\033[0m\n", __func__)

#endif
