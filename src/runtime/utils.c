#include "wacc.h"
#include <stdlib.h>
#include <stdio.h>
wacc_string *wacc_int_to_string(int x) {
  int temp = x;
  int length = 0;
  while (temp > 0) {
    length++;
    temp /= 10;
  }
  wacc_string *string = malloc(sizeof(wacc_string) + length);
  string->length = length;
  sprintf(string->data, "%d", x);
  return string;
}
