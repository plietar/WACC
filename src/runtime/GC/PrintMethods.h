#include <stdio.h>
#include "GC.h"


#ifndef PRINT_METHODS_H
#define PRINT_METHODS_H

void printHeader(object_header *header);
void printTypeInfo(type_info *type);
void printPageLists();
void printMemoryUsage();

#endif
