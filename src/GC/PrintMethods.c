#include "PrintMethods.h"


void printMemoryUsage() {
  printf("-------------------------Memory Usage Summary-----------------------\n");
  printf("Total Memory from OS: \t%d bytes\n", total_pages_handled * PAGE_WORDS * 4);
  printf("Total Memory requested: %d bytes\n", total_memory_requested);
  printf("Total Memory freed: \t%d bytes\n", total_memory_freed);
  printf("Total pages handled: \t%d\n", total_pages_handled);
  printf("Free pages: \t\t%d\n", total_pages_handled - allocated_pages);
  printf("Freed pages: \t\t%d\n", freed_pages);
  printf("Page lists: \n");
  printf("Calls to copy: %d\n", callsToCopy);
  printf("Calls to alloc: %d\n", GC_Alloc_Calls);
}


void printPageLists() {
  printf("Page status\n");
  Page *p = BLACK_PAGES;
  printf("Black pages:\n");
  while (p) {
    printf("Page %#010x of colour %d should be 0\n", p, p->space);
    p = p->next;
  }

  p = GREY_PAGES;
  printf("Grey pages:\n");
  while (p) {
    printf("Page %#010x of colour %d should be 1\n", p, p->space);
    p = p->next;
  }

  p = WHITE_PAGES;
  printf("White pages:\n");
  while (p) {
    printf("Page %#010x of colour %d should be 2\n", p, p->space);
    p = p->next;
  }
}

void printHeader(object_header *header) {
  if (header != NULL) {
    printf("Header at %#010x\n", header); 
    printf("Object at %#010x\n", OBJECT_DATA_START(header));
    printf("\tobjWords: %d\n", header->objWords);
    printf("\tforwardRef: %#010x\n", header->forwardReference);
    printTypeInfo(header->typeInfo);
  } else {
    printf("Null header\n");
  }
}

void printTypeInfo(type_info *type) {
  if (type != NULL) {
    printf("TypeInfo\n"); 
    printf("Type: %s\n", type->typeName);
    printf("\tisArray: %d\n", type->isArray); 
    printf("\tnElem: %d\n", type->nElem); 
    for (int i = 0; i < type->nElem; i++) {
      printf("\telemIsPtr[%d] = %d\n", i, type->elemIsPtr[i]);
    }
  } else {
    printf("Null typeInfo\n");
  }
}
