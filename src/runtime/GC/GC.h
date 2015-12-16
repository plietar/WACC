#include <errno.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <malloc.h>
#include <assert.h>
#include "Page.h"


#ifndef GARBAGE_COLECTOR_H
#define GARBAGE_COLECTOR_H

/*
 * MOSTLY-COPYING GARBAGE COLLECTOR FOR THE WACC LANGUAGE
 *
 * Algorithm inspired by Bartlett's Copying Collector
 *
 * Jaime Rodriguez
 * Paul Lietar
 * Ignacio Navarro
 * Leanne Lyons
 */

//#define NUMBER_PAGES_TO_ALLOCATE 4 // Stress Testing
//#define PAGE_WORDS 32              // Stress Testing
#define PAGE_WORDS 1024              // REAL
#define WORD_SIZE 4

// Memory calculations
#define PAGE_DATA_START(x) ((uint32_t *) (4 * (((uint32_t) x + sizeof(Page) + 3) / 4)))
#define OBJECT_DATA_START(x) ((uint32_t *) (4 * (((uint32_t) x + sizeof(object_header) + 3) / 4)))
#define OBJECT_HEADER_START(x) ((object_header *) (4 * (((uint32_t) x - sizeof(object_header) + 3) / 4)))



// Structs

typedef unsigned int uint;

typedef struct type_info {
  uint8_t isArray;
  uint32_t *typeName;
  uint32_t nElem;
  uint8_t elemIsPtr[];
} __attribute__((packed)) type_info ;

typedef struct object_header {
  uint objWords;
  type_info *typeInfo;
  uint32_t *forwardReference;
} object_header;

typedef struct Heap {
  uint numPages;
  uint32_t *start;
  struct Heap *next;
} Heap;

// Variables
extern uint32_t *top_stack;
extern uint allocated_pages;
extern uint total_pages_handled;
extern uint total_memory_requested;
extern uint total_memory_freed;
extern uint freed_pages;
extern uint GC_Alloc_Calls;
extern uint GC_Last_Calls;
extern uint callsToCopy;
extern uint NUMBER_PAGES_TO_ALLOCATE;

// Memory
extern Page *FREE_PAGES;
extern Page *BLACK_PAGES;
extern Page *GREY_PAGES;
extern Page *WHITE_PAGES;
extern Heap *HEAPS; // List of heaps



// Init
void GCInit(void) __attribute__ ( ( naked ) );
void GCInternalInit(uint *sp);
// Alloc
void GCAlloc(void) __attribute__ ( ( naked ) );
uint32_t *GCInternalAlloc(uint byte_size, type_info *type_information, uint32_t *bottom_stack);
uint32_t *allocateWordsNoGC(uint objWords, Page **list, colour colour);
void allocateHeap(void);
uint *allocateManyPages();  // Unimplemented
// Free
void GCFree();
// Collect
void GCCollect(uint32_t *sp);
void forwardHeapPointers(Page *page);
uint32_t *moveReference(uint32_t *ref);
bool isAmbiguousRoot(uint32_t *ptr);
uint32_t *copy(uint32_t *heapObjPointer, Page *oldPage, object_header *oldHeader);

#endif
