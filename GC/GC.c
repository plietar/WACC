#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define HEAP_SIZE_WORDS 250000 // 1MB
#define PAGE_WORDS 1000
#define WORD_SIZE 4 // Bytes
#define NUMBER_PAGES_TO_ALLOCATE 250

#define PAGE_DATA_START(x) ((uint32_t *) (4 * ((uint32_t) x + sizeof(Page) + 3) / 4))
#define OBJECT_DATA_START(x) ((uint32_t *) (4 * ((uint32_t) x + sizeof(object_header) + 3) / 4))
// CHECK: get header from pointer to the object data
#define OBJECT_HEADER_START(x) ((object_header *) (4 * ((uint32_t) x - sizeof(object_header) + 3) / 4))


// Data Types
typedef unsigned int uint;
typedef enum {BLACK, GREY, WHITE} colour;
//Black pages contain only live objects, and the references contained
//in those objects do not refer to objects in white pages.
//Grey pages contain only live objects, but the references in those
//objects may still refer to objects in white pages.
//White pages contain objects that have not yet been evacuated to a
//non-white page; at the end of collection these pages are garbage and can be reclaimed.

typedef struct Page {
  colour space;
  uint usedWords;
  struct Page *next;
  struct Page *previous;
} Page;

// Assume all objects in the heap variable take 4 bytes.
// NOTE: Compiler must do it this way.
typedef struct type_info {
  bool isArray;
  uint nElem;
  bool elemIsPtr[];
} type_info;

typedef struct object_header {
  uint objWords;
  type_info *typeInfo;
  uint32_t *forwardReference;
} object_header;

typedef struct Heap {
  // All heaps have the same size HEAP_SIZE_WORDS
  uint32_t *start;
  struct Heap *next;
} Heap;

// Variables
uint32_t *top_stack;

// Memory
Page *FREE_PAGES;
Page *BLACK_PAGES;
Page *GREY_PAGES;
Page *WHITE_PAGES;
Heap *HEAPS; // List of heaps
