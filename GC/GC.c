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
void GCInit(uint *sp);
void removePage(Page *page);
void insert(Page *p, Page *list);
void forwardHeapPointers(Page *page);
void GCCollect(uint32_t *sp);
void moveReference(uint32_t **ref);
uint32_t *GCAlloc(uint byte_size, type_info *type_information, uint32_t *bottom_stack);
uint32_t *copy(uint32_t *heapObjPointer, Page *oldPage, object_header *oldHeader);
uint32_t *allocateWordsNoGC(uint objWords, Page *list, colour colour);
bool isAmbiguousRoot(uint32_t *ptr);
void GCInit(uint32_t *sp) {
  allocateHeap();
  top_stack = sp;
  return;
}




void GCCollect(uint32_t *bottom_stack) {
  // Make all pages WHITE
  WHITE_PAGES = BLACK_PAGES;
  BLACK_PAGES = NULL;

  for (uint32_t *ptr = top_stack; top_stack < bottom_stack; ptr++) {
    // TODO: check all heaps
    if (isAmbiguousRoot((uint32_t *) *ptr)) {
      Page *refPage = getPage((uint32_t *) *ptr);
      if (refPage->space == WHITE) {
        removePage(refPage);
        insert(refPage, GREY_PAGES);
        promote(refPage, GREY);
      }
    }
  }

  // Trace all gray pages
  Page *greyPage = GREY_PAGES;
  while (greyPage) {
    GREY_PAGES = greyPage->next;
    forwardHeapPointers(greyPage);
    promote(greyPage, BLACK);
    greyPage->space = BLACK;
    greyPage = greyPage->next;
  }

  Page *whitePage = WHITE_PAGES;
  while (whitePage) {
    whitePage->usedWords = 0;
    whitePage->space = BLACK;

    Page *next = whitePage->next;
    removePage(whitePage);
    insert(whitePage, FREE_PAGES);
    whitePage = next;
  }
  return;
}


// TODO: Do we care about the colour of the page we are allocating?
uint32_t *GCAlloc(uint byte_size, type_info *type_information, uint32_t *bottom_stack) {
  if (FREE_PAGES == NULL && BLACK_PAGES != NULL) {
    GCCollect(bottom_stack);
  }


  // Allocate multiple pages for large objects
  if (byte_size > PAGE_WORDS * WORD_SIZE) {
    return allocateManyPages();
  }

  // Size of the object data
  uint objWords = (byte_size + sizeof(object_header)) / 4;


  // This guarantees space for my object at objPtr position in
  // a BLACK page. The header of the object is empty
  uint32_t *objPtr = allocateWordsNoGC(objWords, BLACK_PAGES, BLACK);

  object_header *header = (object_header *) OBJECT_DATA_START(objPtr);
  header->objWords = objWords;
  header->typeInfo = type_information;
  header->forwardReference = NULL;
  return objPtr;
}

// Return: Address of the object data.
//         Empty header at OBJECT_HEADER_START(address)
uint32_t *allocateWordsNoGC(uint objWords, Page *list, colour colour) {
  Page *page = getValidPage(list, objWords);
  page->space = colour;
  page->usedWords += objWords;
  uint32_t *objHeaderAddress = PAGE_DATA_START(page) + page->usedWords;
  return OBJECT_DATA_START(objHeaderAddress);
}

// Copy object to a new page and return the new address
uint32_t *copy(uint32_t *heapObjPointer, Page *oldPage, object_header *oldHeader) {
  uint32_t *newLoc = allocateWordsNoGC(oldHeader->objWords, GREY_PAGES, GREY);
  object_header *newHeader = OBJECT_HEADER_START(newLoc);
  memcpy((void *) newLoc, (const void*) heapObjPointer, oldHeader->objWords * 4);
  newHeader->typeInfo = oldHeader->typeInfo;
  newHeader->objWords = oldHeader->objWords;
  newHeader->forwardReference = NULL;
  return newLoc;
}

// Find whether a pointer points to one of the heaps
bool isAmbiguousRoot(uint32_t *ptr) {
  Heap *curr = HEAPS;
  while (curr) {
    if (ptr > curr->start && ptr < (curr->start + HEAP_SIZE_WORDS)) {
      return true;
    }
  }
  return false;
}
// CHECK this function
// ref is a pointer to where a pointer to the heap is stored
// Move a live reference that is in a white page to a grey page
void moveReference(uint32_t **ref) {
  uint32_t *heapPointer = *ref;
  Page *pagePointed = getPage(heapPointer);
  if (pagePointed->space == WHITE) {
    object_header *header = OBJECT_HEADER_START(heapPointer);
    if (header->forwardReference == NULL) {
      header->forwardReference = copy(heapPointer, pagePointed, header);
      header->typeInfo = NULL;
      // Same header->size, so the page can still be iterated through
      Page *destinationPage = getPage(header->forwardReference);
      destinationPage->space = GREY;
      removePage(destinationPage);
      insert(destinationPage, GREY_PAGES);
    }
    *ref = header->forwardReference;
  }
}
// Move all the references in a given page to new pages in the
// GREY set
void forwardHeapPointers(Page *page) {
  uint32_t *start = PAGE_DATA_START(page);
  uint32_t *end = ((uint32_t *) page) + PAGE_WORDS;
  object_header *header = (object_header *) start;

  while ((uint32_t *) header < end) {
    if (header->typeInfo != NULL) {
      type_info *typeInfo = header->typeInfo;
      uint32_t *data = OBJECT_DATA_START(header);
      if (typeInfo->isArray) {
        data++; // Skip size field
      }

      for (int i = 0; i < typeInfo->nElem; i++) {
        if (typeInfo->elemIsPtr[i]) {
          moveReference((uint32_t **) (data + i));
        }
      }
    }
    header = (object_header *) ( ((uint32_t *) header) + header->objWords);
  }

}
// Find number of free words in a page
uint getFreeWords(Page *page) {
  if (page == NULL) {
    return 0;
  }
  return PAGE_WORDS - page->usedWords;
}
// Remove a page from the list of pages.
void removePage(Page* page) {
  if (page == NULL) {
    return;
  }

  Page *prev = page->previous;
  Page *next = page->next;

  if (prev != NULL) {
    prev->next = next;
  }
  if (next != NULL) {
    next->previous = prev;
  }
  page->next = NULL;
  page->previous = NULL;
  return;
}

// Insert a page at the front of a list of pages
void insert(Page *p, Page *list) {
  p->next = list;
  list->previous = p;
  list = p;
}

