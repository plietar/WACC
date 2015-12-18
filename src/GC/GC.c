#include "GC.h"
#include "Page.h"
#include "PrintMethods.h"

// Global Variables
uint32_t *top_stack = NULL;
uint allocated_pages = 0;
uint total_pages_handled = 0;
uint total_memory_requested = 0; // Without object headers
uint total_memory_allocated = 0; // Includes object headers
uint total_memory_freed = 0;
uint freed_pages = 0;
uint GC_Alloc_Calls = 0;
uint GC_Last_Calls = 0;
uint callsToCopy = 0;
uint NUMBER_PAGES_TO_ALLOCATE = 256;

// Memory
Page *FREE_PAGES = NULL;
Page *BLACK_PAGES = NULL;
Page *GREY_PAGES = NULL;
Page *WHITE_PAGES = NULL;
Heap *HEAPS = NULL; // List of heaps


void GCInternalInit(uint32_t *sp) {
  top_stack = sp;
  allocateHeap();
}


void GCAlloc() {
  __asm__ ( "push {r4-r12,lr};"
            "mov r2, sp;"
            "bl GCInternalAlloc;"
            "add sp, sp, #36;"
            "pop {pc};"
            );
}

void GCInit() {
   __asm__ ( "push {lr};"
             "mov r0, sp;"
             "bl GCInternalInit;"
             "pop {pc};" );
}

void GCFree() {
  GCCollect(top_stack);
  printPageLists();
  printMemoryUsage();
  Heap *heap = HEAPS;
  while (heap) {
    Heap *tmp = heap->next;
    free((void *) heap);
    heap = tmp;
  }
}


void GCCollect(uint32_t *bottom_stack) {

  // Make all pages WHITE
  Page *toWhite = BLACK_PAGES;
  while (toWhite) {
    promote(toWhite, WHITE);
    toWhite = toWhite->next;
  }

  WHITE_PAGES = BLACK_PAGES;
  BLACK_PAGES = NULL;

  for (uint32_t *ptr = top_stack; ptr >= bottom_stack; ptr--) {
    if (isAmbiguousRoot((uint32_t *) *ptr)) {
      Page *refPage = getPage((uint32_t *) *ptr);
      if (refPage->space == WHITE) {
        removePage(refPage);
        insert(refPage, &GREY_PAGES);
        promote(refPage, GREY);
      }
    }
  }

  // Trace all gray pages
  Page *greyPage = GREY_PAGES;
  while (greyPage) {
    removePage(greyPage);
    forwardHeapPointers(greyPage);
    promote(greyPage, BLACK);
    insert(greyPage, &BLACK_PAGES);
    greyPage = GREY_PAGES;
  }

  Page *whitePage = WHITE_PAGES;
  while (whitePage) {
    removePage(whitePage);
    total_memory_freed += whitePage->usedWords * 4;
    whitePage->usedWords = 0;
    promote(whitePage, BLACK);
    insert(whitePage, &FREE_PAGES);
    whitePage = WHITE_PAGES;
    freed_pages++;
    allocated_pages--;
  }
  if (allocated_pages >= 1 * total_pages_handled / 2) {
    allocateHeap();
  }
  return;
}


uint32_t *GCInternalAlloc(uint byte_size, type_info *type_information, uint32_t *bottom_stack) {
  total_memory_requested += byte_size;
  total_memory_allocated += byte_size + sizeof(object_header) * 4;
  GC_Alloc_Calls++;
  // Debug purposes
  /*
   if (GC_Alloc_Calls == GC_Last_Calls + 100000) {
    GC_Last_Calls = GC_Alloc_Calls;
    printMemoryUsage();
    printf("Number of pages to allocate next time: %d\n", NUMBER_PAGES_TO_ALLOCATE);
  }
  */
  if (allocated_pages >= 5 * total_pages_handled / 6) {
    GCCollect(bottom_stack);
  }

  // Allocate multiple pages for large objects
  if (byte_size > PAGE_WORDS * WORD_SIZE) {
    return allocateManyPages();
  }

  // Size of the object data
  uint objWords = (byte_size + sizeof(object_header) + 3) / 4;


  // This guarantees space for my object at objPtr position in
  // a BLACK page. The header of the object is empty
  uint32_t *objPtr = allocateWordsNoGC(objWords, &BLACK_PAGES, BLACK);
  object_header *header = (object_header *) OBJECT_HEADER_START(objPtr);
  header->objWords = objWords;
  header->typeInfo = type_information;
  header->forwardReference = NULL;
  return objPtr;
}



// Return: Address of the object data.
//         Empty header at OBJECT_HEADER_START(address)
uint32_t *allocateWordsNoGC(uint objWords, Page **list, colour colour) {
  Page *page = getValidPage(list, objWords);
  page->space = colour;
  uint32_t *objHeaderAddress = PAGE_DATA_START(page) + page->usedWords;
  page->usedWords += objWords;
  return OBJECT_DATA_START(objHeaderAddress);
}


// Copy object to a new page and return the new address
uint32_t *copy(uint32_t *heapObjPointer, Page *oldPage, object_header *oldHeader) {
  callsToCopy++;
  uint32_t *newLoc = allocateWordsNoGC(oldHeader->objWords, &GREY_PAGES, GREY);
  object_header *newHeader = OBJECT_HEADER_START(newLoc);
  uint32_t dataSize = (oldHeader->objWords - (sizeof(object_header) / 4) ) * 4;
  memcpy((void *) newLoc, (const void*) heapObjPointer, dataSize);
  newHeader->typeInfo = oldHeader->typeInfo;
  newHeader->objWords = oldHeader->objWords;
  newHeader->forwardReference = NULL;
  return newLoc;
}


// Find whether a pointer points to one of the heaps
bool isAmbiguousRoot(uint32_t *ptr) {
  Heap *curr = HEAPS;
  while (curr) {
    if (ptr >= curr->start &&
        ptr <= (curr->start + PAGE_WORDS * curr->numPages)) {
      return true;
    }
    curr = curr->next;
  }
  return false;
}



// TODO: objects in the heap with no type information. Treat
// all words as possible pointers. For async part.
// Move all the references in a given page to new pages in the
// GREY set
void forwardHeapPointers(Page *page) {
  assert(page->space == GREY);
  uint32_t *start = PAGE_DATA_START(page);
  uint32_t *end = start + page->usedWords;
  object_header *header = (object_header *) start;

  while ((uint32_t *) header < end) {
    if (header->forwardReference == NULL) {
      type_info *typeInfo = header->typeInfo;
      uint32_t *data = OBJECT_DATA_START(header);
      if (header->typeInfo != NULL) {
                if (typeInfo->isArray) {
          uint32_t arraySize = *data;
          data++; // Skip size field
          for (int i = 0; i < arraySize; i++) {
            if (typeInfo->elemIsPtr[0]) {
              if (*(data + i) !=  NULL) {
                *(data + i) = moveReference((uint32_t *) *(data + i));
              }
            }
          }
        } else {
          for (int i = 0; i < typeInfo->nElem; i++) {
            if (typeInfo->elemIsPtr[i]) {
              if (*(data + i) !=  NULL) {
                *(data + i) = moveReference((uint32_t *) *(data + i));
              }
            }
          }
        }
      } else {
        // Async object 
        // Scan object for possible references in the heap.

        for (uint32_t *ptr = data; ptr < header + header->objWords; ptr++) {
          if (isAmbiguousRoot((uint32_t *) *ptr)) {
            Page *refPage = getPage((uint32_t *) *ptr);
            if (refPage->space == WHITE) {
              removePage(refPage);
              insert(refPage, &GREY_PAGES);
              promote(refPage, GREY);
            }
          }
        }
      }
    }
    header = (object_header *) ( ((uint32_t *) header) + header->objWords);
  }
}




// Allocate a new heap from the operating system, initialise all the pages
// in it and add them to the list of free pages
void allocateHeap(void) {
  uint32_t* heapAddr = NULL;
//  uint wordSize = NUMBER_PAGES_TO_ALLOCATE * PAGE_WORDS;
  uint wordSize = NUMBER_PAGES_TO_ALLOCATE * PAGE_WORDS;
  int res = posix_memalign((void**) &heapAddr, PAGE_WORDS * 4, wordSize * 4);
 
  // Build list of free pages
  for (uint32_t *ptr = heapAddr; ptr < heapAddr + wordSize;
       ptr += PAGE_WORDS) {

    Page *p = (Page *) ptr;
    p->space = BLACK;
    p->usedWords = 0;
    insert(p, &FREE_PAGES);
    total_pages_handled++;
  }
  Heap *newHeap = (Heap *) malloc(sizeof(Heap));
  newHeap->numPages = NUMBER_PAGES_TO_ALLOCATE;
  newHeap->start = heapAddr;
  newHeap->next = HEAPS;
  HEAPS = newHeap;
  NUMBER_PAGES_TO_ALLOCATE *= 2;
  return;
}


uint *allocateManyPages(void) {
  return NULL;
}

