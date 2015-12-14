#include <errno.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <malloc.h>
#include <assert.h>

//#define HEAP_SIZE_WORDS 250000 // 1MB
//#define PAGE_WORDS 1024 //REAL
#define PAGE_WORDS 32 
#define WORD_SIZE 4 // Bytes
//#define NUMBER_PAGES_TO_ALLOCATE 250 // REAL
#define NUMBER_PAGES_TO_ALLOCATE 4

#define PAGE_DATA_START(x) ((uint32_t *) (4 * (((uint32_t) x + sizeof(Page) + 3) / 4)))
#define OBJECT_DATA_START(x) ((uint32_t *) (4 * (((uint32_t) x + sizeof(object_header) + 3) / 4)))
// CHECK: get header from pointer to the object data
#define OBJECT_HEADER_START(x) ((object_header *) (4 * (((uint32_t) x - sizeof(object_header) + 3) / 4)))


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
  uint8_t isArray;
  uint32_t nElem;
  uint8_t elemIsPtr[];
} __attribute__((packed)) type_info ;

typedef struct object_header {
  uint objWords;
  type_info *typeInfo;
  uint32_t *forwardReference;
} object_header;

typedef struct Heap {
  // All heaps have the same size PAGE_WORDS * NUMBER_PAGES_TO_ALLOCATE
  uint32_t *start;
  struct Heap *next;
} Heap;

// Variables
uint32_t *top_stack = NULL;
uint allocated_pages = 0;
uint total_pages_handled = 0;
uint GC_Alloc_calls = 0;
// Memory
Page *FREE_PAGES = NULL;
Page *BLACK_PAGES = NULL;
Page *GREY_PAGES = NULL;
Page *WHITE_PAGES = NULL;
Heap *HEAPS = NULL; // List of heaps

// Functions
void GCInternalInit(uint *sp);
void removePage(Page *page);
void insert(Page *p, Page **list);
void forwardHeapPointers(Page *page);
void promote(Page *page, colour c);
void allocateHeap(void);
void GCCollect(uint32_t *sp);
uint32_t *moveReference(uint32_t *ref);
uint32_t *GCInternalAlloc(uint byte_size, type_info *type_information, uint32_t *bottom_stack);
uint32_t *copy(uint32_t *heapObjPointer, Page *oldPage, object_header *oldHeader);
uint32_t *allocateWordsNoGC(uint objWords, Page **list, colour colour);
uint *allocateManyPages();  // Unimplemented
Page *getPage(uint32_t *ptr);
Page *getValidPage(Page **list, uint words);
uint getFreeWords(Page *page);
bool isAmbiguousRoot(uint32_t *ptr);
void GCAlloc(void) __attribute__ ( ( naked ) );
void GCInit(void) __attribute__ ( ( naked ) );
void GCFree();


// Test functions
void printHeader(object_header *header);
void printTypeInfo(type_info *type);
void printPageLists();
void printHeap();

void printHeap(void) {
}

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
  Heap *heap = HEAPS;
  while (heap) {
    Heap *tmp = heap->next;
    free((void *) heap);
    heap = tmp;
  }
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

void GCCollect(uint32_t *bottom_stack) {
//  printf("GCCollect called\n");
  
//  printPageLists();

  // Make all pages WHITE
  Page *toWhite = BLACK_PAGES;
  while (toWhite) {
    promote(toWhite, WHITE);
    toWhite = toWhite->next;
  }

  WHITE_PAGES = BLACK_PAGES;
  BLACK_PAGES = NULL;

//  printf("Top_stack: %#010x\n", top_stack);
//  printf("Bottom_stack: %#010x\n", bottom_stack);
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
  printf("Trace\n");
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
    printf("freeing page %#010x that has %d used words.\n", whitePage, whitePage->usedWords);
    removePage(whitePage);
    whitePage->usedWords = 0;
    promote(whitePage, BLACK);
    insert(whitePage, &FREE_PAGES);
    whitePage = WHITE_PAGES;
    allocated_pages--;
  }
  if (allocated_pages >= total_pages_handled / 2) {
    printf("Allocating more heap to avoid calling GCCollect two times in a row\n");
    allocateHeap();
  }
  return;
}


uint32_t *GCInternalAlloc(uint byte_size, type_info *type_information, uint32_t *bottom_stack) {
  GC_Alloc_calls++;
  printf("%d call to GCInternal Allocator\n", GC_Alloc_calls);
  printf("nElem: %d\n", type_information->nElem);
  if (allocated_pages >= total_pages_handled / 2) {
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
//  printHeader(header);  // TESTING try before changing it too
  return objPtr;
}

void printHeader(object_header *header) {
  if (header != NULL) {
    printf("Header at %#010x\n", header); 
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
    printf("\tisArray: %d\n", type->isArray); 
    printf("\tnElem: %d\n", type->nElem); 
    for (int i = 0; i < type->nElem; i++) {
      printf("\telemIsPtr[%d] = %d\n", i, type->elemIsPtr[i]);
    }
  } else {
    printf("Null typeInfo\n");
  }
}

// Return: Address of the object data.
//         Empty header at OBJECT_HEADER_START(address)
uint32_t *allocateWordsNoGC(uint objWords, Page **list, colour colour) {

//  printf("Allocate words no GC\n");
  Page *page = getValidPage(list, objWords);
  page->space = colour;
  uint32_t *objHeaderAddress = PAGE_DATA_START(page) + page->usedWords;

  printf("Allocating in header: %#010x, of page: %#010x\n", objHeaderAddress,page);
  page->usedWords += objWords;
  return OBJECT_DATA_START(objHeaderAddress);
}


// Copy object to a new page and return the new address
uint32_t *copy(uint32_t *heapObjPointer, Page *oldPage, object_header *oldHeader) {
  printf("Copy function!!\n\n\n\n\n\n");
  uint32_t *newLoc = allocateWordsNoGC(oldHeader->objWords, &GREY_PAGES, GREY);
  object_header *newHeader = OBJECT_HEADER_START(newLoc);
  uint32_t dataSize = (oldHeader->objWords - (sizeof(object_header) / 4) ) * 4;
  memcpy((void *) newLoc, (const void*) heapObjPointer, dataSize);
  printHeader(newHeader);
  newHeader->typeInfo = oldHeader->typeInfo;
  newHeader->objWords = oldHeader->objWords;
  newHeader->forwardReference = NULL;
  return newLoc;
}



// Get a page from the list of pages with enough free space to fit
// 'words' words
Page *getValidPage(Page **list, uint words) {
//  printf("Get Valid Page\n");
  Page *current = *list;
  uint freeWords = getFreeWords(current);

  while (current != NULL && freeWords < words) {
    current = current->next;
    freeWords = getFreeWords(current);
  }

  if (current == NULL) {
    if (FREE_PAGES == NULL) {
      allocateHeap();
    }
    current = FREE_PAGES;
    removePage(FREE_PAGES);
    insert(current, list);
    allocated_pages++;
  }
  return current;
}

// Get page that a pointer is pointing to
// by rounding down to 32 bit alignment
Page *getPage(uint32_t *pointer) {
  uint mask = 0xffffffff ^ 0x1f;
  return (Page *) ((uint) pointer & mask);
}


// Find whether a pointer points to one of the heaps
bool isAmbiguousRoot(uint32_t *ptr) {
  Heap *curr = HEAPS;
  while (curr) {
    if (ptr >= curr->start && ptr <= (curr->start + PAGE_WORDS * NUMBER_PAGES_TO_ALLOCATE)) {
//      printf("Ambiguous root: %#010x\n", ptr);
      return true;
    }
    curr = curr->next;
  }
//  printf("Not ambiguous root: %#010x\n", ptr);
  return false;
}

// Promote the space of a page to the new colour 'c'
void promote(Page *page, colour c) {
  page->space = c;
}

// CHECK this function
// ref is a pointer to where a pointer to the heap is stored
// Move a live reference that is in a white page to a grey page
uint32_t *moveReference(uint32_t *ref) {
  printf("Move reference\n");
  
  printf("%#010x, %d, %d, %d\n",ref, *ref, *(ref + 1), *(ref + 2));
  uint32_t *heapPointer = ref;
  Page *pagePointed = getPage(heapPointer);
  if (pagePointed->space == WHITE) {
    printf("WHITEPAGE !\n");
    object_header *header = OBJECT_HEADER_START(heapPointer);
    if (header->forwardReference == NULL) {
      printf("YEYYYY\n");
      header->forwardReference = copy(heapPointer, pagePointed, header);
      header->typeInfo = NULL;
      // Same header->size, so the page can still be iterated through
      Page *destinationPage = getPage(header->forwardReference);
      destinationPage->space = GREY;
      removePage(destinationPage);
      insert(destinationPage, &GREY_PAGES);
    }
    return header->forwardReference;
//    *ref = header->forwardReference;
  }
  return heapPointer;
}

// TODO: objects in the heap with no type information. Treat
// all words as possible pointers. For async part.
// Move all the references in a given page to new pages in the
// GREY set
void forwardHeapPointers(Page *page) {
  assert(page->space == GREY);

  printf("Forward Heap Pointers\n");
  uint32_t *start = PAGE_DATA_START(page);
  uint32_t *end = start + page->usedWords;
  printf("From: %#010x\n",start);
  printf("To: %#010x\n",end);
  object_header *header = (object_header *) start;

    while ((uint32_t *) header < end) {
//    printf("Currently at : %#010x\n",header);
    if (header->typeInfo != NULL) {
      type_info *typeInfo = header->typeInfo;
      uint32_t *data = OBJECT_DATA_START(header);
      if (typeInfo->isArray) {
        printf("Header of the array\n");
        printHeader(header);


        uint32_t arraySize = *data;
        data++; // Skip size field
        for (int i = 0; i < arraySize; i++) {
          if (typeInfo->elemIsPtr[0]) {
            *(data + i) = moveReference((uint32_t *) *(data + i));
          } 
        }
      } else {
        printf("Header of the pair\n");
        printHeader(header);

        for (int i = 0; i < typeInfo->nElem; i++) {
          if (typeInfo->elemIsPtr[i]) {
            printf("Is Array? : %d\n", typeInfo->isArray);
            printf("nElem: %d\n", typeInfo->nElem);
            uint32_t *pair = *(data + i);
            printf("Pair: %#010x, %#010x, %#010x, %#010x \n", pair, *(data + 1), (data + 0), (data + 1));
            *(data + i) = moveReference(pair);
          }
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

// TODO: double NUMBER_PAGES_TO_ALLOCATE every time you call this function
//
// Allocate a new heap from the operating system, initialise all the pages
// in it and add them to the list of free pages
void allocateHeap(void) {
//  printf("Allocate a heap\n");
  uint32_t* heapAddr = NULL;
  uint wordSize = NUMBER_PAGES_TO_ALLOCATE * PAGE_WORDS;
  int res = posix_memalign((void**) &heapAddr, PAGE_WORDS * 4, wordSize * 4);
//  printf("My heap goes from %#010x to %#010x\n ", heapAddr, heapAddr + NUMBER_PAGES_TO_ALLOCATE * PAGE_WORDS);
  // Build list of free pages
  for (uint32_t *ptr = heapAddr; ptr < heapAddr + wordSize;
       ptr += PAGE_WORDS) {

    Page *p = (Page *) ptr;
    p->space = BLACK;
    p->usedWords = 0;
    insert(p, &FREE_PAGES);
    total_pages_handled++;
//    printf("My page number %d is at %#010x, with %d free words\n", total_pages_handled, p, getFreeWords(p));
  }
  Heap *newHeap = (Heap *) malloc(sizeof(Heap));
  newHeap->start = heapAddr;
  newHeap->next = HEAPS;
  HEAPS = newHeap;
  return;
}


uint *allocateManyPages(void) {
  return NULL;
}

// Remove a page from the list of pages.
void removePage(Page* page) {
  if (page == NULL) {
    return;
  }
  if (page == FREE_PAGES) {
    FREE_PAGES = page->next;
  } else if (page == BLACK_PAGES) {
    BLACK_PAGES = page->next;
  } else if (page == WHITE_PAGES) {
    WHITE_PAGES = page->next;
  } else if (page == GREY_PAGES) {
    GREY_PAGES = page->next;
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
void insert(Page *p, Page **list) {
  p->next = *list;
  if (*list != NULL) {
    (*list)->previous = p;
  }
  *list = p;
}


