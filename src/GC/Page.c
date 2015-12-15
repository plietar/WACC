#include "Page.h"
#include "GC.h"


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

// Get a page from the list of pages with enough free space to fit
// 'words' words
Page *getValidPage(Page **list, uint32_t words) {
  Page *current = *list;
  uint freeWords = getFreeWords(current);

  ///////////////////////////////////////////////////////////////
  // Note: this makes the allocation more loose and thus
  // not all pages are 100% full, but it increases efficiency
  if (freeWords < words) {
    current = NULL;
  }
  //while (current != NULL && freeWords < words) {
  //  current = current->next;
  //  freeWords = getFreeWords(current);
  //    printf("Page: %#010x\n", current);
  //}

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
  return (Page *) ((uint) pointer & ~(PAGE_WORDS * 4  - 1));
}



// Promote the space of a page to the new colour 'c'
void promote(Page *page, colour c) {
  page->space = c;
}

// Move a live reference that is in a white page to a grey page
uint32_t *moveReference(uint32_t *ref) {
  uint32_t *heapPointer = ref;
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
      insert(destinationPage, &GREY_PAGES);
    }
    return header->forwardReference;
  }
  return heapPointer;
}

// Find number of free words in a page
uint32_t getFreeWords(Page *page) {
  if (page == NULL) {
    return 0;
  }
  return PAGE_WORDS - page->usedWords - (sizeof(Page) / 4);
}
