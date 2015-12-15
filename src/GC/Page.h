#include <stdint.h>
#ifndef PAGES_H
#define PAGES_H

/*
 * Based on the paper "Portable, Mostly-Concurrent, Mostly-Copying
 * Gargabe Collection for Multi-Processors" by Antony L Hosking, inspired by Joel F. Bartlett.
 *
 * Black pages contain only live objects, and the references contained
 *    in those objects do not refer to objects in white pages.
 * Grey pages contain only live objects, but the references in those
 *    objects may still refer to objects in white pages.
 * White pages contain objects that have not yet been evacuated to a
 *    non-white page; at the end of collection these pages are garbage and can be reclaimed.
 */


typedef enum {BLACK, GREY, WHITE} colour;

typedef struct Page {
  colour space;
  uint32_t usedWords;
  struct Page *next;
  struct Page *previous;
} Page;


void removePage(Page *page);
void insert(Page *p, Page **list);
Page *getPage(uint32_t *ptr);
Page *getValidPage(Page **list, uint32_t words);
void promote(Page *page, colour c);
uint32_t getFreeWords(Page *page);


#endif
