#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* arena's are taken Lab 7 of from https://www.cl.cam.ac.uk/teaching/1819/ProgC/materials.html */ 

typedef struct arena *arena_t;
struct arena
{
  int size;
  int current;
  int64_t* elements;
};

arena_t create_arena(int size)
{
  arena_t arena = malloc(sizeof(struct arena));
  
  arena->size = size;
  arena->current = 0;
  
  arena->elements = malloc(size * sizeof(int64_t));
  
  return arena;
}

void arena_free(arena_t a)
{
  free(a->elements);
  free(a);
}

int64_t *alloc(arena_t heap, int64_t n)
{
  if (heap->size < heap->current +n) {
    fprintf(stderr, "heap space exhausted(\n");
    exit(1);
  }
  int64_t *new_record = heap->elements + heap->current;
  heap->current = heap->current + n;
  return new_record; 
}

/* read in an integer from the command line */ 
int64_t read() {
  int64_t got = 0;
  printf("> ");
  int result = scanf("%ld", &got);
  if (result == EOF) {
    fprintf(stderr, "stdin died :(\n");
    exit(1);
  }
  return got;
}

/* this is the name given to the compiled slang code. */
/* "giria" is "slang" in Portuguese                   */
int64_t giria(arena_t);

/* main : create heap, pass it to giria, print result, free heap, exit */
/* Currently will only correctly print slang integer values.           */
/* A heap-allocated value will be printed as the pointer's address!    */
/* This needs to be fixed! Must put headers on heap-allocated          */
/* records, not just for garbage collection, but to identify what kind */
/* of value it is ...                                                  */
int main() {
  arena_t heap = create_arena(1024);
  printf("%ld\n", giria(heap));
  arena_free (heap);   
  return 0;
}
