# Memory Management and Garbage Collection

This document provides a comprehensive guide to Emacs's memory management and garbage collection system, exploring the allocation strategies, GC algorithms, and performance considerations that power the Elisp runtime.

## Table of Contents

1. [Overview](#overview)
2. [The Allocation System](#the-allocation-system)
3. [Garbage Collection Algorithm](#garbage-collection-algorithm)
4. [Key Functions Deep Dive](#key-functions-deep-dive)
5. [Special Topics](#special-topics)
6. [Performance and Tuning](#performance-and-tuning)

---

## Overview

Emacs uses a **mark-and-sweep garbage collector** with several sophisticated features:

- **Type-specific allocators** optimized for different Lisp object types
- **Block-based allocation** with free lists for fast allocation
- **Conservative stack scanning** combined with precise heap scanning
- **Weak references** and finalizers support
- **Incremental compaction** for strings
- **Integration with pdumper** for portable dumping

The entire implementation resides primarily in `/home/user/emacs/src/alloc.c` (7,500+ lines), with supplementary allocators in `gmalloc.c` and `ralloc.c`.

```c
/* From src/alloc.c:1 */
/* Storage allocation and gc for GNU Emacs Lisp interpreter.
   Copyright (C) 1985-2025 Free Software Foundation, Inc. */
```

### Key Design Principles

1. **Fast Allocation**: Most allocations happen from pre-allocated blocks via free lists
2. **Minimal Fragmentation**: Block-based allocation with type-specific pools
3. **Precise Marking**: GC knows exact layout of all heap objects
4. **Conservative Scanning**: Stack and registers scanned conservatively
5. **Generational Hints**: Track object age for better GC decisions

---

## The Allocation System

### Memory Type Tracking

Emacs tracks what type of Lisp object each memory region contains for conservative stack scanning:

```c
/* From src/alloc.c:408 */
/* When scanning the C stack for live Lisp objects, Emacs keeps track of
   what memory allocated via lisp_malloc and lisp_align_malloc is intended
   for what purpose. This enumeration specifies the type of memory.  */

enum mem_type
{
  MEM_TYPE_NON_LISP,
  MEM_TYPE_CONS,
  MEM_TYPE_STRING,
  MEM_TYPE_SYMBOL,
  MEM_TYPE_FLOAT,
  /* Since all non-bool pseudovectors are small enough to be
     allocated from vector blocks, this memory type denotes
     large regular vectors and large bool pseudovectors.  */
  MEM_TYPE_VECTORLIKE,
  /* Special type to denote vector blocks.  */
  MEM_TYPE_VECTOR_BLOCK,
  /* Special type to denote reserved memory.  */
  MEM_TYPE_SPARE
};
```

A **red-black tree** tracks all allocated memory regions:

```c
/* From src/alloc.c:461 */
/* A red-black tree is a balanced binary tree with the following
   properties:

   1. Every node is either red or black.
   2. Every leaf is black.
   3. If a node is red, then both of its children are black.
   4. Every simple path from a node to a descendant leaf contains
      the same number of black nodes.
   5. The root is always black. */

struct mem_node
{
  struct mem_node *left, *right;  /* Children, never NULL */
  struct mem_node *parent;         /* Parent or NULL for root */
  void *start, *end;               /* Memory region bounds */
  enum {MEM_BLACK, MEM_RED} color;
  enum mem_type type;              /* What kind of objects */
};
```

### Cons Cell Allocation

Cons cells are allocated from **cons blocks**, with each block containing multiple cons cells tracked via a bitmap for marking:

```c
/* From src/alloc.c:2539 */
#define CONS_BLOCK_SIZE						\
  (((BLOCK_BYTES - sizeof (struct cons_block *)			\
     - (sizeof (struct Lisp_Cons) - sizeof (bits_word))) * CHAR_BIT)	\
   / (sizeof (struct Lisp_Cons) * CHAR_BIT + 1))

struct cons_block
{
  /* Place `conses' at the beginning, to ease up CONS_INDEX's job.  */
  struct Lisp_Cons conses[CONS_BLOCK_SIZE];
  bits_word gcmarkbits[1 + CONS_BLOCK_SIZE / BITS_PER_BITS_WORD];
  struct cons_block *next;
};
```

Allocation tries the free list first, then allocates from the current block:

```c
/* From src/alloc.c:2599 */
DEFUN ("cons", Fcons, Scons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components, and return it.  */)
  (Lisp_Object car, Lisp_Object cdr)
{
  register Lisp_Object val;

  if (cons_free_list)
    {
      ASAN_UNPOISON_CONS (cons_free_list);
      XSETCONS (val, cons_free_list);
      cons_free_list = cons_free_list->u.s.u.chain;
    }
  else
    {
      if (cons_block_index == CONS_BLOCK_SIZE)
	{
	  struct cons_block *new
	    = lisp_align_malloc (sizeof *new, MEM_TYPE_CONS);
	  memset (new->gcmarkbits, 0, sizeof new->gcmarkbits);
	  ASAN_POISON_CONS_BLOCK (new);
	  new->next = cons_block;
	  cons_block = new;
	  cons_block_index = 0;
	}
      ASAN_UNPOISON_CONS (&cons_block->conses[cons_block_index]);
      XSETCONS (val, &cons_block->conses[cons_block_index]);
      cons_block_index++;
    }

  XSETCAR (val, car);
  XSETCDR (val, cdr);
  eassert (!XCONS_MARKED_P (XCONS (val)));
  consing_until_gc -= sizeof (struct Lisp_Cons);
  cons_cells_consed++;
  return val;
}
```

### String Allocation

Strings use a **two-level allocation strategy**:

1. **String objects** (`struct Lisp_String`) allocated from string blocks
2. **String data** allocated from sblocks (sub-allocated memory blocks)

```c
/* From src/alloc.c:1318 */
/* Lisp_Strings are allocated in string_block structures.  When a new
   string_block is allocated, all the Lisp_Strings it contains are
   added to a free-list string_free_list.  When a new Lisp_String is
   needed, it is taken from that list.  During the sweep phase of GC,
   string_blocks that are entirely free are freed, except two which
   we keep.

   String data is allocated from sblock structures.  Strings larger
   than LARGE_STRING_BYTES, get their own sblock, data for smaller
   strings is sub-allocated out of sblocks of size SBLOCK_SIZE.

   Sblocks consist internally of sdata structures, one for each
   Lisp_String.  The sdata structure points to the Lisp_String it
   belongs to.  The Lisp_String points back to the `u.data' member of
   its sdata structure. */
```

String data structure:

```c
/* From src/alloc.c:1352 */
struct sdata
{
  /* Back-pointer to the string this sdata belongs to.  If null, this
     structure is free, and NBYTES contains the string's byte size.  */
  struct Lisp_String *string;

#ifdef GC_CHECK_STRING_BYTES
  ptrdiff_t nbytes;
#endif

  unsigned char data[FLEXIBLE_ARRAY_MEMBER];
};
```

String blocks:

```c
/* From src/alloc.c:1405 */
struct sblock
{
  struct sblock *next;              /* Next in list */
  sdata *next_free;                 /* Next free sdata block */
  sdata data[FLEXIBLE_ARRAY_MEMBER]; /* String data */
};
```

Key constants:

```c
/* From src/alloc.c:1343 */
enum { SBLOCK_SIZE = MALLOC_SIZE_NEAR (8192) };

/* Strings larger than this are considered large strings.  */
#define LARGE_STRING_BYTES 1024
```

### Vector Allocation

Vectors use a sophisticated **block allocator with multiple free lists**:

```c
/* From src/alloc.c:2760 */
enum { VECTOR_BLOCK_SIZE = 4096 };

/* Vector size requests are a multiple of this.  */
enum { roundup_size = COMMON_MULTIPLE (LISP_ALIGNMENT, word_size) };

enum {VECTOR_BLOCK_BYTES = VECTOR_BLOCK_SIZE - vroundup_ct (sizeof (void *))};
```

Vector blocks contain multiple small vectors:

```c
/* From src/alloc.c:2845 */
struct vector_block
{
  char data[VECTOR_BLOCK_BYTES];
  struct vector_block *next;
};
```

Large vectors get their own allocation:

```c
/* From src/alloc.c:2826 */
/* This internal type is used to maintain the list of large vectors
   which are allocated at their own, e.g. outside of vector blocks. */

struct large_vector
{
  struct large_vector *next;
};
```

Free lists organized by size:

```c
/* From src/alloc.c:2855 */
/* Vector free lists, where NTH item points to a chain of free
   vectors of the same NBYTES size, so NTH == VINDEX (NBYTES),
   except for the last element which may contain larger vectors. */

static struct Lisp_Vector *vector_free_lists[VECTOR_FREE_LIST_ARRAY_SIZE];
```

The allocation algorithm:

```c
/* From src/alloc.c:2968 */
static struct Lisp_Vector *
allocate_vector_from_block (ptrdiff_t nbytes)
{
  struct Lisp_Vector *vector;
  struct vector_block *block;
  size_t index, restbytes;

  /* First, try to allocate from a free list
     containing vectors of the requested size.  */
  index = VINDEX (nbytes);
  if (vector_free_lists[index])
    {
      vector = vector_free_lists[index];
      ASAN_UNPOISON_VECTOR_CONTENTS (vector, nbytes - header_size);
      vector_free_lists[index] = next_vector (vector);
      return vector;
    }

  /* Next, check free lists containing larger vectors. */
  for (index = max (VINDEX (nbytes + VBLOCK_BYTES_MIN),
		    last_inserted_vector_free_idx);
       index < VECTOR_FREE_LIST_ARRAY_SIZE; index++)
    if (vector_free_lists[index])
      {
	/* This vector is larger than requested. Split it. */
	vector = vector_free_lists[index];
	size_t vector_nbytes = pseudovector_nbytes (&vector->header);
	vector_free_lists[index] = next_vector (vector);

	/* Excess bytes are used for the smaller vector. */
	restbytes = vector_nbytes - nbytes;
	setup_on_free_list (ADVANCE (vector, nbytes), restbytes);
	return vector;
      }

  /* Finally, need a new vector block.  */
  block = allocate_vector_block ();
  vector = (struct Lisp_Vector *) block->data;

  /* Set up remaining space on free list */
  restbytes = VECTOR_BLOCK_BYTES - nbytes;
  if (restbytes >= VBLOCK_BYTES_MIN)
    setup_on_free_list (ADVANCE (vector, nbytes), restbytes);

  return vector;
}
```

### Float and Symbol Allocation

Floats and symbols use simpler block-based allocation similar to cons cells, with free lists for fast reuse.

### Low-Level Allocators

#### lisp_malloc

The primary allocator for Lisp objects:

```c
/* From src/alloc.c:876 */
void *
lisp_malloc (size_t nbytes, bool clearit, enum mem_type type)
{
  register void *val;

#ifdef GC_MALLOC_CHECK
  allocated_mem_type = type;
#endif

  val = clearit ? calloc (1, nbytes) : malloc (nbytes);

  /* Record this allocation in the mem_node tree */
#ifndef GC_MALLOC_CHECK
  struct mem_node *m = mem_insert (val, (char *) val + nbytes, type);
#endif

  if (!val && nbytes)
    memory_full (nbytes);

  return val;
}
```

#### lisp_align_malloc

For objects requiring special alignment (e.g., cons blocks):

```c
/* From src/alloc.c:930 */
static void *
lisp_align_malloc (size_t nbytes, enum mem_type type)
{
  void *base = malloc (nbytes + BLOCK_ALIGN);
  if (base == 0)
    memory_full (nbytes);

  /* Align to BLOCK_ALIGN boundary */
  void *val = (void *) ROUNDUP ((uintptr_t) base, BLOCK_ALIGN);

  /* Record in mem_node tree */
#ifndef GC_MALLOC_CHECK
  mem_insert (val, (char *) val + nbytes, type);
#endif

  return val;
}
```

### gmalloc.c - GNU malloc

A custom malloc implementation used on some platforms:

```c
/* From src/gmalloc.c:94 */
/* The allocator divides the heap into blocks of fixed size; large
   requests receive one or more whole blocks, and small requests
   receive a fragment of a block.  Fragment sizes are powers of two,
   and all fragments of a block are the same size.  When all the
   fragments in a block have been freed, the block itself is freed.  */

#define BLOCKLOG	(INT_WIDTH > 16 ? 12 : 9)
#define BLOCKSIZE	(1 << BLOCKLOG)
```

### ralloc.c - Relocating Allocator

A block-relocating allocator for buffer text:

```c
/* From src/ralloc.c:1 */
/* Block-relocating memory allocator.

   Only relocate the blocs necessary for SIZE in r_alloc_sbrk,
   rather than all of them.  This means allowing for a possible
   hole between the first bloc and the end of malloc storage.  */
```

The relocating allocator allows buffer text to be moved in memory without updating pointers, enabling efficient memory compaction.

---

## Garbage Collection Algorithm

### The Mark-and-Sweep Strategy

Emacs uses a **non-copying, mark-and-sweep** garbage collector:

1. **Mark Phase**: Traverse all reachable objects from roots, marking them
2. **Sweep Phase**: Scan all allocated objects, freeing unmarked ones

This approach has several advantages:
- No need to update pointers (non-copying)
- Works with conservative stack scanning
- Simple and predictable
- Integrates well with C code

### GC Entry Point

```c
/* From src/alloc.c:5778 */
void
garbage_collect (void)
{
  Lisp_Object tail, buffer;
  char stack_top_variable;
  bool message_p;
  specpdl_ref count = SPECPDL_INDEX ();
  struct timespec start;

  eassert (weak_hash_tables == NULL);

  if (garbage_collection_inhibited)
    return;

  /* Record this function for profiler backtraces */
  record_in_backtrace (QAutomatic_GC, 0, 0);

  /* Compact undo lists early */
  FOR_EACH_LIVE_BUFFER (tail, buffer)
    compact_buffer (XBUFFER (buffer));

  start = current_timespec ();

  /* Prevent recursive GC */
  consing_until_gc = HI_THRESHOLD;

  /* Save stack for conservative scanning */
#if MAX_SAVE_STACK > 0
  if (NILP (Vpurify_flag))
    {
      /* Save a copy of the stack for debugging */
      char const *stack;
      ptrdiff_t stack_size;
      if (&stack_top_variable < stack_bottom)
	{
	  stack = &stack_top_variable;
	  stack_size = stack_bottom - &stack_top_variable;
	}
      else
	{
	  stack = stack_bottom;
	  stack_size = &stack_top_variable - stack_bottom;
	}
      if (stack_size <= MAX_SAVE_STACK)
	{
	  if (stack_copy_size < stack_size)
	    {
	      stack_copy = xrealloc (stack_copy, stack_size);
	      stack_copy_size = stack_size;
	    }
	  no_sanitize_memcpy (stack_copy, stack, stack_size);
	}
    }
#endif

  gc_in_progress = 1;

  /* MARK PHASE: Mark all reachable objects */

  struct gc_root_visitor visitor = { .visit = mark_object_root_visitor };
  visit_static_gc_roots (visitor);

  mark_lread ();
  mark_terminals ();
  mark_kboards ();
  mark_threads ();
  mark_charset ();
  mark_composite ();
  mark_profiler ();

  /* Platform-specific marking */
#ifdef USE_GTK
  xg_mark_data ();
#endif

  /* Mark font caches, then compact them */
  compact_font_caches ();

  /* Mark undo lists after compaction */
  FOR_EACH_LIVE_BUFFER (tail, buffer)
    {
      struct buffer *nextb = XBUFFER (buffer);
      if (!EQ (BVAR (nextb, undo_list), Qt))
	bset_undo_list (nextb, compact_undo_list (BVAR (nextb, undo_list)));
      mark_object (BVAR (nextb, undo_list));
    }

  /* Handle finalizers */
  queue_doomed_finalizers (&doomed_finalizers, &finalizers);
  mark_finalizer_list (&doomed_finalizers);

  /* Handle weak hash tables */
  mark_and_sweep_weak_table_contents ();
  eassert (weak_hash_tables == NULL);

  eassert (mark_stack_empty_p ());

  /* SWEEP PHASE: Free unmarked objects */
  gc_sweep ();

  unmark_main_thread ();

  gc_in_progress = 0;

  /* Update GC threshold */
  consing_until_gc = gc_threshold
    = consing_threshold (gc_cons_threshold, Vgc_cons_percentage, 0);

  unblock_input ();

  /* Run finalizers after GC completes */
  run_finalizers (&doomed_finalizers);

  /* Update statistics */
  if (FLOATP (Vgc_elapsed))
    {
      static struct timespec gc_elapsed;
      gc_elapsed = timespec_add (gc_elapsed,
				 timespec_sub (current_timespec (), start));
      Vgc_elapsed = make_float (timespectod (gc_elapsed));
    }

  gcs_done++;

  if (!NILP (Vpost_gc_hook))
    {
      specpdl_ref gc_count = inhibit_garbage_collection ();
      safe_run_hooks (Qpost_gc_hook);
      unbind_to (gc_count, Qnil);
    }
}
```

### The Marking Phase

#### Mark Stack

To avoid deep C recursion, marking uses an explicit stack:

```c
/* From src/alloc.c:6318 */
/* Entry of the mark stack.  */
struct mark_entry
{
  ptrdiff_t n;		        /* number of values, or 0 if a single value */
  union {
    Lisp_Object value;		/* when n = 0 */
    Lisp_Object *values;	/* when n > 0 */
  } u;
};

struct mark_stack
{
  struct mark_entry *stack;	/* base of stack */
  ptrdiff_t size;		/* allocated size in entries */
  ptrdiff_t sp;			/* current number of entries */
};

static struct mark_stack mark_stk = {NULL, 0, 0};
```

#### The mark_object Function

The core marking function:

```c
/* From src/alloc.c:6720 */
void
mark_object (Lisp_Object obj)
{
  ptrdiff_t sp = mark_stk.sp;
  mark_stack_push_value (obj);
  process_mark_stack (sp);
}
```

#### Processing the Mark Stack

```c
/* From src/alloc.c:6470 */
static void
process_mark_stack (ptrdiff_t base_sp)
{
  while (mark_stk.sp > base_sp)
    {
      Lisp_Object obj = mark_stack_pop ();
    mark_obj: ;
      void *po = XPNTR (obj);

      switch (XTYPE (obj))
	{
	case Lisp_String:
	  {
	    struct Lisp_String *ptr = XSTRING (obj);
	    if (string_marked_p (ptr))
	      break;
	    check_allocated_and_live (live_string_p, MEM_TYPE_STRING, po);
	    set_string_marked (ptr);
	    mark_interval_tree (ptr->u.s.intervals);
	  }
	  break;

	case Lisp_Vectorlike:
	  {
	    struct Lisp_Vector *ptr = XVECTOR (obj);
	    if (vector_marked_p (ptr))
	      break;

	    enum pvec_type pvectype = PSEUDOVECTOR_TYPE (ptr);

	    switch (pvectype)
	      {
	      case PVEC_BUFFER:
		mark_buffer ((struct buffer *) ptr);
		break;

	      case PVEC_FRAME:
		mark_frame (ptr);
		break;

	      case PVEC_HASH_TABLE:
		{
		  struct Lisp_Hash_Table *h = (struct Lisp_Hash_Table *)ptr;
		  set_vector_marked (ptr);
		  if (h->weakness == Weak_None)
		    /* Mark all keys and values */
		    mark_stack_push_values (h->key_and_value,
					    2 * h->table_size);
		  else
		    {
		      /* Defer weak table handling */
		      eassert (h->next_weak == NULL);
		      h->next_weak = weak_hash_tables;
		      weak_hash_tables = h;
		    }
		  break;
		}

	      default:
		mark_vectorlike (&ptr->header);
		break;
	      }
	  }
	  break;

	case Lisp_Cons:
	  {
	    struct Lisp_Cons *ptr = XCONS (obj);
	    if (cons_marked_p (ptr))
	      break;
	    check_allocated_and_live (live_cons_p, MEM_TYPE_CONS, po);
	    set_cons_marked (ptr);
	    /* Optimize tail recursion for lists */
	    mark_object (ptr->u.s.car);
	    obj = ptr->u.s.u.cdr;
	    goto mark_obj;
	  }

	case Lisp_Float:
	  {
	    struct Lisp_Float *f = XFLOAT (obj);
	    if (pdumper_object_p (f))
	      eassert (pdumper_cold_object_p (f));
	    else if (!XFLOAT_MARKED_P (f))
	      XFLOAT_MARK (f);
	    break;
	  }

	case Lisp_Int0:
	case Lisp_Int1:
	  break;

	default:
	  emacs_abort ();
	}
    }
}
```

#### Mark Bits

Different object types use different marking strategies:

**Strings and Vectors**: Mark bit in the size field:

```c
/* From src/alloc.c:265 */
#define XMARK_STRING(S)		((S)->u.s.size |= ARRAY_MARK_FLAG)
#define XUNMARK_STRING(S)	((S)->u.s.size &= ~ARRAY_MARK_FLAG)
#define XSTRING_MARKED_P(S)	(((S)->u.s.size & ARRAY_MARK_FLAG) != 0)

#define XMARK_VECTOR(V)		((V)->header.size |= ARRAY_MARK_FLAG)
#define XUNMARK_VECTOR(V)	((V)->header.size &= ~ARRAY_MARK_FLAG)
#define XVECTOR_MARKED_P(V)	(((V)->header.size & ARRAY_MARK_FLAG) != 0)
```

**Cons Cells**: Bitmap in cons_block:

```c
/* From src/alloc.c:2547 */
#define XCONS_MARKED_P(fptr) \
  GETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX (fptr))

#define XMARK_CONS(fptr) \
  SETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX (fptr))
```

### The Sweep Phase

After marking completes, sweep reclaims unmarked objects:

```c
/* From src/alloc.c:7091 */
static void
gc_sweep (void)
{
  sweep_strings ();
  check_string_bytes (!noninteractive);
  sweep_conses ();
  sweep_floats ();
  sweep_intervals ();
  sweep_symbols ();
  sweep_buffers ();
  sweep_vectors ();
  pdumper_clear_marks ();
  check_string_bytes (!noninteractive);
}
```

#### Sweeping Cons Cells

```c
/* From src/alloc.c:6801 */
static void
sweep_conses (void)
{
  struct cons_block **cprev = &cons_block;
  int lim = cons_block_index;
  object_ct num_free = 0, num_used = 0;

  cons_free_list = 0;

  for (struct cons_block *cblk; (cblk = *cprev); )
    {
      int i = 0;
      int this_free = 0;
      int ilim = (lim + BITS_PER_BITS_WORD - 1) / BITS_PER_BITS_WORD;

      /* Scan the mark bits an int at a time.  */
      for (i = 0; i < ilim; i++)
        {
          if (cblk->gcmarkbits[i] == BITS_WORD_MAX)
            {
              /* Fast path - all cons cells marked.  */
              cblk->gcmarkbits[i] = 0;
              num_used += BITS_PER_BITS_WORD;
            }
          else
            {
              /* Some cons cells not marked - find and free them.  */
              int start = i * BITS_PER_BITS_WORD;
              int stop = min (lim, start + BITS_PER_BITS_WORD);

              for (int pos = start; pos < stop; pos++)
                {
		  struct Lisp_Cons *acons = &cblk->conses[pos];
		  if (!XCONS_MARKED_P (acons))
                    {
		      /* Free this cons */
                      this_free++;
                      cblk->conses[pos].u.s.u.chain = cons_free_list;
                      cons_free_list = &cblk->conses[pos];
                      cons_free_list->u.s.car = dead_object ();
		    }
                  else
                    {
                      num_used++;
		      XUNMARK_CONS (acons);
                    }
                }
            }
        }

      lim = CONS_BLOCK_SIZE;

      /* If block contains only free conses, deallocate it */
      if (this_free == CONS_BLOCK_SIZE && num_free > CONS_BLOCK_SIZE)
        {
          *cprev = cblk->next;
          cons_free_list = cblk->conses[0].u.s.u.chain;
          lisp_align_free (cblk);
        }
      else
        {
          num_free += this_free;
          cprev = &cblk->next;
        }
    }
  gcstat.total_conses = num_used;
  gcstat.total_free_conses = num_free;
}
```

#### Sweeping Strings

String sweeping is more complex due to the two-level allocation:

```c
/* From src/alloc.c:1826 */
static void
sweep_strings (void)
{
  struct string_block *b, *next;
  struct string_block *live_blocks = NULL;

  string_free_list = NULL;
  gcstat.total_strings = gcstat.total_free_strings = 0;
  gcstat.total_string_bytes = 0;

  /* Scan string_blocks, free unmarked Lisp_Strings */
  for (b = string_blocks; b; b = next)
    {
      int i, nfree = 0;
      struct Lisp_String *free_list_before = string_free_list;

      next = b->next;

      for (i = 0; i < STRING_BLOCK_SIZE; ++i)
	{
	  struct Lisp_String *s = b->strings + i;

	  if (s->u.s.data)
	    {
	      /* String was not on free-list before.  */
	      if (XSTRING_MARKED_P (s))
		{
		  /* String is live; unmark it and balance intervals.  */
		  XUNMARK_STRING (s);
		  s->u.s.intervals = balance_intervals (s->u.s.intervals);

		  gcstat.total_strings++;
		  gcstat.total_string_bytes += STRING_BYTES (s);
		}
	      else
		{
		  /* String is dead; free it and mark sdata as dead */
		  sdata *data = SDATA_OF_STRING (s);
		  data->string = NULL;
		  SDATA_NBYTES (data) = STRING_BYTES (s);

		  s->u.s.data = NULL;
		  s->u.next = string_free_list;
		  string_free_list = s;
		  ++nfree;
		}
	    }
	  else
	    {
	      /* String was already free */
	      ++nfree;
	    }
	}

      /* If block is entirely free, release it (keep at least 2) */
      if (nfree == STRING_BLOCK_SIZE && gcstat.total_free_strings > STRING_BLOCK_SIZE)
	{
	  lisp_free (b);
	  string_free_list = free_list_before;
	}
      else
	{
	  gcstat.total_free_strings += nfree;
	  b->next = live_blocks;
	  live_blocks = b;
	}
    }

  string_blocks = live_blocks;

  /* Compact and free string data */
  compact_small_strings ();
  free_large_strings ();
}
```

#### Sweeping Vectors

```c
/* From src/alloc.c:3241 */
static void
sweep_vectors (void)
{
  struct vector_block *block, **bprev = &vector_blocks;
  struct large_vector *lv, **lvprev = &large_vectors;
  struct Lisp_Vector *vector, *next;

  gcstat.total_vectors = 0;
  gcstat.total_vector_slots = gcstat.total_free_vector_slots = 0;
  memset (vector_free_lists, 0, sizeof (vector_free_lists));
  last_inserted_vector_free_idx = VECTOR_FREE_LIST_ARRAY_SIZE;

  /* Sweep vector blocks */
  for (block = vector_blocks; block; block = *bprev)
    {
      bool free_this_block = false;
      ptrdiff_t nbytes;

      for (vector = (struct Lisp_Vector *) block->data;
	   VECTOR_IN_BLOCK (vector, block); vector = next)
	{
	  if (PSEUDOVECTOR_TYPE (vector) == PVEC_FREE)
	    {
	      /* Already free - skip to next */
	      next = ADVANCE (vector, pseudovector_nbytes (&vector->header));
	    }
	  else if (vector_marked_p (vector))
	    {
	      /* Live vector - unmark and count */
	      XUNMARK_VECTOR (vector);

	      gcstat.total_vectors++;
	      nbytes = vectorlike_nbytes (&vector->header);
	      gcstat.total_vector_slots += nbytes / word_size;
	      next = ADVANCE (vector, nbytes);
	    }
	  else
	    {
	      /* Dead vector - clean up and add to free list */
	      ptrdiff_t total_bytes;

	      nbytes = vectorlike_nbytes (&vector->header);
	      total_bytes = nbytes;

	      /* Run cleanup for special vector types */
	      cleanup_vector (vector);

	      /* Coalesce with following free vectors */
	      next = ADVANCE (vector, nbytes);
	      while (VECTOR_IN_BLOCK (next, block)
		     && PSEUDOVECTOR_TYPE (next) == PVEC_FREE)
		{
		  nbytes = pseudovector_nbytes (&next->header);
		  total_bytes += nbytes;
		  next = ADVANCE (next, nbytes);
		}

	      /* Add to appropriate free list */
	      eassert (total_bytes % roundup_size == 0);
	      setup_on_free_list (vector, total_bytes);
	      gcstat.total_free_vector_slots += total_bytes / word_size;
	    }
	}

      /* Keep at least one vector block */
      if (block == vector_blocks && block->next == NULL)
	bprev = &block->next;
      else
	{
	  *bprev = block->next;
	  xfree (block);
	}
    }

  /* Sweep large vectors */
  for (lv = large_vectors; lv; lv = *lvprev)
    {
      vector = large_vector_vec (lv);
      if (vector_marked_p (vector))
	{
	  XUNMARK_VECTOR (vector);
	  gcstat.total_vectors++;
	  gcstat.total_vector_slots += vectorlike_nbytes (&vector->header) / word_size;
	  lvprev = &lv->next;
	}
      else
	{
	  *lvprev = lv->next;
	  cleanup_vector (vector);
	  lisp_free (lv);
	}
    }
}
```

---

## Key Functions Deep Dive

### garbage_collect

See "The Mark-and-Sweep Strategy" section above for the complete implementation.

### mark_object

The core marking primitive. See "The Marking Phase" for details.

### Conservative Stack Scanning

The GC scans the C stack conservatively to find roots:

```c
/* From src/alloc.c:4185 */
/* Conservative C stack marking requires a method to identify possibly
   live Lisp objects given a pointer value.  We do this by keeping
   track of blocks of Lisp data that are allocated in a red-black tree
   (see also the comment of mem_node which is the type of nodes in
   that tree).  Function lisp_malloc adds information for an allocated
   block to the red-black tree with calls to mem_insert, and function
   lisp_free removes it with mem_delete.  Functions live_string_p etc
   call mem_find to lookup information about a given pointer in the
   tree, and use that to determine if the pointer points into a Lisp
   object or not.  */
```

Finding memory regions:

```c
/* From src/alloc.c:4212 */
static struct mem_node *
mem_find (void *start)
{
  struct mem_node *p;

  if (start < min_heap_address || start > max_heap_address)
    return MEM_NIL;

  /* Make the search always successful to speed up the loop below.  */
  mem_z.start = start;
  mem_z.end = (char *) start + 1;

  p = mem_root;
  while (start < p->start || start >= p->end)
    p = start < p->start ? p->left : p->right;
  return p;
}
```

---

## Special Topics

### Weak Hash Tables

Weak hash tables allow keys or values to be collected if not referenced elsewhere:

```c
/* From src/alloc.c:5664 */
/* List of weak hash tables we found during marking the Lisp heap.
   NULL on entry to garbage_collect and after it returns.  */
static struct Lisp_Hash_Table *weak_hash_tables;
```

Weak table processing happens after regular marking:

```c
/* From src/alloc.c:5670 */
static void
mark_and_sweep_weak_table_contents (void)
{
  struct Lisp_Hash_Table *h;
  bool marked;

  /* Mark all keys and values that are in use.  Keep on marking until
     there is no more change.  This is necessary for cases like
     value-weak table A containing an entry X -> Y, where Y is used in a
     key-weak table B, Z -> Y.  If B comes after A in the list of weak
     tables, X -> Y might be removed from A, although when looking at B
     one finds that it shouldn't.  */
  do
    {
      marked = false;
      for (h = weak_hash_tables; h; h = h->next_weak)
        marked |= sweep_weak_table (h, false);
    }
  while (marked);

  /* Remove hash table entries that aren't used.  */
  while (weak_hash_tables)
    {
      h = weak_hash_tables;
      weak_hash_tables = h->next_weak;
      h->next_weak = NULL;
      sweep_weak_table (h, true);
    }
}
```

### Finalizers

Finalizers allow running cleanup code when objects become unreachable:

```c
/* From src/alloc.c:552 */
/* Head of a circularly-linked list of extant finalizers. */
struct Lisp_Finalizer finalizers;

/* Head of a circularly-linked list of finalizers that must be invoked
   because we deemed them unreachable.  This list must be global, and
   not a local inside garbage_collect, in case we GC again while
   running finalizers.  */
struct Lisp_Finalizer doomed_finalizers;
```

During GC, unreachable finalizers are queued:

```c
/* From src/alloc.c:3895 */
static void
queue_doomed_finalizers (struct Lisp_Finalizer *dest,
                         struct Lisp_Finalizer *src)
{
  struct Lisp_Finalizer *finalizer = src->next;
  while (finalizer != src)
    {
      struct Lisp_Finalizer *next = finalizer->next;
      if (!vectorlike_marked_p (&finalizer->header)
          && !NILP (finalizer->function))
        {
          unchain_finalizer (finalizer);
          finalizer_insert (dest, finalizer);
        }
      finalizer = next;
    }
}
```

Then run after GC completes:

```c
/* From src/alloc.c:5960 */
/* GC is complete: now we can run our finalizer callbacks.  */
run_finalizers (&doomed_finalizers);
```

### pdumper Integration

The portable dumper creates a snapshot of Emacs state. Objects in the dump are treated specially:

```c
/* From src/alloc.c:6407 */
if (pdumper_object_p (po))
  {
    if (!pdumper_object_p_precise (po))
      emacs_abort ();
    return;
  }
```

Dumped objects:
- Are never freed
- Don't have mark bits set
- Use special predicates for liveness checks

After sweeping, clear marks for dumped objects:

```c
/* From src/alloc.c:7101 */
pdumper_clear_marks ();
```

### Memory Reserve

Emacs keeps spare memory to handle allocation failures gracefully:

```c
/* From src/alloc.c:332 */
/* Points to memory space allocated as "spare", to be freed if we run
   out of memory.  We keep one large block, four cons-blocks, and
   two string blocks.  */

static char *spare_memory[7];

#define SPARE_MEMORY (1 << 14)
```

On memory exhaustion:

```c
/* From src/alloc.c:4104 */
void
memory_full (size_t nbytes)
{
  if (!initialized)
    fatal ("memory exhausted");

  /* Free the spare memory */
  for (int i = 0; i < ARRAYELTS (spare_memory); i++)
    if (spare_memory[i])
      {
        if (i == 0)
          free (spare_memory[i]);
        else if (i >= 1 && i <= 4)
          lisp_align_free (spare_memory[i]);
        else
          lisp_free (spare_memory[i]);
        spare_memory[i] = 0;
      }

  xsignal (Qnil, Vmemory_signal_data);
}
```

---

## Performance and Tuning

### GC Triggering

GC is triggered when `consing_until_gc` becomes negative:

```c
/* From src/alloc.c:282 */
/* maybe_gc collects garbage if this goes negative.  */
EMACS_INT consing_until_gc;
```

Each allocation decrements this counter:

```c
/* From src/alloc.c:2631 */
consing_until_gc -= sizeof (struct Lisp_Cons);
```

### GC Thresholds

Two variables control when GC runs:

```c
/* From src/alloc.c:7385 */
DEFVAR_INT ("gc-cons-threshold", gc_cons_threshold,
      doc: /* Number of bytes of consing between garbage collections.
Garbage collection can happen automatically once this many bytes have been
allocated since the last garbage collection.  All data types count.

By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.  But be
sure to get back to the normal value soon enough, to avoid system-wide
memory pressure. */);
```

And:

```c
DEFVAR_LISP ("gc-cons-percentage", Vgc_cons_percentage,
      doc: /* Portion of the heap used for allocation.
Garbage collection can happen automatically once this portion of the heap
has been allocated since the last garbage collection.
If this portion is smaller than `gc-cons-threshold', this is ignored.  */);
```

The threshold is calculated dynamically:

```c
/* From src/alloc.c:5703 */
static EMACS_INT
consing_threshold (intmax_t threshold, Lisp_Object percentage,
		   intmax_t since_gc)
{
  if (!NILP (Vmemory_full))
    return memory_full_cons_threshold;
  else
    {
      threshold = max (threshold, GC_DEFAULT_THRESHOLD / 10);
      if (FLOATP (percentage))
	{
	  double tot = (XFLOAT_DATA (percentage)
			* (total_bytes_of_live_objects () + since_gc));
	  if (threshold < tot)
	    {
	      if (tot < HI_THRESHOLD)
		return tot;
	      else
		return HI_THRESHOLD;
	    }
	}
      return min (threshold, HI_THRESHOLD);
    }
}
```

### Avoiding GC Pauses

**Techniques**:

1. **Increase `gc-cons-threshold`** temporarily during performance-critical sections:
   ```elisp
   (let ((gc-cons-threshold most-positive-fixnum))
     ;; Performance-critical code
     ...)
   ```

2. **Pre-allocate** objects when possible to avoid allocation during critical sections

3. **Use `garbage-collection-messages`** to monitor GC frequency:
   ```elisp
   (setq garbage-collection-messages t)
   ```

4. **Inhibit GC** explicitly (use sparingly):
   ```c
   /* From src/alloc.c:341 */
   intptr_t garbage_collection_inhibited;
   ```

5. **Batch allocations** to amortize GC cost

### Memory Profiling

**Built-in tools**:

1. **`garbage-collect`** returns statistics:
   ```elisp
   (garbage-collect)
   ;; => ((conses 16 274839 55940)
   ;;     (symbols 48 22252 3)
   ;;     (strings 32 72874 4451)
   ;;     ...)
   ```

2. **`memory-use-counts`** shows allocation counts:
   ```c
   /* From src/alloc.c:7163 */
   DEFUN ("memory-use-counts", Fmemory_use_counts, ...)
   ```

3. **`memory-info`** shows system memory:
   ```c
   /* From src/alloc.c:7105 */
   DEFUN ("memory-info", Fmemory_info, ...)
   ```

**Statistics tracked**:

```c
/* From src/alloc.c:308 */
static struct gcstat
{
  object_ct total_conses, total_free_conses;
  object_ct total_symbols, total_free_symbols;
  object_ct total_strings, total_free_strings;
  byte_ct total_string_bytes;
  object_ct total_vectors, total_vector_slots, total_free_vector_slots;
  object_ct total_floats, total_free_floats;
  object_ct total_intervals, total_free_intervals;
  object_ct total_buffers;
  byte_ct total_hash_table_bytes;
} gcstat;
```

### Common Patterns

**Pattern 1: Temporary High Threshold**
```elisp
(defun process-large-data (data)
  (let ((gc-cons-threshold (* 100 1024 1024))) ; 100MB
    (process data)))
```

**Pattern 2: Explicit GC Between Tasks**
```elisp
(defun batch-processor (items)
  (dolist (item items)
    (process-item item)
    (garbage-collect))) ; Clean up between items
```

**Pattern 3: Monitor GC Performance**
```elisp
(let ((start-time (float-time))
      (gc-start (garbage-collect)))
  ;; Do work
  (let ((gc-end (garbage-collect)))
    (message "GC diff: %S, Time: %.2fs"
             (mapcar (lambda (a b)
                      (list (car a)
                            (- (nth 2 a) (nth 2 b))))
                    gc-end gc-start)
             (- (float-time) start-time))))
```

### Performance Characteristics

**Allocation Costs**:
- **Cons**: O(1) from free list, O(1) amortized for new blocks
- **String**: O(1) for struct, O(n) for data
- **Vector**: O(1) from free list, O(log n) to find free space
- **Symbol**: O(1) from free list

**GC Costs**:
- **Mark Phase**: O(live objects), depth-first traversal
- **Sweep Phase**: O(all allocated objects)
- **Total**: O(heap size), not generational

**Memory Overhead**:
- Cons: ~16 bytes + mark bit
- String: ~32 bytes struct + data + alignment
- Vector: header + contents + alignment
- Symbol: ~48 bytes

---

## Summary

Emacs's memory management system is a carefully tuned implementation that balances:

1. **Performance**: Fast allocation via free lists and block allocation
2. **Simplicity**: Non-copying GC works well with C integration
3. **Flexibility**: Multiple specialized allocators for different types
4. **Debugging**: Comprehensive checking and statistics

Key insights:

- **Block allocation** minimizes malloc overhead and fragmentation
- **Free lists** make allocation O(1) for common cases
- **Mark-and-sweep** is simple, predictable, and C-friendly
- **Conservative stack scanning** handles C/Lisp interaction safely
- **Weak references** and **finalizers** provide advanced memory management
- **pdumper integration** enables fast startup with pre-allocated objects

For most Elisp code, the GC is transparent and efficient. Understanding these internals helps when:
- Optimizing performance-critical code
- Debugging memory issues
- Interfacing with C code
- Tuning GC parameters for specific workloads

The implementation in `src/alloc.c` is a masterclass in systems programming, balancing decades of evolution with modern performance requirements.
