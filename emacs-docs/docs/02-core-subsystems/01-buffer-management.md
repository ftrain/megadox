# Buffer Management Subsystem

## Table of Contents

1. [Introduction](#introduction)
2. [The Gap Buffer: Core Data Structure](#the-gap-buffer-core-data-structure)
3. [Buffer Structure and Organization](#buffer-structure-and-organization)
4. [Text Insertion and Deletion](#text-insertion-and-deletion)
5. [The Marker System](#the-marker-system)
6. [Text Properties and Intervals](#text-properties-and-intervals)
7. [Buffer-Local Variables](#buffer-local-variables)
8. [Buffers and Windows](#buffers-and-windows)
9. [The Elisp Layer](#the-elisp-layer)
10. [Design Rationale](#design-rationale)

---

## Introduction

The buffer management subsystem is the heart of Emacs text editing. A **buffer** is Emacs' fundamental data structure for representing editable text. Every piece of text you see in Emacs—whether it's a file, a directory listing, a shell session, or temporary scratch space—lives in a buffer.

### Core Responsibilities

The buffer subsystem handles:
- **Efficient text storage** using the gap buffer data structure
- **Position tracking** through the marker system
- **Text properties** via interval trees
- **Buffer-local state** including variables, modes, and keymaps
- **Integration with the display** system and windows

### Key Design Principles

1. **Efficiency for interactive editing**: Most operations (insertion/deletion at point) are O(1)
2. **Multibyte character support**: Seamless handling of Unicode
3. **Undo support**: All modifications are tracked
4. **Separation of concerns**: Text storage is independent of display

---

## The Gap Buffer: Core Data Structure

### Concept

The **gap buffer** is an elegant data structure optimized for text editing. Instead of using a simple array or linked list, it maintains a "gap" (empty space) in the middle of the buffer text. This gap moves to wherever the user is editing, making insertions and deletions at that point extremely fast.

### Visual Representation

```
Without a gap (conceptual):
[H][e][l][l][o][ ][w][o][r][l][d]
                 ↑ cursor here

With a gap buffer:
[H][e][l][l][o][ ][ ][ ][ ][ ][w][o][r][l][d]
                 ↑               ↑
                GPT          GAP_END

The gap provides space for fast insertion without reallocating.
```

### Implementation Details

The gap buffer implementation is split between `struct buffer_text` (src/buffer.h:240-304) and the gap manipulation functions (src/insdel.c).

**Data Structure** (src/buffer.h:240-304):

```c
struct buffer_text
{
    unsigned char *beg;        /* Actual address of buffer contents */

    ptrdiff_t gpt;             /* Char pos of gap in buffer */
    ptrdiff_t z;               /* Char pos of end of buffer */
    ptrdiff_t gpt_byte;        /* Byte pos of gap in buffer */
    ptrdiff_t z_byte;          /* Byte pos of end of buffer */
    ptrdiff_t gap_size;        /* Size of buffer's gap */

    modiff_count modiff;       /* Modification counter */
    modiff_count chars_modiff; /* Character change counter */

    INTERVAL intervals;        /* Text properties tree */
    struct Lisp_Marker *markers; /* Chain of markers */

    // ... additional fields
};
```

**Key Invariants:**
- The gap starts at byte position `gpt_byte` and extends for `gap_size` bytes
- Buffer text before the gap: `[BEG_BYTE, gpt_byte)`
- Buffer text after the gap: `[gpt_byte + gap_size, z_byte + gap_size)`
- Total buffer size: `z_byte` (excluding gap)

### Critical Macros (src/buffer.h:38-94)

```c
/* Position of beginning of buffer (always 1 in char positions) */
enum { BEG = 1, BEG_BYTE = BEG };

/* Position of point in buffer */
#define PT (current_buffer->pt + 0)          /* Make it non-lvalue */
#define PT_BYTE (current_buffer->pt_byte + 0)

/* Position of gap in buffer */
#define GPT (current_buffer->text->gpt)
#define GPT_BYTE (current_buffer->text->gpt_byte)

/* Position of end of buffer */
#define Z (current_buffer->text->z)
#define Z_BYTE (current_buffer->text->z_byte)

/* Size of the gap */
#define GAP_SIZE (current_buffer->text->gap_size)
```

**Why the "+ 0" trick?**
Making `PT` non-assignable (src/buffer.h:44-47) prevents accidental direct assignment. You must use `SET_PT()` instead, which properly updates all related state (markers, text properties, display, etc.).

### Address Calculation

Getting the actual memory address of a buffer position requires accounting for the gap (src/buffer.h:1072-1078):

```c
INLINE unsigned char *
BYTE_POS_ADDR (ptrdiff_t n)
{
  return (n < GPT_BYTE ? 0 : GAP_SIZE) + n + BEG_ADDR - BEG_BYTE;
}
```

This is crucial because:
- Positions before the gap map directly to memory
- Positions after the gap must skip over the gap in memory
- The calculation is extremely fast (no branches on modern CPUs)

### Gap Movement

When you edit text at a different location, the gap must move. This is handled by `gap_left()` and `gap_right()` (src/insdel.c:104-220).

**Moving the gap left** (src/insdel.c:110-166):
```c
static void
gap_left (ptrdiff_t charpos, ptrdiff_t bytepos, bool newgap)
{
  unsigned char *to, *from;
  ptrdiff_t i;
  ptrdiff_t new_s1;

  if (!newgap)
    BUF_COMPUTE_UNCHANGED (current_buffer, charpos, GPT);

  i = GPT_BYTE;
  to = GAP_END_ADDR;
  from = GPT_ADDR;
  new_s1 = GPT_BYTE;

  /* Copy characters up to move the gap down */
  while (1)
    {
      i = new_s1 - bytepos;
      if (i == 0)
        break;
      /* Check for quit every 32KB */
      if (QUITP) { /* ... */ }
      if (i > 32000)
        i = 32000;
      new_s1 -= i;
      from -= i, to -= i;
      memmove (to, from, i);
    }

  GPT_BYTE = bytepos;
  GPT = charpos;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor */
}
```

**Design notes:**
- Uses `memmove()` for safe overlapping memory copy
- Processes in 32KB chunks to allow `C-g` to interrupt long moves
- Puts a null byte anchor at the gap start for C string safety
- The "newgap" parameter is used when creating/expanding the gap

---

## Buffer Structure and Organization

### The `struct buffer` (src/buffer.h:319-743)

Every buffer in Emacs is represented by a `struct buffer`. This is a large structure with two main categories of data:

1. **Lisp-visible fields**: Buffer name, filename, modes, keymaps, etc.
2. **Internal fields**: Text storage, markers, position tracking, etc.

**Core fields** (src/buffer.h:319-627):

```c
struct buffer
{
  union vectorlike_header header;  /* For Lisp GC */

  /* === Lisp-visible buffer properties === */
  Lisp_Object name_;              /* Buffer name */
  Lisp_Object filename_;          /* Visited file name */
  Lisp_Object directory_;         /* Default directory */
  Lisp_Object mode_name_;         /* Mode name ("Emacs-Lisp", "C", etc.) */
  Lisp_Object major_mode_;        /* Major mode symbol */
  Lisp_Object keymap_;            /* Local keymap */
  Lisp_Object syntax_table_;      /* Syntax table */
  Lisp_Object mark_;              /* The mark (a marker) */
  Lisp_Object local_var_alist_;   /* Buffer-local variables */

  /* === Text storage === */
  struct buffer_text own_text;    /* This buffer's text */
  struct buffer_text *text;       /* Points to own_text or shared text */

  /* === Position tracking === */
  ptrdiff_t pt;                   /* Point (character position) */
  ptrdiff_t pt_byte;              /* Point (byte position) */
  ptrdiff_t begv;                 /* Beginning of visible region */
  ptrdiff_t begv_byte;            /* BEGV in bytes */
  ptrdiff_t zv;                   /* End of visible region */
  ptrdiff_t zv_byte;              /* ZV in bytes */

  /* === Buffer relationships === */
  struct buffer *base_buffer;     /* For indirect buffers */
  int indirections;               /* Number of indirect buffers */
  int window_count;               /* Number of windows showing this */

  /* === Overlays === */
  struct itree_tree *overlays;    /* Interval tree of overlays */
};
```

### Buffer Allocation (src/buffer.c:595-682)

When you create a buffer with `get-buffer-create`, here's what happens (src/buffer.c:595-682):

```c
DEFUN ("get-buffer-create", Fget_buffer_create, ...)
{
  // ... check if buffer exists ...

  b = allocate_buffer();

  /* An ordinary buffer uses its own text storage */
  b->text = &b->own_text;
  b->base_buffer = NULL;
  b->indirections = 0;
  b->window_count = 0;

  /* Allocate gap buffer with initial gap of 20 bytes */
  BUF_GAP_SIZE (b) = 20;
  alloc_buffer_text (b, BUF_GAP_SIZE (b) + 1);  /* +1 for null terminator */

  /* Initialize positions - empty buffer */
  b->pt = BEG;
  b->begv = BEG;
  b->zv = BEG;
  b->pt_byte = BEG_BYTE;
  b->begv_byte = BEG_BYTE;
  b->zv_byte = BEG_BYTE;

  BUF_GPT (b) = BEG;
  BUF_GPT_BYTE (b) = BEG_BYTE;
  BUF_Z (b) = BEG;
  BUF_Z_BYTE (b) = BEG_BYTE;

  /* Initialize modification counters */
  BUF_MODIFF (b) = 1;
  BUF_CHARS_MODIFF (b) = 1;
  BUF_OVERLAY_MODIFF (b) = 1;
  BUF_SAVE_MODIFF (b) = 1;

  /* Put anchor null bytes */
  *(BUF_GPT_ADDR (b)) = *(BUF_Z_ADDR (b)) = 0;

  // ... set up buffer-local variables ...

  return buffer;
}
```

**Initial state visualization:**
```
Empty buffer:
[GAP: 20 bytes]['\0']
^              ^
BEG,GPT        Z
pt=1, z=1, gap_size=20
```

### Narrowing and Accessible Region

Emacs supports **narrowing**: restricting editing to a subset of the buffer. This is tracked by BEGV (beginning of visible) and ZV (end of visible) (src/buffer.h:40-55):

```
Full buffer:    [BEG .... BEGV ......... ZV .... Z]
                         ^               ^
                    Visible region (narrowed)
```

The visible region is the only part accessible to most editing commands. This enables:
- Restricting syntax highlighting to visible text
- Limiting search/replace operations
- Implementing "widening" and "narrowing" commands

---

## Text Insertion and Deletion

### Core Insertion Function

All text insertion ultimately goes through `insert_1_both()` (referenced in src/insdel.c:681-691). The process is:

1. **Prepare the buffer** for modification (undo, read-only checks)
2. **Ensure gap is big enough** (expand if needed)
3. **Move gap to insertion point** (if not already there)
4. **Copy text into the gap** and adjust gap pointers
5. **Update markers** and text properties
6. **Signal changes** for undo and redisplay

**Simplified insertion flow:**

```c
void insert(const char *string, ptrdiff_t nbytes)
{
  if (nbytes > 0)
    {
      ptrdiff_t len = chars_in_text(string, nbytes);

      // Core insertion with:
      // - string: text to insert
      // - len: character count
      // - nbytes: byte count
      // - inherit=0: don't inherit properties
      // - prepare=1: prepare buffer for change
      // - before_markers=0: normal marker adjustment
      insert_1_both(string, len, nbytes, 0, 1, 0);

      ptrdiff_t opoint = PT - len;
      signal_after_change(opoint, 0, len);
      update_compositions(opoint, PT, CHECK_BORDER);
    }
}
```

### Making the Gap Larger (src/insdel.c:467-512)

When there's not enough space in the gap for an insertion:

```c
static void
make_gap_larger (ptrdiff_t nbytes_added)
{
  ptrdiff_t current_size = Z_BYTE - BEG_BYTE + GAP_SIZE;

  if (BUF_BYTES_MAX - current_size < nbytes_added)
    buffer_overflow();

  /* Get enough space to last a while */
  nbytes_added = min(nbytes_added + GAP_BYTES_DFL,
                     BUF_BYTES_MAX - current_size);

  enlarge_buffer_text(current_buffer, nbytes_added);

  /* Prevent quitting during gap manipulation */
  Vinhibit_quit = Qt;

  /* Move gap to end, add space, move back */
  real_gap_loc = GPT;
  GPT = Z + GAP_SIZE;
  GAP_SIZE = nbytes_added;
  gap_left(real_gap_loc + old_gap_size, ...);
  GAP_SIZE += old_gap_size;

  Vinhibit_quit = tem;
}
```

**Strategy:**
- Add extra space (GAP_BYTES_DFL = 2000 bytes) beyond what's immediately needed
- This amortizes the cost of reallocation
- For large operations, add space proportional to buffer size (up to Z/64)
- This prevents O(n²) behavior when repeatedly growing a buffer

### Deletion

Deletion is even simpler than insertion (conceptually):

1. **Move gap to deletion point**
2. **Expand gap to include deleted text**
3. **Update markers** to collapse onto deletion point or move after it
4. **Update text properties**

The deleted text is now "in the gap" and will be overwritten by future insertions.

### Character vs. Byte Positions

Emacs supports multibyte characters (Unicode), so it tracks both:
- **Character positions**: What users think of as positions (pt, z, begv, zv)
- **Byte positions**: Actual memory offsets (pt_byte, z_byte, etc.)

In a unibyte buffer, these are identical. In a multibyte buffer:
- ASCII characters: 1 byte each
- Unicode characters: 1-4 bytes each (UTF-8 encoding)

**Conversion** is handled by `buf_charpos_to_bytepos()` and `buf_bytepos_to_charpos()` (src/marker.c:167-421), which use a clever optimization: they search from the **nearest known position** (PT, GPT, BEGV, ZV, or markers) rather than always scanning from the beginning.

---

## The Marker System

### What Are Markers?

A **marker** is a position in a buffer that automatically updates when text is inserted or deleted. This is essential for:
- Maintaining point in non-current buffers
- Implementing the mark (for regions)
- Tracking positions for overlays and text properties
- Undo system position tracking

### Marker Structure (src/lisp.h, referenced in src/buffer.h:288-295)

```c
struct Lisp_Marker
{
  /* Core position tracking */
  ptrdiff_t charpos;           /* Character position */
  ptrdiff_t bytepos;           /* Byte position */

  /* Buffer linkage */
  struct buffer *buffer;       /* Which buffer */
  struct Lisp_Marker *next;    /* Next marker in buffer's chain */

  /* Behavior flags */
  bool_bf insertion_type : 1;  /* Advance on insertion? */
};
```

All markers for a buffer are linked in a singly-linked list starting from `buffer->text->markers` (src/buffer.h:295).

### Marker Adjustment

When text is inserted or deleted, all markers must be updated. This is done by functions in src/insdel.c:

**For insertion** (src/insdel.c:287-316):
```c
void adjust_markers_for_insert(ptrdiff_t from, ptrdiff_t from_byte,
                               ptrdiff_t to, ptrdiff_t to_byte,
                               bool before_markers)
{
  struct Lisp_Marker *m;
  ptrdiff_t nchars = to - from;
  ptrdiff_t nbytes = to_byte - from_byte;

  for (m = BUF_MARKERS(current_buffer); m; m = m->next)
    {
      if (m->bytepos == from_byte)
        {
          /* At insertion point: advance if marker says so */
          if (m->insertion_type || before_markers)
            {
              m->bytepos = to_byte;
              m->charpos = to;
            }
        }
      else if (m->bytepos > from_byte)
        {
          /* After insertion: shift forward */
          m->bytepos += nbytes;
          m->charpos += nchars;
        }
    }
}
```

**For deletion** (src/insdel.c:249-276):
```c
void adjust_markers_for_delete(ptrdiff_t from, ptrdiff_t from_byte,
                               ptrdiff_t to, ptrdiff_t to_byte)
{
  struct Lisp_Marker *m;

  for (m = BUF_MARKERS(current_buffer); m; m = m->next)
    {
      if (m->charpos > to)
        {
          /* After deletion: shift backward */
          m->charpos -= to - from;
          m->bytepos -= to_byte - from_byte;
        }
      else if (m->charpos > from)
        {
          /* Inside deletion: collapse to deletion point */
          m->charpos = from;
          m->bytepos = from_byte;
        }
    }
}
```

### Position Caching with Markers

A clever optimization: when converting between character and byte positions (src/marker.c:167-270), the system searches through **existing markers** to find one close to the target position. If the search covered a long distance (>5000 positions), it **creates a new marker** to cache that position for future lookups.

These cache markers are temporary and get garbage collected normally, but while they exist, they dramatically speed up repeated position conversions.

---

## Text Properties and Intervals

### Concept

**Text properties** allow attaching arbitrary data to ranges of text:
- Font faces for syntax highlighting
- Mouse click handlers
- Help text tooltips
- Read-only regions
- Invisible text

The challenge: efficiently storing and querying properties for potentially millions of characters.

### The Interval Tree Data Structure

Emacs uses a specialized balanced binary tree where each node (an "interval") represents a contiguous range of text with identical properties (src/intervals.h:29-66):

```c
struct interval
{
  /* Tree structure */
  ptrdiff_t total_length;      /* Length of this + children */
  ptrdiff_t position;          /* Cache of character position */
  struct interval *left;       /* Preceding intervals */
  struct interval *right;      /* Following intervals */

  /* Parent (either another interval or the containing object) */
  union {
    struct interval *interval;
    Lisp_Object obj;           /* Buffer or string */
  } up;
  bool_bf up_obj : 1;          /* Is parent an object? */

  /* Cached property flags (for speed) */
  bool_bf write_protect : 1;
  bool_bf visible : 1;
  bool_bf front_sticky : 1;
  bool_bf rear_sticky : 1;

  /* The actual properties */
  Lisp_Object plist;           /* Property list */
};
```

**Visual representation:**
```
Text: "Hello world"
Properties:
  [0-5):  face=bold
  [5-6):  no properties
  [6-11): face=italic

Interval tree:
         [0-11: total_len=11]
              /          \
    [0-5: bold]          [5-11]
                        /      \
                  [5-6: nil]  [6-11: italic]
```

### Key Invariants (src/intervals.h:99-119)

```c
/* Total length includes self and all children */
#define TOTAL_LENGTH(i) ((i)->total_length)

/* Length of this interval alone */
#define LENGTH(i) (TOTAL_LENGTH(i)     \
                   - RIGHT_TOTAL_LENGTH(i) \
                   - LEFT_TOTAL_LENGTH(i))
```

The tree is kept **balanced** through rotations (src/intervals.c:269-300+), ensuring O(log n) operations.

### Interval Operations

**Finding an interval** at a position (src/intervals.c, referenced in src/intervals.h:262):
```c
INTERVAL find_interval(INTERVAL tree, ptrdiff_t position)
{
  /* Binary search through the tree */
  while (!LEAF_INTERVAL_P(tree))
    {
      if (position < LEFT_TOTAL_LENGTH(tree))
        tree = tree->left;
      else
        {
          position -= LEFT_TOTAL_LENGTH(tree) + LENGTH(tree);
          tree = tree->right;
        }
    }
  return tree;
}
```

**Splitting an interval** (src/intervals.h:259-261) when inserting text with different properties creates new tree nodes and rebalances.

**Merging adjacent intervals** (src/intervals.h:265) with identical properties saves memory and speeds up searches.

### Property Inheritance and Stickiness

When you insert text at a boundary between two intervals, which properties should the new text inherit? This is controlled by **stickiness** (src/intervals.h:62-64):

- `front_sticky`: Properties stick to text inserted before the interval
- `rear_sticky`: Properties stick to text inserted after the interval

Example:
```
Text: "AB"
      [A: face=bold, front_sticky=true]
      [B: face=italic]

Insert "X" between A and B:
Result: "AXB"
        [AX: face=bold]  ← X inherited bold because of front_sticky
        [B: face=italic]
```

---

## Buffer-Local Variables

### Concept

Most Emacs Lisp variables have a single global value. But some variables can have different values in different buffers. Examples:
- `major-mode`: C-mode vs. Lisp-mode vs. Text-mode
- `tab-width`: Different indentation per buffer
- `case-fold-search`: Case-sensitive search in some buffers

### Implementation Strategy

Emacs uses two approaches for buffer-local variables:

1. **Built-in per-buffer variables** (≤50 variables): Stored directly in `struct buffer`
2. **Lisp-level buffer-local variables** (unlimited): Stored in `local_var_alist`

### Built-in Per-Buffer Variables (src/buffer.h:310-643)

The most commonly used buffer-local variables are stored as actual fields in `struct buffer`:

```c
struct buffer
{
  // ... many fields ...
  Lisp_Object mode_line_format_;
  Lisp_Object abbrev_mode_;
  Lisp_Object tab_width_;
  Lisp_Object fill_column_;
  Lisp_Object syntax_table_;
  // ... etc ...
};
```

**Note the trailing underscore:** You access these through the `BVAR()` macro (src/buffer.h:308):
```c
#define BVAR(buf, field) ((buf)->field ## _)

// Usage:
BVAR(current_buffer, mode_line_format)  // → current_buffer->mode_line_format_
```

This indirection allows future refactoring of storage without changing call sites.

### The Per-Buffer Index System

Each built-in per-buffer variable has an **index** stored in `buffer_local_flags` (src/buffer.c:89). This index is used in the `local_flags` array (src/buffer.h:643) to track whether a buffer has overridden the default:

```c
/* In struct buffer: */
char local_flags[MAX_PER_BUFFER_VARS];  // 50 elements

/* Usage: */
if (PER_BUFFER_VALUE_P(buffer, idx))
  /* This buffer has a local value for variable idx */
else
  /* Use default from buffer_defaults */
```

### Lisp-Level Buffer-Local Variables (src/buffer.h:362)

For variables not built into the structure:

```c
Lisp_Object local_var_alist_;  /* Alist of (SYMBOL . VALUE) */
```

When you do `(make-variable-buffer-local 'foo)` or `(setq-local foo value)`, the variable is added to this alist for the current buffer.

### Default Values (src/buffer.c:70)

```c
struct buffer buffer_defaults;
```

This is a global `struct buffer` that holds default values. When a buffer doesn't have a local value for a variable, it uses the value from `buffer_defaults`.

---

## Buffers and Windows

### Conceptual Relationship

A **buffer** holds text. A **window** displays a buffer. The relationship is many-to-many:
- One buffer can be displayed in multiple windows
- One window displays exactly one buffer at a time
- A buffer can exist without being displayed in any window

### Tracking Window Display (src/buffer.h:634-636)

```c
struct buffer
{
  int window_count;  /* Number of windows showing this buffer */
  // ...
};
```

This counter is updated by the window system when windows are created/deleted or their buffers changed.

### Point in Non-Current Buffers

**Challenge:** Point (PT) is a single position, but a buffer might be displayed in multiple windows with different points.

**Solution:** Each window has its own `pointm` marker (src/window.h). When a buffer is not current:
- Its PT is saved to `pt_marker` (src/buffer.h:492)
- Each window showing it has its own point in its `pointm`

When you switch to a buffer in a window:
1. Current buffer's PT → saved to current window's `pointm`
2. New buffer's `pt_marker` → restored to PT (if buffer wasn't current)
3. Or window's `pointm` → PT (if buffer was already current elsewhere)

### Indirect Buffers (src/buffer.h:599-632)

An **indirect buffer** shares text storage with a **base buffer**:

```c
struct buffer
{
  struct buffer_text own_text;     /* Storage for ordinary buffers */
  struct buffer_text *text;        /* Points to own_text or base->own_text */

  struct buffer *base_buffer;      /* NULL for ordinary buffers */
  int indirections;                /* Count of indirect buffers sharing our text */
};
```

**Uses:**
- Multiple views of the same text with different narrowing
- Different modes on the same text (e.g., C mode vs. text mode)
- Avoiding text duplication

**Key point:** Indirect buffers share:
- Text content
- Markers
- Text properties

But have separate:
- Point, mark, narrowing
- Major mode
- Buffer-local variables

---

## The Elisp Layer

### Buffer Menu (lisp/buff-menu.el)

The traditional buffer list (`C-x C-b`) is implemented in buff-menu.el. It uses `tabulated-list-mode` to display:
- Buffer names
- Sizes
- Modes
- Associated files

**Key structure** (lisp/buff-menu.el:165-200):
```elisp
(defvar-keymap Buffer-menu-mode-map
  :doc "Local keymap for Buffer-menu-mode buffers."
  :parent tabulated-list-mode-map
  "d"   #'Buffer-menu-delete      ; Mark for deletion
  "s"   #'Buffer-menu-save        ; Save buffer
  "x"   #'Buffer-menu-execute     ; Execute marks
  "f"   #'Buffer-menu-this-window ; Visit buffer
  ;; ... many more commands ...
  )
```

### IBuffer (lisp/ibuffer.el)

IBuffer is an advanced buffer list with:
- **Filtering**: Show only buffers matching criteria
- **Grouping**: Organize by mode, directory, etc.
- **Marking**: Dired-like bulk operations

**Design** (lisp/ibuffer.el:86-154):
```elisp
(defcustom ibuffer-formats
  '((mark modified read-only locked
          " " (name 18 18 :left :elide)
          " " (size 9 -1 :right)
          " " (mode 16 16 :left :elide)
          " " filename-and-process))
  "List of ways to display buffer lines.
Each format specifies columns to display...")
```

The format is extensible through `define-ibuffer-column`, allowing users to add custom columns.

### Basic Editing Commands (lisp/simple.el)

simple.el contains fundamental editing operations that work with buffers:
- Movement: `beginning-of-line`, `end-of-line`, `forward-word`
- Insertion: `newline`, `self-insert-command`
- Deletion: `delete-backward-char`, `delete-forward-char`
- Killing: `kill-line`, `kill-region`

These are mostly thin wrappers around C primitives, but add:
- Interactive specifications (for `M-x` and keybindings)
- Argument handling (prefix arguments)
- Integration with kill ring, undo, etc.

---

## Design Rationale

### Why the Gap Buffer?

**Alternatives considered:**
1. **Simple array**: O(n) insertion/deletion
2. **Linked list**: O(1) insertion/deletion but poor cache locality
3. **Rope** (tree of strings): Complex, good for large files
4. **Piece table**: Good for undo, used by some editors

**Why gap buffer wins for Emacs:**
- Interactive editing is usually localized (typing in one spot)
- Gap moves to edit point → O(1) for common case
- Simple implementation (critical for 1980s)
- Excellent cache locality for sequential access
- Easy to implement multibyte character support

### Why Separate Character and Byte Positions?

Before Unicode, Emacs used only character positions. With multibyte support:
- **Option 1**: Convert on every access (too slow)
- **Option 2**: Track both positions everywhere (chosen approach)

The dual tracking adds complexity but enables:
- Fast memory access (use byte positions)
- Correct character semantics (use char positions)
- Optimization: in unibyte buffers, they're identical

### Why Intervals Instead of Simpler Property Storage?

**Alternatives:**
1. **Property per character**: Too much memory (millions of characters)
2. **Hash table of ranges**: Hard to update efficiently
3. **Interval tree** (chosen): Automatic merging of adjacent identical properties

**Benefits:**
- O(log n) queries
- Automatic memory optimization (merging)
- Efficient updates (splitting/merging only what's needed)

The complexity is high but unavoidable given the requirements:
- Millions of characters
- Hundreds of thousands of properties (syntax highlighting, etc.)
- Interactive responsiveness

### Why Buffer-Local Variables?

Modes need different behavior in different buffers. Options:
1. **Global variables**: Doesn't work (conflicts between buffers)
2. **Object-oriented**: Each mode is an object with methods
3. **Buffer-local variables** (chosen): Lisp-friendly, flexible

The current design allows:
- Gradual migration (start global, make buffer-local as needed)
- Easy customization (same variable name everywhere)
- Efficient built-in variables (in struct buffer)
- Unlimited Lisp-level variables (in alist)

---

## Summary: Key Insights

### Data Structure Hierarchy

```
struct buffer                  (The buffer object)
  ├─ struct buffer_text        (Gap buffer storage)
  │   ├─ unsigned char *beg    (Actual memory)
  │   ├─ gap position/size     (Gap metadata)
  │   ├─ INTERVAL intervals    (Property tree root)
  │   └─ Lisp_Marker *markers  (Marker chain)
  ├─ Position state            (pt, begv, zv)
  ├─ Buffer-local vars         (Built-in + alist)
  └─ Display state             (windows, overlays)
```

### Critical Invariants

1. **Gap invariant**: `text->beg` always points to allocated memory containing all buffer text except the gap
2. **Position ordering**: `BEG ≤ BEGV ≤ PT ≤ ZV ≤ Z`
3. **Byte/char relationship**: In unibyte buffers, char_pos == byte_pos
4. **Modification counts**: Incremented on every change, used for:
   - Undo system
   - Display optimization
   - Cache invalidation

### Performance Characteristics

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| Insert at point | O(1) amortized | Gap is already there |
| Insert elsewhere | O(n) | Must move gap |
| Delete at point | O(1) | Expand gap |
| Find marker at position | O(m) | m = # of markers |
| Get text property | O(log n) | Interval tree search |
| Convert char↔byte pos | O(m + d) | m = marker search, d = distance to scan |

### Code Organization

| File | Primary Responsibility |
|------|----------------------|
| src/buffer.c | Buffer creation, switching, management |
| src/buffer.h | Buffer structure definitions |
| src/insdel.c | Gap buffer mechanics, insertion/deletion |
| src/marker.c | Marker operations, char/byte conversion |
| src/intervals.c | Interval tree algorithms |
| src/textprop.c | Text property API (uses intervals) |
| lisp/simple.el | Basic editing commands |
| lisp/buff-menu.el | Traditional buffer list |
| lisp/ibuffer.el | Advanced buffer list |

---

## Further Reading

For deeper understanding:

1. **Gap buffer tutorial**: src/insdel.c contains detailed comments
2. **Interval tree operations**: src/intervals.c has extensive documentation
3. **Multibyte character handling**: Look at character.c and character.h
4. **Undo system integration**: See undo.c for how it uses buffer modification counts
5. **Display integration**: See xdisp.c for how the display system uses buffers

**Historical context:**
- Original gap buffer: TECO editor (1960s)
- Emacs adoption: Richard Stallman's original Emacs (1976)
- Multibyte support: Added in Emacs 20 (1997)
- Interval tree: Added for text properties in Emacs 19 (1993)
