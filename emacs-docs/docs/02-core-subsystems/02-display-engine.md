# The Emacs Display Engine: A Literate Programming Guide

## Table of Contents

1. [Introduction](#introduction)
2. [Architecture Overview](#architecture-overview)
3. [The Redisplay Cycle](#the-redisplay-cycle)
4. [Glyph Matrices: The Heart of Display](#glyph-matrices)
5. [The Display Iterator](#the-display-iterator)
6. [Face Management and Realization](#face-management)
7. [Bidirectional Text Rendering](#bidirectional-text)
8. [Line Wrapping and Truncation](#line-wrapping)
9. [Fringe Indicators](#fringe-indicators)
10. [Performance Optimizations](#performance-optimizations)
11. [Window System Integration](#window-system-integration)

---

## Introduction

The Emacs display engine is one of the most sophisticated text rendering systems ever built. Originally written by Gerd Moellmann and refined over decades, it handles the complex task of transforming buffer content into pixels on screen while maintaining exceptional performance even with large files.

This document provides a literate programming perspective on the display engine, weaving together code excerpts with detailed explanations of the algorithms and data structures that make Emacs's display capabilities possible.

### Core Principles

The display engine is built on three fundamental principles:

1. **Separation of Concerns**: Display code is completely separate from buffer-modifying code. Functions that modify buffers don't need to worry about updating the display.

2. **Incremental Updates**: Only the portions of the display that have changed are redrawn, minimizing expensive redraw operations.

3. **Abstract Display Elements**: The engine works with abstract "glyphs" that can represent characters, images, or other display elements uniformly.

### Source Files

The display engine comprises approximately 50,000 lines of carefully optimized C code spread across several files:

| File | Lines | Purpose |
|------|-------|---------|
| `src/xdisp.c` | ~39,000 | Core redisplay logic, window updating |
| `src/dispnew.c` | ~7,000 | Glyph matrix management, screen updates |
| `src/xfaces.c` | ~6,000 | Face management, font selection |
| `src/indent.c` | ~2,000 | Indentation, column calculations |
| `src/bidi.c` | ~2,500 | Bidirectional text support |
| `src/fringe.c` | ~1,500 | Fringe bitmap management |

---

## Architecture Overview

### The Display Pipeline

The display process follows a three-phase pipeline as documented in `xdisp.c`:

```
┌─────────────────────────────────────────────────────────────────┐
│                    Phase 1: Decide What to Redisplay            │
│  redisplay_internal() examines frames and windows to determine  │
│  which ones need updating based on flags and buffer changes     │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│              Phase 2: Build Desired Glyph Matrices              │
│  redisplay_window() constructs the "desired matrix" describing  │
│  how each window should appear on screen                        │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│               Phase 3: Update Physical Display                  │
│  update_frame() compares desired vs current matrices and        │
│  performs minimal screen updates to show the changes            │
└─────────────────────────────────────────────────────────────────┘
```

From `src/xdisp.c:78-88`:

```c
/*
   At its highest level, redisplay can be divided into 3 distinct
   steps, all of which are visible in `redisplay_internal':

    . decide which frames need their windows to be considered for redisplay
    . for each window whose display might need to be updated, compute
      a structure, called "glyph matrix", which describes how it
      should look on display
    . actually update the display of windows on the glass where the
      newly obtained glyph matrix differs from the one produced by the
      previous redisplay cycle
*/
```

### Asynchronous Redisplay Triggers

The display engine can be invoked both synchronously (from the command loop) and asynchronously (from window system events). From `src/xdisp.c:39-71`:

```
   +--------------+   redisplay     +----------------+
   | Lisp machine |---------------->| Redisplay code |<--+
   +--------------+   (xdisp.c)     +----------------+   |
         ^                              |                |
         +----------------------------------+             |
           Block input to prevent this when               |
           called asynchronously!                         |
                                                          |
               note_mouse_highlight (asynchronous)       |
                                                          |
                           X mouse events  ---------------+
                                                          |
                   expose_frame (asynchronous)           |
                                                          |
                          X expose events  ---------------+
```

This diagram illustrates a critical design constraint: C functions that might trigger asynchronous redisplay must use `block_input()`/`unblock_input()` to prevent reentrancy issues.

---

## The Redisplay Cycle

### Entry Point: `redisplay_internal()`

The sole entry point into the display engine is `redisplay_internal()` in `src/xdisp.c:17137`. This function orchestrates the entire redisplay process.

From `src/xdisp.c:17137-17225`:

```c
redisplay_internal (void)
{
  struct window *w = XWINDOW (selected_window);
  struct window *sw;
  struct frame *fr;
  bool must_finish = false, match_p;
  struct text_pos tlbufpos, tlendpos;
  int number_of_visible_frames;
  struct frame *sf;
  bool polling_stopped_here = false;
  Lisp_Object tail, frame;

  /* Set a limit to the number of retries we perform due to horizontal
     scrolling, this avoids getting stuck in an uninterruptible
     infinite loop (Bug #24633).  */
  enum { MAX_HSCROLL_RETRIES = 16 };
  int hscroll_retries = 0;

  /* Limit the number of retries for when frame(s) become garbaged as
     result of redisplaying them.  Some packages set various redisplay
     hooks, such as window-scroll-functions, to run Lisp that always
     calls APIs which cause the frame's garbaged flag to become set,
     so we loop indefinitely.  */
  enum {MAX_GARBAGED_FRAME_RETRIES = 2 };
  int garbaged_frame_retries = 0;

  /* False means that only the selected_window needs to be updated.
     True means that other windows may need to be updated as well,
     so we need to consult `needs_no_update` for all windows.  */
  bool consider_all_windows_p;

  /* True means redisplay has to redisplay the miniwindow.  */
  bool update_miniwindow_p = false;

  redisplay_trace ("redisplay_internal %d\n", redisplaying_p);

  /* I don't think this happens but let's be paranoid.  In particular,
     this was observed happening when Emacs shuts down due to losing X
     connection, in which case accessing SELECTED_FRAME and the frame
     structure is likely to barf.  */
  if (redisplaying_p)
    return;

  /* No redisplay if running in batch mode or frame is not yet fully
     initialized, or redisplay is explicitly turned off by setting
     Vinhibit_redisplay.  */
  if ((FRAME_INITIAL_P (SELECTED_FRAME ())
       && redisplay_skip_initial_frame)
      || !NILP (Vinhibit_redisplay))
    return;
```

**Key Design Decisions:**

1. **Reentrancy Protection**: The function immediately returns if already redisplaying, preventing infinite recursion.

2. **Retry Limits**: Hard limits prevent infinite loops from misbehaving Lisp code or pathological buffer states.

3. **Early Exit Conditions**: Multiple guards prevent unnecessary work (batch mode, uninitialized frames, explicit inhibition).

### The Retry Loop

Redisplay operates in a retry loop to handle cases where the display state changes during redisplay itself:

```c
 retry:
  /* Remember the currently selected window.  */
  sw = w;

  forget_escape_and_glyphless_faces ();

  inhibit_free_realized_faces = false;

  /* If face_change, init_iterator will free all realized faces, which
     includes the faces referenced from current matrices.  So, we
     can't reuse current matrices in this case.  */
  if (face_change)
    windows_or_buffers_changed = 47;
```

The `retry:` label allows redisplay to restart if fonts are loaded, frame geometry changes, or other global state modifications occur during the redisplay process.

### Frame Visibility and Matrix Adjustment

From `src/xdisp.c:17258-17290`:

```c
  /* Set the visible flags for all frames.  Do this before checking for
     resized or garbaged frames; they want to know if their frames are
     visible.  See the comment in frame.h for FRAME_SAMPLE_VISIBILITY.  */
  number_of_visible_frames = 0;

  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);

      /* frame_redisplay_p true basically means the frame is visible. */
      if (frame_redisplay_p (f))
	{
	  ++number_of_visible_frames;
	  /* Adjust matrices for visible frames only.  */
	  if (f->fonts_changed)
	    {
	      adjust_frame_glyphs (f);
	      /* Disable all redisplay optimizations for this frame.
		 This is because adjust_frame_glyphs resets the
		 enabled_p flag for all glyph rows of all windows, so
		 many optimizations will fail anyway, and some might
		 fail to test that flag and do bogus things as
		 result.  */
	      SET_FRAME_GARBAGED (f);
	      f->fonts_changed = false;
	    }
	  /* If cursor type has been changed on the frame
	     other than selected, consider all frames.  */
	  if (f != sf && f->cursor_type_changed)
	    fset_redisplay (f);
	}
      clear_desired_matrices (f);
    }
```

This code demonstrates a key optimization: only visible frames have their matrices adjusted. The `fonts_changed` flag triggers complete matrix reallocation when font loading changes window dimensions.

---

## Glyph Matrices: The Heart of Display

### Understanding Glyph Matrices

A glyph matrix is a two-dimensional array of glyphs where:
- Each **row** corresponds to a screen line
- Each **glyph** in a row represents a display element (character, image, etc.)

From `src/dispextern.h:783-847`:

```c
struct glyph_matrix
{
  /* The pool from which glyph memory is allocated, if any.  This is
     null for frame matrices and for window matrices managing their
     own storage.  */
  struct glyph_pool *pool;

  /* Vector of glyph row structures.  The row at nrows - 1 is reserved
     for the mode line.  */
  struct glyph_row *rows;

  /* Number of elements allocated for the vector rows above.  */
  ptrdiff_t rows_allocated;

  /* The number of rows used by the window if all lines were displayed
     with the smallest possible character height.  */
  int nrows;

  /* Origin within the frame matrix if this is a window matrix on a
     frame having a frame matrix.  Both values are zero for
     window-based redisplay.  */
  int matrix_x, matrix_y;

  /* Width and height of the matrix in columns and rows.  */
  int matrix_w, matrix_h;

  /* If this structure describes a window matrix of window W,
     window_pixel_left is the value of W->pixel_left, window_pixel_top
     the value of W->pixel_top, window_height and window_width are width
     and height of W, as returned by window_box, and window_vscroll is
     the value of W->vscroll at the time the matrix was last adjusted.
     Only set for window-based redisplay.  */
  int window_pixel_left, window_pixel_top;
  int window_height, window_width;
  int window_vscroll;

  /* Number of glyphs reserved for left and right marginal areas when
     the matrix was last adjusted.  */
  int left_margin_glyphs, right_margin_glyphs;

  /* Flag indicating that scrolling should not be tried in
     update_window.  This flag is set by functions like try_window_id
     which do their own scrolling.  */
  bool_bf no_scrolling_p : 1;

  /* True means window displayed in this matrix has a tab line.  */
  bool_bf tab_line_p : 1;

  /* True means window displayed in this matrix has a header
     line.  */
  bool_bf header_line_p : 1;

#ifdef GLYPH_DEBUG
  /* A string identifying the method used to display the matrix.  */
  char method[512];
#endif

  /* The buffer this matrix displays.  Set in
     mark_window_display_accurate_1.  */
  struct buffer *buffer;

  /* Values of BEGV and ZV as of last redisplay.  Set in
     mark_window_display_accurate_1.  */
  ptrdiff_t begv, zv;
};
```

### Current vs Desired Matrices

Each window maintains **two** glyph matrices:

1. **Current Matrix**: Records what is currently displayed on screen
2. **Desired Matrix**: Describes what should be displayed

The update process compares these matrices to determine minimal changes needed.

### Glyph Row Structure

Each row in a glyph matrix contains detailed information about a screen line. From `src/dispextern.h:906-1055`:

```c
struct glyph_row
{
  /* Pointers to beginnings of areas.  The end of an area A is found at
     A + 1 in the vector.  The last element of the vector is the end
     of the whole row.

     Kludge alert: Even if used[TEXT_AREA] == 0, glyphs[TEXT_AREA][0]'s
     position field is used.  It is -1 if this row does not correspond
     to any text; it is some buffer position if the row corresponds to
     an empty display line that displays a line end.  This is what old
     redisplay used to do.  (Except in code for terminal frames, this
     kludge is no longer used, I believe. --gerd).

     See also start, end, displays_text_p and ends_at_zv_p for cleaner
     ways to do it.  The special meaning of positions 0 and -1 will be
     removed some day, so don't use it in new code.  */
  struct glyph *glyphs[1 + LAST_AREA];

  /* Number of glyphs actually filled in areas.  This could have size
     LAST_AREA, but it's 1 + LAST_AREA to simplify offset calculations.  */
  short used[1 + LAST_AREA];

  /* Hash code.  This hash code is available as soon as the row
     is constructed, i.e. after a call to display_line.  */
  unsigned hash;

  /* Window-relative x and y-position of the top-left corner of this
     row.  If y < 0, this means that eabs (y) pixels of the row are
     invisible because it is partially visible at the top of a window.
     If x < 0, this means that eabs (x) pixels of the first glyph of
     the text area of the row are invisible because the glyph is
     partially visible.  */
  int x, y;

  /* Width of the row in pixels without taking face extension at the
     end of the row into account, and without counting truncation
     and continuation glyphs at the end of a row on ttys.  */
  int pixel_width;

  /* Logical ascent/height of this line.  The value of ascent is zero
     and height is 1 on terminal frames.  */
  int ascent, height;

  /* Physical ascent/height of this line.  If max_ascent > ascent,
     this line overlaps the line above it on the display.  Otherwise,
     if max_height > height, this line overlaps the line beneath it.  */
  int phys_ascent, phys_height;

  /* Portion of row that is visible.  Partially visible rows may be
     found at the top and bottom of a window.  This is 1 for tty
     frames.  It may be < 0 in case of completely invisible rows.  */
  int visible_height;
```

**Key Features:**

- **Three Areas**: Each row can have left margin, text area, and right margin glyphs
- **Hash Codes**: Enable fast equality comparisons for optimization
- **Pixel Geometry**: Tracks exact position and dimensions for precise rendering
- **Visibility Tracking**: Distinguishes fully visible, partially visible, and invisible rows

### The Glyph Structure

Individual glyphs are the atomic units of display. From `src/dispextern.h:460-560`:

```c
struct glyph
{
  /* Position from which this glyph was drawn.  If `object' below is a
     Lisp string, this is an index into that string.  If it is a
     buffer, this is a position in that buffer.  In addition, some
     special glyphs have special values for this:

      glyph standing for newline at end of line    0
      empty space after the end of the line       -1
      overlay arrow on a TTY                      -1
      glyph displaying line number                -1
      glyph at EOB that ends in a newline         -1
      left truncation glyphs:                     -1
      right truncation/continuation glyphs        next buffer position
      glyph standing for newline of an empty line buffer position of newline
      stretch glyph at left edge of R2L lines     buffer position of newline  */
  ptrdiff_t charpos;

  /* Lisp object source of this glyph.  Currently either a buffer or a
     string, if the glyph was produced from characters which came from
     a buffer or a string; or nil if the glyph was inserted by
     redisplay for its own purposes, such as padding, truncation, or
     continuation glyphs, or the overlay-arrow glyphs on TTYs.  */
  Lisp_Object object;

  /* Frame on which the glyph was produced.  The face_id of this glyph
     refers to the face_cache of this frame.  This is used on tty
     frames only.  */
  struct frame *frame;

  /* Width in pixels.  */
  short pixel_width;

  /* Ascent and descent in pixels.  */
  short ascent, descent;

  /* Vertical offset.  If < 0, the glyph is displayed raised, if > 0
     the glyph is displayed lowered.  */
  short voffset;

  /* Which kind of glyph this is---character, image etc.  Value
     should be an enumerator of type enum glyph_type.  */
  unsigned type : 3;

  /* True means this glyph was produced from multibyte text.  False
     means it was produced from unibyte text, i.e. charsets aren't
     applicable, and encoding is not performed.  */
  bool_bf multibyte_p : 1;

  /* True means draw a box line at the left or right side of this
     glyph.  This is part of the implementation of the face attribute
     `:box'.  */
  bool_bf left_box_line_p : 1;
  bool_bf right_box_line_p : 1;

  /* True means this glyph's physical ascent or descent is greater
     than its logical ascent/descent, i.e. it may potentially overlap
     glyphs above or below it.  */
  bool_bf overlaps_vertically_p : 1;

  /* For terminal frames, true means glyph is a padding glyph.  Padding
     glyphs are used for characters whose visual shape consists of
     more than one glyph (e.g. Asian characters).  All but the first
     glyph of such a glyph sequence have the padding_p flag set.  This
     flag is used only to minimize code changes.  A better way would
     probably be to use the width field of glyphs to express padding.

     For graphic frames, true means the pixel width of the glyph in a
     font is 0, but 1-pixel is padded on displaying for correct cursor
     displaying.  The member `pixel_width' above is set to 1.  */
  bool_bf padding_p : 1;

  /* True means the actual glyph is not available, draw using `struct
     glyphless' below instead.  This can happen when a font couldn't
     be loaded, or a character doesn't have a glyph in a font.  */
  bool_bf glyph_not_available_p : 1;

  /* True means don't display cursor here.  */
  bool_bf avoid_cursor_p : 1;

  /* Resolved bidirectional level of this character [0..127].  */
  unsigned resolved_level : 7;

  /* Resolved bidirectional type of this character, see enum
     bidi_type_t below.  Note that according to UAX#9, only some
     values (STRONG_L, STRONG_R, WEAK_AN, WEAK_EN, WEAK_BN, and
     NEUTRAL_B) can appear in the resolved type, so we only reserve
     space for those that can.  */
  unsigned bidi_type : 3;

#define FACE_ID_BITS	20

  /* Face of the glyph.  This is a realized face ID,
     an index in the face cache of the frame.  */
  unsigned face_id : FACE_ID_BITS;
```

This structure is a masterclass in bit-packing optimization, using bitfields extensively to minimize memory usage while maintaining rich metadata about each display element.

### Frame Matrices: Text-Mode Terminal Optimization

On text-mode terminals (TTYs), an additional optimization uses **frame matrices** to enable efficient scrolling. From `src/xdisp.c:307-335`:

```c
/*
   Frame matrices.

   That just couldn't be all, could it?  What about terminal types not
   supporting operations on sub-windows of the screen (a.k.a. "TTY" or
   "text-mode terminals")?  To update the display on such a terminal,
   window-based glyph matrices are not well suited.  To be able to
   reuse part of the display (scrolling lines up and down), we must
   instead have a view of the whole screen.  This is what `frame
   matrices' are for.  They are a trick.

   Frames on text terminals have a glyph pool.  Windows on such a
   frame sub-allocate their glyph memory from their frame's glyph
   pool.  The frame itself is given its own glyph matrices.  By
   coincidence---or maybe something else---rows in window glyph
   matrices are slices of corresponding rows in frame matrices.  Thus
   writing to window matrices implicitly updates a frame matrix which
   provides us with the view of the whole screen that we originally
   wanted to have without having to move many bytes around.  Then
   updating all the visible windows on text-terminal frames is done by
   using the frame matrices, which allows frame-global optimization of
   what is actually written to the glass.

   Frame matrices don't have marginal areas, only a text area.  That
   is, the entire row of glyphs that spans the width of a text-mode
   frame is treated as a single large "text area" for the purposes of
   manipulating and updating a frame glyph matrix.
*/
```

This "trick" is brilliant: by making window matrix rows point into frame matrix rows, updates to windows automatically update the frame-wide view needed for terminal scrolling optimization.

---

## The Display Iterator

### Purpose and Design

The display iterator (`struct it`) is the workhorse of text layout. It traverses buffer or string text, handling:

- Text properties and overlays
- Face changes
- Display properties (images, space specs)
- Bidirectional text reordering
- Character composition
- Invisible text

From `src/xdisp.c:222-246`:

```c
/*
   Iteration over buffer and strings.

   Characters and pixmaps displayed for a range of buffer text depend
   on various settings of buffers and windows, on overlays and text
   properties, on display tables, on selective display.  The good news
   is that all this hairy stuff is hidden behind a small set of
   interface functions taking an iterator structure (`struct it')
   argument.

   Iteration over things to be displayed is then simple.  It is
   started by initializing an iterator with a call to `init_iterator',
   passing it the buffer position where to start iteration.  For
   iteration over strings, pass -1 as the position to `init_iterator',
   and call `reseat_to_string' when the string is ready, to initialize
   the iterator for that string.  Thereafter, calls to
   `get_next_display_element' fill the iterator structure with
   relevant information about the next thing to display.  Calls to
   `set_iterator_to_next' move the iterator to the next thing.

   Besides this, an iterator also contains information about the
   display environment in which glyphs for display elements are to be
   produced.  It has fields for the width and height of the display,
   the information whether long lines are truncated or continued, a
   current X and Y position, the face currently in effect, and lots of
   other stuff you can better see in dispextern.h.
*/
```

### Iterator Structure

From `src/dispextern.h:2391-2640`:

```c
struct it
{
  /* The window in which we iterate over current_buffer (or a string).  */
  Lisp_Object window;
  struct window *w;

  /* The window's frame.  */
  struct frame *f;

  /* Method to use to load this structure with the next display element.  */
  enum it_method method;

  /* The next position at which to check for face changes, invisible
     text, overlay strings, end of text etc., which see.  */
  ptrdiff_t stop_charpos;

  /* Previous stop position, i.e. the last one before the current
     iterator position in `current'.  */
  ptrdiff_t prev_stop;

  /* Last stop position iterated across whose bidi embedding level is
     equal to the current paragraph's base embedding level.  */
  ptrdiff_t base_level_stop;

  /* Maximum string or buffer position + 1.  ZV when iterating over
     current_buffer.  When iterating over a string in display_string,
     this can be smaller or greater than the number of string
     characters, depending on the values of PRECISION and FIELD_WIDTH
     with which display_string was called.  */
  ptrdiff_t end_charpos;
```

### The Stop Position Mechanism

One of the most important optimizations in the iterator is the **stop position**. From `src/xdisp.c:248-288`:

```c
/*
   The "stop position".

   Some of the fields maintained by the iterator change relatively
   infrequently.  These include the face of the characters, whether
   text is invisible, the object (buffer or display or overlay string)
   being iterated, character composition info, etc.  For any given
   buffer or string position, the sources of information that affects
   the display can be determined by calling the appropriate
   primitives, such as `Fnext_single_property_change', but both these
   calls and the processing of their return values is relatively
   expensive.  To optimize redisplay, the display engine checks these
   sources of display information only when needed, not for every
   character.  To that end, it always maintains the position of the
   next place where it must stop and re-examine all those potential
   sources.  This is called "the stop position" and is stored in the
   `stop_charpos' field of the iterator.  The stop position is updated
   by `compute_stop_pos', which is called whenever the iteration
   reaches the current stop position and processes it.  Processing a
   stop position is done by `handle_stop', which invokes a series of
   handlers, one each for every potential source of display-related
   information; see the `it_props' array for those handlers.  For
   example, one handler is `handle_face_prop', which detects changes
   in face properties, and supplies the face ID that the iterator will
   use for all the glyphs it generates up to the next stop position;
   this face ID is the result of "realizing" the face specified by the
   relevant text properties at this position (see xfaces.c).  Each
   handler called by `handle_stop' processes the sources of display
   information for which it is "responsible", and returns a value
   which tells `handle_stop' what to do next.

   Once `handle_stop' returns, the information it stores in the
   iterator fields will not be refreshed until the iteration reaches
   the next stop position, which is computed by `compute_stop_pos'
   called at the end of `handle_stop'.  `compute_stop_pos' examines
   the buffer's or string's interval tree to determine where the text
   properties change, finds the next position where overlays and
   character composition can change, and stores in `stop_charpos' the
   closest position where any of these factors should be reconsidered.

   Handling of the stop position is done as part of the code in
   `get_next_display_element'.
*/
```

This mechanism is crucial for performance: instead of checking text properties and overlays at every character, the iterator only checks at boundaries where these properties might change.

### Iterator State Stack

The iterator maintains a stack to handle nested display elements (like overlay strings within display properties):

```c
  /* Stack of saved values.  New entries are pushed when we begin to
     process an overlay string or a string from a `glyph' property.
     Entries are popped when we return to deliver display elements
     from what we previously had.  */
  struct iterator_stack_entry
  {
    Lisp_Object string;
    int string_nchars;
    ptrdiff_t end_charpos;
    ptrdiff_t stop_charpos;
    ptrdiff_t prev_stop;
    ptrdiff_t base_level_stop;
    struct composition_it cmp_it;
    int face_id;

    /* Save values specific to a given method.  */
    union {
      /* method == GET_FROM_IMAGE */
      struct {
	Lisp_Object object;
	struct it_slice slice;
	ptrdiff_t image_id;
      } image;
      /* method == GET_FROM_STRETCH */
      struct {
	Lisp_Object object;
      } stretch;
      /* method == GET_FROM_XWIDGET */
      struct {
	Lisp_Object object;
      } xwidget;
    } u;

    /* Current text and display positions.  */
    struct text_pos position;
    struct display_pos current;
    Lisp_Object from_overlay;
    enum glyph_row_area area;
    enum it_method method;
    bidi_dir_t paragraph_embedding;
    bool_bf multibyte_p : 1;
    bool_bf string_from_display_prop_p : 1;
    bool_bf string_from_prefix_prop_p : 1;
    bool_bf display_ellipsis_p : 1;
    bool_bf avoid_cursor_p : 1;
    bool_bf bidi_p : 1;
    bool_bf from_disp_prop_p : 1;
    enum line_wrap_method line_wrap;

    /* Properties from display property that are reset by another display
       property.  */
    short voffset;
    Lisp_Object space_width;
    Lisp_Object font_height;
  }
  stack[IT_STACK_SIZE];

  /* Stack pointer.  */
  int sp;
```

---

## Face Management and Realization

### The Face System Architecture

Faces in Emacs have a two-tier architecture:

1. **Lisp Faces**: High-level face definitions with named attributes
2. **Realized Faces**: Low-level, platform-specific rendering information

From `src/xfaces.c:22-217`:

```c
/* Faces.

   When using Emacs with X, the display style of characters can be
   changed by defining `faces'.  Each face can specify the following
   display attributes:

   1. Font family name.

   2. Font foundry name.

   3. Relative proportionate width, aka character set width or set
   width (swidth), e.g. `semi-compressed'.

   4. Font height in 1/10pt.

   5. Font weight, e.g. `bold'.

   6. Font slant, e.g. `italic'.

   7. Foreground color.

   8. Background color.

   9. Whether or not characters should be underlined, and in what color.

   10. Whether or not characters should be displayed in inverse video.

   11. A background stipple, a bitmap.

   12. Whether or not characters should be overlined, and in what color.

   13. Whether or not characters should be strike-through, and in what
   color.

   14. Whether or not a box should be drawn around characters, the box
   type, and, for simple boxes, in what color.

   15. Font-spec, or nil.  This is a special attribute.

   A font-spec is a collection of font attributes (specs).

   When this attribute is specified, the face uses a font matching
   with the specs as is except for what overwritten by the specs in
   the fontset (see below).  In addition, the other font-related
   attributes (1st thru 5th) are updated from the spec.

   On the other hand, if one of the other font-related attributes are
   specified, the corresponding specs in this attribute is set to nil.

   16. A face name or list of face names from which to inherit attributes.

   17. A fontset name.  This is another special attribute.

   A fontset is a mappings from characters to font-specs, and the
   specs overwrite the font-spec in the 14th attribute.

   18. A "distant background" color, to be used when the foreground is
   too close to the background and is hard to read.

   19. Whether to extend the face to end of line when the face
   "covers" the newline that ends the line.

   On the C level, a Lisp face is completely represented by its array
   of attributes.  In that array, the zeroth element is Qface, and the
   rest are the 19 face attributes described above.  The
   lface_attribute_index enumeration, defined on dispextern.h, with
   values given by the LFACE_*_INDEX constants, is used to reference
   the individual attributes.

   Faces are frame-local by nature because Emacs allows you to define the
   same named face (face names are symbols) differently for different
   frames.  Each frame has an alist of face definitions for all named
   faces.  The value of a named face in such an alist is a Lisp vector
   with the symbol `face' in slot 0, and a slot for each of the face
   attributes mentioned above.

   There is also a global face map `Vface_new_frame_defaults',
   containing conses of (FACE_ID . FACE_DEFINITION).  Face definitions
   from this table are used to initialize faces of newly created
   frames.

   A face doesn't have to specify all attributes.  Those not specified
   have a value of `unspecified'.  Faces specifying all attributes but
   the 14th are called `fully-specified'.


   Face merging.

   The display style of a given character in the text is determined by
   combining several faces.  This process is called `face merging'.
   Face merging combines the attributes of each of the faces being
   merged such that the attributes of the face that is merged later
   override those of a face merged earlier in the process.  In
   particular, this replaces any 'unspecified' attributes with
   non-'unspecified' values.  Also, if a face inherits from another
   (via the :inherit attribute), the attributes of the parent face,
   recursively, are applied where the inheriting face doesn't specify
   non-'unspecified' values.  Any aspect of the display style that
   isn't specified by overlays or text properties is taken from the
   'default' face.  Since it is made sure that the default face is
   always fully-specified, face merging always results in a
   fully-specified face.


   Face realization.

   After all face attributes for a character have been determined by
   merging faces of that character, that face is `realized'.  The
   realization process maps face attributes to what is physically
   available on the system where Emacs runs.  The result is a
   `realized face' in the form of a struct face which is stored in the
   face cache of the frame on which it was realized.

   Face realization is done in the context of the character to display
   because different fonts may be used for different characters.  In
   other words, for characters that have different font
   specifications, different realized faces are needed to display
   them.

   Font specification is done by fontsets.  See the comment in
   fontset.c for the details.  In the current implementation, all ASCII
   characters share the same font in a fontset.

   Faces are at first realized for ASCII characters, and, at that
   time, assigned a specific realized fontset.  Hereafter, we call
   such a face as `ASCII face'.  When a face for a multibyte character
   is realized, it inherits (thus shares) a fontset of an ASCII face
   that has the same attributes other than font-related ones.

   Thus, all realized faces have a realized fontset.
```

### Face Realization Process

Face realization transforms abstract face attributes into concrete rendering parameters:

```
Lisp Face (symbolic)
        ↓
   Face Merging (inherit, combine with default)
        ↓
  Fully Specified Face
        ↓
   Font Selection (match available fonts)
        ↓
  Color Allocation (map colors to pixels)
        ↓
Realized Face (cached, ready to render)
```

### Font Selection

From `src/xfaces.c:162-198`:

```c
/*
   Font selection.

   Font selection tries to find the best available matching font for a
   given (character, face) combination.

   If the face specifies a fontset name, that fontset determines a
   pattern for fonts of the given character.  If the face specifies a
   font name or the other font-related attributes, a fontset is
   realized from the default fontset.  In that case, that
   specification determines a pattern for ASCII characters and the
   default fontset determines a pattern for multibyte characters.

   Available fonts on the system on which Emacs runs are then matched
   against the font pattern.  The result of font selection is the best
   match for the given face attributes in this font list.

   Font selection can be influenced by the user.

   1. The user can specify the relative importance he gives the face
   attributes width, height, weight, and slant by setting
   face-font-selection-order (faces.el) to a list of face attribute
   names.  The default is '(:width :height :weight :slant), and means
   that font selection first tries to find a good match for the font
   width specified by a face, then---within fonts with that
   width---tries to find a best match for the specified font height,
   etc.

   2. Setting face-font-family-alternatives allows the user to
   specify alternative font families to try if a family specified by a
   face doesn't exist.

   3. Setting face-font-registry-alternatives allows the user to
   specify all alternative font registries to try for a face
   specifying a registry.

   4. Setting face-ignored-fonts allows the user to ignore specific
   fonts.
*/
```

---

## Bidirectional Text Rendering

### The Bidi Challenge

Bidirectional text (mixing left-to-right and right-to-left scripts) presents unique challenges:

1. Logical order (storage) differs from visual order (display)
2. Character reordering must follow Unicode Bidirectional Algorithm (UBA)
3. Cursor movement becomes non-monotonic with respect to buffer positions
4. Line wrapping must respect visual boundaries

### Bidi Architecture

From `src/bidi.c:23-106`:

```c
/* A sequential implementation of the Unicode Bidirectional algorithm,
   (UBA) as per UAX#9, a part of the Unicode Standard.

   Unlike the Reference Implementation and most other implementations,
   this one is designed to be called once for every character in the
   buffer or string.  That way, we can leave intact the design of the
   Emacs display engine, whereby an iterator object is used to
   traverse buffer or string text character by character, and generate
   the necessary data for displaying each character in 'struct glyph'
   objects.  (See xdisp.c for the details of that iteration.)  The
   functions on this file replace the original linear iteration in the
   logical order of the text with a non-linear iteration in the visual
   order, i.e. in the order characters should be shown on display.

   The main entry point is bidi_move_to_visually_next.  Each time it
   is called, it finds the next character in the visual order, and
   returns its information in a special structure.  The caller is then
   expected to process this character for display or any other
   purposes, and call bidi_move_to_visually_next for the next
   character.  See the comments in bidi_move_to_visually_next for more
   details about its algorithm that finds the next visual-order
   character by resolving their levels on the fly.

   Two other entry points are bidi_paragraph_init and
   bidi_mirror_char.  The first determines the base direction of a
   paragraph, while the second returns the mirrored version of its
   argument character.

   A few auxiliary entry points are used to initialize the bidi
   iterator for iterating an object (buffer or string), push and pop
   the bidi iterator state, and save and restore the state of the bidi
   cache.

   If you want to understand the code, you will have to read it
   together with the relevant portions of UAX#9.  The comments include
   references to UAX#9 rules, for that very reason.
```

### Bidi Processing Hierarchy

From `src/bidi.c:68-106`:

```c
/*
   Here's the overview of the design of the reordering engine
   implemented by this file.

   Basic implementation structure
   ------------------------------

   The sequential processing steps described by UAX#9 are implemented
   as recursive levels of processing, all of which examine the next
   character in the logical order.  This hierarchy of processing looks
   as follows, from the innermost (deepest) to the outermost level,
   omitting some subroutines used by each level:

     bidi_fetch_char         -- fetch next character
     bidi_resolve_explicit   -- resolve explicit levels and directions
     bidi_resolve_weak       -- resolve weak types
     bidi_resolve_brackets   -- resolve "paired brackets" neutral types
     bidi_resolve_neutral    -- resolve neutral types
     bidi_level_of_next_char -- resolve implicit levels

   Each level calls the level below it, and works on the result
   returned by the lower level, including all of its sub-levels.

   Unlike all the levels below it, bidi_level_of_next_char can return
   the information about either the next or previous character in the
   logical order, depending on the current direction of scanning the
   buffer or string.  For the next character, it calls all the levels
   below it; for the previous character, it uses the cache, described
   below.

   Thus, the result of calling bidi_level_of_next_char is the resolved
   level of the next or the previous character in the logical order.
   Based on this information, the function bidi_move_to_visually_next
   finds the next character in the visual order and updates the
   direction in which the buffer is scanned, either forward or
   backward, to find the next character to be displayed.  (Text is
   scanned backwards when it needs to be reversed for display, i.e. if
   the visual order is the inverse of the logical order.)  This
   implements the last, reordering steps of the UBA, by successively
   calling bidi_level_of_next_char until the character of the required
   embedding level is found; the scan direction is dynamically updated
   as a side effect.  See the commentary before the 'while' loop in
   bidi_move_to_visually_next, for the details.
*/
```

### Integration with Display Iterator

The bidi engine integrates seamlessly with the display iterator. From `src/xdisp.c:374-466`:

```c
/*
   Bidirectional display.

   Bidirectional display adds quite some hair to this already complex
   design.  The good news are that a large portion of that hairy stuff
   is hidden in bidi.c behind only 3 interfaces.  bidi.c implements a
   reordering engine which is called by `set_iterator_to_next' and
   returns the next character to display in the visual order.  See
   commentary on bidi.c for more details.  As far as redisplay is
   concerned, the effect of calling `bidi_move_to_visually_next', the
   main interface of the reordering engine, is that the iterator gets
   magically placed on the buffer or string position that is to be
   displayed next in the visual order.  In other words, a linear
   iteration through the buffer/string is replaced with a non-linear
   one.  All the rest of the redisplay is oblivious to the bidi
   reordering.

   Well, almost oblivious---there are still complications, most of
   them due to the fact that buffer and string positions no longer
   change monotonously with glyph indices in a glyph row.  Moreover,
   for continued lines, the buffer positions may not even be
   monotonously changing with vertical positions.  Also, accounting
   for face changes, overlays, etc. becomes more complex because
   non-linear iteration could potentially skip many positions with
   such changes, and then cross them again on the way back (see
   `handle_stop_backwards')...

   One other prominent effect of bidirectional display is that some
   paragraphs of text need to be displayed starting at the right
   margin of the window---the so-called right-to-left, or R2L
   paragraphs.  R2L paragraphs are displayed with R2L glyph rows,
   which have their `reversed_p' flag set.  The bidi reordering engine
   produces characters in such rows starting from the character which
   should be the rightmost on display.  `PRODUCE_GLYPHS' then reverses
   the order, when it fills up the glyph row whose `reversed_p' flag
   is set, by prepending each new glyph to what is already there,
   instead of appending it.  When the glyph row is complete, the
   function `extend_face_to_end_of_line' fills the empty space to the
   left of the leftmost character with special glyphs, which will
   display as, well, empty.  On text terminals, these special glyphs
   are simply blank characters.  On graphics terminals, there's a
   single stretch glyph of a suitably computed width.  Both the blanks
   and the stretch glyph are given the face of the background of the
   line.  This way, the terminal-specific back-end can still draw the
   glyphs left to right, even for R2L lines.
*/
```

This is elegant: the bidi engine handles reordering complexity, while the rest of the display code remains largely unchanged.

---

## Line Wrapping and Truncation

### Wrapping Modes

Emacs supports three line handling modes:

1. **Truncate**: Long lines are cut off at window edge with `$` indicator
2. **Word Wrap**: Lines break at word boundaries
3. **Character Wrap**: Lines break at any character

### The `display_line()` Function

The core line layout function is `display_line()` in `src/xdisp.c:25542`. This function constructs one row of the desired glyph matrix.

From `src/xdisp.c:25542-25691`:

```c
display_line (struct it *it, int cursor_vpos)
{
  struct glyph_row *row = it->glyph_row;
  Lisp_Object overlay_arrow_string;
  struct it wrap_it;
  void *wrap_data = NULL;
  bool may_wrap = false;
  int wrap_x UNINIT;
  int wrap_row_used = -1;
  int wrap_row_ascent UNINIT, wrap_row_height UNINIT;
  int wrap_row_phys_ascent UNINIT, wrap_row_phys_height UNINIT;
  int wrap_row_extra_line_spacing UNINIT;
  ptrdiff_t wrap_row_min_pos UNINIT, wrap_row_min_bpos UNINIT;
  ptrdiff_t wrap_row_max_pos UNINIT, wrap_row_max_bpos UNINIT;
  int cvpos;
  ptrdiff_t min_pos = ZV + 1, max_pos = 0;
  ptrdiff_t min_bpos UNINIT, max_bpos UNINIT;
  bool pending_handle_line_prefix = false;
  int tab_line = window_wants_tab_line (it->w);
  int header_line = window_wants_header_line (it->w);
  bool hscroll_this_line = (cursor_vpos >= 0
			    && it->vpos == cursor_vpos - tab_line - header_line
			    && hscrolling_current_line_p (it->w));
  int first_visible_x = it->first_visible_x;
  int last_visible_x = it->last_visible_x;
  int x_incr = 0;
  int this_line_subject_to_line_prefix = 0;

  /* We always start displaying at hpos zero even if hscrolled.  */
  eassert (it->hpos == 0 && it->current_x == 0);

  if (MATRIX_ROW_VPOS (row, it->w->desired_matrix)
      >= it->w->desired_matrix->nrows)
    {
      it->w->nrows_scale_factor++;
      it->f->fonts_changed = true;
      return false;
    }

  /* Clear the result glyph row and enable it.  */
  prepare_desired_row (it->w, row, false);

  row->y = it->current_y;
  row->start = it->start;
  row->continuation_lines_width = it->continuation_lines_width;
  row->displays_text_p = true;
  row->starts_in_middle_of_char_p = it->starts_in_middle_of_char_p;
  it->starts_in_middle_of_char_p = false;
  it->stretch_adjust = 0;
  it->line_number_produced_p = false;

  /* If we are going to display the cursor's line, account for the
     hscroll of that line.  We subtract the window's min_hscroll,
     because that was already accounted for in init_iterator.  */
  if (hscroll_this_line)
    x_incr =
      (window_hscroll_limited (it->w, it->f) - it->w->min_hscroll)
      * FRAME_COLUMN_WIDTH (it->f);

  bool line_number_needed = should_produce_line_number (it);
```

### Word Wrapping Algorithm

The word wrap implementation saves iterator state at potential wrap points:

```c
  /* Main loop: fill the glyph row.  */
  while (true)
    {
      /* Get the next display element.  */
      if (!get_next_display_element (it))
        {
          /* End of buffer/string reached.  */
          break;
        }

      /* Check if this is a good place to wrap.  */
      if (may_wrap && it->line_wrap == WORD_WRAP)
        {
          /* Save wrap point state.  */
          SAVE_IT (wrap_it, *it, wrap_data);
          wrap_x = it->current_x;
          wrap_row_used = row->used[TEXT_AREA];
          /* ... save other wrap state ... */
        }

      /* Produce glyphs.  */
      PRODUCE_GLYPHS (it);

      /* Check if glyph fits on line.  */
      if (it->current_x > it->last_visible_x)
        {
          /* Doesn't fit.  */
          if (it->line_wrap == WORD_WRAP && wrap_row_used > 0)
            {
              /* Word wrap: restore to saved wrap point.  */
              RESTORE_IT (it, &wrap_it, wrap_data);
              /* ... handle wrap ... */
            }
          else
            {
              /* Truncate or continue to next line.  */
              /* ... */
            }
          break;
        }

      /* Glyph fits, advance iterator.  */
      set_iterator_to_next (it, true);
    }
```

This elegant state-save/restore mechanism enables efficient word wrapping without complex backtracking.

---

## Fringe Indicators

### Fringe Architecture

The fringe is the narrow vertical area at each side of a window used to display:

- Continuation/truncation indicators
- Overlay arrows (debugging, org-mode)
- Buffer boundaries
- Custom bitmaps

From `src/fringe.c:37-68`:

```c
/* Fringe bitmaps are represented in three different ways:

   Logical bitmaps are used internally to denote things like
   'continuation', 'overlay-arrow', etc.

   Physical bitmaps specify the visual appearance of the bitmap,
   e.g. 'left-arrow', 'right-arrow', 'left-curly-arrow', etc.

   User defined bitmaps are physical bitmaps.

   Internally, fringe bitmaps for a specific display row are
   represented as an index into the table of all defined bitmaps.
   This index is stored in the `fringe' property of the physical
   bitmap symbol.

   Logical bitmaps are mapped to physical bitmaps through the
   buffer-local `fringe-indicator-alist' variable.

   Each element of this alist is a cons cell (LOGICAL . PHYSICAL),
   mapping a logical bitmap to a physical bitmap.
   PHYSICAL is either a symbol to use in both left and right fringe,
   or a cons cell (LEFT . RIGHT) specifying different physical
   bitmaps to use in left and right fringe.

   When a logical bitmap is to be displayed in a fringe, the
   `fringe-indicator-alist' is first searched for a mapping in the
   buffer-local value, then in the global value of the alist.

   If no physical bitmap is found for the logical bitmap, or if the
   bitmap that is found is nil, no bitmap is shown for the logical
   bitmap.

   The `left-fringe' and `right-fringe' display properties
   must specify physical bitmap symbols.
*/
```

### Bitmap Definitions

Standard bitmaps are defined as static bit patterns. Example from `src/fringe.c:140-154`:

```c
/* Right truncation arrow bitmap `->'.  */
/*
  ..xxxx..
  ...xxxx.
  ....xxxx
  .....xxx
  ....xxxx
  ...xxxx.
  ..xxxx..
 */
static unsigned short right_truncation_bits[] = {
  0x3c, 0x3e, 0x1f, 0x0f, 0x1f, 0x3e, 0x3c};
```

### Fringe Bitmap Structure

From `src/fringe.c:78-88`:

```c
struct fringe_bitmap
{
  unsigned short *bits;
  unsigned height;
  unsigned width;
  signed char align;    /* ALIGN_TOP, ALIGN_CENTER, ALIGN_BOTTOM */
  bool_bf dynamic : 1;
};
```

---

## Performance Optimizations

### The Optimization Strategy

The display engine employs multiple optimization levels, attempting fast paths first and falling back to full redisplay only when necessary.

From `src/xdisp.c:134-173`:

```c
/*
   You will find a lot of redisplay optimizations when you start
   looking at the innards of `redisplay_window'.  The overall goal of
   all these optimizations is to make redisplay fast because it is
   done frequently.  Some of these optimizations are implemented by
   the following functions:

    . try_cursor_movement

      This optimization is applicable if the text in the window did
      not change and did not scroll, only point moved, and it did not
      move off the displayed portion of the text.  In that case, the
      window's glyph matrix is still valid, and only the position of
      the cursor might need to be updated.

    . try_window_reusing_current_matrix

      This function reuses the current glyph matrix of a window when
      text has not changed, but the window start changed (e.g., due to
      scrolling).

    . try_window_id

      This function attempts to update a window's glyph matrix by
      reusing parts of its current glyph matrix.  It finds and reuses
      the part that was not changed, and regenerates the rest.  (The
      "id" part in the function's name stands for "insert/delete", not
      for "identification" or somesuch.)

    . try_window

      This function performs the full, unoptimized, generation of a
      single window's glyph matrix, assuming that its fonts were not
      changed and that the cursor will not end up in the scroll
      margins.  (Loading fonts requires re-adjustment of dimensions of
      glyph matrices, which makes this method impossible to use.)

   The optimizations are tried in sequence (some can be skipped if
   it is known that they are not applicable).  If none of the
   optimizations were successful, redisplay calls redisplay_windows,
   which performs a full redisplay of all windows.
*/
```

### `try_window()`: The Unoptimized Path

Even the "unoptimized" path is highly efficient. From `src/xdisp.c:21461-21556`:

```c
try_window (Lisp_Object window, struct text_pos pos, int flags)
{
  struct window *w = XWINDOW (window);
  struct it it;
  struct glyph_row *last_text_row = NULL;
  struct frame *f = XFRAME (w->frame);
  int cursor_vpos = w->cursor.vpos;

  /* Make POS the new window start.  */
  set_marker_both (w->start, Qnil, CHARPOS (pos), BYTEPOS (pos));

  /* Mark cursor position as unknown.  No overlay arrow seen.  */
  w->cursor.vpos = -1;
  overlay_arrow_seen = false;

  /* Initialize iterator and info to start at POS.  */
  start_display (&it, w, pos);
  it.glyph_row->reversed_p = false;

  /* Display all lines of W.  */
  while (it.current_y < it.last_visible_y)
    {
      int last_row_scale = it.w->nrows_scale_factor;
      int last_col_scale = it.w->ncols_scale_factor;
      if (display_line (&it, cursor_vpos))
	last_text_row = it.glyph_row - 1;
      if (f->fonts_changed
	  && !((flags & TRY_WINDOW_IGNORE_FONTS_CHANGE)
	       /* If the matrix dimensions are insufficient, we _must_
		  fail and let dispnew.c reallocate the matrix.  */
	       && last_row_scale == it.w->nrows_scale_factor
	       && last_col_scale == it.w->ncols_scale_factor))
	return 0;
    }
```

The function simply:
1. Initializes an iterator at the window start position
2. Calls `display_line()` for each visible line
3. Returns success/failure status

**Simplicity is performance**: By avoiding special cases, the code is easier to optimize at the compiler level.

### Scroll Margin Optimization

From `src/xdisp.c:21500-21529`:

```c
  /* Don't let the cursor end in the scroll margins.  However, when
     the window is vscrolled, we leave it to vscroll to handle the
     margins, see window_scroll_pixel_based.  */
  if ((flags & TRY_WINDOW_CHECK_MARGINS)
      && w->vscroll == 0
      && !MINI_WINDOW_P (w))
    {
      int top_scroll_margin = window_scroll_margin (w, MARGIN_IN_PIXELS);
      int bot_scroll_margin = top_scroll_margin;
      if (window_wants_header_line (w))
	top_scroll_margin += CURRENT_HEADER_LINE_HEIGHT (w);
      if (window_wants_tab_line (w))
	top_scroll_margin += CURRENT_TAB_LINE_HEIGHT (w);
      start_display (&it, w, pos);

      if ((w->cursor.y >= 0
	   && w->cursor.y < top_scroll_margin
	   && CHARPOS (pos) > BEGV)
	  /* rms: considering make_cursor_line_fully_visible_p here
	     seems to give wrong results.  We don't want to recenter
	     when the last line is partly visible, we want to allow
	     that case to be handled in the usual way.  */
	  || w->cursor.y > (it.last_visible_y - partial_line_height (&it)
			    - bot_scroll_margin - 1))
	{
	  w->cursor.vpos = -1;
	  clear_glyph_matrix (w->desired_matrix);
	  return -1;
	}
    }
```

This check prevents the cursor from landing in scroll margins, improving user experience.

### Hash-Based Row Comparison

Glyph rows have hash codes for fast comparison. From `src/dispextern.h:928-930`:

```c
  /* Hash code.  This hash code is available as soon as the row
     is constructed, i.e. after a call to display_line.  */
  unsigned hash;
```

During screen update, identical rows (same hash) can be skipped, avoiding unnecessary terminal I/O.

---

## Window System Integration

### Abstraction Through Backend Functions

The display engine abstracts window system specifics through function pointers in `struct terminal` and `struct frame`.

### The Update Dispatch

From `src/dispnew.c:4115-4123`:

```c
update_frame (struct frame *f, bool inhibit_scrolling)
{
  if (FRAME_WINDOW_P (f))
    update_window_frame (f);
  else if (FRAME_INITIAL_P (f))
    update_initial_frame (f);
  else
    update_tty_frame (f);
}
```

**Three Update Paths:**

1. **Window Frames** (X11, Windows, macOS): Use GUI toolkit facilities
2. **Initial Frames**: Special handling for daemon mode
3. **TTY Frames**: Terminal control sequences

### Terminal-Specific Rendering

Each backend implements:

- `write_glyphs()`: Output glyphs to screen
- `clear_end_of_line()`: Erase to end of line
- `ins_del_lines()`: Insert/delete lines (for scrolling)
- `set_terminal_modes()`: Initialize terminal state
- `cursor_to()`: Move cursor to position

These function pointers in `struct terminal` allow the core display code to remain platform-independent.

### Frame Matrix Usage on TTYs

Text terminals use frame matrices for efficient scrolling. From `dispnew.c`:

```c
/* Build frame matrix from window matrices.  */
build_frame_matrix_from_window_tree (frame->current_matrix, root_window);

/* Perform scrolling operations.  */
scrolling (frame);

/* Update terminal output.  */
write_matrix (frame, true, true);
```

The scrolling optimization detects that lines have merely moved vertically and uses terminal scroll commands instead of rewriting entire lines.

---

## Advanced Topics

### Long Line Optimization

Modern Emacs includes special optimizations for extremely long lines (10,000+ characters). From `src/xdisp.c:25616-25631`:

```c
      if (current_buffer->long_line_optimizations_p
	  && it->line_wrap == TRUNCATE
	  && window_hscroll_limited (it->w, it->f) > large_hscroll_threshold)
	{
	  /* Special optimization for very long and truncated lines
	     which are hscrolled far to the left: jump directly to the
	     (approximate) position that is visible, instead of slowly
	     walking there.  */
	  ptrdiff_t chars_to_skip =
	    it->first_visible_x / FRAME_COLUMN_WIDTH (it->f);
	  move_result = fast_move_it_horizontally (it, chars_to_skip);

	  if (move_result == MOVE_X_REACHED)
	    it->current_x = it->first_visible_x;
	  else	/* use arbitrary value < first_visible_x */
	    it->current_x = it->first_visible_x - FRAME_COLUMN_WIDTH (it->f);
	}
```

This optimization skips calculating layout for off-screen portions of very long lines.

### Simulating Display Without Rendering

Functions like `move_it_by_lines()` use the display engine to calculate layout without producing glyphs. From `src/xdisp.c:337-372`:

```c
/*
   Simulating display.

   Some of Emacs commands and functions need to take display layout
   into consideration.  For example, C-n moves to the next screen
   line, but to implement that, Emacs needs to find the buffer
   position which is directly below the cursor position on display.
   This is not trivial when buffer display includes variable-size
   elements such as different fonts, tall images, etc.

   To solve this problem, the display engine implements several
   functions that can move through buffer text in the same manner as
   `display_line' and `display_string' do, but without producing any
   glyphs for the glyph matrices.  The workhorse of this is
   `move_it_in_display_line_to'.  Its code and logic are very similar
   to `display_line', but it differs in two important aspects: it
   doesn't produce glyphs for any glyph matrix, and it returns a
   status telling the caller how it ended the iteration: whether it
   reached the required position, hit the end of line, arrived at the
   window edge without exhausting the buffer's line, etc.  Since the
   glyphs are not produced, the layout information available to the
   callers of this function is what is recorded in `struct it' by the
   iteration process.

   Several higher-level functions call `move_it_in_display_line_to' to
   perform more complex tasks: `move_it_by_lines' can move N lines up
   or down from a given buffer position and `move_it_to' can move to a
   given buffer position or to a given X or Y pixel coordinate.

   These functions are called by the display engine itself as well,
   when it needs to make layout decisions before producing the glyphs.
   For example, one of the first things to decide when redisplaying a
   window is where to put the `window-start' position; if the window
   is to be recentered (the default), Emacs makes that decision by
   starting from the position of point, then moving up the number of
   lines corresponding to half the window height using
   `move_it_by_lines'.
*/
```

This "simulation mode" enables commands like `next-line` to work correctly with variable-height lines, images, and other complex display elements.

---

## Debugging the Display Engine

### GLYPH_DEBUG Mode

Compiling with `GLYPH_DEBUG` enabled adds extensive debugging infrastructure:

```c
#ifdef GLYPH_DEBUG
  /* A string identifying the method used to display the matrix.  */
  char method[512];
#endif
```

Each glyph matrix records how it was constructed, enabling diagnosis of optimization paths taken.

### Redisplay History

From `src/dispnew.c:144-193`:

```c
#ifdef GLYPH_DEBUG

/* One element of the ring buffer containing redisplay history
   information.  */

struct redisplay_history
{
  char trace[512 + 100];
};

/* The size of the history buffer.  */

#define REDISPLAY_HISTORY_SIZE	30

/* The redisplay history buffer.  */

static struct redisplay_history redisplay_history[REDISPLAY_HISTORY_SIZE];

/* Next free entry in redisplay_history.  */

static int history_idx;

/* A tick that's incremented each time something is added to the
   history.  */

static uintmax_t history_tick;

/* Add to the redisplay history how window W has been displayed.
   MSG is a trace containing the information how W's glyph matrix
   has been constructed.  */

static void
add_window_display_history (struct window *w, const char *msg)
{
  char *buf;
  void *ptr = w;

  if (history_idx >= REDISPLAY_HISTORY_SIZE)
    history_idx = 0;
  buf = redisplay_history[history_idx].trace;
  ++history_idx;

  snprintf (buf, sizeof redisplay_history[0].trace,
	    "%"PRIuMAX": window %p %s\n%s",
	    history_tick++,
	    ptr,
	    ((BUFFERP (w->contents)
	      && STRINGP (BVAR (XBUFFER (w->contents), name)))
	     ? SSDATA (BVAR (XBUFFER (w->contents), name))
	     : "???"),
	    msg);
}
```

The function `dump-redisplay-history` provides access to this circular buffer for debugging complex redisplay issues.

---

## Conclusion

The Emacs display engine represents decades of refinement in text rendering technology. Its design principles—separation of concerns, incremental updates, and abstract display elements—enable it to handle everything from simple text to complex bidirectional layouts with images, while maintaining excellent performance.

Key takeaways:

1. **Three-Phase Architecture**: Separation of decision (what to redisplay), generation (desired matrices), and rendering (physical updates) enables powerful optimizations.

2. **Glyph Matrices**: The central data structure provides a uniform representation of display content, enabling efficient comparison and minimal updates.

3. **The Display Iterator**: A sophisticated state machine handles the complexity of text properties, overlays, and bidirectional text while presenting a simple interface.

4. **Performance Through Layers**: Multiple optimization levels (try_cursor_movement → try_window_reusing_current_matrix → try_window_id → try_window) ensure fast common cases without sacrificing correctness.

5. **Platform Abstraction**: Clean separation between core logic and platform-specific rendering enables Emacs to run efficiently on everything from 1970s terminals to modern graphics workstations.

The display engine's complexity is justified by its capabilities: no other text editor can match Emacs's combination of flexibility (arbitrary text properties, images, variable fonts), correctness (proper bidirectional text), and performance (instant response even in multi-megabyte files).

For developers working on the display code, understanding these architectural principles is essential. The code is dense and highly optimized, but it follows consistent patterns throughout. Start with the high-level flow in `redisplay_internal()`, understand the iterator concept, and work through specific optimization paths as needed.

---

## References

- UAX#9: Unicode Bidirectional Algorithm - https://www.unicode.org/reports/tr9/
- Gerd Moellmann's original design notes (in source comments)
- GNU Emacs Internals Manual
- `src/xdisp.c` - Primary display engine implementation
- `src/dispnew.c` - Glyph matrix management
- `src/xfaces.c` - Face realization
- `src/bidi.c` - Bidirectional text support

---

**Document Version**: 1.0
**Last Updated**: 2025-11-18
**Authors**: Based on analysis of Emacs 30.x source code
