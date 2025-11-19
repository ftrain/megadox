# Chapter 05: Display Engine

**Status**: Planning
**Estimated Pages**: 180-220
**Prerequisites**: Chapters 01-04
**Dependencies**: Chapters 02 (Core Subsystems), 04 (Buffer Management)

## Chapter Overview

This chapter provides comprehensive coverage of Emacs' display engine, one of the most complex and performance-critical subsystems. It covers redisplay algorithms, glyph production, face system, font rendering, image handling, and bidirectional text support.

**Note**: The display engine (xdisp.c) is one of the largest and most complex files in Emacs (~35,000 lines). This chapter requires careful study and is essential for understanding how Emacs renders text.

## Learning Objectives

After reading this chapter, you should be able to:

1. Understand the complete redisplay pipeline from buffer text to screen
2. Explain the optimistic redisplay algorithm and optimizations
3. Trace glyph production from characters to rendered output
4. Understand face merging and the face cache
5. Comprehend font selection and rendering backends
6. Work with images and display properties
7. Understand bidirectional text rendering (UAX#9)
8. Optimize display-related performance issues

## Chapter Structure

### 01-display-architecture.md (25-30 pages)

**Topics:**
- Display system overview
- xdisp.c architecture (one of the largest Emacs source files)
- Display pipeline stages
  1. Text → Glyphs (glyph production)
  2. Glyphs → Matrix (window matrices)
  3. Matrix → Screen (actual rendering)
- Terminal abstraction layer
- Redisplay triggering

**Key Concepts:**
- Window matrices vs. frame matrices
- Glyph rows and glyph strings
- Display iterators
- Redisplay flags and optimization
- Terminal methods (term.c interface)

**Code Examples:**
```c
// Display iterator structure
struct it {
  struct window *w;           // Window being displayed
  struct buffer *buffer;      // Buffer being displayed
  ptrdiff_t charpos;         // Current character position
  ptrdiff_t bytepos;         // Current byte position
  struct glyph_row *glyph_row;  // Current glyph row
  // ... many more fields
};
```

**Figures:**
- Display pipeline flowchart
- Component interaction diagram
- Data structure relationships

**Critical Files:**
- src/xdisp.c (~35,000 lines - the heart of display)
- src/dispnew.c (display update and terminal I/O)
- src/dispextern.h (display data structures)

### 02-redisplay-algorithm.md (30-35 pages)

**Topics:**
- Redisplay overview
- Optimistic redisplay strategy
- try_window family of functions
  - try_window_id (optimistic, ID-based)
  - try_window_reusing_current_matrix
  - try_window (fallback)
- Window matrices
  - Current matrix (what's on screen)
  - Desired matrix (what should be on screen)
  - Matrix comparison
- Glyph matrices
  - Matrix rows
  - Row comparison
  - Change detection
- Optimization techniques
  - Unchanged text detection
  - Scrolling optimization
  - Cursor movement optimization
- Performance characteristics

**Key Concepts:**
- Redisplay cycle
- Matrix-based rendering
- Incremental update
- Change flags (redisplay flags)
- Window start and end
- Overlay handling during redisplay

**Code Examples:**
```c
// Core redisplay entry point
void redisplay_internal (void)
{
  // Mark windows for redisplay
  mark_window_display_accurate (window, true);

  // Try optimistic approaches first
  if (try_window_id (window))
    return;

  if (try_window_reusing_current_matrix (window))
    return;

  // Fall back to full redisplay
  try_window (window, startp, TRY_WINDOW_CHECK_MARGINS);
}
```

**Algorithms:**
- Matrix comparison algorithm
- Scrolling cost calculation
- Line insertion/deletion optimization

**Performance Notes:**
- Common bottlenecks
- Profiling techniques
- Optimization strategies

### 03-glyph-production.md (25-30 pages)

**Topics:**
- Character to glyph conversion
- Display iterator (struct it)
  - Iterator initialization
  - Iterator movement
  - Iterator state
- Font handling
  - Font selection
  - Glyph metrics
  - Font caching
- Face merging
  - Face inheritance
  - Face composition
  - Face cache
- Bidirectional text integration
  - Logical vs. visual order
  - Reordering levels
  - Bracket matching

**Key Concepts:**
- Glyph production pipeline
- Display specification evaluation
- Composition (e.g., for ligatures)
- Tab expansion
- Line truncation vs. wrapping
- Display tables

**Code Examples:**
```c
// Initialize display iterator
void init_iterator (struct it *it, struct window *w,
                   ptrdiff_t charpos, ptrdiff_t bytepos,
                   struct glyph_row *row, enum face_id base_face_id)
{
  it->w = w;
  it->f = XFRAME (w->frame);
  it->current = /* ... */;
  // ... extensive initialization
}

// Move iterator to next character
bool next_element_from_buffer (struct it *it)
{
  // Get next character
  int c = FETCH_CHAR (it->current.pos.bytepos);

  // Apply face
  it->face_id = FACE_FOR_CHAR (it->f, it->face, c, /* ... */);

  // Handle composition, display props, etc.
  // ...
}
```

**Data Structures:**
```
Glyph structure:
┌─────────────────┐
│ type            │ CHAR_GLYPH, IMAGE_GLYPH, etc.
│ u.ch            │ Character code (for CHAR_GLYPH)
│ face_id         │ Face index
│ pixel_width     │ Width in pixels
│ ascent, descent │ Vertical metrics
└─────────────────┘
```

### 04-face-system.md (20-25 pages)

**Topics:**
- Face definition and structure
- Face attributes
  - Foreground/background colors
  - Font family, size, slant, weight
  - Underline, overline, strike-through
  - Box, stipple
- Face merging rules
  - Inheritance hierarchy
  - Attribute priority
  - Default face
- Face caching
  - Face cache structure
  - Cache lookup
  - Cache invalidation
- Face realization
  - Merged face computation
  - Font selection
  - Color allocation
- Custom faces and themes

**Key Concepts:**
- Face ID vs. face spec
- Realized faces
- Face cache per frame
- Face merging algorithm
- Face remapping

**Code Examples:**
```c
// Realize a face
struct face *realize_face (struct face_cache *cache,
                          Lisp_Object attrs[], int former_face_id)
{
  struct face *face = make_empty_face (cache);

  // Merge attributes
  merge_face_vectors (cache->f, attrs, face->lface, 0);

  // Select font
  face->font = font_load_for_lface (cache->f, face->lface);

  // Allocate colors
  face->foreground = load_color (cache->f, face, face->lface[LFACE_FOREGROUND_INDEX],
                                 LFACE_FOREGROUND_INDEX);

  // Cache the face
  cache_face (cache, face, face->hash);

  return face;
}
```

**Figures:**
- Face inheritance diagram
- Face merging flowchart
- Face cache structure

### 05-font-rendering.md (25-30 pages)

**Topics:**
- Font backend abstraction
  - Font driver interface
  - Multiple backend support
- FreeType integration
  - Font loading
  - Glyph rendering
  - Hinting
- HarfBuzz shaping
  - Complex script support
  - Ligature handling
  - Contextual forms
- Font metrics
  - Ascent, descent, height
  - Advance width
  - Bounding boxes
- Font caching
  - Font cache structure
  - Glyph cache
  - Cache eviction
- Platform-specific backends
  - Xft (X11)
  - Core Text (macOS)
  - DirectWrite (Windows)

**Key Concepts:**
- Font objects vs. font specs
- Font selection algorithm
- Glyph caching strategy
- Font fallback mechanism
- Complex text layout

**Code Examples:**
```c
// Font driver structure (interface)
struct font_driver {
  Lisp_Object type;
  Lisp_Object (*list) (struct frame *, Lisp_Object);
  Lisp_Object (*match) (struct frame *, Lisp_Object);
  Lisp_Object (*open) (struct frame *, Lisp_Object, int);
  void (*close) (struct font *);
  int (*encode_char) (struct font *, int);
  void (*text_extents) (struct font *, unsigned *, int, struct font_metrics *);
  int (*draw) (struct glyph_string *, int, int, int, int, bool);
  // ... more methods
};
```

**Performance Analysis:**
- Font selection overhead
- Glyph cache hit rates
- Shaping performance
- Optimization techniques

### 06-images.md (15-20 pages)

**Topics:**
- Image types and formats
  - Built-in support (XPM, PBM, XBM)
  - External libraries (PNG, JPEG, GIF, TIFF)
  - SVG rendering
- Image cache
  - Cache structure
  - Cache key computation
  - Cache size limits
- Image slices and manipulation
  - Image slicing
  - Image rotation
  - Image scaling
- ImageMagick integration (optional)
- Native image support
- Image display properties

**Key Concepts:**
- Image descriptors
- Image objects
- Image cache management
- Lazy image loading
- Image transformations

**Code Examples:**
```elisp
;; Image creation
(create-image "image.png" 'png nil :scale 2.0)

;; Display image
(insert-image (create-image "icon.svg" 'svg))

;; Image slice
(insert-image img nil nil nil
              :slice '(100 100 200 200))
```

### 07-display-properties.md (15-18 pages)

**Topics:**
- Display specifications
  - Display property syntax
  - Display strings
  - Display images
  - Space specifications
- Overlay strings
  - Before-string and after-string
  - Overlay priority
- Invisible text
  - Selective display
  - Outline mode
  - Folding
- Display margins
  - Left and right margins
  - Line number display
- Display tables
  - Character display customization

**Key Concepts:**
- Display spec evaluation
- Overlay interaction
- Invisibility handling
- Display vs. buffer position

**Code Examples:**
```elisp
;; Display property
(put-text-property start end 'display "replacement text")

;; Display image
(put-text-property start end 'display
  (create-image "icon.png" 'png))

;; Space specification
(put-text-property start end 'display
  '(space :width 10))

;; Invisible text
(put-text-property start end 'invisible t)
```

### 08-bidi.md (20-25 pages)

**Topics:**
- Bidirectional text overview
- UAX#9 implementation
  - Unicode Bidirectional Algorithm
  - Embedding levels
  - Bracket matching
  - Mirroring
- Reordering engine
  - Level runs
  - Visual ordering
  - Cursor movement
- Performance considerations
  - Bidi cache
  - Optimization strategies
  - Common bottlenecks
- RTL (Right-to-Left) editing
  - RTL mode
  - Mixed LTR/RTL text
  - Cursor behavior

**Key Concepts:**
- Logical vs. visual order
- Embedding levels (0-61)
- Paragraph direction
- Isolate sequences
- Bidi categories

**Code Examples:**
```c
// Bidi iterator
struct bidi_it {
  ptrdiff_t charpos;         // Logical position
  ptrdiff_t bytepos;         // Byte position
  int level;                 // Embedding level
  bidi_type_t type;          // Character type
  // ... more fields
};

// Move bidi iterator
void bidi_move_to_visually_next (struct bidi_it *bidi_it)
{
  // Compute next visual position
  // Handle level changes, reordering, etc.
}
```

**Figures:**
- Bidi reordering example
- Level assignment diagram
- Visual vs. logical order

### 09-scrolling.md (12-15 pages)

**Topics:**
- Scroll algorithms
  - Line-based scrolling
  - Pixel-based scrolling
  - Smooth scrolling
- Scroll bars
  - Scroll bar representation
  - Scroll bar interaction
  - Platform differences
- Pixel scrolling
  - Vscroll implementation
  - Sub-line scrolling
- Smooth scrolling
  - Animation
  - Performance
- Scroll margins and conserve

**Key Concepts:**
- Window start position
- Vscroll offset
- Scroll bar state
- Redisplay interaction

**Code Examples:**
```elisp
;; Pixel scroll mode
(pixel-scroll-mode 1)

;; Smooth scrolling
(setq scroll-conservatively 101)
(setq scroll-margin 5)

;; Mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
```

## Key Takeaways

1. **Complexity**: The display engine is the most complex subsystem in Emacs
2. **Optimization**: Heavy optimization for common cases (unchanged text)
3. **Matrix-Based**: Uses matrix comparison for efficient updates
4. **Font Complexity**: Font selection and rendering is sophisticated
5. **Bidi Support**: Full Unicode bidirectional text support
6. **Performance Critical**: Display performance affects perceived responsiveness

## Prerequisites

### Required Knowledge

- Strong C programming skills
- Understanding of graphics rendering concepts
- Knowledge of Unicode and text encoding
- Familiarity with font rendering concepts

### Recommended Background

- Computer graphics fundamentals
- Algorithm optimization
- Cache design
- Typography basics
- Unicode standard (especially UAX#9)

## Cross-References

### This Chapter References

- [@chap:01] Architecture (terminal abstraction)
- [@chap:02] Core Subsystems (data structures)
- [@chap:04] Buffer Management (text representation)
- [@chap:06] Window/Frame System (window structure)
- [@chap:07] Text Properties (display properties)
- [@chap:15] Internationalization (character encoding)
- [@chap:16] Font Rendering (detailed font coverage)

### Referenced By

- [@chap:06] Window/Frame System
- [@chap:08] Major Modes (font-lock)
- [@chap:21] Advanced Topics (performance optimization)

## Critical Performance Considerations

### Redisplay Bottlenecks

1. **Long lines**: Can cause exponential slowdown
2. **Many overlays**: Overlay sorting is O(n log n)
3. **Complex faces**: Face merging is expensive
4. **Bidirectional text**: Bidi reordering adds overhead
5. **Font selection**: Can be slow with many fonts
6. **Image rendering**: Large images affect scrolling

### Optimization Techniques

1. **Limit line length**: Consider auto-fill-mode
2. **Reduce overlay count**: Merge adjacent overlays
3. **Cache faces**: Reuse face IDs
4. **Optimize font selection**: Use font-family explicitly
5. **Lazy image loading**: Use image-load-path efficiently

## Debugging Display Issues

### Common Techniques

```elisp
;; Enable redisplay debugging
(setq inhibit-trace-redisplay nil)

;; Profile redisplay
M-x profiler-start RET cpu RET
;; ... perform slow operation ...
M-x profiler-report RET

;; Check redisplay state
M-: (window-start) RET
M-: (window-end nil t) RET

;; Trace redisplay
(trace-function 'redisplay)
```

### GDB Debugging

```bash
gdb ./src/emacs
(gdb) break try_window_id
(gdb) run
(gdb) print *it
(gdb) print *w
```

## Exercises

1. **Trace Redisplay**: Step through redisplay for a simple buffer
2. **Glyph Production**: Follow character-to-glyph conversion
3. **Face Merging**: Understand face merging for overlapping faces
4. **Bidi Example**: Trace bidi reordering for mixed LTR/RTL text
5. **Performance Analysis**: Profile display of a large file
6. **Custom Display**: Implement a custom display property

## Further Reading

### Papers and Standards
- [@unicode:bidi:2023] Unicode Bidirectional Algorithm (UAX#9)
- [@knuth:breaking:1981] Line breaking algorithm
- [@behdad:harfbuzz:2020] Text shaping with HarfBuzz

### Source Code
- src/xdisp.c (35,000+ lines - read carefully!)
- src/dispnew.c
- src/xfaces.c
- src/bidi.c

### External Resources
- FreeType documentation
- HarfBuzz manual
- Unicode standard

## Status and Todo

- [ ] Draft 01-display-architecture.md
- [ ] Draft 02-redisplay-algorithm.md
- [ ] Draft 03-glyph-production.md
- [ ] Draft 04-face-system.md
- [ ] Draft 05-font-rendering.md
- [ ] Draft 06-images.md
- [ ] Draft 07-display-properties.md
- [ ] Draft 08-bidi.md
- [ ] Draft 09-scrolling.md
- [ ] Create display pipeline diagrams
- [ ] Create matrix structure diagrams
- [ ] Create face merging flowcharts
- [ ] Create bidi reordering examples
- [ ] Test all code examples
- [ ] Performance benchmarks
- [ ] Write debugging guide
- [ ] Peer review
- [ ] Technical review by display experts

## Warnings

⚠️ **This is a complex chapter**. The display engine is one of the most sophisticated parts of Emacs. Take your time, refer back to earlier chapters, and don't hesitate to experiment with the code.

⚠️ **xdisp.c is massive**. At ~35,000 lines, it's one of the largest source files in Emacs. Don't try to read it all at once. Focus on specific functions and build understanding incrementally.

⚠️ **Performance matters**. Display performance directly affects user experience. Pay attention to performance notes and optimization techniques.

## Changelog

- 2025-11-18: Initial chapter structure and README created
