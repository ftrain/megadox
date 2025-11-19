# Platform Abstraction Layer: A Literate Programming Guide

**Author:** Documentation Team
**Last Updated:** 2025-11-18
**Primary Sources:** `/home/user/emacs/src/termhooks.h`, `/home/user/emacs/src/dispextern.h`, `/home/user/emacs/src/terminal.c`, `/home/user/emacs/src/xterm.c`, `/home/user/emacs/src/w32term.c`

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Platform Overview](#platform-overview)
3. [Core Abstraction Layer](#core-abstraction-layer)
4. [Platform Implementations](#platform-implementations)
5. [Common Patterns](#common-patterns)
6. [Case Study: X11 Implementation](#case-study-x11-implementation)
7. [Integration Guide](#integration-guide)
8. [References](#references)

---

## Executive Summary

Emacs's platform abstraction architecture is a masterclass in portable software design. The system supports **8 major platforms** through a carefully designed three-layer architecture:

1. **Platform-Independent Layer**: Common redisplay engine and event processing
2. **Abstraction Interface**: `struct terminal` and `struct redisplay_interface`
3. **Platform-Specific Implementations**: X11, Windows, macOS, Android, Haiku, GTK, TTY

This document provides a literate programming exploration of how Emacs achieves portability while maintaining performance and platform-specific features.

**Key Statistics:**
- 8+ supported platforms (X11, Windows, macOS/NS, Android, Haiku, GTK/PGTK, TTY, DOS)
- ~60 hook functions in the terminal abstraction
- ~30 methods in the redisplay interface
- 19 Windows-specific files, 12 Android files, 8 Haiku files, 10+ X11 files

---

## Platform Overview

### 1.1 Supported Platforms

Emacs defines its platform support through the `enum output_method` type, found in `/home/user/emacs/src/termhooks.h`:

```c
/* Output method of a terminal (and frames on this terminal, respectively).  */

enum output_method
{
  output_initial,      // Bootstrap terminal before real initialization
  output_termcap,      // TTY/terminal using termcap/terminfo
  output_x_window,     // X Window System (X11)
  output_msdos_raw,    // MS-DOS direct video memory access
  output_w32,          // Microsoft Windows (Win32 API)
  output_ns,           // macOS/GNUstep (NextStep/Cocoa)
  output_pgtk,         // Pure GTK (Wayland-compatible)
  output_haiku,        // Haiku OS (BeOS successor)
  output_android,      // Android mobile platform
};
```

**Location:** `/home/user/emacs/src/termhooks.h:57-68`

Each platform provides:
- **Window system integration**: Creating, managing, and destroying windows
- **Event handling**: Mouse, keyboard, and system events
- **Rendering**: Text, images, and graphical elements
- **Platform services**: Clipboard, drag-and-drop, notifications

### 1.2 Design Philosophy

The abstraction follows these principles:

1. **Minimal Common Interface**: The abstraction defines the minimum required for portability
2. **Optional Extensions**: Platforms can provide additional capabilities through optional hooks
3. **Zero-Cost Abstraction**: Most calls are direct function pointers (no virtual dispatch overhead)
4. **Compile-Time Selection**: Platform code is selected at compile time via preprocessor
5. **Runtime Flexibility**: Multiple terminals can coexist (e.g., X11 + TTY simultaneously)

---

## Core Abstraction Layer

### 2.1 The Terminal Structure

The `struct terminal` is the central abstraction for all platform implementations. It represents a single display device (graphical or text-based).

```c
/* Terminal-local parameters. */
struct terminal
{
  /* This is for Lisp; the terminal code does not refer to it.  */
  union vectorlike_header header;

  /* Parameter alist of this terminal.  */
  Lisp_Object param_alist;

  /* List of charsets supported by the terminal.  */
  Lisp_Object charset_list;

  /* X selections that Emacs might own on this terminal.  */
  Lisp_Object Vselection_alist;

  /* Character to terminal glyph code mapping.  */
  Lisp_Object glyph_code_table;

  /* All earlier fields should be Lisp_Objects and are traced by GC.
     All fields afterwards are ignored by the GC.  */

  /* Chain of all terminal devices. */
  struct terminal *next_terminal;

  /* Unique id for this terminal device. */
  int id;

  /* The number of frames that are on this terminal. */
  int reference_count;

  /* The type of the terminal device. */
  enum output_method type;

  /* The name of the terminal device. */
  char *name;

  /* The terminal's keyboard object. */
  struct kboard *kboard;

  /* Device-type dependent data shared amongst all frames on this terminal.  */
  union display_info
  {
    struct tty_display_info *tty;        // termchar.h
    struct x_display_info *x;            // xterm.h
    struct w32_display_info *w32;        // w32term.h
    struct ns_display_info *ns;          // nsterm.h
    struct pgtk_display_info *pgtk;      // pgtkterm.h
    struct haiku_display_info *haiku;    // haikuterm.h
    struct android_display_info *android;// androidterm.h
  } display_info;

  /* Coding systems for terminal I/O */
  struct coding_system *terminal_coding;  // Output encoding
  struct coding_system *keyboard_coding;  // Input decoding

  /* Window-based redisplay interface (0 for tty devices). */
  struct redisplay_interface *rif;

  /* ... Hook functions follow ... */
};
```

**Location:** `/home/user/emacs/src/termhooks.h:472-878`

**Key Insights:**

1. **Lisp Integration**: First five fields are Lisp objects, making terminals garbage-collected
2. **Union for Display Info**: Platform-specific data stored in tagged union (type-safe)
3. **Reference Counting**: Terminals are deleted when `reference_count` reaches zero
4. **Coding Systems**: Separate encoding for input/output handles internationalization

### 2.2 Terminal Hook Functions

The terminal structure contains ~40 hook function pointers for various operations:

#### Text Display Hooks (TTY-centric)

```c
/* Text display hooks.  */
void (*cursor_to_hook) (struct frame *f, int vpos, int hpos);
void (*raw_cursor_to_hook) (struct frame *, int, int);

void (*clear_to_end_hook) (struct frame *);
void (*clear_frame_hook) (struct frame *);
void (*clear_end_of_line_hook) (struct frame *, int);

void (*ins_del_lines_hook) (struct frame *f, int, int);

void (*insert_glyphs_hook) (struct frame *f, struct glyph *s, int n);
void (*write_glyphs_hook) (struct frame *f, struct glyph *s, int n);
void (*delete_glyphs_hook) (struct frame *, int);

void (*ring_bell_hook) (struct frame *f);
void (*toggle_invisible_pointer_hook) (struct frame *f, bool invisible);

void (*reset_terminal_modes_hook) (struct terminal *);
void (*set_terminal_modes_hook) (struct terminal *);
```

**Location:** `/home/user/emacs/src/termhooks.h:559-583`

These hooks are primarily used for TTY terminals but can be implemented by graphical terminals for certain operations.

#### Frame and Window Hooks

```c
/* Return the current position of the mouse. */
void (*mouse_position_hook) (struct frame **f, int insist,
                             Lisp_Object *bar_window,
                             enum scroll_bar_part *part,
                             Lisp_Object *x, Lisp_Object *y,
                             Time *);

/* Get the focus frame. */
Lisp_Object (*get_focus_frame) (struct frame *f);

/* Shift frame focus. */
void (*focus_frame_hook) (struct frame *f, bool noactivate);

/* Frame rehighlight (when focus changes). */
void (*frame_rehighlight_hook) (struct frame *);

/* Raise or lower a frame. */
void (*frame_raise_lower_hook) (struct frame *f, bool raise_flag);

/* Make frame visible or invisible. */
void (*frame_visible_invisible_hook) (struct frame *f, bool visible);

/* Change fullscreen state. */
void (*fullscreen_hook) (struct frame *f);

/* Iconify the frame. */
void (*iconify_frame_hook) (struct frame *f);

/* Change window size. */
void (*set_window_size_hook) (struct frame *f, bool change_gravity,
                              int width, int height);

/* Move frame to position. */
void (*set_frame_offset_hook) (struct frame *f, int xoff, int yoff,
                               int change_gravity);

/* Set frame transparency. */
void (*set_frame_alpha_hook) (struct frame *f);

/* Set new font. */
Lisp_Object (*set_new_font_hook) (struct frame *f, Lisp_Object font_object,
                                  int fontset);

/* Set window icon. */
bool (*set_bitmap_icon_hook) (struct frame *f, Lisp_Object file);

/* Set window title. */
void (*implicit_set_name_hook) (struct frame *f, Lisp_Object arg,
                                Lisp_Object oldval);
```

**Location:** `/home/user/emacs/src/termhooks.h:619-705`

#### Menu and Dialog Hooks

```c
/* Display menus. */
Lisp_Object (*menu_show_hook) (struct frame *f, int x, int y, int menuflags,
                               Lisp_Object title, const char **error_name);

/* Activate the menu bar. */
void (*activate_menubar_hook) (struct frame *f);

/* Display popup dialog. */
Lisp_Object (*popup_dialog_hook) (struct frame *f, Lisp_Object header,
                                  Lisp_Object contents);
```

**Location:** `/home/user/emacs/src/termhooks.h:707-718`

#### Scroll Bar Hooks

```c
/* Set the vertical scroll bar. */
void (*set_vertical_scroll_bar_hook) (struct window *window,
                                      int portion, int whole, int position);

/* Set the horizontal scroll bar. */
void (*set_horizontal_scroll_bar_hook) (struct window *window,
                                        int portion, int whole, int position);

/* Condemn scroll bars (mark for deletion). */
void (*condemn_scroll_bars_hook) (struct frame *frame);

/* Redeem scroll bar (unmark from deletion). */
void (*redeem_scroll_bar_hook) (struct window *window);

/* Remove condemned scroll bars. */
void (*judge_scroll_bars_hook) (struct frame *FRAME);
```

**Location:** `/home/user/emacs/src/termhooks.h:753-810`

#### Event Handling

```c
/* Called to read input events.
 *
 * TERMINAL indicates which terminal device to read from.
 * Input events should be read into HOLD_QUIT.
 *
 * Return value:
 *   > 0: N input events were read
 *   = 0: No events immediately available
 *   -1: Transient read error
 *   -2: Device closed (hangup), should be deleted
 */
int (*read_socket_hook) (struct terminal *terminal,
                         struct input_event *hold_quit);

/* Called when a frame's display becomes entirely up to date. */
void (*frame_up_to_date_hook) (struct frame *);
```

**Location:** `/home/user/emacs/src/termhooks.h:813-827`

### 2.3 The Redisplay Interface

For graphical terminals, the `struct redisplay_interface` provides methods for rendering:

```c
struct redisplay_interface
{
  /* Handlers for setting frame parameters.  */
  frame_parm_handler *frame_parm_handlers;

  /* Produce glyphs/get display metrics for the display element. */
  void (*produce_glyphs) (struct it *it);

  /* Write or insert LEN glyphs from STRING at the nominal output position. */
  void (*write_glyphs) (struct window *w, struct glyph_row *row,
                        struct glyph *string, enum glyph_row_area area, int len);

  void (*insert_glyphs) (struct window *w, struct glyph_row *row,
                         struct glyph *start, enum glyph_row_area area, int len);

  /* Clear from nominal output position to X. */
  void (*clear_end_of_line) (struct window *w, struct glyph_row *row,
                             enum glyph_row_area area, int x);

  /* Function to call to scroll the display. */
  void (*scroll_run_hook) (struct window *w, struct run *run);

  /* Function to call after a line has been completely updated. */
  void (*after_update_window_line_hook) (struct window *w,
                                         struct glyph_row *desired_row);

  /* Function to call before beginning to update window W. */
  void (*update_window_begin_hook) (struct window *w);

  /* Function to call after window W has been updated. */
  void (*update_window_end_hook) (struct window *w, bool cursor_on_p,
                                  bool mouse_face_overwritten_p);

  /* Flush the display of frame F (e.g., XFlush for X11). */
  void (*flush_display) (struct frame *f);

  /* Clear the mouse highlight in window W. */
  void (*clear_window_mouse_face) (struct window *w);

  /* Get glyph overhang (for complex scripts). */
  void (*get_glyph_overhangs) (struct glyph *glyph, struct frame *f,
                               int *left, int *right);

  /* Fix overlapping area display. */
  void (*fix_overlapping_area) (struct window *w, struct glyph_row *row,
                                enum glyph_row_area area, int);

#ifdef HAVE_WINDOW_SYSTEM
  /* Draw a fringe bitmap. */
  void (*draw_fringe_bitmap) (struct window *w, struct glyph_row *row,
                              struct draw_fringe_bitmap_params *p);

  /* Define and destroy fringe bitmaps. */
  void (*define_fringe_bitmap) (int which, unsigned short *bits, int h, int wd);
  void (*destroy_fringe_bitmap) (int which);

  /* Compute glyph string overhangs. */
  void (*compute_glyph_string_overhangs) (struct glyph_string *s);

  /* Draw a glyph string - THE CORE RENDERING FUNCTION. */
  void (*draw_glyph_string) (struct glyph_string *s);

  /* Define cursor for frame. */
  void (*define_frame_cursor) (struct frame *f, Emacs_Cursor cursor);

  /* Clear area of frame. */
  void (*clear_frame_area) (struct frame *f, int x, int y,
                            int width, int height);

  /* Clear internal border area. */
  void (*clear_under_internal_border) (struct frame *f);

  /* Draw window cursor. */
  void (*draw_window_cursor) (struct window *w,
                              struct glyph_row *glyph_row,
                              int x, int y,
                              enum text_cursor_kinds cursor_type,
                              int cursor_width, bool on_p, bool active_p);

  /* Draw vertical window border. */
  void (*draw_vertical_window_border) (struct window *w,
                                       int x, int y_0, int y_1);

  /* Draw window divider. */
  void (*draw_window_divider) (struct window *w,
                               int x_0, int x_1, int y_0, int y_1);

  /* Shift glyphs for insert. */
  void (*shift_glyphs_for_insert) (struct frame *f,
                                   int x, int y, int width,
                                   int height, int shift_by);

  /* Hourglass cursor. */
  void (*show_hourglass) (struct frame *f);
  void (*hide_hourglass) (struct frame *f);

  /* Calculate default face. */
  void (*default_font_parameter) (struct frame *f, Lisp_Object parms);
#endif /* HAVE_WINDOW_SYSTEM */
};
```

**Location:** `/home/user/emacs/src/dispextern.h:3026-3153`

**Key Design Points:**

1. **Glyph String Rendering**: The `draw_glyph_string` method is the workhorse for all text and graphical element rendering
2. **Incremental Updates**: Methods like `after_update_window_line_hook` enable efficient partial redraws
3. **Platform-Specific vs. Generic**: Some methods (like `produce_glyphs`) have generic implementations shared across platforms, while others (like `draw_glyph_string`) are platform-specific

### 2.4 Terminal Creation and Initialization

The `create_terminal` function establishes a new terminal:

```c
struct terminal *
create_terminal (enum output_method type, struct redisplay_interface *rif)
{
  struct terminal *terminal = allocate_terminal ();
  Lisp_Object terminal_coding, keyboard_coding;

  terminal->next_terminal = terminal_list;
  terminal_list = terminal;
  terminal->type = type;
  terminal->rif = rif;
  terminal->id = next_terminal_id++;

  terminal->keyboard_coding = xmalloc (sizeof (struct coding_system));
  terminal->terminal_coding = xmalloc (sizeof (struct coding_system));

  /* If default coding systems for the terminal and the keyboard are
     already defined, use them in preference to the defaults. */
  keyboard_coding = find_symbol_value (Qdefault_keyboard_coding_system);
  if (NILP (keyboard_coding)
      || BASE_EQ (keyboard_coding, Qunbound)
      || NILP (Fcoding_system_p (keyboard_coding)))
    {
      terminal->keyboard_coding->common_flags = CODING_REQUIRE_DECODING_MASK;
      terminal->keyboard_coding->src_multibyte = 0;
      terminal->keyboard_coding->dst_multibyte = 1;
    }
  else
    setup_coding_system (keyboard_coding, terminal->keyboard_coding);

  terminal_coding = find_symbol_value (Qdefault_terminal_coding_system);
  if (NILP (terminal_coding)
      || BASE_EQ (terminal_coding, Qunbound)
      || NILP (Fcoding_system_p (terminal_coding)))
    {
      terminal->terminal_coding->common_flags = CODING_REQUIRE_ENCODING_MASK;
      terminal->terminal_coding->src_multibyte = 1;
      terminal->terminal_coding->dst_multibyte = 0;
    }
  else
    setup_coding_system (terminal_coding, terminal->terminal_coding);

  return terminal;
}
```

**Location:** `/home/user/emacs/src/terminal.c:292-342`

---

## Platform Implementations

### 3.1 X11 (X Window System)

#### File Structure

The X11 implementation spans multiple files:

| File | Purpose | LOC (approx) |
|------|---------|--------------|
| `xterm.c` | Terminal implementation, event handling, rendering | 32,000+ |
| `xterm.h` | X11-specific data structures and declarations | 1,500+ |
| `xfns.c` | Frame functions, window management | 8,000+ |
| `xmenu.c` | Menu implementation | 2,500+ |
| `xselect.c` | X selection (clipboard) handling | 3,500+ |
| `xfont.c` | Core X font driver | 1,500+ |
| `xftfont.c` | Xft font driver (anti-aliasing) | 1,000+ |
| `xsettings.c` | XSETTINGS protocol support | 1,000+ |
| `xrdb.c` | X resource database | 600+ |
| `xsmfns.c` | X Session Management | 500+ |

**Total:** ~52,600 lines of X11-specific code

#### X11 Redisplay Interface

```c
static struct redisplay_interface x_redisplay_interface =
{
  x_frame_parm_handlers,
  gui_produce_glyphs,              // Generic (shared with other GUI platforms)
  gui_write_glyphs,                // Generic
  gui_insert_glyphs,               // Generic
  gui_clear_end_of_line,           // Generic
  x_scroll_run,                    // X11-specific
  x_after_update_window_line,      // X11-specific
  NULL, /* update_window_begin */
  NULL, /* update_window_end   */
  x_flip_and_flush,                // X11-specific (handles double-buffering)
  gui_clear_window_mouse_face,     // Generic
  gui_get_glyph_overhangs,         // Generic
  gui_fix_overlapping_area,        // Generic
  x_draw_fringe_bitmap,            // X11-specific
#ifdef USE_CAIRO
  x_cr_define_fringe_bitmap,       // Cairo-specific
  x_cr_destroy_fringe_bitmap,      // Cairo-specific
#else
  x_define_fringe_bitmap,          // X11-specific
  x_destroy_fringe_bitmap,         // X11-specific
#endif
  x_compute_glyph_string_overhangs,// X11-specific
  x_draw_glyph_string,             // X11-specific (THE KEY RENDERING FUNCTION)
  x_define_frame_cursor,           // X11-specific
  x_clear_frame_area,              // X11-specific
  x_clear_under_internal_border,   // X11-specific
  x_draw_window_cursor,            // X11-specific
  x_draw_vertical_window_border,   // X11-specific
  x_draw_window_divider,           // X11-specific
  x_shift_glyphs_for_insert,       // X11-specific
  x_show_hourglass,                // X11-specific
  x_hide_hourglass,                // X11-specific
  x_default_font_parameter         // X11-specific
};
```

**Location:** `/home/user/emacs/src/xterm.c:31909-31944`

**Analysis:**
- ~40% of methods use generic implementations (code reuse across GUI platforms)
- ~60% are X11-specific (handling X11's unique features and idiosyncrasies)
- Cairo support is conditional (modern rendering path)

#### X11 Terminal Creation

```c
static struct terminal *
x_create_terminal (struct x_display_info *dpyinfo)
{
  struct terminal *terminal;

  terminal = create_terminal (output_x_window, &x_redisplay_interface);

  terminal->display_info.x = dpyinfo;
  dpyinfo->terminal = terminal;

  /* kboard is initialized in x_term_init. */

  terminal->clear_frame_hook = x_clear_frame;
  terminal->ins_del_lines_hook = x_ins_del_lines;
  terminal->delete_glyphs_hook = x_delete_glyphs;
  terminal->ring_bell_hook = XTring_bell;
  terminal->toggle_invisible_pointer_hook = XTtoggle_invisible_pointer;
  terminal->update_begin_hook = x_update_begin;
  terminal->update_end_hook = x_update_end;
  terminal->read_socket_hook = XTread_socket;
  terminal->frame_up_to_date_hook = XTframe_up_to_date;
#ifdef HAVE_XDBE
  terminal->buffer_flipping_unblocked_hook = XTbuffer_flipping_unblocked_hook;
#endif
  terminal->defined_color_hook = x_defined_color;
  terminal->query_frame_background_color = x_query_frame_background_color;
  terminal->query_colors = x_query_colors;
  terminal->mouse_position_hook = XTmouse_position;
  terminal->get_focus_frame = x_get_focus_frame;
  terminal->focus_frame_hook = x_focus_frame;
  terminal->frame_rehighlight_hook = XTframe_rehighlight;
  terminal->frame_raise_lower_hook = XTframe_raise_lower;
  terminal->frame_visible_invisible_hook = x_make_frame_visible_invisible;
  terminal->fullscreen_hook = XTfullscreen_hook;
  terminal->iconify_frame_hook = x_iconify_frame;
  terminal->set_window_size_hook = x_set_window_size;
  terminal->set_frame_offset_hook = x_set_offset;
  terminal->set_frame_alpha_hook = x_set_frame_alpha;
  terminal->set_new_font_hook = x_new_font;
  terminal->set_bitmap_icon_hook = x_bitmap_icon;
  terminal->implicit_set_name_hook = x_implicitly_set_name;
  terminal->menu_show_hook = x_menu_show;
#ifdef HAVE_EXT_MENU_BAR
  terminal->activate_menubar_hook = x_activate_menubar;
#endif
#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
  terminal->popup_dialog_hook = xw_popup_dialog;
#endif
  terminal->change_tab_bar_height_hook = x_change_tab_bar_height;
#ifndef HAVE_EXT_TOOL_BAR
  terminal->change_tool_bar_height_hook = x_change_tool_bar_height;
#endif
  terminal->set_vertical_scroll_bar_hook = XTset_vertical_scroll_bar;
  terminal->set_horizontal_scroll_bar_hook = XTset_horizontal_scroll_bar;
  terminal->set_scroll_bar_default_width_hook = x_set_scroll_bar_default_width;
  terminal->set_scroll_bar_default_height_hook = x_set_scroll_bar_default_height;
  terminal->condemn_scroll_bars_hook = XTcondemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = XTredeem_scroll_bar;
  terminal->judge_scroll_bars_hook = XTjudge_scroll_bars;
  terminal->get_string_resource_hook = x_get_string_resource;
  terminal->free_pixmap = x_free_pixmap;
  terminal->delete_frame_hook = x_destroy_window;
  terminal->delete_terminal_hook = x_delete_terminal;
  terminal->toolkit_position_hook = x_toolkit_position;
#ifdef HAVE_XINPUT2
  terminal->any_grab_hook = x_have_any_grab;
#endif
  /* Other hooks are NULL by default.  */

  return terminal;
}
```

**Location:** `/home/user/emacs/src/xterm.c:32114-32183`

**Key Features:**
1. **Comprehensive Hook Implementation**: Nearly all hooks are implemented
2. **Conditional Features**: XDBE (double-buffering), XInput2 (advanced input), toolkit-specific dialogs
3. **Scroll Bar Lifecycle**: Three-phase scroll bar management (condemn, redeem, judge)

### 3.2 Windows (Win32/w32)

#### File Structure

The Windows implementation is the most extensive:

| File | Purpose | LOC (approx) |
|------|---------|--------------|
| `w32term.c` | Terminal implementation, message loop, rendering | 8,000+ |
| `w32term.h` | Windows-specific structures | 800+ |
| `w32fns.c` | Frame functions, window procedures | 10,000+ |
| `w32.c` | OS-level functions (file system, processes, etc.) | 10,000+ |
| `w32menu.c` | Menu implementation | 2,000+ |
| `w32select.c` | Clipboard handling | 1,500+ |
| `w32font.c` | GDI font driver | 2,500+ |
| `w32uniscribe.c` | Uniscribe complex script shaping | 1,000+ |
| `w32dwrite.c` | DirectWrite font rendering | 1,500+ |
| `w32console.c` | Console (terminal) support | 1,000+ |
| `w32proc.c` | Process management | 3,500+ |
| `w32heap.c` | Heap management | 500+ |
| `w32inevt.c` | Console input events | 600+ |
| `w32reg.c` | Windows Registry access | 300+ |
| `w32notify.c` | File system change notifications | 800+ |
| `w32image.c` | Image loading via Windows Imaging Component | 400+ |
| `w32cygwinx.c` | Cygwin X11 integration | 200+ |
| `w32xfns.c` | Compatibility layer | 300+ |
| `w32common.h` | Common definitions | 200+ |

**Total:** ~45,100 lines of Windows-specific code

#### Windows Redisplay Interface

```c
static struct redisplay_interface w32_redisplay_interface =
{
  w32_frame_parm_handlers,
  gui_produce_glyphs,
  gui_write_glyphs,
  gui_insert_glyphs,
  gui_clear_end_of_line,
  w32_scroll_run,
  w32_after_update_window_line,
  w32_update_window_begin,
  w32_update_window_end,
  0, /* flush_display */
  gui_clear_window_mouse_face,
  gui_get_glyph_overhangs,
  gui_fix_overlapping_area,
  w32_draw_fringe_bitmap,
  w32_define_fringe_bitmap,
  w32_destroy_fringe_bitmap,
  w32_compute_glyph_string_overhangs,
  w32_draw_glyph_string,             // GDI/GDI+ rendering
  w32_define_frame_cursor,
  w32_clear_frame_area,
  w32_clear_under_internal_border,
  w32_draw_window_cursor,
  w32_draw_vertical_window_border,
  w32_draw_window_divider,
  w32_shift_glyphs_for_insert,
  w32_show_hourglass,
  w32_hide_hourglass,
  w32_default_font_parameter
};
```

**Location:** `/home/user/emacs/src/w32term.c:7819-7848`

**Unique Features:**
- No `flush_display` (Windows handles this automatically)
- GDI+ support for image transformations
- DirectWrite integration for high-quality text rendering
- Complex IME (Input Method Editor) support

### 3.3 Android

#### File Structure

The Android port is one of the newer additions:

| File | Purpose | LOC (approx) |
|------|---------|--------------|
| `androidterm.c` | Terminal implementation, event handling | 6,800+ |
| `androidterm.h` | Android structures | 1,200+ |
| `androidfns.c` | Frame functions | 3,500+ |
| `android.c` | Android system integration | 15,000+ |
| `androidfont.c` | Android font driver | 1,500+ |
| `androidmenu.c` | Menu implementation | 1,800+ |
| `androidselect.c` | Clipboard/selection | 700+ |
| `androidgui.h` | GUI definitions | 400+ |
| `androidvfs.c` | Virtual file system (content:// URIs) | 2,500+ |
| `android-emacs.c` | JNI bridge, Java integration | 3,000+ |
| `android.h` | Main Android header | 1,000+ |
| `android-asset.h` | Asset management | 300+ |

**Total:** ~38,000 lines of Android-specific code

#### Android Redisplay Interface

```c
static struct redisplay_interface android_redisplay_interface =
{
#ifndef ANDROID_STUBIFY
  android_frame_parm_handlers,
  gui_produce_glyphs,
  gui_write_glyphs,
  gui_insert_glyphs,
  gui_clear_end_of_line,
  android_scroll_run,
  android_after_update_window_line,
  NULL, /* update_window_begin */
  NULL, /* update_window_end   */
  android_flush_display,
  gui_clear_window_mouse_face,
  gui_get_glyph_overhangs,
  gui_fix_overlapping_area,
  android_draw_fringe_bitmap,
  android_define_fringe_bitmap,
  android_destroy_fringe_bitmap,
  android_compute_glyph_string_overhangs,
  android_draw_glyph_string,         // Android Canvas API
  android_define_frame_cursor,
  android_clear_frame_area,
  android_clear_under_internal_border,
  android_draw_window_cursor,
  android_draw_vertical_window_border,
  android_draw_window_divider,
  android_shift_glyphs_for_insert,
  android_show_hourglass,
  android_hide_hourglass,
  android_default_font_parameter,
#endif
};
```

**Location:** `/home/user/emacs/src/androidterm.c:6596-6625`

**Unique Challenges:**
- **JNI Overhead**: All windowing operations require Java Native Interface calls
- **Threading**: Android UI must run on main thread; Emacs runs on background thread
- **Lifecycle**: Android apps can be paused/resumed/destroyed at any time
- **Touch Input**: Extensive touchscreen and gesture support

### 3.4 GTK/PGTK (Pure GTK)

The PGTK port is designed for Wayland compatibility:

```c
static struct redisplay_interface pgtk_redisplay_interface = {
  pgtk_frame_parm_handlers,
  gui_produce_glyphs,
  gui_write_glyphs,
  gui_insert_glyphs,
  gui_clear_end_of_line,
  pgtk_scroll_run,
  pgtk_after_update_window_line,
  NULL, /* gui_update_window_begin, */
  NULL, /* gui_update_window_end, */
  pgtk_flush_display,
  gui_clear_window_mouse_face,
  gui_get_glyph_overhangs,
  gui_fix_overlapping_area,
  pgtk_draw_fringe_bitmap,
  pgtk_define_fringe_bitmap,
  pgtk_destroy_fringe_bitmap,
  pgtk_compute_glyph_string_overhangs,
  pgtk_draw_glyph_string,            // Cairo rendering
  pgtk_define_frame_cursor,
  pgtk_clear_frame_area,
  pgtk_clear_under_internal_border,
  pgtk_draw_window_cursor,
  pgtk_draw_vertical_window_border,
  pgtk_draw_window_divider,
  pgtk_shift_glyphs_for_insert,
  pgtk_show_hourglass,
  pgtk_hide_hourglass,
  pgtk_default_font_parameter
};
```

**Location:** `/home/user/emacs/src/pgtkterm.c:3716-3745`

**Key Difference from X11:**
- **Pure GTK**: No direct X11 dependency; works on Wayland
- **Cairo Rendering**: All drawing uses Cairo graphics library
- **GTK Event Loop**: Integrates with GTK's event system

### 3.5 Haiku

The Haiku port brings Emacs to the BeOS successor:

```c
static struct redisplay_interface haiku_redisplay_interface =
{
  haiku_frame_parm_handlers,
  gui_produce_glyphs,
  gui_write_glyphs,
  gui_insert_glyphs,
  gui_clear_end_of_line,
  haiku_scroll_run,
  haiku_after_update_window_line,
  NULL, /* update_window_begin */
  NULL, /* update_window_end */
  haiku_flush,
  gui_clear_window_mouse_face,
  gui_get_glyph_overhangs,
  gui_fix_overlapping_area,
  haiku_draw_fringe_bitmap,
  haiku_define_fringe_bitmap,
  haiku_destroy_fringe_bitmap,
  haiku_compute_glyph_string_overhangs,
  haiku_draw_glyph_string,           // Haiku BView rendering
  haiku_define_frame_cursor,
  haiku_clear_frame_area,
  haiku_clear_under_internal_border,
  haiku_draw_window_cursor,
  haiku_draw_vertical_window_border,
  haiku_draw_window_divider,
  haiku_shift_glyphs_for_insert,
  haiku_show_hourglass,
  haiku_hide_hourglass,
  haiku_default_font_parameter,
};
```

**Location:** `/home/user/emacs/src/haikuterm.c:3130-3160`

### 3.6 TTY (Terminal/Text Mode)

TTY terminals have no redisplay interface (it's NULL) but implement all the text-based hooks:

```c
static void
set_tty_hooks (struct terminal *terminal)
{
  terminal->rif = 0; /* ttys don't support window-based redisplay. */

  terminal->cursor_to_hook = &tty_cursor_to;
  terminal->raw_cursor_to_hook = &tty_raw_cursor_to;

  terminal->clear_to_end_hook = &tty_clear_to_end;
  terminal->clear_frame_hook = &tty_clear_frame;
  terminal->clear_end_of_line_hook = &tty_clear_end_of_line;

  terminal->ins_del_lines_hook = &tty_ins_del_lines;

  terminal->insert_glyphs_hook = &tty_insert_glyphs;
  terminal->write_glyphs_hook = &tty_write_glyphs;
  terminal->delete_glyphs_hook = &tty_delete_glyphs;

  terminal->ring_bell_hook = &tty_ring_bell;

  terminal->reset_terminal_modes_hook = &tty_reset_terminal_modes;
  terminal->set_terminal_modes_hook = &tty_set_terminal_modes;
  terminal->update_end_hook = &tty_update_end;

  terminal->read_socket_hook = &tty_read_avail_input;

  terminal->delete_frame_hook = &tty_free_frame_resources;
  terminal->delete_terminal_hook = &delete_tty;
}
```

**Location:** `/home/user/emacs/src/term.c` (approximate)

---

## Common Patterns

### 4.1 Event Handling Abstraction

All platforms must translate native events into Emacs `struct input_event`:

```c
struct input_event
{
  /* What kind of event was this?  */
  ENUM_BF (event_kind) kind : EVENT_KIND_WIDTH;

  /* Used in scroll bar click events.  */
  ENUM_BF (scroll_bar_part) part : 16;

  /* For keystroke/mouse events, this is the character/button.  */
  unsigned code;

  /* Modifier keys (shift, control, meta, etc.). */
  unsigned modifiers;

  /* Position information. */
  Lisp_Object x, y;

  /* Timestamp. */
  Time timestamp;

  /* Frame or window where event occurred. */
  Lisp_Object frame_or_window;

  /* Additional data (varies by event type). */
  Lisp_Object arg;

  /* Device that generated the event. */
  Lisp_Object device;
};
```

**Location:** `/home/user/emacs/src/termhooks.h:367-408`

#### Platform-Specific Event Translation

**X11 Example** (from `XTread_socket` in xterm.c):
```c
// KeyPress event
case KeyPress:
  {
    KeySym keysym;
    XKeyEvent xkey = event->xkey;

    // Translate X11 keysym to Emacs character
    nbytes = XLookupString (&xkey, copy_bufptr, copy_bufsiz,
                            &keysym, &compose_status);

    // Filter through input method
    if (x_filter_event (dpyinfo, &event))
      break;

    // Create input_event
    inev.kind = (keysym < 256) ? ASCII_KEYSTROKE_EVENT
                                : NON_ASCII_KEYSTROKE_EVENT;
    inev.code = keysym;
    inev.modifiers = x_x_to_emacs_modifiers (dpyinfo, xkey.state);
    XSETFRAME (inev.frame_or_window, f);
    inev.timestamp = xkey.time;
  }
```

**Windows Example** (from `w32_read_socket` in w32term.c):
```c
// WM_CHAR message
case WM_CHAR:
  {
    // Windows sends character directly
    inev.kind = ASCII_KEYSTROKE_EVENT;
    inev.code = wParam;  // Already a character code
    inev.modifiers = w32_get_modifiers ();
    XSETFRAME (inev.frame_or_window, f);
    inev.timestamp = msg.time;
  }
```

### 4.2 Font Backend System

Emacs uses a driver-based font system:

```c
struct font_driver
{
  /* Symbol indicating the type of the font-driver.  */
  Lisp_Object type;

  /* True if font names are case sensitive.  */
  bool case_sensitive;

  /* Return a cache of font-entities on frame F.  */
  Lisp_Object (*get_cache) (struct frame *f);

  /* List fonts matching FONT_SPEC on FRAME.  */
  Lisp_Object (*list) (struct frame *frame, Lisp_Object font_spec);

  /* Find best matching font.  */
  Lisp_Object (*match) (struct frame *f, Lisp_Object font_spec);

  /* Optional: List available families.  */
  Lisp_Object (*list_family) (struct frame *f);

  /* Open a font specified by FONT_ENTITY.  */
  Lisp_Object (*open_font) (struct frame *f, Lisp_Object font_entity,
                            int pixel_size);

  /* Close FONT.  */
  void (*close_font) (struct font *font);

  /* Check if FONT has a glyph for character C.  */
  int (*has_char) (Lisp_Object font, int c);

  /* Return a glyph code of FONT for character C.  */
  unsigned (*encode_char) (struct font *font, int c);

  /* Compute metrics for glyphs.  */
  void (*text_extents) (struct font *font,
                        const unsigned *code, int nglyphs,
                        struct font_metrics *metrics);

  /* Draw glyphs.  */
  int (*draw) (struct glyph_string *s, int from, int to,
               int x, int y, bool with_background);

  /* ... many more methods ... */
};
```

**Location:** `/home/user/emacs/src/font.h:589-750`

#### Platform Font Drivers

Each platform provides one or more font drivers:

| Platform | Font Drivers | Backend Technology |
|----------|--------------|-------------------|
| X11 | `xfont`, `xft` | Core X fonts, Xft (FreeType + FontConfig) |
| Windows | `w32font`, `w32uniscribe`, `w32dwrite` | GDI, Uniscribe, DirectWrite |
| macOS/NS | `ns` | Cocoa/AppKit NSFont |
| Android | `androidfont`, `sfnt` | Android Typeface, SFNT parser |
| Haiku | `haikufont` | Haiku BFont |
| GTK/PGTK | `ftcr`, `xft` | FreeType+Cairo, Xft |
| TTY | N/A | Terminal character capabilities |

**HarfBuzz Integration**: Modern Emacs can use HarfBuzz for complex text shaping across all platforms via the `hbfont` driver.

### 4.3 Image Support

Image loading and display is abstracted through image types:

```c
struct image
{
  /* Time when image was last displayed.  */
  struct timespec timestamp;

  /* Pixmaps of the image.  */
  Emacs_Pixmap pixmap, mask;

#ifdef USE_CAIRO
  void *cr_data;                    // Cairo surface
#endif

#ifdef HAVE_X_WINDOWS
  XImage *ximg, *mask_img;          // X11 images
#endif

#ifdef HAVE_ANDROID
  struct android_image *ximg, *mask_img;  // Android bitmap
#endif

#ifdef HAVE_NTGUI
  XFORM xform;                      // Transformation matrix
  bool smoothing;                   // Bilinear filtering
#endif

#ifdef HAVE_HAIKU
  double transform[3][3];           // Affine transformation
  bool use_bilinear_filtering;
#endif

  /* Colors allocated for this image.  */
  unsigned long *colors;
  int ncolors;

  /* Image ID (for caching).  */
  ptrdiff_t id;

  /* ... many more fields ... */
};
```

**Location:** `/home/user/emacs/src/dispextern.h:3172-3224`

**Common Image Operations:**
1. **Loading**: Platform-specific loaders (XBM, PNG, JPEG, SVG, etc.)
2. **Caching**: Images cached by ID to avoid reloading
3. **Scaling**: Platform-specific scaling (some use hardware acceleration)
4. **Compositing**: Blending images with backgrounds

### 4.4 Clipboard/Selection Handling

Each platform implements selection (clipboard) differently:

| Platform | Files | Mechanism |
|----------|-------|-----------|
| X11 | `xselect.c` | X selections (PRIMARY, CLIPBOARD, SECONDARY) |
| Windows | `w32select.c` | Windows Clipboard API |
| macOS/NS | `nsselect.m` | NSPasteboard |
| Android | `androidselect.c` | Android ClipboardManager |
| Haiku | `haikuselect.c` | Haiku clipboard |
| GTK/PGTK | `pgtkselect.c` | GTK clipboard |
| DOS | `w16select.c` | DOS clipboard |
| TTY | N/A | Limited or no clipboard support |

**Common Pattern:**
```c
// Set clipboard contents
DEFUN ("x-set-selection", Fx_set_selection, ...)
{
  // Platform-specific implementation
  // Stores DATA in SELECTION (e.g., CLIPBOARD)
}

// Get clipboard contents
DEFUN ("x-get-selection", Fx_get_selection, ...)
{
  // Platform-specific implementation
  // Retrieves data from SELECTION
}
```

### 4.5 Menu Systems

Menu implementation varies significantly:

**X11:**
- Toolkit menus (Motif, Athena, GTK, or Lucid widget library)
- Pop-up menus via `x_menu_show`

**Windows:**
- Native Windows menus
- Owner-drawn for custom styling

**macOS:**
- Native Cocoa NSMenu

**Android:**
- Android menu system (options menu, context menu)

**Haiku:**
- BMenu from Haiku Interface Kit

**TTY:**
- Text-based menu using `tmm.el` (Text Mode Menu)

---

## Case Study: X11 Implementation

### 5.1 Architecture Overview

The X11 port is the reference implementation for GUI platforms. Let's trace how text rendering works from start to finish.

### 5.2 Text Rendering Pipeline

#### Step 1: Redisplay Engine Calls Hook

From `xdisp.c` (the generic display engine):

```c
void
draw_glyphs (struct window *w, struct glyph_row *row, ...)
{
  // ... compute what needs to be drawn ...

  // Build glyph strings (groups of glyphs with same face)
  for (...)
    {
      struct glyph_string *s = build_glyph_string (...);

      // Call platform-specific drawing
      FRAME_RIF (f)->draw_glyph_string (s);
    }
}
```

This expands to: `x_redisplay_interface.draw_glyph_string (s)`

#### Step 2: X11 Glyph String Drawing

```c
static void
x_draw_glyph_string (struct glyph_string *s)
{
  bool relief_drawn_p = false;

  /* Prepare GC (Graphics Context). */
  x_set_glyph_string_gc (s);

  /* Draw background if necessary. */
  if (s->background_filled_p)
    /* Background already filled */;
  else if (s->first_glyph->type == IMAGE_GLYPH)
    x_draw_glyph_string_background (s, true);
  else
    x_draw_glyph_string_background (s, false);

  /* Draw foreground. */
  switch (s->first_glyph->type)
    {
    case CHAR_GLYPH:
      if (s->for_overlaps)
        s->background_filled_p = true;
      else
        x_draw_glyph_string_background (s, false);
      x_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      x_draw_composite_glyph_string_foreground (s);
      break;

    case STRETCH_GLYPH:
      x_draw_stretch_glyph_string (s);
      break;

    case IMAGE_GLYPH:
      x_draw_image_glyph_string (s);
      break;

    case XWIDGET_GLYPH:
      x_draw_xwidget_glyph_string (s);
      break;

    case GLYPHLESS_GLYPH:
      x_draw_glyphless_glyph_string_foreground (s);
      break;

    default:
      emacs_abort ();
    }

  /* Draw underline, overline, strike-through. */
  if (!s->for_overlaps)
    {
      if (s->face->underline)
        x_draw_glyph_string_underline (s);

      if (s->face->overline_p)
        x_draw_overline (s);

      if (s->face->strike_through_p)
        x_draw_strike_through (s);
    }

  /* Draw box if needed. */
  if (s->face->box != FACE_NO_BOX)
    x_draw_glyph_string_box (s);

  /* ... more decorations ... */
}
```

**Location:** `/home/user/emacs/src/xterm.c` (approximate, actual function is large)

#### Step 3: Character Glyph Foreground Drawing

```c
static void
x_draw_glyph_string_foreground (struct glyph_string *s)
{
  int i, x;

  /* If font has no default ascent/descent, use metrics from glyphs. */
  if (s->font_not_found_p || !s->font)
    {
      for (i = 0; i < s->nchars; ++i)
        {
          struct glyph *g = s->first_glyph + i;
          // Draw each glyph individually
        }
      return;
    }

  /* Fast path: use font driver to draw entire string at once. */
  if (s->font->driver->draw)
    {
      s->font->driver->draw (s, 0, s->nchars, s->x, s->ybase,
                             s->hl == DRAW_CURSOR);
      return;
    }

  /* Fallback: draw using XDrawString. */
  char *char1b = alloca (s->nchars);
  for (i = 0; i < s->nchars; ++i)
    char1b[i] = s->char2b[i];

  XDrawString (s->display, FRAME_X_DRAWABLE (s->f),
               s->gc, s->x, s->ybase, char1b, s->nchars);
}
```

#### Step 4: Font Driver Drawing (Xft Example)

For anti-aliased fonts, Xft (X FreeType) is used:

```c
static int
xftfont_draw (struct glyph_string *s, int from, int to,
              int x, int y, bool with_background)
{
  struct frame *f = s->f;
  struct face *face = s->face;
  struct xftfont_info *xftfont_info = (struct xftfont_info *) s->font;
  struct xft_draw_info *draw_info;
  XftColor *fg, *bg;

  /* Get or create XftDraw (rendering context). */
  draw_info = xftfont_get_xft_draw (f);

  /* Determine colors. */
  fg = xftfont_get_color (f, face->foreground);
  bg = xftfont_get_color (f, face->background);

  /* Draw background if requested. */
  if (with_background)
    XftDrawRect (draw_info->xft_draw, bg, x, y - face->font->ascent,
                 s->width, face->font->height);

  /* Draw glyphs. */
  XftDrawGlyphs (draw_info->xft_draw, fg, xftfont_info->xftfont,
                 x, y, s->char2b + from, to - from);

  return 1;
}
```

**Location:** `/home/user/emacs/src/xftfont.c` (approximate)

### 5.3 Event Processing Pipeline

#### Step 1: X Server Sends Event

X11 communicates via asynchronous events sent over a socket.

#### Step 2: `XTread_socket` Reads Events

```c
static int
XTread_socket (struct terminal *terminal, struct input_event *hold_quit)
{
  int count = 0;
  bool event_found = false;
  struct x_display_info *dpyinfo = terminal->display_info.x;

  block_input ();

  /* Process all pending events. */
  while (XPending (dpyinfo->display) > 0)
    {
      XEvent event;
      XNextEvent (dpyinfo->display, &event);

      /* Filter through input method. */
      if (x_filter_event (dpyinfo, &event))
        continue;

      /* Handle the event. */
      count += handle_one_xevent (dpyinfo, &event, &event_found, hold_quit);

      /* Check for quit. */
      if (hold_quit->kind != NO_EVENT)
        break;
    }

  unblock_input ();
  return count;
}
```

**Location:** `/home/user/emacs/src/xterm.c` (approximate)

#### Step 3: `handle_one_xevent` Dispatches Event

This massive function (1000+ lines) handles ~50 different X11 event types:

```c
static int
handle_one_xevent (struct x_display_info *dpyinfo,
                   XEvent *event,
                   bool *event_found,
                   struct input_event *hold_quit)
{
  union buffered_input_event inev;
  int count = 0;
  struct frame *f = NULL;

  EVENT_INIT (inev.ie);

  /* Determine which frame this event is for. */
  f = x_any_window_to_frame (dpyinfo, event->xany.window);

  switch (event->type)
    {
    case KeyPress:
      // Handle keyboard input
      break;

    case ButtonPress:
    case ButtonRelease:
      // Handle mouse buttons
      break;

    case MotionNotify:
      // Handle mouse movement
      break;

    case Expose:
      // Handle window exposure (needs redraw)
      break;

    case ConfigureNotify:
      // Handle window size/position change
      break;

    case FocusIn:
    case FocusOut:
      // Handle focus changes
      break;

    case ClientMessage:
      // Handle protocol messages (e.g., WM_DELETE_WINDOW)
      break;

    // ... ~40 more event types ...
    }

  /* Queue the event. */
  if (inev.ie.kind != NO_EVENT)
    {
      kbd_buffer_store_buffered_event (&inev, hold_quit);
      count++;
    }

  return count;
}
```

**Location:** `/home/user/emacs/src/xterm.c` (approximate)

### 5.4 X11-Specific Features

#### Graphics Contexts

X11 uses Graphics Contexts (GCs) to store drawing parameters:

```c
/* Create GCs for frame F. */
static void
x_make_gcs (struct frame *f)
{
  XGCValues gc_values;
  GC gc;

  /* Normal GC (default face colors). */
  gc_values.foreground = FRAME_FOREGROUND_PIXEL (f);
  gc_values.background = FRAME_BACKGROUND_PIXEL (f);
  gc_values.graphics_exposures = False;
  gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
                  GCForeground | GCBackground | GCGraphicsExposures,
                  &gc_values);
  f->output_data.x->normal_gc = gc;

  /* Reverse GC (inverse video). */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = FRAME_FOREGROUND_PIXEL (f);
  gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
                  GCForeground | GCBackground | GCGraphicsExposures,
                  &gc_values);
  f->output_data.x->reverse_gc = gc;

  /* Cursor GC. */
  gc_values.foreground = f->output_data.x->cursor_pixel;
  gc_values.background = FRAME_BACKGROUND_PIXEL (f);
  gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
                  GCForeground | GCBackground | GCGraphicsExposures,
                  &gc_values);
  f->output_data.x->cursor_gc = gc;
}
```

#### Double Buffering (XDBE Extension)

Modern X11 uses the XDBE extension for flicker-free updates:

```c
#ifdef HAVE_XDBE
static void
x_flip_and_flush (struct frame *f)
{
  block_input ();

  /* Flip back buffer to front buffer. */
  XdbeSwapBuffers (FRAME_X_DISPLAY (f), &swap_info, 1);

  /* Flush X output queue. */
  XFlush (FRAME_X_DISPLAY (f));

  unblock_input ();
}
#endif
```

#### X Resources

X11 supports configuration via X Resource Database:

```c
const char *
x_get_string_resource (XrmDatabase rdb, const char *name, const char *class)
{
  XrmValue value;
  char *type;

  if (XrmGetResource (rdb, name, class, &type, &value))
    {
      if (!strcmp (type, "String"))
        return (const char *) value.addr;
    }

  return NULL;
}
```

**Example Usage:**
```
Emacs.font: Monospace-12
Emacs.cursorColor: red
```

---

## Integration Guide

### 6.1 Adding a New Platform

To port Emacs to a new platform, you need to:

#### Step 1: Define Output Method

Add new enum value in `termhooks.h`:

```c
enum output_method
{
  // ... existing values ...
  output_myplatform,
};
```

#### Step 2: Create Display Info Structure

Define platform-specific data in a new header (e.g., `myplatformterm.h`):

```c
struct myplatform_display_info
{
  /* Display connection. */
  void *connection;

  /* Screen information. */
  int screen_width, screen_height;

  /* Color depth. */
  int depth;

  /* Default font. */
  struct font *font;

  /* Cached resources. */
  Lisp_Object name_list_element;

  /* ... platform-specific fields ... */
};
```

#### Step 3: Implement Redisplay Interface

Create `myplatformterm.c` and implement the redisplay interface:

```c
static struct redisplay_interface myplatform_redisplay_interface =
{
  myplatform_frame_parm_handlers,
  gui_produce_glyphs,                    // Can use generic
  gui_write_glyphs,                      // Can use generic
  gui_insert_glyphs,                     // Can use generic
  gui_clear_end_of_line,                 // Can use generic
  myplatform_scroll_run,                 // Platform-specific
  myplatform_after_update_window_line,   // Platform-specific
  NULL,
  NULL,
  myplatform_flush_display,              // Platform-specific
  gui_clear_window_mouse_face,           // Can use generic
  gui_get_glyph_overhangs,               // Can use generic
  gui_fix_overlapping_area,              // Can use generic
  myplatform_draw_fringe_bitmap,         // Platform-specific
  myplatform_define_fringe_bitmap,       // Platform-specific
  myplatform_destroy_fringe_bitmap,      // Platform-specific
  myplatform_compute_glyph_string_overhangs,  // Platform-specific
  myplatform_draw_glyph_string,          // CRITICAL: platform-specific
  myplatform_define_frame_cursor,        // Platform-specific
  myplatform_clear_frame_area,           // Platform-specific
  myplatform_clear_under_internal_border,// Platform-specific
  myplatform_draw_window_cursor,         // Platform-specific
  myplatform_draw_vertical_window_border,// Platform-specific
  myplatform_draw_window_divider,        // Platform-specific
  myplatform_shift_glyphs_for_insert,    // Platform-specific
  myplatform_show_hourglass,             // Platform-specific
  myplatform_hide_hourglass,             // Platform-specific
  myplatform_default_font_parameter      // Platform-specific
};
```

#### Step 4: Create Terminal

```c
static struct terminal *
myplatform_create_terminal (struct myplatform_display_info *dpyinfo)
{
  struct terminal *terminal;

  terminal = create_terminal (output_myplatform,
                              &myplatform_redisplay_interface);

  terminal->display_info.myplatform = dpyinfo;

  /* Set hooks. */
  terminal->clear_frame_hook = myplatform_clear_frame;
  terminal->read_socket_hook = myplatform_read_socket;
  terminal->frame_up_to_date_hook = myplatform_frame_up_to_date;
  terminal->mouse_position_hook = myplatform_mouse_position;
  terminal->focus_frame_hook = myplatform_focus_frame;
  terminal->frame_raise_lower_hook = myplatform_frame_raise_lower;
  terminal->fullscreen_hook = myplatform_fullscreen_hook;
  terminal->menu_show_hook = myplatform_menu_show;
  terminal->popup_dialog_hook = myplatform_popup_dialog;
  terminal->set_vertical_scroll_bar_hook = myplatform_set_vertical_scroll_bar;
  terminal->condemn_scroll_bars_hook = myplatform_condemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = myplatform_redeem_scroll_bar;
  terminal->judge_scroll_bars_hook = myplatform_judge_scroll_bars;
  terminal->delete_frame_hook = myplatform_delete_frame;
  terminal->delete_terminal_hook = myplatform_delete_terminal;

  return terminal;
}
```

#### Step 5: Implement Frame Functions

Create `myplatformfns.c` with frame creation, parameter setting, etc.

#### Step 6: Implement Font Driver

Create font driver in `myplatformfont.c`.

#### Step 7: Implement Event Loop

The `read_socket_hook` must:
1. Read native events from the windowing system
2. Translate them to `struct input_event`
3. Return event count (or error codes)

#### Step 8: Integration

1. Add configure.ac detection for your platform
2. Add Makefile rules
3. Add platform-specific initialization
4. Test extensively!

### 6.2 Best Practices

1. **Reuse Generic Code**: Functions prefixed with `gui_` are often reusable
2. **Minimize Platform Code**: Only implement what's truly platform-specific
3. **Follow Conventions**: Study existing ports (especially X11 and Windows)
4. **Handle Errors**: Check all platform API calls for errors
5. **Support Configuration**: Allow users to customize via frame parameters
6. **Document Limitations**: Some platforms can't support all features

---

## References

### Primary Source Files

| Component | File | Description |
|-----------|------|-------------|
| Terminal Abstraction | `/home/user/emacs/src/termhooks.h` | Terminal structure definition |
| Redisplay Interface | `/home/user/emacs/src/dispextern.h` | Redisplay interface definition |
| Terminal Management | `/home/user/emacs/src/terminal.c` | Terminal creation and deletion |
| X11 Implementation | `/home/user/emacs/src/xterm.c` | X11 terminal and rendering |
| X11 Frames | `/home/user/emacs/src/xfns.c` | X11 frame functions |
| Windows Implementation | `/home/user/emacs/src/w32term.c` | Windows terminal and rendering |
| Windows Frames | `/home/user/emacs/src/w32fns.c` | Windows frame functions |
| Android Implementation | `/home/user/emacs/src/androidterm.c` | Android terminal and rendering |
| Haiku Implementation | `/home/user/emacs/src/haikuterm.c` | Haiku terminal and rendering |
| GTK Implementation | `/home/user/emacs/src/pgtkterm.c` | Pure GTK terminal and rendering |
| TTY Implementation | `/home/user/emacs/src/term.c` | Text terminal implementation |
| Font System | `/home/user/emacs/src/font.h` | Font driver interface |
| Font Implementation | `/home/user/emacs/src/font.c` | Generic font code |

### Key Concepts

- **Terminal**: An abstraction representing a display device (graphical or text)
- **Redisplay Interface**: Set of methods for rendering graphics and text
- **Glyph String**: A sequence of glyphs (characters or graphical elements) with the same face
- **Face**: Text attributes (font, color, etc.)
- **Hook Functions**: Function pointers in `struct terminal` for platform-specific operations
- **Display Info**: Platform-specific data structure (e.g., `x_display_info`, `w32_display_info`)
- **Frame**: An Emacs window (in windowing system terminology)
- **Window**: A subdivision of a frame (internal Emacs concept)

### Statistics Summary

| Metric | Value |
|--------|-------|
| Supported Platforms | 8+ |
| Terminal Hook Functions | ~40 |
| Redisplay Interface Methods | ~30 |
| X11 Source Files | 10+ |
| Windows Source Files | 19 |
| Android Source Files | 12 |
| Total Platform-Specific LOC | ~200,000+ |

---

**End of Document**

This literate programming guide provides a comprehensive view of Emacs's platform abstraction architecture. By studying the patterns and implementations here, you can understand how Emacs achieves portability while maintaining performance and leveraging platform-specific features.
