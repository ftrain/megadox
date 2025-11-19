# X11 Window System Integration

## Overview

This document provides comprehensive coverage of Emacs's window system integration, with a primary focus on X11 implementation as the reference platform. X11 has been the main development and testing platform for Emacs's graphical features since X10 support was first added, making it the most mature and feature-complete implementation.

### Architecture Overview

The X11 integration is implemented across several key source files:

| File | Lines | Purpose |
|------|-------|---------|
| `src/xterm.c` | ~33,000 | Event loop, rendering, and main terminal interface |
| `src/xfns.c` | ~10,600 | Frame creation and management functions |
| `src/xmenu.c` | ~2,900 | Menu bar and popup menu handling |
| `src/xselect.c` | ~3,500 | Selection (clipboard) handling |
| `src/xsettings.c` | ~1,800 | XSETTINGS protocol for desktop integration |
| `src/xrdb.c` | ~650 | X Resource Database management |
| `src/xfont.c` | ~1,000 | Core X font backend |
| `src/xftfont.c` | ~800 | Xft/FreeType font backend |

## 1. X11 Integration Architecture

### 1.1 Display Connection and Initialization

The X11 integration centers around the `x_display_info` structure, which maintains all state for a connection to an X server:

```c
/* From src/xterm.h */
struct x_display_info
{
  /* Chain of all x_display_info structures */
  struct x_display_info *next;

  /* Generic display parameters */
  struct terminal *terminal;

  /* Xlib display connection */
  Display *display;

  /* File descriptor for the connection */
  int connection;

  /* Security status */
  bool untrusted;

  /* Screen and visual information */
  Screen *screen;
  Visual *visual;
  XVisualInfo visual_info;
  Colormap cmap;
  int n_planes;
  double resx, resy;  /* DPI */

#ifdef HAVE_XRENDER
  XRenderPictFormat *pict_format;
#endif

  /* Resource database */
  XrmDatabase rdb;

  /* Window manager communication atoms */
  Atom Xatom_wm_protocols;
  Atom Xatom_wm_take_focus;
  Atom Xatom_wm_save_yourself;
  Atom Xatom_wm_delete_window;
  Atom Xatom_wm_change_state;

  /* Selection atoms */
  Atom Xatom_CLIPBOARD;
  Atom Xatom_TIMESTAMP;
  Atom Xatom_TEXT;
  Atom Xatom_UTF8_STRING;
  Atom Xatom_TARGETS;
  /* ... many more atoms ... */

  /* Focus tracking */
  struct frame *x_focus_frame;
  struct frame *x_focus_event_frame;
  struct frame *highlight_frame;

  /* Mouse tracking */
  struct frame *last_mouse_frame;
  struct frame *last_mouse_glyph_frame;
  struct scroll_bar *last_mouse_scroll_bar;
  Time last_user_time;

  /* Graphics contexts */
  GC scratch_cursor_gc;
  Mouse_HLInfo mouse_highlight;

  /* Modifier key mappings */
  unsigned int meta_mod_mask;
  unsigned int shift_lock_mask;
  unsigned int alt_mod_mask;
  unsigned int super_mod_mask;
  unsigned int hyper_mod_mask;
};
```

**Key Initialization Steps** (in `x_term_init` from `xterm.c`):

1. **Open Display Connection**: Call `XOpenDisplay()` to connect to X server
2. **Visual Selection**: Choose appropriate visual (TrueColor preferred)
3. **Colormap Creation**: Create colormap based on visual
4. **Atom Initialization**: Intern all required atoms for WM communication
5. **Extension Detection**: Query for XRender, Xfixes, XInput2, Xrandr, etc.
6. **Resource Loading**: Load X resources from various sources
7. **Input Method Setup**: Initialize XIM for internationalized input
8. **Event Mask Setup**: Configure which events to receive

### 1.2 X Resources and XSETTINGS

#### X Resource Database (xrdb.c)

The X Resource Database provides a hierarchical configuration system. Emacs loads resources from multiple sources in priority order:

```c
/* Resource loading order (highest to lowest priority):
 * 1. Command line options (-xrm)
 * 2. RESOURCE_MANAGER property on root window
 * 3. .Xdefaults in home directory
 * 4. XENVIRONMENT file or .Xdefaults-hostname
 * 5. Application defaults
 */
```

**Resource Specification Format**:
```
Emacs.font: Monospace-12
Emacs*background: white
Emacs*foreground: black
emacs.geometry: 80x40+100+100
Emacs.menuBar: on
Emacs.toolBar: off
```

**Implementation** (`src/xrdb.c`):
- `x_get_string_resource()`: Retrieve string resource value
- `x_get_resource()`: General resource retrieval with class/name lookup
- `x_load_resources()`: Load resources from all sources into database
- Support for `%C`, `%N`, `%T`, `%L` escape sequences in search paths

#### XSETTINGS Protocol (xsettings.c)

XSETTINGS provides runtime desktop environment integration, allowing Emacs to respond to theme changes, DPI changes, and other desktop-wide settings.

```c
/* XSETTINGS mechanism:
 * 1. XSETTINGS manager sets _XSETTINGS_SETTINGS property on root
 * 2. Emacs monitors this property with PropertyNotify events
 * 3. When changed, parse settings and apply updates
 */
```

**Monitored Settings**:
- `Xft/DPI`: Screen DPI for font rendering
- `Xft/Antialias`: Font antialiasing preference
- `Xft/Hinting`: Font hinting preference
- `Xft/RGBA`: Subpixel rendering order
- `Net/ThemeName`: GTK theme name
- `Gtk/FontName`: Default GTK font
- `Gtk/ToolbarStyle`: Toolbar display style

**Application Flow**:
1. On startup, read `_XSETTINGS_SETTINGS` property
2. Install `PropertyNotify` handler on root window
3. When property changes, re-read and parse settings
4. Generate `CONFIG_CHANGED_EVENT` to update Emacs state
5. Update fonts, themes, and UI elements as needed

### 1.3 Toolkit Integration

Emacs supports three major X11 toolkit configurations:

#### No Toolkit Configuration

**Characteristics**:
- Simplest window structure: one X window per frame
- Native Emacs scrollbars
- XMenu library for popup menus (from X11R2)
- Direct control over all X operations
- Minimal dependencies

**Window Structure**:
```
FRAME_X_WINDOW (f) == top-level window
  └── Direct drawing and event handling
```

#### X Toolkit Intrinsics (Xt) Configuration

Two variants: **Lucid** and **Motif/LessTif**

**Lucid Widgets**:
- Custom Lucid Widget Library (`lwlib/`) for menus
- Xaw (or Xaw3d/neXtaw) for dialogs and optional scrollbars
- EmacsFrame widget (custom, in `widget.c`)

**Motif/LessTif**:
- Motif widgets for menus, dialogs, file panels
- More native look but larger dependency

**Window Hierarchy**:
```
Outer Widget (ApplicationShell)
  └── Menu Bar Widget (optional)
  └── Edit Widget (EmacsFrame)
      └── Drawing area for buffer display
```

**Key Macros**:
- `FRAME_OUTER_WINDOW(f)`: Top-level shell window
- `FRAME_X_WINDOW(f)`: Edit widget window (where drawing happens)
- `FRAME_MENUBAR_WINDOW(f)`: Menu bar widget window

**Special Considerations**:
- Properties for WM must be set on outer widget
- Drawing operations target edit widget
- Menu bar events require special redirection
- Complex event dispatch through Xt event loop

#### GTK Configuration

**GTK+ 2 and GTK 3 Support**:
- Full GTK widget set for all UI elements
- Menu bars, toolbars, dialogs, file choosers
- GtkFixed container for edit area
- May use client-side decorations (GTK3)

**Window Structure**:
```
GtkWindow (may not be real X window in GTK3)
  └── GtkFixed widget
      └── FRAME_X_WINDOW: outer window for drawing
```

**Special Features**:
- CSS styling support (GTK3)
- Native file choosers and dialogs
- Better desktop integration
- Complications with client-side windows

**Event Handling**:
- Events come through GTK callback system
- `handle_one_xevent()` called from GTK event handlers
- `*finish` parameter for safe GTK event processing

### 1.4 Font Backends

Emacs X11 supports multiple font backends with automatic fallback:

#### Core X Fonts (xfont.c)

**Legacy bitmap and scalable fonts using core X11 protocol**

```c
struct xfont_info {
  struct font font;
  Display *display;
  XFontStruct *xfont;
  unsigned x_display_id;
};
```

**Characteristics**:
- XLFD (X Logical Font Description) naming
- Server-side font storage
- XFontStruct provides metrics
- Limited Unicode support
- Mostly deprecated but still available

**Font Selection**:
```c
/* XLFD pattern example:
 * -misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1
 */
```

#### Xft/FreeType Backend (xftfont.c)

**Modern client-side font rendering with FreeType**

```c
struct xftface_info {
  bool bg_allocated_p;
  bool fg_allocated_p;
  XftColor xft_fg;
  XftColor xft_bg;
};
```

**Features**:
- Client-side rendering using FreeType
- Full Unicode support via fontconfig
- Antialiasing and hinting
- Subpixel rendering (ClearType-style)
- Automatic font substitution and fallback
- Complex text shaping (via HarfBuzz integration)

**Rendering Pipeline**:
1. Query fontconfig for font matching pattern
2. Open font via FreeType
3. Allocate XftColors for foreground/background
4. Create XftDraw context for target window
5. Use `XftDrawStringUtf8()` or shaped glyphs
6. Optionally use XRender for compositing

**XRENDER Integration**:
```c
#ifdef HAVE_XRENDER
  /* Use XRender for alpha blending and antialiasing */
  XRenderPictFormat *pict_format = dpyinfo->pict_format;
  /* Supports proper alpha channel handling */
#endif
```

#### Font Driver Selection

Priority order (first available is used):
1. **Xft** (if `HAVE_XFT` defined) - preferred for modern systems
2. **X Core** (always available) - fallback for old systems

Applications can force font backend via:
```elisp
(set-frame-font "xft:Monospace-10")  ; Force Xft
(set-frame-font "fixed")              ; Use core X font
```

## 2. Graphics and Rendering

### 2.1 Graphics Contexts

**Graphics Contexts (GCs)** are X server-side objects containing drawing attributes. Unlike other window systems, GCs are fundamental to X11.

#### GC Types in Emacs

```c
/* From struct x_output in xterm.h */
struct x_output {
  /* Standard GCs for common operations */
  GC normal_gc;        /* Default face colors */
  GC reverse_gc;       /* Inverted colors */
  GC cursor_gc;        /* Cursor in default face */

  /* Special purpose GCs */
  GC white_relief_gc;  /* For 3D relief effects */
  GC black_relief_gc;
  GC relief_background;

  /* Other drawing state... */
};
```

#### GC Creation and Management

**Initial GC Setup** (in `x_make_gc` from `xfns.c`):

```c
void x_make_gc(struct frame *f)
{
  XGCValues gc_values;

  /* Normal GC - foreground and background from default face */
  gc_values.foreground = FRAME_FOREGROUND_PIXEL(f);
  gc_values.background = FRAME_BACKGROUND_PIXEL(f);
  gc_values.font = FRAME_FONT(f)->fid;  /* If using core X fonts */
  f->output_data.x->normal_gc =
    XCreateGC(FRAME_X_DISPLAY(f), FRAME_X_WINDOW(f),
              GCForeground | GCBackground | GCFont, &gc_values);

  /* Reverse GC - swapped colors */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL(f);
  gc_values.background = FRAME_FOREGROUND_PIXEL(f);
  f->output_data.x->reverse_gc =
    XCreateGC(FRAME_X_DISPLAY(f), FRAME_X_WINDOW(f),
              GCForeground | GCBackground | GCFont, &gc_values);

  /* Cursor GC... */
  /* Relief GCs... */
}
```

#### Per-Face GC Computation

**Face GC Preparation** (in `prepare_face_for_display` from `xfaces.c`):

```c
/* Each face gets a GC computed when first displayed */
void prepare_face_for_display(struct frame *f, struct face *face)
{
  if (face->gc == 0) {
    XGCValues xgcv;
    unsigned long mask = GCForeground | GCBackground;

    xgcv.foreground = face->foreground;
    xgcv.background = face->background;

    /* Add font if using core X fonts */
    if (face->font) {
      xgcv.font = face->font->fid;
      mask |= GCFont;
    }

    /* Add graphics exposures control */
    xgcv.graphics_exposures = False;
    mask |= GCGraphicsExposures;

    face->gc = XCreateGC(FRAME_X_DISPLAY(f), FRAME_X_WINDOW(f),
                         mask, &xgcv);
  }
}
```

#### Dynamic GC Modification

For special rendering (cursor, mouse highlight), GCs are modified temporarily:

```c
/* In x_set_glyph_string_gc from xterm.c */
void x_set_glyph_string_gc(struct glyph_string *s)
{
  if (s->hl == DRAW_CURSOR) {
    /* Drawing cursor - may need custom GC */
    if (/* cursor is in non-default face */) {
      /* Create temporary GC with adjusted colors */
      XGCValues xgcv;
      xgcv.foreground = cursor_fg;
      xgcv.background = cursor_bg;
      s->gc = XCreateGC(s->display, s->window,
                        GCForeground | GCBackground, &xgcv);
    } else {
      /* Use standard cursor GC */
      s->gc = s->f->output_data.x->cursor_gc;
    }
  } else {
    /* Use face's GC */
    s->gc = s->face->gc;
  }
}
```

### 2.2 Color Allocation

X11 color handling is unique due to visual classes and colormaps.

#### Visual Classes

```c
/* Visual class determines color allocation strategy */
enum {
  TrueColor,    /* Direct RGB mapping, most common on modern systems */
  DirectColor,  /* Like TrueColor but with programmable color map */
  PseudoColor,  /* 8-bit indexed color with dynamic allocation */
  StaticColor,  /* 8-bit indexed color, predefined palette */
  GrayScale,    /* Dynamic grayscale */
  StaticGray    /* Static grayscale */
};
```

#### TrueColor Visual (Modern Systems)

**Direct RGB pixel computation**, no allocation needed:

```c
/* From x_make_truecolor_pixel in xterm.c */
unsigned long x_make_truecolor_pixel(Display_Info *dpyinfo,
                                     int r, int g, int b)
{
  unsigned long pixel;
  unsigned long red_mult, green_mult, blue_mult;
  int red_shift, green_shift, blue_shift;

  /* Extract shift and multiplier from visual masks */
  /* For typical 24-bit TrueColor: R=0xFF0000, G=0x00FF00, B=0x0000FF */

  pixel = (((r * red_mult) >> 8) << red_shift)
        | (((g * green_mult) >> 8) << green_shift)
        | (((b * blue_mult) >> 8) << blue_shift);

  return pixel;
}
```

#### Non-TrueColor Visuals (Legacy Systems)

**Requires explicit color allocation**:

```c
/* From x_alloc_nearest_color_1 in xterm.c */
bool x_alloc_nearest_color_1(Display *dpy, Colormap cmap, XColor *color)
{
  /* Try to allocate exact color */
  if (XAllocColor(dpy, cmap, color))
    return true;

  /* Allocation failed (colormap full), find closest existing color */
  XColor cells[256];
  int ncolors = DisplayCells(dpy, XScreenNumberOfScreen(screen));

  /* Read all allocated colors */
  for (int i = 0; i < ncolors; i++)
    cells[i].pixel = i;
  XQueryColors(dpy, cmap, cells, ncolors);

  /* Find closest match using Euclidean distance in RGB space */
  int best = 0;
  unsigned long min_distance = ULONG_MAX;

  for (int i = 0; i < ncolors; i++) {
    unsigned long distance =
      (long)(color->red - cells[i].red) * (color->red - cells[i].red) +
      (long)(color->green - cells[i].green) * (color->green - cells[i].green) +
      (long)(color->blue - cells[i].blue) * (color->blue - cells[i].blue);

    if (distance < min_distance) {
      min_distance = distance;
      best = i;
    }
  }

  *color = cells[best];
  return true;
}
```

#### Color Management Strategy

**Allocation Points**:
- Face realization (when face is first displayed)
- Frame foreground/background changes
- Color property changes

**Deallocation Points**:
- Face unrealization
- Frame destruction
- Color property changes (old color freed)

**Functions**:
- `load_color()`: Allocate pixel for RGB color
- `unload_color()`: Free allocated color cell
- `x_query_colors()`: Get RGB values from pixel values

### 2.3 Double Buffering

**XDBE (X Double Buffer Extension)** eliminates flicker during redisplay.

#### Implementation

```c
/* From src/xterm.h */
#ifdef HAVE_XDBE
#define FRAME_X_DOUBLE_BUFFERED_P(f) \
  (FRAME_X_WINDOW(f) != FRAME_X_RAW_DRAWABLE(f))

/* FRAME_X_WINDOW(f)        - Front buffer (visible window)
 * FRAME_X_RAW_DRAWABLE(f)  - Back buffer (XdbeBackBuffer)
 * Drawing always happens to back buffer
 */
#endif
```

#### Buffer Setup (in `x_window` from `xfns.c`)

```c
#ifdef HAVE_XDBE
  if (use_xdbe) {
    XdbeBackBuffer back_buffer;
    back_buffer = XdbeAllocateBackBufferName(
      FRAME_X_DISPLAY(f),
      FRAME_X_WINDOW(f),
      XdbeBackground  /* Swap action: undefined -> background */
    );

    if (back_buffer != None) {
      f->output_data.x->xdbe_back_buffer = back_buffer;
      /* All drawing operations now target back_buffer */
    }
  }
#endif
```

#### Rendering Cycle

```c
/* From xterm.c */

/* 1. Begin update - prepare back buffer */
static void x_update_window_begin(struct window *w)
{
  struct frame *f = XFRAME(WINDOW_FRAME(w));

  if (FRAME_X_DOUBLE_BUFFERED_P(f)) {
    /* Back buffer already allocated, ready for drawing */
  }
}

/* 2. Draw operations - all target back buffer */
static void x_draw_glyph_string(struct glyph_string *s)
{
  /* All Xlib drawing calls (XFillRectangle, XDrawString, etc.)
   * automatically use FRAME_X_DRAWABLE(f), which is the back buffer
   * when double buffering is enabled
   */
}

/* 3. End update - swap buffers */
static void x_update_window_end(struct window *w, bool cursor_on_p,
                                bool mouse_face_overwritten_p)
{
  struct frame *f = XFRAME(WINDOW_FRAME(w));

  if (FRAME_X_DOUBLE_BUFFERED_P(f)) {
    /* Mark that buffer needs to be swapped */
    FRAME_X_NEED_BUFFER_FLIP(f) = true;
  }
}

/* 4. Show frame - perform actual swap */
static void x_flush(struct frame *f)
{
  if (FRAME_X_DOUBLE_BUFFERED_P(f) && FRAME_X_NEED_BUFFER_FLIP(f)) {
    XdbeSwapInfo swap_info;
    swap_info.swap_window = FRAME_X_WINDOW(f);
    swap_info.swap_action = XdbeBackground;

    XdbeSwapBuffers(FRAME_X_DISPLAY(f), &swap_info, 1);
    FRAME_X_NEED_BUFFER_FLIP(f) = false;
  }

  XFlush(FRAME_X_DISPLAY(f));
}
```

**Benefits**:
- Eliminates tearing and flicker
- Clean atomic updates
- Allows partial redraws while maintaining consistency
- Slight memory overhead (second buffer)

### 2.4 Glyph String Rendering

The core rendering function is `x_draw_glyph_string` (in `xterm.c`), called by the redisplay engine.

#### Glyph String Structure

```c
struct glyph_string {
  /* Display connection and target */
  Display *display;
  Window window;

  /* Rendering area */
  int x, y, width, height;
  int ybase;  /* Baseline for text */

  /* Visual properties */
  struct face *face;
  struct font *font;
  GC gc;

  /* Content */
  struct glyph *first_glyph;
  int nchars;
  unsigned *char2b;  /* Unicode characters */

  /* Rendering hints */
  enum draw_glyphs_face hl;  /* DRAW_NORMAL, DRAW_CURSOR, etc. */
  bool background_filled_p;

  /* Clipping */
  XRectangle clip;
  int clip_head, clip_tail;

  /* Links to adjacent strings */
  struct glyph_string *next, *prev;

  /* Type-specific data */
  /* ... for images, stretch glyphs, etc. ... */
};
```

#### Rendering Pipeline

```c
static void x_draw_glyph_string(struct glyph_string *s)
{
  bool relief_drawn_p = false;

  /* 1. Setup GC for this string */
  x_set_glyph_string_gc(s);

  /* 2. Set clipping region */
  if (s->clip_head || s->clip_tail) {
    XRectangle clip_rect;
    /* Compute clip rectangle */
    XSetClipRectangles(s->display, s->gc, 0, 0, &clip_rect, 1, Unsorted);
  }

  /* 3. Fill background if needed */
  if (s->background_filled_p) {
    /* Background already filled, skip */
  } else if (/* background needs filling */) {
    if (s->stippled_p) {
      /* Fill with stipple pattern */
      XSetFillStyle(s->display, s->gc, FillOpaqueStippled);
      XFillRectangle(s->display, FRAME_X_DRAWABLE(s->f),
                     s->gc, s->x, s->y, s->width, s->height);
    } else {
      /* Solid color background */
      XSetForeground(s->display, s->gc, s->face->background);
      XFillRectangle(s->display, FRAME_X_DRAWABLE(s->f),
                     s->gc, s->x, s->y, s->width, s->height);
    }
  }

  /* 4. Draw the actual content based on type */
  switch (s->first_glyph->type) {
    case CHAR_GLYPH:
      /* Text rendering */
      if (s->font == s->face->font) {
        /* Simple case: can use font directly */
        if (s->font->driver == &xfont_driver) {
          /* Core X font */
          XDrawString16(s->display, FRAME_X_DRAWABLE(s->f), s->gc,
                       s->x, s->ybase, (XChar2b *)s->char2b, s->nchars);
        } else if (s->font->driver == &xftfont_driver) {
          /* Xft font - client-side rendering */
          XftDraw *xft_draw = /* get or create XftDraw */;
          XftColor xft_color;
          XftDrawStringUtf8(xft_draw, &xft_color, s->font->xft_font,
                           s->x, s->ybase, utf8_text, utf8_len);
        }
      } else {
        /* Fallback font needed - more complex */
        /* Draw character by character with appropriate fonts */
      }
      break;

    case IMAGE_GLYPH:
      /* Image rendering */
      x_draw_image_glyph_string(s);
      break;

    case STRETCH_GLYPH:
      /* Stretch space - just background (already filled) */
      break;

    case COMPOSITE_GLYPH:
      /* Composite character (e.g., emoji, ligatures) */
      x_draw_composite_glyph_string_foreground(s);
      break;

    case GLYPHLESS_GLYPH:
      /* Display representation for glyphless characters */
      x_draw_glyphless_glyph_string_foreground(s);
      break;
  }

  /* 5. Draw text decorations */
  if (s->face->underline) {
    x_draw_glyph_string_underline(s);
  }

  if (s->face->overline) {
    x_draw_glyph_string_overline(s);
  }

  if (s->face->strike_through) {
    x_draw_glyph_string_strike_through(s);
  }

  /* 6. Draw box (border around text) */
  if (s->face->box != FACE_NO_BOX) {
    x_draw_glyph_string_box(s);
    relief_drawn_p = true;
  }

  /* 7. Reset clipping */
  XSetClipMask(s->display, s->gc, None);
}
```

### 2.5 Image Rendering

Images are rendered through the unified image API with X11-specific backend.

#### Image Types Supported

- XBM (X Bitmap) - native X format
- XPM (X Pixmap) - native X color format
- PNG, JPEG, GIF, TIFF - via external libraries
- SVG - via librsvg
- ImageMagick - via ImageMagick library

#### X11 Image Rendering

```c
static void x_draw_image_glyph_string(struct glyph_string *s)
{
  struct image *img = IMAGE_FROM_ID(s->f, s->img->id);

  if (img->pixmap) {
    /* Have X pixmap for image */
    if (img->mask) {
      /* Image has transparency - use clip mask */
      XSetClipMask(s->display, s->gc, img->mask);
      XSetClipOrigin(s->display, s->gc, s->x, s->y);
    }

    /* Copy pixmap to window */
    XCopyArea(s->display, img->pixmap, FRAME_X_DRAWABLE(s->f), s->gc,
              0, 0, img->width, img->height, s->x, s->y);

    if (img->mask) {
      XSetClipMask(s->display, s->gc, None);
    }
  }
#ifdef HAVE_XRENDER
  else if (img->picture) {
    /* Use XRender for alpha compositing */
    XRenderComposite(s->display, PictOpOver,
                     img->picture,        /* source */
                     img->mask_picture,   /* mask (alpha channel) */
                     FRAME_X_PICTURE(s->f), /* destination */
                     0, 0,                /* src x, y */
                     0, 0,                /* mask x, y */
                     s->x, s->y,          /* dst x, y */
                     img->width, img->height);
  }
#endif
}
```

## 3. Event Processing

### 3.1 Event Loop Architecture

The X11 event loop integrates with Emacs's main loop using file descriptor monitoring.

#### Top-Level Flow

```c
/* From keyboard.c - main Emacs loop */
while (true) {
  /* 1. Use pselect() to wait for input */
  int nfds = pselect(max_fd + 1, &readfds, NULL, NULL, timeout, &mask);

  /* 2. Check if X connection has data */
  if (FD_ISSET(x_connection_fd, &readfds)) {
    /* 3. Read X events */
    XTread_socket(terminal, &hold_quit);
  }

  /* 4. Process Emacs events from keyboard buffer */
  /* ... */
}
```

#### XTread_socket - Main Event Reader

```c
/* From xterm.c - reads and processes X events */
int XTread_socket(struct terminal *terminal, struct input_event *hold_quit)
{
  int count = 0;
  bool event_found = false;
  struct x_display_info *dpyinfo = terminal->display_info.x;

  block_input();

  /* Process all pending events */
  while (XPending(dpyinfo->display)) {
    XEvent xev;
    XNextEvent(dpyinfo->display, &xev);

    /* Filter through input method first */
    if (x_filter_event(dpyinfo, &xev))
      continue;

    /* Handle the event */
    count += handle_one_xevent(dpyinfo, &xev, &finish, hold_quit);

    if (finish == X_EVENT_GOTO_OUT)
      break;
  }

  unblock_input();
  return count;
}
```

### 3.2 Event Translation - handle_one_xevent

This massive function (thousands of lines) translates X events into Emacs events.

#### Event Type Handling

```c
static int handle_one_xevent(struct x_display_info *dpyinfo,
                            XEvent *event,
                            int *finish,
                            struct input_event *hold_quit)
{
  int count = 0;
  struct frame *f = NULL;

  /* Identify which frame this event belongs to */
  f = x_any_window_to_frame(dpyinfo, event->xany.window);

  switch (event->type) {

    /* ===== Keyboard Events ===== */
    case KeyPress: {
      KeySym keysym;
      char copy_buffer[81];
      int modifiers;

      /* Translate X key event to keysym */
      int nbytes = XLookupString(&event->xkey, copy_buffer,
                                 sizeof(copy_buffer), &keysym, NULL);

      /* Or use XIM for composed input */
      #ifdef HAVE_X_I18N
      if (FRAME_XIC(f)) {
        Status status;
        nbytes = XmbLookupString(FRAME_XIC(f), &event->xkey,
                                copy_buffer, sizeof(copy_buffer),
                                &keysym, &status);
      }
      #endif

      /* Convert X modifiers to Emacs modifiers */
      modifiers = x_x_to_emacs_modifiers(dpyinfo, event->xkey.state);

      /* Create Emacs keyboard event */
      inev.kind = (keysym < 256) ? ASCII_KEYSTROKE_EVENT
                                 : NON_ASCII_KEYSTROKE_EVENT;
      inev.code = keysym;
      inev.modifiers = modifiers;
      XSETFRAME(inev.frame_or_window, f);
      inev.timestamp = event->xkey.time;

      kbd_buffer_store_event(&inev);
      count++;
      break;
    }

    /* ===== Mouse Events ===== */
    case ButtonPress:
    case ButtonRelease: {
      /* Translate mouse button and modifiers */
      int button = event->xbutton.button;
      int modifiers = x_x_to_emacs_modifiers(dpyinfo, event->xbutton.state);

      /* Determine event type */
      if (event->type == ButtonPress) {
        inev.kind = MOUSE_CLICK_EVENT;
        dpyinfo->last_mouse_frame = f;
      } else {
        inev.kind = MOUSE_CLICK_EVENT;  /* Still reported as click */
      }

      /* Map X button numbers to Emacs */
      switch (button) {
        case Button1: inev.code = 0; break;  /* Left */
        case Button2: inev.code = 1; break;  /* Middle */
        case Button3: inev.code = 2; break;  /* Right */
        case Button4: /* Wheel up */
          inev.kind = WHEEL_EVENT;
          inev.code = 0;
          modifiers |= up_modifier;
          break;
        case Button5: /* Wheel down */
          inev.kind = WHEEL_EVENT;
          inev.code = 0;
          modifiers |= down_modifier;
          break;
      }

      /* Set position */
      inev.x = event->xbutton.x;
      inev.y = event->xbutton.y;
      inev.modifiers = modifiers;
      XSETFRAME(inev.frame_or_window, f);

      kbd_buffer_store_event(&inev);
      count++;
      break;
    }

    case MotionNotify: {
      /* Mouse motion */
      inev.kind = MOUSE_MOVEMENT_EVENT;
      inev.x = event->xmotion.x;
      inev.y = event->xmotion.y;
      XSETFRAME(inev.frame_or_window, f);

      /* Update mouse highlight */
      note_mouse_movement(f, &event->xmotion);

      kbd_buffer_store_event(&inev);
      count++;
      break;
    }

    /* ===== Focus Events ===== */
    case FocusIn:
    case FocusOut: {
      x_detect_focus_change(dpyinfo, f, event, &inev);
      if (inev.kind != NO_EVENT) {
        kbd_buffer_store_event(&inev);
        count++;
      }
      break;
    }

    /* ===== Exposure Events ===== */
    case Expose: {
      /* Part of window needs redrawing */
      f->output_data.x->has_been_visible = true;

      /* Mark region for redisplay */
      expose_frame(f, event->xexpose.x, event->xexpose.y,
                   event->xexpose.width, event->xexpose.height);
      break;
    }

    /* ===== Window Configuration ===== */
    case ConfigureNotify: {
      /* Window moved or resized */
      if (event->xconfigure.width != FRAME_PIXEL_WIDTH(f)
          || event->xconfigure.height != FRAME_PIXEL_HEIGHT(f)) {
        /* Size changed */
        change_frame_size(f, event->xconfigure.width,
                         event->xconfigure.height, false, true, false);
        SET_FRAME_GARBAGED(f);
        cancel_mouse_face(f);
      }

      /* Check for position change */
      x_check_expected_move(f, event->xconfigure.x, event->xconfigure.y);
      break;
    }

    /* ===== Window Manager Events ===== */
    case ClientMessage: {
      if (event->xclient.message_type == dpyinfo->Xatom_wm_protocols) {
        Atom protocol = event->xclient.data.l[0];

        if (protocol == dpyinfo->Xatom_wm_delete_window) {
          /* WM wants to delete window */
          inev.kind = DELETE_WINDOW_EVENT;
          XSETFRAME(inev.frame_or_window, f);
          kbd_buffer_store_event(&inev);
          count++;
        }
        else if (protocol == dpyinfo->Xatom_wm_take_focus) {
          /* WM wants us to take focus */
          x_focus_frame(f);
        }
      }
      break;
    }

    /* ===== Selection Events ===== */
    case SelectionRequest: {
      x_handle_selection_request(&event->xselectionrequest);
      break;
    }

    case SelectionClear: {
      x_handle_selection_clear(&event->xselectionclear);
      break;
    }

    case SelectionNotify: {
      x_handle_selection_notify(&event->xselection);
      break;
    }

    /* ===== Property Changes ===== */
    case PropertyNotify: {
      x_handle_property_notify(&event->xproperty);
      break;
    }

    /* ===== XInput2 Events ===== */
#ifdef HAVE_XINPUT2
    case GenericEvent: {
      if (event->xcookie.extension == dpyinfo->xi2_opcode) {
        XGetEventData(dpyinfo->display, &event->xcookie);
        count += xi_handle_event(dpyinfo, &event->xcookie, &inev);
        XFreeEventData(dpyinfo->display, &event->xcookie);
      }
      break;
    }
#endif

    /* Many more event types... */
  }

  return count;
}
```

### 3.3 Input Method Support (XIM)

For international text input (e.g., Chinese, Japanese, Korean):

```c
#ifdef HAVE_X_I18N
/* XIM provides pre-edit and composition */

/* Create input context for frame */
if (FRAME_X_XIM(f)) {
  FRAME_X_XIC(f) = XCreateIC(
    FRAME_X_XIM(f),
    XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
    XNClientWindow, FRAME_X_WINDOW(f),
    XNFocusWindow, FRAME_X_WINDOW(f),
    NULL
  );
}

/* During KeyPress handling */
Status status;
KeySym keysym;
char buffer[128];

int nchars = XmbLookupString(
  FRAME_X_XIC(f),
  &event->xkey,
  buffer, sizeof(buffer),
  &keysym, &status
);

switch (status) {
  case XLookupChars:
  case XLookupBoth:
    /* Got composed text - process UTF-8 string */
    break;
  case XLookupKeySym:
    /* Regular key without composition */
    break;
}
#endif
```

### 3.4 XInput2 Extension

Modern input device support for touchscreens, tablets, multi-touch:

```c
#ifdef HAVE_XINPUT2
/* Enable XI2 events */
XIEventMask mask;
unsigned char mask_bits[XIMaskLen(XI_LASTEVENT)] = {0};

mask.deviceid = XIAllDevices;
mask.mask_len = sizeof(mask_bits);
mask.mask = mask_bits;

XISetMask(mask_bits, XI_Motion);
XISetMask(mask_bits, XI_ButtonPress);
XISetMask(mask_bits, XI_ButtonRelease);
XISetMask(mask_bits, XI_Enter);
XISetMask(mask_bits, XI_Leave);
XISetMask(mask_bits, XI_TouchBegin);
XISetMask(mask_bits, XI_TouchUpdate);
XISetMask(mask_bits, XI_TouchEnd);

XISelectEvents(dpyinfo->display, FRAME_X_WINDOW(f), &mask, 1);
#endif
```

## 4. Window Management

### 4.1 Frame Creation

Frame creation involves complex interaction with window manager.

#### Frame Creation Steps

```c
/* From x_window in xfns.c */
static void x_window(struct frame *f)
{
  XSetWindowAttributes attributes;
  unsigned long attribute_mask;

  /* 1. Setup window attributes */
  attributes.background_pixel = FRAME_BACKGROUND_PIXEL(f);
  attributes.border_pixel = f->output_data.x->border_pixel;
  attributes.bit_gravity = StaticGravity;
  attributes.backing_store = NotUseful;
  attributes.save_under = True;
  attributes.event_mask = STANDARD_EVENT_SET;
  attributes.colormap = FRAME_X_COLORMAP(f);
  attribute_mask = (CWBackPixel | CWBorderPixel | CWBitGravity
                   | CWEventMask | CWColormap);

  /* 2. Create window */
  FRAME_X_WINDOW(f) = XCreateWindow(
    FRAME_X_DISPLAY(f),
    FRAME_DISPLAY_INFO(f)->root_window,
    f->left_pos, f->top_pos,
    FRAME_PIXEL_WIDTH(f), FRAME_PIXEL_HEIGHT(f),
    f->border_width,
    FRAME_DISPLAY_INFO(f)->n_planes,
    InputOutput,
    FRAME_X_VISUAL(f),
    attribute_mask, &attributes
  );

  /* 3. Set window manager hints */
  x_set_wm_hints(f);

  /* 4. Set WM protocols */
  Atom protocols[2];
  int n_protocols = 0;
  protocols[n_protocols++] = FRAME_DISPLAY_INFO(f)->Xatom_wm_delete_window;
  protocols[n_protocols++] = FRAME_DISPLAY_INFO(f)->Xatom_wm_take_focus;
  XSetWMProtocols(FRAME_X_DISPLAY(f), FRAME_X_WINDOW(f),
                  protocols, n_protocols);

  /* 5. Setup double buffering if available */
#ifdef HAVE_XDBE
  if (dpyinfo->supports_xdbe) {
    FRAME_X_RAW_DRAWABLE(f) = XdbeAllocateBackBufferName(
      FRAME_X_DISPLAY(f), FRAME_X_WINDOW(f), XdbeBackground
    );
  } else {
    FRAME_X_RAW_DRAWABLE(f) = FRAME_X_WINDOW(f);
  }
#else
  FRAME_X_RAW_DRAWABLE(f) = FRAME_X_WINDOW(f);
#endif

  /* 6. Create graphics contexts */
  x_make_gc(f);

  /* 7. Set various properties */
  x_set_name(f, f->name, true);
  x_set_icon_name(f, f->icon_name);

  /* 8. Map window to make it visible */
  XMapWindow(FRAME_X_DISPLAY(f), FRAME_X_WINDOW(f));
}
```

### 4.2 Window Manager Hints

#### WM_NORMAL_HINTS (Size Hints)

```c
void x_wm_set_size_hint(struct frame *f, long flags, bool user_position)
{
  XSizeHints size_hints;

  /* Base size (frame without text area) */
  size_hints.base_width = FRAME_TEXT_COLS_TO_PIXEL_WIDTH(f, 0);
  size_hints.base_height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT(f, 0);

  /* Size increments (for text resize) */
  size_hints.width_inc = FRAME_COLUMN_WIDTH(f);
  size_hints.height_inc = FRAME_LINE_HEIGHT(f);

  /* Min/max sizes */
  size_hints.min_width = size_hints.base_width;
  size_hints.min_height = size_hints.base_height;
  size_hints.max_width = x_display_pixel_width(FRAME_DISPLAY_INFO(f));
  size_hints.max_height = x_display_pixel_height(FRAME_DISPLAY_INFO(f));

  /* Position */
  if (user_position) {
    size_hints.flags |= USPosition;
    size_hints.x = f->left_pos;
    size_hints.y = f->top_pos;
  }

  size_hints.flags |= PSize | PResizeInc | PMinSize | PMaxSize | PBaseSize;

  XSetWMNormalHints(FRAME_X_DISPLAY(f), FRAME_OUTER_WINDOW(f), &size_hints);
}
```

#### WM_HINTS (Window Manager Hints)

```c
void x_wm_set_wm_hints(struct frame *f)
{
  XWMHints wm_hints;

  wm_hints.flags = InputHint | StateHint;
  wm_hints.input = True;  /* We want input */
  wm_hints.initial_state = f->want_fullscreen ? IconicState : NormalState;

  /* Icon pixmap */
  if (f->output_data.x->icon_bitmap > 0) {
    wm_hints.flags |= IconPixmapHint;
    wm_hints.icon_pixmap = f->output_data.x->icon_bitmap;
  }

  /* Window group */
  wm_hints.flags |= WindowGroupHint;
  wm_hints.window_group = FRAME_DISPLAY_INFO(f)->client_leader_window;

  XSetWMHints(FRAME_X_DISPLAY(f), FRAME_OUTER_WINDOW(f), &wm_hints);
}
```

#### _NET_WM Hints (Extended Window Manager Hints)

```c
/* Set window type */
Atom window_type = XInternAtom(display, "_NET_WM_WINDOW_TYPE_NORMAL", False);
XChangeProperty(display, window,
                XInternAtom(display, "_NET_WM_WINDOW_TYPE", False),
                XA_ATOM, 32, PropModeReplace,
                (unsigned char *)&window_type, 1);

/* Set window state (fullscreen, maximized, etc.) */
if (fullscreen) {
  Atom state = XInternAtom(display, "_NET_WM_STATE_FULLSCREEN", False);
  XChangeProperty(display, window,
                  XInternAtom(display, "_NET_WM_STATE", False),
                  XA_ATOM, 32, PropModeReplace,
                  (unsigned char *)&state, 1);
}
```

#### Motif Window Manager Hints

For borderless windows or custom decorations:

```c
#define MWM_HINTS_DECORATIONS (1L << 1)
#define MWM_DECOR_ALL         (1L << 0)

typedef struct {
  unsigned long flags;
  unsigned long functions;
  unsigned long decorations;
  long input_mode;
  unsigned long status;
} MwmHints;

void x_set_mwm_hints(struct frame *f, bool decorated)
{
  MwmHints hints;
  Atom prop = XInternAtom(FRAME_X_DISPLAY(f), "_MOTIF_WM_HINTS", False);

  hints.flags = MWM_HINTS_DECORATIONS;
  hints.decorations = decorated ? MWM_DECOR_ALL : 0;

  XChangeProperty(FRAME_X_DISPLAY(f), FRAME_OUTER_WINDOW(f),
                  prop, prop, 32, PropModeReplace,
                  (unsigned char *)&hints, 5);
}
```

### 4.3 Desktop Integration

#### Desktop Notifications (via D-Bus)

Emacs uses D-Bus for desktop notifications (implemented in Lisp calling D-Bus):

```elisp
;; From notifications.el
(defun notifications-notify (&rest params)
  "Send notification via D-Bus to notification daemon"
  (dbus-call-method :session
                    "org.freedesktop.Notifications"
                    "/org/freedesktop/Notifications"
                    "org.freedesktop.Notifications"
                    "Notify"
                    app-name
                    replaces-id
                    app-icon
                    summary
                    body
                    actions
                    hints
                    timeout))
```

#### System Tray Integration

For system tray icon (when compiled with GTK or toolkit):

```c
/* GTK system tray implementation */
#ifdef USE_GTK
GtkStatusIcon *icon = gtk_status_icon_new_from_file(icon_file);
gtk_status_icon_set_tooltip_text(icon, tooltip);
g_signal_connect(icon, "activate", G_CALLBACK(tray_icon_callback), NULL);
#endif
```

#### XEmbed Protocol

For embedding in other applications:

```c
/* XEMBED protocol support */
void x_embed_frame(struct frame *f, Window embedder_window)
{
  /* Send XEMBED_EMBEDDED_NOTIFY message */
  XClientMessageEvent xev;
  xev.type = ClientMessage;
  xev.window = FRAME_OUTER_WINDOW(f);
  xev.message_type = dpyinfo->Xatom_XEMBED;
  xev.format = 32;
  xev.data.l[0] = CurrentTime;
  xev.data.l[1] = XEMBED_EMBEDDED_NOTIFY;
  xev.data.l[2] = 0;
  xev.data.l[3] = embedder_window;
  xev.data.l[4] = 0;  /* XEMBED version */

  XSendEvent(dpyinfo->display, embedder_window, False, NoEventMask,
             (XEvent *)&xev);
}
```

### 4.4 Multi-Monitor Support

#### Xrandr Extension

```c
#ifdef HAVE_XRANDR
/* Query monitor configuration */
void x_get_monitor_attributes(struct x_display_info *dpyinfo)
{
  XRRScreenResources *resources;
  XRROutputInfo *output_info;
  XRRCrtcInfo *crtc_info;

  resources = XRRGetScreenResources(dpyinfo->display, dpyinfo->root_window);

  for (int i = 0; i < resources->noutput; i++) {
    output_info = XRRGetOutputInfo(dpyinfo->display, resources,
                                   resources->outputs[i]);

    if (output_info->connection == RR_Connected) {
      crtc_info = XRRGetCrtcInfo(dpyinfo->display, resources,
                                 output_info->crtc);

      /* Store monitor geometry */
      MonitorInfo *monitor = &dpyinfo->monitors[n_monitors++];
      monitor->geom.x = crtc_info->x;
      monitor->geom.y = crtc_info->y;
      monitor->geom.width = crtc_info->width;
      monitor->geom.height = crtc_info->height;
      monitor->name = xstrdup(output_info->name);

      /* Calculate DPI */
      monitor->mm_width = output_info->mm_width;
      monitor->mm_height = output_info->mm_height;

      XRRFreeCrtcInfo(crtc_info);
    }

    XRRFreeOutputInfo(output_info);
  }

  XRRFreeScreenResources(resources);
}
#endif
```

## 5. Comparison with Other Platforms

### 5.1 Architecture Comparison

| Aspect | X11 | Windows (W32) | macOS (NS) | Pure GTK (PGTK) |
|--------|-----|---------------|------------|-----------------|
| **Main File** | xterm.c (33K) | w32term.c (27K) | nsterm.m (30K) | pgtkterm.c (20K) |
| **Protocol** | X11 protocol | Win32 API | Cocoa/AppKit | GTK3/4 + Wayland |
| **Event Model** | Xlib events | Windows messages | NSEvent | GLib main loop |
| **Graphics** | Xlib/XRender/Cairo | GDI/GDI+ | Quartz 2D | Cairo only |
| **Font Backend** | Xft/Core X | Uniscribe/DirectWrite | Core Text | Pango/Cairo |
| **Color Model** | Visual-dependent | Direct RGB | Color spaces | Direct RGB |
| **Double Buffer** | XDBE (optional) | Built-in | Built-in | Cairo surfaces |

### 5.2 Event Processing Differences

#### X11 Event Loop

```c
/* X11: pselect on file descriptor */
while (XPending(display)) {
  XNextEvent(display, &event);
  handle_one_xevent(dpyinfo, &event, &finish, hold_quit);
}
```

#### Windows Event Loop

```c
/* Windows: GetMessage/PeekMessage */
MSG msg;
while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
  TranslateMessage(&msg);
  DispatchMessage(&msg);  /* Calls window procedure */
}

/* Window procedure handles events */
LRESULT CALLBACK w32_wnd_proc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg) {
    case WM_PAINT: /* ... */ break;
    case WM_KEYDOWN: /* ... */ break;
    /* ... */
  }
}
```

#### macOS Event Loop

```objective-c
/* macOS: NSEvent from NSApplication */
NSEvent *event;
while ((event = [NSApp nextEventMatchingMask:NSAnyEventMask
                       untilDate:[NSDate distantFuture]
                       inMode:NSDefaultRunLoopMode
                       dequeue:YES])) {
  [NSApp sendEvent:event];  /* Dispatches to EmacsView */
}

/* EmacsView handles events */
@implementation EmacsView
- (void)keyDown:(NSEvent *)event { /* ... */ }
- (void)mouseDown:(NSEvent *)event { /* ... */ }
@end
```

#### Pure GTK Event Loop

```c
/* PGTK: GLib main loop */
while (g_main_context_iteration(NULL, TRUE)) {
  /* GTK callbacks are invoked automatically */
}

/* GTK signal handlers */
g_signal_connect(widget, "key-press-event",
                 G_CALLBACK(key_press_event_cb), frame);
g_signal_connect(widget, "button-press-event",
                 G_CALLBACK(button_press_event_cb), frame);
```

### 5.3 Graphics System Differences

#### X11 Graphics Contexts vs Other Systems

**X11**:
- Server-side GCs with cached state
- Explicit GC creation and management
- `XCreateGC()`, `XChangeGC()`, `XSetForeground()`, etc.
- GCs persist across drawing operations

**Windows**:
- Device Contexts (DCs) are temporary
- `BeginPaint()`/`EndPaint()` for each update
- State set per operation: `SetTextColor()`, `SelectObject()`
- No persistent GC equivalent

**macOS**:
- Graphics contexts are implicit in Cocoa
- `NSGraphicsContext` automatically managed
- State set via `NSColor`, `NSFont` objects
- Quartz handles state management

**Pure GTK**:
- Cairo context for all drawing
- `cairo_t *cr` parameter to draw functions
- State machine: `cairo_set_source_rgb()`, `cairo_stroke()`, etc.
- More modern, stateless API

#### Drawing API Comparison

**Text Drawing**:

```c
/* X11 with Xft */
XftDrawStringUtf8(xft_draw, &xft_color, font, x, y, text, len);

/* Windows */
TextOutW(hdc, x, y, text, len);

/* macOS */
[string drawAtPoint:NSMakePoint(x, y) withAttributes:attrs];

/* PGTK/Cairo */
pango_cairo_show_layout(cr, layout);
```

**Rectangle Drawing**:

```c
/* X11 */
XFillRectangle(display, drawable, gc, x, y, width, height);

/* Windows */
Rectangle(hdc, x, y, x + width, y + height);

/* macOS */
NSRectFill(NSMakeRect(x, y, width, height));

/* PGTK/Cairo */
cairo_rectangle(cr, x, y, width, height);
cairo_fill(cr);
```

### 5.4 Font System Differences

#### Font Backend Comparison

| Platform | Backend | Features |
|----------|---------|----------|
| **X11** | Core X fonts | Bitmap/scalable, XLFD, server-side |
| **X11** | Xft/FreeType | Client-side, fontconfig, antialiasing, Unicode |
| **Windows** | GDI fonts | LOGFONT structure, basic rendering |
| **Windows** | DirectWrite | Modern, advanced shaping, emoji support |
| **macOS** | Core Text | AAT shaping, color emoji, advanced typography |
| **PGTK** | Pango/Cairo | Fontconfig, HarfBuzz shaping, internationalization |

#### Font Selection Examples

```c
/* X11 - XLFD */
"-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1"

/* X11 - Fontconfig pattern */
"Monospace-12:weight=bold:slant=italic"

/* Windows - LOGFONT */
LOGFONT lf = {0};
lf.lfHeight = -12;
lf.lfWeight = FW_BOLD;
strcpy(lf.lfFaceName, "Consolas");

/* macOS - font descriptor */
NSDictionary *attrs = @{
  NSFontFamilyAttribute: @"Menlo",
  NSFontSizeAttribute: @12.0
};
NSFont *font = [NSFont fontWithDescriptor:[NSFontDescriptor fontDescriptorWithFontAttributes:attrs] size:12.0];

/* PGTK - Pango */
PangoFontDescription *desc = pango_font_description_from_string("Monospace 12");
```

### 5.5 Color Handling

#### Color Allocation

**X11**: Visual-dependent, may require allocation
```c
XColor color;
color.red = r << 8;
color.green = g << 8;
color.blue = b << 8;
if (visual_class == TrueColor) {
  pixel = x_make_truecolor_pixel(r, g, b);
} else {
  XAllocColor(display, colormap, &color);
  pixel = color.pixel;
}
```

**Windows/macOS/PGTK**: Direct RGB
```c
/* Windows */
COLORREF color = RGB(r, g, b);

/* macOS */
NSColor *color = [NSColor colorWithRed:r/255.0 green:g/255.0 blue:b/255.0 alpha:1.0];

/* PGTK */
cairo_set_source_rgb(cr, r/255.0, g/255.0, b/255.0);
```

### 5.6 Clipboard/Selection

#### X11 Selections

**Three separate selections**:
- PRIMARY: Middle-click paste, selected text
- CLIPBOARD: Ctrl+C/V clipboard
- SECONDARY: Rarely used

**Implementation** (`xselect.c`):
- ICCCM protocol for selection ownership
- Incremental transfers for large data (INCR)
- Multiple formats via TARGETS atom
- Asynchronous with SelectionRequest/SelectionNotify

```c
/* Become selection owner */
XSetSelectionOwner(display, XA_PRIMARY, window, timestamp);

/* Respond to SelectionRequest */
void x_handle_selection_request(XSelectionRequestEvent *event) {
  /* Convert selection to requested format (UTF8_STRING, etc.) */
  /* Send SelectionNotify with converted data */
}
```

#### Windows Clipboard

**Single clipboard**:
- No selection concept
- Clipboard opened/closed explicitly

```c
OpenClipboard(hwnd);
EmptyClipboard();
HGLOBAL hglob = GlobalAlloc(GMEM_MOVEABLE, size);
/* Copy data */
SetClipboardData(CF_UNICODETEXT, hglob);
CloseClipboard();
```

#### macOS Pasteboard

**NSPasteboard**:
- Multiple pasteboards (general, find, drag)
- Type-based data storage

```objective-c
NSPasteboard *pb = [NSPasteboard generalPasteboard];
[pb clearContents];
[pb setString:text forType:NSPasteboardTypeString];
```

#### PGTK Clipboard

**GTK Clipboard API**:
- Abstracts X11 selections on X11
- Native clipboard on Wayland

```c
GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
gtk_clipboard_set_text(clipboard, text, -1);
```

### 5.7 Unique X11 Features

#### Features Unique to or Most Advanced on X11

1. **Window Manager Independence**: Emacs can run with any ICCCM-compliant WM
2. **Remote Display**: `DISPLAY=remote:0 emacs` works naturally
3. **X Resources**: Hierarchical configuration system
4. **Fine-grained Visual Control**: Choice of visual depth and class
5. **Shape Extension**: Non-rectangular windows
6. **XInput2**: Advanced multi-device input
7. **XDND Protocol**: Drag-and-drop with multiple protocols
8. **Three Selections**: PRIMARY, CLIPBOARD, SECONDARY
9. **XRender Extension**: Advanced compositing and antialiasing
10. **Xft Integration**: Direct FreeType/fontconfig usage

### 5.8 Platform-Specific Challenges

#### X11 Challenges

- **Complexity**: Many toolkits, extensions, and configurations to support
- **Visual Variety**: Must handle multiple visual classes
- **Asynchronous Nature**: Events and requests are asynchronous
- **Window Manager Variations**: Different WMs behave differently
- **Font Complexity**: Two completely different font systems
- **Color Allocation**: Non-trivial on non-TrueColor visuals

#### Windows Challenges

- **DPI Scaling**: Complex per-monitor DPI awareness
- **RTL Support**: Right-to-left languages need special handling
- **GDI Limitations**: Legacy GDI has many limitations
- **Message Pump**: Must integrate with Windows message loop
- **Unicode Conversions**: Constant UTF-16 ↔ UTF-8 conversions

#### macOS Challenges

- **Sandboxing**: App Store requirements restrict functionality
- **Objective-C Bridge**: C ↔ Objective-C impedance mismatch
- **Fullscreen Mode**: macOS native fullscreen is very different
- **Menu Bar**: Global menu bar vs frame menu bar
- **Emoji/Color Fonts**: Complex color glyph rendering

#### Pure GTK Challenges

- **Wayland Immaturity**: Some features still incomplete
- **GTK3 vs GTK4**: Incompatible APIs
- **Client-Side Decorations**: Complications with window decorations
- **Limited Control**: GTK abstracts away low-level control
- **Backend Abstraction**: Must work on X11, Wayland, Broadway

## 6. Key Implementation Files

### Source File Reference

#### Core X11 Implementation

| File | Purpose | Key Functions |
|------|---------|---------------|
| `src/xterm.c` | Main terminal interface | `XTread_socket`, `handle_one_xevent`, `x_draw_glyph_string` |
| `src/xterm.h` | X11 data structures | `struct x_display_info`, `struct x_output` |
| `src/xfns.c` | Frame functions | `x_window`, `x_create_frame`, `x_set_*` parameter functions |
| `src/xmenu.c` | Menu handling | `x_menu_show`, menu bar creation |
| `src/xselect.c` | Selection/clipboard | `x_handle_selection_request`, ICCCM implementation |
| `src/xsettings.c` | Desktop integration | XSETTINGS protocol, theme integration |
| `src/xrdb.c` | Resource database | X resource loading and parsing |

#### Font Backends

| File | Purpose |
|------|---------|
| `src/xfont.c` | Core X font driver |
| `src/xftfont.c` | Xft/FreeType font driver |
| `src/ftfont.c` | FreeType base functionality (shared with other platforms) |
| `src/font.c` | Generic font API |

#### Supporting Files

| File | Purpose |
|------|---------|
| `src/widget.c` | EmacsFrame widget (Xt configuration) |
| `src/xgselect.c` | GTK event integration |
| `src/xsmfns.c` | X Session Management |
| `src/xwidget.c` | WebKit widget embedding |

### Header Dependencies

```
xterm.h
  ├─ includes dispextern.h (redisplay interface)
  ├─ includes X11/Xlib.h (Xlib API)
  ├─ includes X11/Xutil.h (convenience functions)
  └─ defines x_display_info, x_output structures

dispextern.h
  ├─ defines redisplay_interface (platform-independent)
  ├─ defines glyph, glyph_string structures
  └─ common across all platforms

frame.h
  ├─ defines generic frame structure
  └─ platform-specific output_data union
```

## 7. Configuration and Build Options

### X11 Build Configuration

Key configure options:

```bash
# Basic X11 support (always enabled if X11 detected)
./configure --with-x

# Without X11 (text-mode only)
./configure --without-x

# Toolkit selection
./configure --with-x-toolkit=gtk3    # GTK3 (recommended)
./configure --with-x-toolkit=gtk2    # GTK2 (legacy)
./configure --with-x-toolkit=lucid   # Lucid widgets
./configure --with-x-toolkit=motif   # Motif/LessTif
./configure --with-x-toolkit=no      # No toolkit

# Font backends
./configure --with-xft               # Xft font support (recommended)
./configure --without-xft            # Core X fonts only

# Optional X extensions
./configure --with-xdbe              # Double buffering
./configure --with-xrender           # XRender extension
./configure --with-xinput2           # XInput2 (multi-touch, etc.)
./configure --with-xrandr            # Xrandr (multi-monitor)
./configure --with-xfixes            # Xfixes extension

# Image format support
./configure --with-png --with-jpeg --with-gif --with-tiff
./configure --with-rsvg              # SVG support
./configure --with-imagemagick       # ImageMagick

# Cairo support (modern rendering)
./configure --with-cairo             # Use Cairo for rendering
```

### Preprocessor Conditionals

Major compile-time flags:

```c
#ifdef HAVE_X11                    /* X11 support enabled */
#ifdef HAVE_X_WINDOWS             /* Generic X Windows */
#ifdef USE_X_TOOLKIT              /* Using Xt toolkit */
#ifdef USE_GTK                    /* Using GTK */
#ifdef USE_MOTIF                  /* Using Motif */
#ifdef USE_LUCID                  /* Using Lucid widgets */

#ifdef HAVE_XFT                   /* Xft fonts available */
#ifdef HAVE_XRENDER               /* XRender extension */
#ifdef HAVE_XDBE                  /* Double buffering */
#ifdef HAVE_XINPUT2               /* XInput2 support */
#ifdef HAVE_XRANDR                /* Xrandr extension */
#ifdef HAVE_XFIXES                /* Xfixes extension */

#ifdef HAVE_X_I18N                /* Input method support */
#ifdef HAVE_X11R6_XIM             /* X11R6 XIM */

#ifdef USE_CAIRO                  /* Cairo rendering */
```

## 8. Performance Considerations

### Optimization Strategies

1. **Graphics Context Reuse**: GCs are cached in faces and frames
2. **Color Caching**: Allocated colors stored to avoid reallocations
3. **Font Caching**: Font structures cached per-display
4. **Double Buffering**: Eliminates redundant redraws
5. **Exposure Compression**: Multiple expose events compressed
6. **Batch Rendering**: Glyph strings combine multiple glyphs

### Performance Tuning

```elisp
;; Reduce X traffic
(setq x-wait-for-event-timeout nil)  ; Don't wait for events

;; Font optimization
(setq font-use-system-font t)  ; Use system font settings

;; Improve scrolling
(setq fast-but-imprecise-scrolling t)

;; Reduce redraws
(setq redisplay-skip-fontification-on-input t)
```

## 9. Debugging X11 Issues

### Debug Tools

#### X Event Tracing

```bash
# Set environment variables
export XLIB_SKIP_ARGB_VISUALS=1  # Avoid ARGB visual issues
export XLIB_DEBUG=1              # Enable Xlib debugging

# Run with X synchronous mode (slow but catches errors immediately)
emacs --eval '(x-synchronize t)'
```

#### GDB Breakpoints

```gdb
# Break on X errors
break x_error_handler
break x_io_error_quitter

# Break on event handling
break handle_one_xevent
break XTread_socket

# Break on rendering
break x_draw_glyph_string
break x_flush
```

#### X Tools

```bash
# Monitor X protocol
xscope

# Window inspector
xwininfo -tree -root
xprop -root

# Event monitoring
xev

# Resource inspection
xrdb -query
```

### Common Issues

1. **Visual Mismatch**: Wrong visual selected → color problems
2. **Colormap Full**: Can't allocate colors → closest match used
3. **Font Not Found**: Font pattern doesn't match → fallback used
4. **Window Manager Issues**: Hints not respected → positioning problems
5. **Input Method Problems**: XIM conflicts → garbled input
6. **Extension Missing**: Feature requires extension → disabled or fallback

## 10. Future Directions

### X11 Evolution

- **Wayland Transition**: Pure GTK build provides Wayland support
- **XRender Deprecation**: Cairo becoming standard
- **Modern Extensions**: XInput2, XPresent, etc.
- **HiDPI Support**: Better scaling and monitor handling
- **Color Management**: ICC profiles and color spaces

### Code Modernization

- Reduce Xt dependencies (Xt is legacy)
- Increase Cairo usage for rendering
- Improve font fallback mechanisms
- Better multi-monitor support
- Enhanced accessibility

## 11. References

### Specifications

- **X Window System Protocol** (X11R7.7)
- **ICCCM** (Inter-Client Communication Conventions Manual)
- **EWMH** (Extended Window Manager Hints / NetWM)
- **XDND** (X Drag and Drop)
- **XEMBED** Protocol
- **XRender** Extension Specification
- **XInput2** Protocol

### Source Documentation

Key documentation in source files:
- `src/xterm.c`: Lines 1-400+ contain extensive documentation
- `src/xfns.c`: Frame function documentation
- `lisp/x-dnd.el`: Drag and drop protocol documentation
- `lisp/term/x-win.el`: X window system initialization

### External Resources

- X.Org Foundation: https://www.x.org/
- Xlib Manual: https://www.x.org/releases/current/doc/
- XCB Documentation: https://xcb.freedesktop.org/
- FreeDesktop.org Specifications: https://www.freedesktop.org/wiki/Specifications/

---

**Document Metadata**
- **Last Updated**: 2025-01-18
- **Emacs Version**: 31.0.50 (development)
- **Primary Author**: Generated from source analysis
- **Scope**: X11 window system integration with platform comparisons
