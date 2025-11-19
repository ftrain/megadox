# Keyboard and Event Handling System

## Table of Contents

1. [Overview](#overview)
2. [Core Data Structures](#core-data-structures)
3. [Event Loop Architecture](#event-loop-architecture)
4. [Event Reading and Processing](#event-reading-and-processing)
5. [Keymap System](#keymap-system)
6. [Key Sequence Reading](#key-sequence-reading)
7. [Command Execution](#command-execution)
8. [Keyboard Macros](#keyboard-macros)
9. [Special Event Types](#special-event-types)
10. [Multi-Keyboard Support](#multi-keyboard-support)

## Overview

The keyboard and event handling system is one of Emacs's most complex subsystems, comprising over 14,000 lines in `src/keyboard.c` alone. It manages the complete flow from hardware events to command execution, handling keyboard input, mouse events, menu interactions, and more.

### Key Components

```
Hardware Event → Input Queue → Event Loop → Key Reading →
Keymap Lookup → Command Dispatch → Interactive Execution
```

**Primary Files:**
- `src/keyboard.c` (14,582 lines) - Event loop, key sequence reading, command dispatch
- `src/keymap.c` (4,001 lines) - Keymap data structures and lookup algorithms
- `src/callint.c` - Interactive command execution
- `src/macros.c` - Keyboard macro recording and playback
- `lisp/bindings.el` - Global key bindings
- `lisp/keymap.el` - High-level keymap functions

## Core Data Structures

### 1. KBOARD - Per-Keyboard State

Each physical keyboard or display has its own KBOARD structure to maintain independent state for macro recording, prefix arguments, and event queues.

**Definition:** `src/keyboard.h:81-185`

```c
struct kboard
{
    KBOARD *next_kboard;

    /* Terminal-local keymap overrides */
    Lisp_Object Voverriding_terminal_local_map_;

    /* Last command executed (for repeat, undo tracking) */
    Lisp_Object Vlast_command_;
    Lisp_Object Vreal_last_command_;

    /* User-supplied keyboard translation table */
    Lisp_Object Vkeyboard_translate_table_;

    /* Last repeatable command */
    Lisp_Object Vlast_repeatable_command_;

    /* Prefix argument state */
    Lisp_Object Vprefix_arg_;
    Lisp_Object Vlast_prefix_arg_;

    /* Unread events specific to this keyboard */
    Lisp_Object kbd_queue_;

    /* Keyboard macro state */
    Lisp_Object defining_kbd_macro_;
    Lisp_Object *kbd_macro_buffer;      // Recording buffer
    Lisp_Object *kbd_macro_ptr;         // Current position
    Lisp_Object *kbd_macro_end;         // End of finalized section
    ptrdiff_t kbd_macro_bufsize;
    Lisp_Object Vlast_kbd_macro_;

    /* Window system identification */
    Lisp_Object Vwindow_system_;

    /* Key translation maps */
    Lisp_Object Vlocal_function_key_map_;
    Lisp_Object Vinput_decode_map_;

    /* Echo state */
    Lisp_Object echo_string_;
    Lisp_Object echo_prompt_;

    /* Reference count for shared displays */
    int reference_count;

    /* Queue status flags */
    bool_bf kbd_queue_has_data;
    bool_bf immediate_echo : 1;
};
```

**Key Concepts:**

1. **Single vs Any-KBOARD State:**
   - **Any-KBOARD:** Accept input from any keyboard, switch on first complete key
   - **Single-KBOARD:** Running a command, only accept input from its keyboard

2. **Event Queue:** Each KBOARD has its own `kbd_queue_` for events arriving while Emacs is processing another KBOARD

3. **Macro Recording:** Each KBOARD independently tracks macro definition state

**Access Macro:** `src/keyboard.h:38`
```c
#define KVAR(kboard, field) ((kboard)->field ## _)
```

### 2. Input Events

**Definition:** `src/termhooks.h:72-250`

```c
enum event_kind
{
    NO_EVENT,                    // No event
    ASCII_KEYSTROKE_EVENT,       // ASCII character with modifiers
    MULTIBYTE_CHAR_KEYSTROKE_EVENT,  // Multibyte character
    NON_ASCII_KEYSTROKE_EVENT,   // Function keys
    TIMER_EVENT,                 // Timer fired
    MOUSE_CLICK_EVENT,           // Mouse button click
    WHEEL_EVENT,                 // Mouse wheel scroll
    HORIZ_WHEEL_EVENT,          // Horizontal wheel
    SCROLL_BAR_CLICK_EVENT,     // Scroll bar interaction
    SELECTION_REQUEST_EVENT,     // X selection request
    SELECTION_CLEAR_EVENT,       // X selection cleared
    DELETE_WINDOW_EVENT,         // Window close request
    MENU_BAR_EVENT,             // Menu bar selection
    TAB_BAR_EVENT,              // Tab bar interaction
    TOOL_BAR_EVENT,             // Tool bar button
    ICONIFY_EVENT,              // Window iconified
    DEICONIFY_EVENT,            // Window restored
    DRAG_N_DROP_EVENT,          // File drag and drop
    USER_SIGNAL_EVENT,          // User-defined signal
    HELP_EVENT,                 // Help request
    FOCUS_IN_EVENT,             // Window gained focus
    FOCUS_OUT_EVENT,            // Window lost focus
    MOVE_FRAME_EVENT,           // Frame moved
    SELECT_WINDOW_EVENT,        // Window selection
    SAVE_SESSION_EVENT,         // Session save request
    // Platform-specific events...
};
```

**Event Structure:** `src/termhooks.h:300+`

```c
struct input_event
{
    enum event_kind kind;        // Event type
    Lisp_Object code;           // Character code or function key ID
    Lisp_Object frame_or_window; // Where the event occurred
    Lisp_Object arg;            // Event-specific argument
    int modifiers;              // Modifier keys (shift, control, etc.)
    int x, y;                   // Mouse position (for mouse events)
    Time timestamp;             // Event timestamp
};
```

**Event Buffer:** `src/keyboard.h:381-383`

```c
enum { KBD_BUFFER_SIZE = 4096 };

extern union buffered_input_event kbd_buffer[KBD_BUFFER_SIZE];
extern union buffered_input_event *kbd_fetch_ptr;
extern union buffered_input_event *kbd_store_ptr;
```

The event buffer is a circular array. Terminal-specific code stores events, and `read_char` fetches them.

### 3. Keymap Data Structures

Emacs supports two keymap formats: **sparse** and **dense** (full).

**Sparse Keymap Structure:**
```lisp
(keymap                              ; Symbol marking keymap
 "Menu name"                         ; Optional menu name string
 (KEY . BINDING)                     ; Individual bindings
 (KEY . BINDING)
 ...
 PARENT-KEYMAP)                      ; Optional parent
```

**Dense Keymap Structure:** `src/keymap.c:93-108`
```lisp
(keymap                              ; Symbol marking keymap
 CHAR-TABLE                          ; Bindings for chars without modifiers
 "Menu name"                         ; Optional menu name
 (KEY . BINDING)                     ; Additional bindings
 ...
 PARENT-KEYMAP)                      ; Optional parent
```

**Char-table** (created by `make-keymap`): Efficient storage for 256+ character bindings
**Alist** (created by `make-sparse-keymap`): For fewer bindings

**Key Binding Types:**

1. **Command** - A function symbol or lambda
2. **Keymap** - Prefix key (leads to more keys)
3. **String** - Keyboard macro
4. **Vector** - Key sequence to execute
5. **nil** - Undefined
6. **Symbol** - Indirect through symbol's function definition
7. **Cons `(STRING . DEFN)`** - Menu item with string
8. **Menu item** - `(menu-item NAME BINDING . PROPERTIES)`

### 4. Keymap Hierarchy

When looking up a key, Emacs searches multiple keymaps in order:

**From `read_key_sequence`:** `src/keyboard.c:10988-10993`

```c
// Build active keymap list
current_binding = active_maps (first_event, second_event);
```

**Active Map Priority (highest to lowest):**

1. **Overriding maps** (if non-nil):
   - `overriding-terminal-local-map` (KBOARD-specific)
   - `overriding-local-map` (buffer-local override)

2. **Character property maps** (at point):
   - `keymap` property
   - `local-map` property

3. **Minor mode maps:** `minor-mode-overriding-map-alist`, `minor-mode-map-alist`

4. **Local map:** `current-local-map` (major mode map)

5. **Global map:** `current-global-map`

**Key Translation Maps** (applied during reading):
- `input-decode-map` (terminal-specific, decode escape sequences)
- `local-function-key-map` (terminal-specific, function keys)
- `key-translation-map` (user translations)

## Event Loop Architecture

### Command Loop Hierarchy

```
main()
  └─> command_loop()
       ├─> top_level_1()           // Run startup file
       └─> command_loop_2()        // Error handling wrapper
            └─> command_loop_1()    // Main loop
                 ├─> read_key_sequence()
                 ├─> command-execute
                 │    └─> call-interactively
                 ├─> pre-command-hook
                 └─> post-command-hook
```

### 1. `command_loop()` - Top-Level Entry

**Location:** `src/keyboard.c:1113-1148`

```c
Lisp_Object
command_loop (void)
{
#ifdef HAVE_STACK_OVERFLOW_HANDLING
    // Stack overflow recovery support
    if (sigsetjmp (return_to_command_loop, 1) != 0)
    {
        // Recover from stack overflow
        init_eval ();
        Vinternal__top_level_message = recover_top_level_message;
    }
    else
        Vinternal__top_level_message = regular_top_level_message;
#endif

    if (command_loop_level > 0 || minibuf_level > 0)
    {
        // Recursive edit or minibuffer
        Lisp_Object val;
        val = internal_catch (Qexit, command_loop_2, Qerror);
        executing_kbd_macro = Qnil;
        return val;
    }
    else
        // Top level - loop forever
        while (1)
        {
            internal_catch (Qtop_level, top_level_1, Qnil);
            internal_catch (Qtop_level, command_loop_2, Qerror);
            executing_kbd_macro = Qnil;

            // Exit in batch mode on EOF
            if (noninteractive)
                Fkill_emacs (Qt, Qnil);
        }
}
```

**Key Points:**
- Outermost catch point for `(throw 'top-level)`
- Handles recursive editing levels
- Recovers from stack overflow (on supported platforms)
- Never returns in interactive mode

### 2. `command_loop_1()` - Main Command Loop

**Location:** `src/keyboard.c:1318-1700+`

This is the heart of Emacs's event processing.

```c
static Lisp_Object
command_loop_1 (void)
{
    // Initialize command state
    kset_prefix_arg (current_kboard, Qnil);
    kset_last_prefix_arg (current_kboard, Qnil);
    Vdeactivate_mark = Qnil;
    waiting_for_input = false;
    cancel_echoing ();

    this_command_key_count = 0;
    this_single_command_key_start = 0;

    if (NILP (Vmemory_full))
    {
        // Run post-command-hook from previous command
        if (!NILP (Vpost_command_hook) && !NILP (Vrun_hooks))
            safe_run_hooks_maybe_narrowed (Qpost_command_hook,
                                          XWINDOW (selected_window));

        // Resize echo area if needed
        if (!NILP (echo_area_buffer[0]))
            resize_echo_area_exactly ();

        // Process delayed warnings
        if (!NILP (Vdelayed_warnings_list))
            safe_run_hooks (Qdelayed_warnings_hook);
    }

    // Save last command
    kset_last_command (current_kboard, Vthis_command);
    kset_real_last_command (current_kboard, Vreal_this_command);

    while (true)  // Main command loop
    {
        // Check frame is alive
        if (! FRAME_LIVE_P (XFRAME (selected_frame)))
            Fkill_emacs (Qnil, Qnil);

        // Ensure current window's buffer is selected
        set_buffer_internal (XBUFFER (XWINDOW (selected_window)->contents));

        // Clear command-specific variables
        Vthis_command = Qnil;
        Vreal_this_command = Qnil;
        Vthis_original_command = Qnil;

        // *** READ KEY SEQUENCE ***
        raw_keybuf_count = 0;
        Lisp_Object keybuf[READ_KEY_ELTS];
        int i = read_key_sequence (keybuf, Qnil, false, true,
                                   true, false, false);

        // Handle special cases
        if (i == 0)    // EOF (only in keyboard macro)
            return Qnil;
        if (i == -1)   // Rejected menu
        {
            cancel_echoing ();
            this_command_key_count = 0;
            goto finalize;
        }

        last_command_event = keybuf[i - 1];

        // Get the command binding
        cmd = read_key_sequence_cmd;

        // Apply command remapping
        Vthis_original_command = cmd;
        if (!NILP (read_key_sequence_remapped))
            cmd = read_key_sequence_remapped;

        // *** EXECUTE COMMAND ***
        Vthis_command = cmd;
        Vreal_this_command = cmd;

        // Run pre-command-hook
        safe_run_hooks_maybe_narrowed (Qpre_command_hook,
                                      XWINDOW (selected_window));

        if (NILP (Vthis_command))
            call0 (Qundefined);  // Key undefined
        else
        {
            // Add undo boundary
            call0 (Qundo_auto__add_boundary);

            // Execute the command
            calln (Qcommand_execute, Vthis_command);
        }

        // Run post-command-hook
        safe_run_hooks_maybe_narrowed (Qpost_command_hook,
                                      XWINDOW (selected_window));

        // Update command history
        kset_last_command (current_kboard, Vthis_command);
        kset_real_last_command (current_kboard, Vreal_this_command);

        // Reset for next command
        this_command_key_count = 0;
        this_single_command_key_start = 0;

    finalize:
        // Handle auto-save, garbage collection, etc.
        ...
    }
}
```

**Command Loop Flow:**

1. **Post-processing from previous command** - hooks, echo area, warnings
2. **Read key sequence** - Get user input via `read_key_sequence()`
3. **Command lookup** - Find binding in active keymaps
4. **Pre-command hook** - User-defined pre-execution code
5. **Command execution** - Call `command-execute` → `call-interactively`
6. **Post-command hook** - User-defined post-execution code
7. **Cleanup** - Update state, handle auto-save, GC

### 3. `recursive_edit_1()` - Recursive Editing

**Location:** `src/keyboard.c:708-761`

Recursive editing allows running a command loop within a command, used by the debugger, `recursive-edit`, and some interactive commands.

```c
Lisp_Object
recursive_edit_1 (void)
{
    specpdl_ref count = SPECPDL_INDEX ();
    Lisp_Object val;

    if (command_loop_level > 0)
    {
        // Bind standard streams
        specbind (Qstandard_output, Qt);
        specbind (Qstandard_input, Qt);
        specbind (Qsymbols_with_pos_enabled, Qnil);
        specbind (Qprint_symbols_bare, Qnil);
    }

    // Allow redisplay in debugger
    specbind (Qinhibit_redisplay, Qnil);
    redisplaying_p = 0;

    // Prevent undo boundaries in parent buffers
    specbind (Qundo_auto__undoably_changed_buffers, Qnil);

    // Run nested command loop
    val = command_loop ();

    if (EQ (val, Qt))
        quit ();  // User aborted
    if (STRINGP (val))
        xsignal1 (Qerror, val);  // Error message
    if (FUNCTIONP (val))
        call0 (val);  // Callback function

    return unbind_to (count, Qnil);
}
```

**Exit values:**
- `nil` - Normal exit
- `t` - Abort (calls `quit()`)
- String - Error (signals error with message)
- Function - Call function then return

## Event Reading and Processing

### 1. `read_char()` - Single Event Reading

**Location:** `src/keyboard.c:2534-3200+`

This is the lowest-level function that reads a single input event.

```c
Lisp_Object
read_char (int commandflag,          // Command vs non-command reading
           Lisp_Object map,           // Keymap for help events
           Lisp_Object prev_event,    // Previous event (for doubleclick)
           bool *used_mouse_menu,     // Output: was menu used?
           struct timespec *end_time) // Timeout
{
    Lisp_Object c;
    struct kboard *orig_kboard = current_kboard;

 retry:

    // Check various event sources in priority order:

    // 1. Unread post-input-method events
    if (CONSP (Vunread_post_input_method_events))
    {
        c = XCAR (Vunread_post_input_method_events);
        Vunread_post_input_method_events =
            XCDR (Vunread_post_input_method_events);
        reread = true;
        goto reread_first;
    }

    // 2. Unread command events (highest priority for real input)
    if (CONSP (Vunread_command_events))
    {
        c = XCAR (Vunread_command_events);
        Vunread_command_events = XCDR (Vunread_command_events);

        // Handle special prefixes: (no-record . EVENT), (t . EVENT)
        if (CONSP (c) && EQ (XCAR (c), Qno_record))
        {
            c = XCDR (c);
            recorded = true;
        }
        reread = true;
        goto reread_for_input_method;
    }

    // 3. Input method events
    if (CONSP (Vunread_input_method_events))
    {
        c = XCAR (Vunread_input_method_events);
        Vunread_input_method_events =
            XCDR (Vunread_input_method_events);
        reread = true;
        goto reread_for_input_method;
    }

    // 4. Executing keyboard macro
    if (!NILP (Vexecuting_kbd_macro) && !at_end_of_macro_p ())
    {
        Vlast_event_frame = internal_last_event_frame = Qmacro;
        c = Faref (Vexecuting_kbd_macro,
                  make_int (executing_kbd_macro_index));

        // Handle meta bit in string macros
        if (STRINGP (Vexecuting_kbd_macro)
            && (XFIXNAT (c) & 0x80) && (XFIXNAT (c) <= 0xff))
            XSETFASTINT (c, CHAR_META | (XFIXNAT (c) & ~0x80));

        executing_kbd_macro_index++;
        goto from_macro;
    }

    // 5. Pending switch-frame event
    if (!NILP (unread_switch_frame))
    {
        c = unread_switch_frame;
        unread_switch_frame = Qnil;
        goto reread_first;
    }

    // 6. Redisplay if needed
    if (commandflag >= 0)
    {
        // Swallow non-user-visible events (X selections, etc.)
        if (input_pending || detect_input_pending_run_timers (0))
            swallow_events (false);

        // Redisplay loop
        while (!(input_pending && input_was_pending))
        {
            input_was_pending = input_pending;
            if (help_echo_showing_p &&
                !BASE_EQ (selected_window, minibuf_window))
                redisplay_preserve_echo_area (5);
            else
                redisplay ();

            if (!input_pending)
                break;
            swallow_events (false);
        }
    }

    // 7. Read from actual input (keyboard, mouse, etc.)
    // This involves waiting for input if necessary

    /* ... complex input reading logic ... */

    // Try reading from current KBOARD
    if (KBOARD_HAS_INPUT (current_kboard))
        c = read_event_from_queue ();

    // If no input on current KBOARD, try others or wait
    if (NILP (c))
    {
        if (commandflag >= 0)
        {
            // Wait for input with timeout
            c = read_event_from_main_queue (end_time,
                                           local_getcjmp,
                                           &used_mouse_menu);
        }
    }

 reread_for_input_method:
    // Apply input method translation
    if (!NILP (Vinput_method_function) && !reread)
    {
        c = apply_input_method (c);
    }

 reread_first:
    // Record in lossage
    if (!recorded && !CONSP (c))
        record_char (c);

 from_macro:
    // Post-processing: help events, etc.

    return c;
}
```

**Event Source Priority:**
1. `unread-post-input-method-events` (after input method processing)
2. `unread-command-events` (explicit unreading)
3. `unread-input-method-events` (before input method)
4. Keyboard macro playback
5. Pending switch-frame
6. Actual hardware input

**Key Features:**
- **Handles multiple input sources** in defined priority order
- **Triggers redisplay** when appropriate
- **Applies input methods** for international character input
- **Records to lossage** for `view-lossage` command
- **Manages KBOARD switching** in multi-keyboard scenarios

### 2. Event Queue Management

**Storing Events:** `src/keyboard.c` (various functions)

```c
void
kbd_buffer_store_event (struct input_event *event)
{
    // Store event in circular buffer
    if (kbd_store_ptr == kbd_buffer + KBD_BUFFER_SIZE)
        kbd_store_ptr = kbd_buffer;

    // Copy event to buffer
    *kbd_store_ptr = *event;

    // Advance store pointer
    ++kbd_store_ptr;

    // Set input_pending flag
    input_pending = true;
}
```

**Reading from Queue:**

```c
static Lisp_Object
read_event_from_queue (void)
{
    if (kbd_fetch_ptr == kbd_store_ptr)
        return Qnil;  // Empty queue

    struct input_event *event = &kbd_fetch_ptr->ie;

    // Advance fetch pointer
    if (++kbd_fetch_ptr == kbd_buffer + KBD_BUFFER_SIZE)
        kbd_fetch_ptr = kbd_buffer;

    // Convert to Lisp event
    return event_to_lisp (event);
}
```

## Keymap System

### 1. Keymap Lookup Algorithm

**Primary Function:** `access_keymap_1()` - `src/keymap.c:327-489`

This function performs the core keymap lookup.

```c
Lisp_Object
access_keymap_1 (Lisp_Object map,
                 Lisp_Object idx,        // Key to look up
                 bool t_ok,              // Accept default binding?
                 bool noinherit,         // Don't check parent?
                 bool autoload)          // Autoload keymaps?
{
    // Normalize the key
    idx = EVENT_HEAD (idx);  // Extract head from mouse events

    if (SYMBOLP (idx))
        idx = reorder_modifiers (idx);  // Canonical modifier order
    else if (FIXNUMP (idx))
        // Mask to valid character range
        XSETFASTINT (idx, XFIXNUM (idx) & (CHAR_META | (CHAR_META - 1)));

    // *** META -> ESC MAPPING ***
    // Handle meta modifier specially
    if (FIXNUMP (idx) && XFIXNAT (idx) & meta_modifier)
    {
        // Look for meta-map (ESC prefix map)
        Lisp_Object event_meta_binding, event_meta_map;

        event_meta_binding = access_keymap_1 (map, meta_prefix_char,
                                             t_ok, noinherit, autoload);
        event_meta_map = get_keymap (event_meta_binding, 0, autoload);

        if (CONSP (event_meta_map))
        {
            // Found meta-map, look up key without meta modifier
            map = event_meta_map;
            idx = make_fixnum (XFIXNAT (idx) & ~meta_modifier);
        }
        else if (t_ok)
            idx = Qt;  // Only accept default binding
        else
            return NILP (event_meta_binding) ? Qnil : Qunbound;
    }

    // *** SEARCH KEYMAP CHAIN ***
    {
        Lisp_Object tail;
        Lisp_Object t_binding = Qunbound;  // Default binding
        Lisp_Object retval = Qunbound;
        Lisp_Object retval_tail = Qnil;

        // Iterate through keymap structure
        for (tail = (CONSP (map) && EQ (Qkeymap, XCAR (map)))
                   ? XCDR (map) : map;
             (CONSP (tail) ||
              (tail = get_keymap (tail, 0, autoload), CONSP (tail)));
             tail = XCDR (tail))
        {
            Lisp_Object val = Qunbound;
            Lisp_Object binding = XCAR (tail);
            Lisp_Object submap = get_keymap (binding, 0, autoload);

            // Check for parent keymap marker
            if (EQ (binding, Qkeymap))
            {
                if (noinherit || NILP (retval))
                    break;  // Stop here, rest is inherited

                // Merge with parent keymap
                if (!BASE_EQ (retval, Qunbound))
                {
                    Lisp_Object parent_entry;
                    parent_entry = get_keymap (
                        access_keymap_1 (tail, idx, t_ok, 0, autoload),
                        0, autoload);

                    if (KEYMAPP (parent_entry))
                    {
                        // Chain keymaps together
                        if (CONSP (retval_tail))
                            XSETCDR (retval_tail, parent_entry);
                        else
                        {
                            retval_tail = Fcons (retval, parent_entry);
                            retval = Fcons (Qkeymap, retval_tail);
                        }
                    }
                    break;
                }
            }
            // Recursively search sub-keymap
            else if (CONSP (submap))
            {
                val = access_keymap_1 (submap, idx, t_ok,
                                      noinherit, autoload);
            }
            // Check alist entry: (KEY . BINDING)
            else if (CONSP (binding))
            {
                Lisp_Object key = XCAR (binding);

                if (EQ (key, idx))
                    val = XCDR (binding);
                else if (t_ok && EQ (key, Qt))
                {
                    t_binding = XCDR (binding);  // Save default
                    t_ok = 0;  // Only use first default
                }
            }
            // Check vector entry (dense keymap)
            else if (VECTORP (binding))
            {
                if (FIXNUMP (idx) && XFIXNAT (idx) < ASIZE (binding))
                    val = AREF (binding, XFIXNAT (idx));
            }
            // Check char-table entry (full keymap)
            else if (CHAR_TABLE_P (binding))
            {
                // Only characters without modifiers are in char-table
                if (FIXNUMP (idx) &&
                    (XFIXNAT (idx) & CHAR_MODIFIER_MASK) == 0)
                {
                    val = Faref (binding, idx);
                    // nil means explicitly unbound in char-tables
                    if (NILP (val))
                        val = Qunbound;
                }
            }

            // Process found binding
            if (!BASE_EQ (val, Qunbound))
            {
                // Qt binding shadows parent but is treated as nil
                if (EQ (val, Qt))
                    val = Qnil;

                // Trace indirect definitions, menu items
                val = get_keyelt (val, autoload);

                if (!KEYMAPP (val))
                {
                    if (NILP (retval) || BASE_EQ (retval, Qunbound))
                        retval = val;
                    if (!NILP (val))
                        break;  // Non-nil binding shadows everything
                }
                else if (NILP (retval) || BASE_EQ (retval, Qunbound))
                    retval = val;
                else if (CONSP (retval_tail))
                {
                    // Chain multiple keymap bindings
                    XSETCDR (retval_tail, list1 (val));
                    retval_tail = XCDR (retval_tail);
                }
                else
                {
                    retval_tail = list1 (val);
                    retval = Fcons (Qkeymap,
                                   Fcons (retval, retval_tail));
                }
            }
            maybe_quit ();  // Allow C-g during long search
        }

        // Return found binding or default
        return BASE_EQ (Qunbound, retval)
               ? get_keyelt (t_binding, autoload) : retval;
    }
}
```

**Key Algorithm Steps:**

1. **Normalize key** - Extract event head, reorder modifiers
2. **Handle Meta mapping** - Check for ESC-prefix keymap
3. **Iterate keymap elements** - Alist, vector, char-table, sub-keymaps
4. **Process inheritance** - Chain parent keymaps
5. **Return binding** - First non-nil or default (Qt)

**Lookup Precedence** (within a single keymap):
1. Explicit binding for the key
2. Sub-keymap bindings
3. Default binding (key `t`)
4. Parent keymap

### 2. Key Lookup Through Keymap Hierarchy

**Function:** `active_maps()` - Builds list of active keymaps

**Lookup Process:**

```
For each keymap in active-maps:
    binding = access_keymap (keymap, key)
    if binding is a command:
        return binding
    if binding is a keymap:
        mark as prefix, continue reading keys
    if binding is nil:
        continue to next keymap
```

**Example Lookup for `C-x C-f`:**

```
1. Read 'C-x':
   - Search active keymaps for C-x binding
   - Find: global-map[C-x] → ctl-x-map (a keymap)
   - Mark as prefix, continue

2. Read 'C-f':
   - Search ctl-x-map for C-f binding
   - Find: ctl-x-map[C-f] → find-file (a command)
   - Complete! Execute find-file
```

### 3. Menu Item Handling

**Function:** `get_keyelt()` - `src/keymap.c:679-750`

Traces indirect definitions and handles menu items.

```c
static Lisp_Object
get_keyelt (Lisp_Object object, bool autoload)
{
    while (1)
    {
        if (!(CONSP (object)))
            return object;  // This is the final value

        // Handle new-format menu items: (menu-item NAME BINDING ...)
        if (EQ (XCAR (object), Qmenu_item))
        {
            if (CONSP (XCDR (object)))
            {
                Lisp_Object tem;
                object = XCDR (XCDR (object));  // Skip name
                tem = object;
                if (CONSP (object))
                    object = XCAR (object);  // Get binding

                // Evaluate :filter property
                if (CONSP (tem) && CONSP (XCDR (tem)))
                {
                    Lisp_Object filter = Fplist_get (XCDR (tem), QCfilter);
                    if (!NILP (filter))
                        object = menu_item_eval_property (
                            list2 (filter, list2 (Qquote, object)));
                }
            }
            else
                object = Qnil;
        }
        // Handle old-format menu items: (STRING . DEFN)
        else if (STRINGP (XCAR (object)))
        {
            object = XCDR (object);
            if (!CONSP (object))
                object = Qnil;
        }
        // Handle keymap indirection: (KEYMAP . INDEX)
        else if (!NILP (object))
        {
            Lisp_Object map = get_keymap (XCAR (object), 0, autoload);
            Lisp_Object key = XCDR (object);

            if (CONSP (map))
                object = access_keymap (map, key, 0, 0, autoload);
            else
                object = Qnil;
        }
        else
            return Qnil;
    }
}
```

**Menu Item Formats:**

**New format:**
```lisp
(menu-item "Find File"      ; Name shown in menu
           find-file        ; Command to execute
           :help "Read a file into Emacs"
           :keys "C-x C-f"  ; Key equivalent
           :filter FUNCTION ; Dynamic filtering
           :enable FORM)    ; Enable condition
```

**Old format:**
```lisp
("Find File" . find-file)   ; Simple string + binding
```

## Key Sequence Reading

### `read_key_sequence()` - The Heart of Key Reading

**Location:** `src/keyboard.c:10841-12500+` (massive 1600+ line function)

This function reads a complete key sequence, handling prefix keys, function key translation, and command remapping.

**Signature:**
```c
static int
read_key_sequence (Lisp_Object *keybuf,      // Output buffer
                   Lisp_Object prompt,       // Prompt string
                   bool dont_downcase_last,  // Case sensitivity
                   bool can_return_switch_frame,
                   bool fix_current_buffer,
                   bool prevent_redisplay,
                   bool disable_text_conversion_p)
```

**Key Data Structures:**

```c
// Current length of key sequence
int t = 0;

// Mock input: replayed keys after function key translation
int mock_input = 0;

// Translation state for three keymaps:
keyremap fkey;      // local-function-key-map
keyremap keytran;   // key-translation-map
keyremap indec;     // input-decode-map

// Delayed events
Lisp_Object delayed_switch_frame;
```

**Translation Structure:**
```c
struct keyremap
{
    Lisp_Object parent;   // Original translation map
    Lisp_Object map;      // Current position in map
    int start;            // Start of sequence being translated
    int end;              // End of translated portion
};
```

**Main Algorithm:**

```c
read_key_sequence (Lisp_Object *keybuf, ...)
{
    int t = 0;  // Current position in keybuf
    int mock_input = 0;

    keyremap indec, fkey, keytran;

 replay_entire_sequence:
    // Initialize translation maps
    indec.map = indec.parent = KVAR (current_kboard, Vinput_decode_map);
    fkey.map = fkey.parent = KVAR (current_kboard, Vlocal_function_key_map);
    keytran.map = keytran.parent = Vkey_translation_map;

    indec.start = indec.end = 0;
    fkey.start = fkey.end = 0;
    keytran.start = keytran.end = 0;

 replay_sequence:
    // Build active keymap list
    current_binding = active_maps (first_event, second_event);

    t = 0;
    first_unbound = READ_KEY_ELTS + 1;

    // *** MAIN READING LOOP ***
    while (!NILP (current_binding)
           ? KEYMAPP (current_binding)  // Keep reading if prefix
           : (keytran.start < t))       // Or translating
    {
        Lisp_Object key;
        bool used_mouse_menu = false;

        // Where the last real key started
        int last_real_key_start;

        // *** READ NEXT KEY ***

        if (t < mock_input)
        {
            // Replaying translated keys
            key = keybuf[t];
            used_mouse_menu = used_mouse_menu_history[t];
        }
        else
        {
            // Read actual input
            key = read_char (NILP (prompt),
                           current_binding,
                           last_nonmenu_event,
                           &used_mouse_menu,
                           NULL);

            // Handle function keys, mouse events, help events...
            key = process_special_events (key, ...);
        }

        // Add key to buffer
        keybuf[t] = key;
        used_mouse_menu_history[t] = used_mouse_menu;
        t++;

        // *** APPLY TRANSLATIONS ***

        // 1. Input decode map (terminal escape sequences)
        if (indec.end < t)
        {
            Lisp_Object translation;
            translation = apply_keyremap (&indec, keybuf, t, ...);

            if (!NILP (translation))
            {
                // Replace sequence with translation
                mock_input = translate_sequence (keybuf, &indec,
                                                translation);
                goto replay_sequence;
            }
        }

        // 2. Function key map (e.g., F1 → help)
        if (fkey.end < t)
        {
            Lisp_Object translation;
            translation = apply_keyremap (&fkey, keybuf, t, ...);

            if (!NILP (translation))
            {
                mock_input = translate_sequence (keybuf, &fkey,
                                                translation);
                goto replay_sequence;
            }
        }

        // 3. Key translation map (user-defined)
        if (keytran.end < t)
        {
            Lisp_Object translation;
            translation = apply_keyremap (&keytran, keybuf, t, ...);

            if (!NILP (translation))
            {
                mock_input = translate_sequence (keybuf, &keytran,
                                                translation);
                goto replay_sequence;
            }
        }

        // *** LOOKUP IN KEYMAPS ***

        // Look up current sequence in active keymaps
        Lisp_Object new_binding;
        new_binding = lookup_in_keymap_list (current_binding,
                                            keybuf, t, ...);

        if (NILP (new_binding))
        {
            // No binding found
            if (t > 0)
            {
                // Try case conversion (e.g., C-X → C-x)
                new_binding = try_case_conversion (keybuf, t, ...);
            }

            if (NILP (new_binding))
            {
                // Truly unbound - sequence is complete
                current_binding = Qnil;
                break;
            }
        }

        current_binding = new_binding;

        // *** CHECK FOR COMPLETION ***

        if (!KEYMAPP (current_binding))
        {
            // Found a command - sequence complete!
            read_key_sequence_cmd = current_binding;

            // Apply command remapping
            read_key_sequence_remapped =
                Fcommand_remapping (current_binding, Qnil, Qnil);

            break;
        }

        // current_binding is a keymap - it's a prefix
        // Continue reading...
    }

    // *** FINALIZATION ***

    // Update this_command_keys
    for (i = 0; i < t; i++)
        add_command_key (keybuf[i]);

    // Return number of keys read
    return t;
}
```

**Key Translation Process:**

**Example: `ESC [ A` → `<up>`**

1. Read `ESC` - Look in input-decode-map, find prefix
2. Read `[` - Still a prefix in input-decode-map
3. Read `A` - Complete sequence `ESC [ A`
4. Look up in input-decode-map: Found `<up>`
5. **Replace sequence:** keybuf[0] = `<up>`, t = 1, mock_input = 1
6. **Replay:** Look up `<up>` in active keymaps
7. Find binding for `<up>` → `previous-line`

**Keymap Application Order:**

```
input-decode-map        (terminal-specific, first)
   ↓
local-function-key-map  (function keys)
   ↓
key-translation-map     (user translations, last)
   ↓
Active keymaps          (actual command lookup)
```

### Translation Map Functions

**Applying a Translation:**

```c
static Lisp_Object
apply_keyremap (keyremap *map,
                Lisp_Object *keybuf,
                int t,
                ...)
{
    // Extend translation as far as possible
    while (map->end < t)
    {
        Lisp_Object key = keybuf[map->end];
        Lisp_Object binding;

        // Look up next key in translation map
        binding = access_keymap (map->map, key, 1, 1, 1);

        if (NILP (binding))
        {
            // No translation for this sequence
            map->end = t;
            map->map = map->parent;
            return Qnil;
        }

        if (!KEYMAPP (binding))
        {
            // Found complete translation
            return binding;
        }

        // binding is a keymap - continue
        map->map = binding;
        map->end++;
    }

    return Qnil;
}
```

## Command Execution

### `call-interactively` - Interactive Command Execution

**Location:** `src/callint.c:253-900+`

This function executes commands with arguments gathered according to their `interactive` spec.

```c
DEFUN ("call-interactively", Fcall_interactively, Scall_interactively,
       1, 3, 0,
       doc: /* Call FUNCTION, providing args according to its interactive
calling specs... */)
  (Lisp_Object function, Lisp_Object record_flag, Lisp_Object keys)
{
    specpdl_ref speccount = SPECPDL_INDEX ();

    Lisp_Object prefix_arg = Vcurrent_prefix_arg;
    Lisp_Object enable = Fget (function, Qenable_recursive_minibuffers);

    // Get the interactive specification
    Lisp_Object specs = Finteractive_form (function);

    if (NILP (specs))
        // Not an interactive function
        return Ffuncall (1, &function);

    // specs is either:
    // - A string: "(interactive \"sString: \")"
    // - A list: "(interactive (list (read-string \"String: \")))"

    Lisp_Object spec_string = Qnil;
    Lisp_Object spec_list = Qnil;

    if (STRINGP (XCAR (specs)))
        spec_string = XCAR (specs);
    else
        spec_list = Feval (XCAR (specs), Qt);  // Evaluate the form

    // *** PROCESS INTERACTIVE STRING ***

    if (!NILP (spec_string))
    {
        const char *string = SSDATA (spec_string);
        const char *tem;

        // Check special prefixes:
        // "*" - Error if buffer read-only
        // "@" - Select window from mouse event
        // "^" - Handle shift-selection

        while (*string == '*' || *string == '@' || *string == '^')
        {
            if (*string == '*')
            {
                if (!NILP (BVAR (current_buffer, read_only)))
                    xsignal1 (Qbuffer_read_only, Fcurrent_buffer ());
                string++;
            }
            else if (*string == '@')
            {
                // Select window from event
                Lisp_Object event = extract_event (keys);
                Lisp_Object window = posn_window (event_start (event));
                if (WINDOWP (window))
                    Fselect_window (window, Qnil);
                string++;
            }
            else if (*string == '^')
            {
                // Handle shift-selection
                if (!NILP (Vshift_select_mode))
                    call0 (Qhandle_shift_selection);
                string++;
            }
        }

        // *** PARSE INTERACTIVE CODES ***

        // Build argument list by parsing interactive code letters
        ptrdiff_t nargs = 0;
        Lisp_Object *args = alloca (sizeof (Lisp_Object) * strlen (string));

        tem = string;
        while (*tem)
        {
            // Each code letter specifies how to read one argument
            switch (*tem)
            {
            case 'a':  // Function name
                args[nargs++] = Fcompleting_read (...);
                break;

            case 'b':  // Existing buffer name
                args[nargs++] = Fread_buffer (...);
                break;

            case 'B':  // Possibly nonexistent buffer
                args[nargs++] = Fread_buffer (...);
                break;

            case 'c':  // Character
                args[nargs++] = Fread_char (...);
                break;

            case 'C':  // Command name
                args[nargs++] = Fcompleting_read (...);
                break;

            case 'd':  // Point position (no I/O)
                args[nargs++] = make_fixnum (PT);
                break;

            case 'D':  // Directory name
                args[nargs++] = read_file_name (...);
                break;

            case 'e':  // Event
                args[nargs++] = extract_event (keys, nargs);
                break;

            case 'f':  // Existing file
                args[nargs++] = read_file_name (...);
                break;

            case 'F':  // Possibly nonexistent file
                args[nargs++] = read_file_name (...);
                break;

            case 'k':  // Key sequence
                args[nargs++] = Fread_key_sequence (...);
                break;

            case 'K':  // Key sequence (for remapping)
                args[nargs++] = Fread_key_sequence (...);
                break;

            case 'm':  // Mark position
                check_mark (false);
                args[nargs++] = make_fixnum (marker_position (
                    BVAR (current_buffer, mark)));
                break;

            case 'n':  // Number from minibuffer
                args[nargs++] = Fread_number (...);
                break;

            case 'N':  // Numeric prefix or number
                if (NILP (prefix_arg))
                    args[nargs++] = Fread_number (...);
                else
                    args[nargs++] = Fprefix_numeric_value (prefix_arg);
                break;

            case 'p':  // Prefix arg as number
                args[nargs++] = Fprefix_numeric_value (prefix_arg);
                break;

            case 'P':  // Prefix arg in raw form
                args[nargs++] = prefix_arg;
                break;

            case 'r':  // Region: point and mark
                check_mark (true);
                {
                    ptrdiff_t mark_pos = marker_position (
                        BVAR (current_buffer, mark));
                    ptrdiff_t point_pos = PT;

                    // Ensure smallest first
                    if (point_pos < mark_pos)
                    {
                        args[nargs++] = make_fixnum (point_pos);
                        args[nargs++] = make_fixnum (mark_pos);
                    }
                    else
                    {
                        args[nargs++] = make_fixnum (mark_pos);
                        args[nargs++] = make_fixnum (point_pos);
                    }
                }
                break;

            case 's':  // String from minibuffer
                args[nargs++] = Fread_string (...);
                break;

            case 'S':  // Symbol
                args[nargs++] = Fintern (Fread_string (...), Qnil);
                break;

            case 'v':  // Variable name
                args[nargs++] = Fread_variable (...);
                break;

            case 'x':  // Lisp expression (not evaluated)
                args[nargs++] = Fread_minibuffer (...);
                break;

            case 'X':  // Lisp expression (evaluated)
                args[nargs++] = Feval (Fread_minibuffer (...), Qt);
                break;

            case 'z':  // Coding system
                args[nargs++] = Fread_coding_system (...);
                break;

            case 'Z':  // Coding system or nil
                if (NILP (prefix_arg))
                    args[nargs++] = Qnil;
                else
                    args[nargs++] = Fread_coding_system (...);
                break;

            default:
                error ("Invalid interactive code: %c", *tem);
            }

            // Skip to next code (skip prompt string after newline)
            tem++;
            if (*tem == '\n')
            {
                tem++;
                // Skip prompt
                while (*tem && *tem != '\n')
                    tem++;
            }
        }

        // *** CALL FUNCTION WITH ARGS ***

        // Record in command history if needed
        if (!NILP (record_flag) || arg_from_tty)
            record_command (function, args, nargs);

        // Actually call the function
        Lisp_Object val = Ffuncall (nargs + 1,
                                   cons (function, args_to_list (args, nargs)));

        return unbind_to (speccount, val);
    }

    // *** PROCESS INTERACTIVE LIST ***

    else if (!NILP (spec_list))
    {
        // spec_list is a pre-computed list of arguments
        ptrdiff_t nargs = list_length (spec_list);
        Lisp_Object *args = alloca (sizeof (Lisp_Object) * (nargs + 1));

        args[0] = function;
        Lisp_Object tail = spec_list;
        for (ptrdiff_t i = 1; i <= nargs; i++, tail = XCDR (tail))
            args[i] = XCAR (tail);

        // Record and call
        if (!NILP (record_flag))
            record_command (function, args + 1, nargs);

        return unbind_to (speccount, Ffuncall (nargs + 1, args));
    }
}
```

**Interactive Code Summary:**

| Code | Meaning | I/O? | Example |
|------|---------|------|---------|
| `a` | Function name | Yes | `(interactive "aFunction: ")` |
| `b` | Existing buffer | Yes | `(interactive "bBuffer: ")` |
| `B` | Buffer (may not exist) | Yes | `(interactive "BCreate buffer: ")` |
| `c` | Character | Yes | `(interactive "cChar: ")` |
| `C` | Command name | Yes | `(interactive "CCommand: ")` |
| `d` | Point position | No | `(interactive "d")` |
| `D` | Directory name | Yes | `(interactive "DDirectory: ")` |
| `e` | Event | No | `(interactive "e")` |
| `f` | Existing file | Yes | `(interactive "fFile: ")` |
| `F` | File (may not exist) | Yes | `(interactive "FNew file: ")` |
| `k` | Key sequence | Yes | `(interactive "kKey: ")` |
| `m` | Mark position | No | `(interactive "m")` |
| `n` | Number | Yes | `(interactive "nNumber: ")` |
| `N` | Prefix or number | Maybe | `(interactive "NCount: ")` |
| `p` | Prefix as number | No | `(interactive "p")` |
| `P` | Prefix (raw) | No | `(interactive "P")` |
| `r` | Region (beg, end) | No | `(interactive "r")` |
| `s` | String | Yes | `(interactive "sString: ")` |
| `v` | Variable name | Yes | `(interactive "vVariable: ")` |
| `x` | Lisp expr (unevaled) | Yes | `(interactive "xEval: ")` |
| `X` | Lisp expr (evaled) | Yes | `(interactive "XEval: ")` |

**Special Prefixes:**
- `*` - Error if buffer is read-only
- `@` - Select window from mouse event
- `^` - Handle shift-selection

## Keyboard Macros

**Location:** `src/macros.c` (entire file)

Keyboard macros allow recording and replaying sequences of keystrokes.

### Recording Macros

**Start Recording:** `start-kbd-macro` - `src/macros.c:42-110`

```c
DEFUN ("start-kbd-macro", Fstart_kbd_macro, Sstart_kbd_macro, 1, 2, "P",
       doc: /* Record subsequent keyboard input, defining a keyboard macro... */)
  (Lisp_Object append, Lisp_Object no_exec)
{
    if (!NILP (KVAR (current_kboard, defining_kbd_macro)))
        error ("Already defining kbd macro");

    // Allocate macro buffer if needed
    if (!current_kboard->kbd_macro_buffer)
    {
        current_kboard->kbd_macro_buffer = xmalloc (30 * word_size);
        current_kboard->kbd_macro_bufsize = 30;
        current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_buffer;
        current_kboard->kbd_macro_end = current_kboard->kbd_macro_buffer;
    }

    update_mode_lines = 19;  // Update mode line display

    if (NILP (append))
    {
        // Start fresh macro
        current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_buffer;
        current_kboard->kbd_macro_end = current_kboard->kbd_macro_buffer;
        message1 ("Defining kbd macro...");
    }
    else
    {
        // Append to existing macro
        // Copy last-kbd-macro into buffer
        Lisp_Object last_macro = KVAR (current_kboard, Vlast_kbd_macro);
        ptrdiff_t len = CHECK_VECTOR_OR_STRING (last_macro);

        // Ensure buffer is large enough
        if (current_kboard->kbd_macro_bufsize - 30 < len)
            current_kboard->kbd_macro_buffer =
                xpalloc (...);

        // Copy events
        for (ptrdiff_t i = 0; i < len; i++)
        {
            Lisp_Object c = Faref (last_macro, make_fixnum (i));
            current_kboard->kbd_macro_buffer[i] = c;
        }

        current_kboard->kbd_macro_ptr =
            current_kboard->kbd_macro_buffer + len;
        current_kboard->kbd_macro_end =
            current_kboard->kbd_macro_buffer + len;

        message1 ("Appending to kbd macro...");

        // Re-execute the macro if requested
        if (NILP (no_exec))
            Fexecute_kbd_macro (last_macro, make_fixnum (1), Qnil);
    }

    // Mark as defining
    kset_defining_kbd_macro (current_kboard, Qt);

    return Qnil;
}
```

**During Recording:**

When `defining_kbd_macro` is non-nil, `command_loop_1` stores each command:

```c
// In command_loop_1():
if (!NILP (KVAR (current_kboard, defining_kbd_macro)))
{
    // Grow buffer if needed
    if (current_kboard->kbd_macro_ptr ==
        current_kboard->kbd_macro_buffer +
        current_kboard->kbd_macro_bufsize)
    {
        // Reallocate with more space
        ptrdiff_t size = current_kboard->kbd_macro_bufsize;
        current_kboard->kbd_macro_buffer =
            xpalloc (current_kboard->kbd_macro_buffer,
                    &current_kboard->kbd_macro_bufsize,
                    1, -1, sizeof *current_kboard->kbd_macro_buffer);

        // Update pointers
        current_kboard->kbd_macro_ptr =
            current_kboard->kbd_macro_buffer + size;
        current_kboard->kbd_macro_end =
            current_kboard->kbd_macro_buffer + size;
    }

    // Store the key
    *current_kboard->kbd_macro_ptr++ = key;
}
```

**End Recording:** `end-kbd-macro` - `src/macros.c:112-140`

```c
DEFUN ("end-kbd-macro", Fend_kbd_macro, Send_kbd_macro, 0, 2, "p",
       doc: /* Finish defining a keyboard macro... */)
  (Lisp_Object repeat, Lisp_Object loopfunc)
{
    if (NILP (KVAR (current_kboard, defining_kbd_macro)))
        error ("Not defining kbd macro");

    // Finalize the macro
    kset_defining_kbd_macro (current_kboard, Qnil);
    update_mode_lines = 20;

    // Move end pointer to exclude end-kbd-macro itself
    current_kboard->kbd_macro_end = current_kboard->kbd_macro_ptr;

    // Create Lisp vector from recorded events
    ptrdiff_t len = current_kboard->kbd_macro_end -
                    current_kboard->kbd_macro_buffer;
    Lisp_Object macro = Fmake_vector (make_fixnum (len), Qnil);

    for (ptrdiff_t i = 0; i < len; i++)
        ASET (macro, i, current_kboard->kbd_macro_buffer[i]);

    // Save as last-kbd-macro
    kset_last_kbd_macro (current_kboard, macro);

    message1 ("Keyboard macro defined");

    // Execute if repeat count given
    if (!NILP (repeat))
    {
        if (FIXNUMP (repeat))
            return Fexecute_kbd_macro (macro, repeat, loopfunc);
    }

    return Qnil;
}
```

### Executing Macros

**Execution:** `execute-kbd-macro` - `src/macros.c:240-330`

```c
DEFUN ("execute-kbd-macro", Fexecute_kbd_macro, Sexecute_kbd_macro,
       1, 3, 0,
       doc: /* Execute MACRO (a keyboard macro)... */)
  (Lisp_Object macro, Lisp_Object count, Lisp_Object loopfunc)
{
    if (NILP (count))
        count = make_fixnum (1);
    else
        CHECK_FIXNUM (count);

    if (!STRINGP (macro) && !VECTORP (macro))
        error ("Keyboard macro must be string or vector");

    // Save previous macro execution state
    Lisp_Object save_macro = Vexecuting_kbd_macro;
    EMACS_INT save_index = executing_kbd_macro_index;
    EMACS_INT save_iterations = executing_kbd_macro_iterations;

    // Set up new macro execution
    Vexecuting_kbd_macro = macro;
    executing_kbd_macro_index = 0;
    executing_kbd_macro_iterations = 0;

    // Execute COUNT times
    for (EMACS_INT i = 0; i < XFIXNUM (count); i++)
    {
        executing_kbd_macro_iterations = i;

        // Reset to beginning
        executing_kbd_macro_index = 0;

        // Command loop will read from Vexecuting_kbd_macro
        // instead of real input
        command_loop ();

        // Call loop function if provided
        if (!NILP (loopfunc))
            call0 (loopfunc);

        // Check for quit
        maybe_quit ();
    }

    // Restore previous state
    Vexecuting_kbd_macro = save_macro;
    executing_kbd_macro_index = save_index;
    executing_kbd_macro_iterations = save_iterations;

    return Qnil;
}
```

**Macro Playback:**

During macro execution, `read_char` checks:

```c
// In read_char():
if (!NILP (Vexecuting_kbd_macro) && !at_end_of_macro_p ())
{
    // Read from macro instead of real input
    Vlast_event_frame = internal_last_event_frame = Qmacro;

    c = Faref (Vexecuting_kbd_macro,
              make_int (executing_kbd_macro_index));

    // Handle meta modifier in string macros
    if (STRINGP (Vexecuting_kbd_macro)
        && (XFIXNAT (c) & 0x80) && (XFIXNAT (c) <= 0xff))
        XSETFASTINT (c, CHAR_META | (XFIXNAT (c) & ~0x80));

    executing_kbd_macro_index++;
    goto from_macro;
}
```

## Special Event Types

### 1. Mouse Events

**Structure:** Mouse events are lists:

```lisp
(EVENT-TYPE              ; e.g., mouse-1, mouse-2, mouse-3
 POSITION)               ; Position descriptor

POSITION:
(WINDOW                  ; Window of event
 AREA-OR-POS             ; Text area or (X . Y) in chars
 (X . Y)                 ; Pixel coordinates
 TIMESTAMP               ; Time in milliseconds
 OBJECT                  ; String/image/nil
 TEXT-POS                ; Buffer/string position
 (COL . ROW)             ; Column and row
 IMAGE)                  ; Image description if on image
```

**Example:**
```lisp
(mouse-1
 (#<window 3 on *scratch*>
  50                     ; Character position
  (30 . 100)            ; Pixel position
  123456                ; Timestamp
  nil                   ; Not on string/image
  50                    ; Buffer position
  (5 . 10)              ; Column 5, row 10
  nil))                 ; No image
```

**Mouse Event Processing:**

```c
// When terminal code detects mouse click:
void
make_mouse_event (int x, int y, int button, int modifiers)
{
    struct input_event event;

    event.kind = MOUSE_CLICK_EVENT;
    event.code = button;  // 0=mouse-1, 1=mouse-2, 2=mouse-3
    event.modifiers = modifiers;  // shift, control, meta, etc.
    event.x = x;
    event.y = y;
    event.frame_or_window = selected_frame;
    event.timestamp = current_time_ms ();
    event.arg = Qnil;

    kbd_buffer_store_event (&event);
}
```

**Converting to Lisp:**

```c
Lisp_Object
make_lispy_event (struct input_event *event)
{
    if (event->kind == MOUSE_CLICK_EVENT)
    {
        // Determine window and position
        Lisp_Object window = window_from_coordinates (
            XFRAME (event->frame_or_window),
            event->x, event->y, ...);

        Lisp_Object position = make_lispy_position (
            window, event->x, event->y, event->timestamp);

        // Build event list: (mouse-N POSITION)
        Lisp_Object head = intern (mouse_button_names[event->code]);
        head = apply_modifiers (event->modifiers, head);

        return list2 (head, position);
    }
    ...
}
```

### 2. Menu Events

**Menu Bar Event:**
```lisp
(menu-bar               ; Event type
 (FILE                 ; Menu name
  open-file))          ; Menu item
```

**Tool Bar Event:**
```lisp
(tool-bar               ; Event type
 save-buffer)          ; Tool item
```

**Processing:**

When user clicks menu bar, terminal code generates:

```c
event.kind = MENU_BAR_EVENT;
event.arg = menu_item_selection;  // Lisp list: (MENU ITEM)
event.frame_or_window = frame;
```

In `read_key_sequence`, menu events are expanded to key sequences:

```c
if (EVENT_KIND (key) == menu-bar)
{
    // Expand to equivalent key sequence
    // E.g., (menu-bar file open) → [menu-bar file open]
    key = expand_menu_event (key);
}
```

### 3. Drag and Drop Events

**Structure:**
```lisp
(drag-n-drop
 POSITION              ; Where files were dropped
 FILES)                ; List of file names
```

**Example:**
```lisp
(drag-n-drop
 (#<window 3 on *scratch*> ...)
 ("/home/user/file1.txt" "/home/user/file2.c"))
```

### 4. Touch Screen Events (Modern Emacs)

**Touch Begin:**
```lisp
(touchscreen-begin
 POSITION              ; Touch position
 TOOL-ID)              ; Unique touch identifier
```

**Touch End:**
```lisp
(touchscreen-end
 POSITION
 TOOL-ID)
```

## Multi-Keyboard Support

### KBOARD Management

**Initialization:** `src/keyboard.c:12900+`

```c
KBOARD *
allocate_kboard (Lisp_Object type)
{
    KBOARD *kb = xzalloc (sizeof *kb);

    // Initialize Lisp fields
    kset_default_minibuffer_frame (kb, Qnil);
    kset_last_command (kb, Qnil);
    kset_real_last_command (kb, Qnil);
    kset_keyboard_translate_table (kb, Qnil);
    kset_prefix_arg (kb, Qnil);
    kset_last_prefix_arg (kb, Qnil);
    kset_kbd_queue (kb, Qnil);
    kset_defining_kbd_macro (kb, Qnil);
    kset_last_kbd_macro (kb, Qnil);
    kset_system_key_alist (kb, Qnil);
    kset_window_system (kb, type);

    // Initialize keymaps
    kset_local_function_key_map (kb, Fmake_sparse_keymap (Qnil));
    Fset_keymap_parent (KVAR (kb, Vlocal_function_key_map),
                       Vfunction_key_map);

    kset_input_decode_map (kb, Fmake_sparse_keymap (Qnil));

    // Initialize echo state
    kset_echo_string (kb, Qnil);
    kset_echo_prompt (kb, Qnil);
    kb->echo_after_prompt = -1;

    // No macro buffer yet
    kb->kbd_macro_buffer = NULL;
    kb->kbd_macro_bufsize = 0;

    kb->reference_count = 0;
    kb->kbd_queue_has_data = 0;
    kb->immediate_echo = 0;

    // Add to global list
    kb->next_kboard = all_kboards;
    all_kboards = kb;

    return kb;
}
```

**Switching KBOARDs:**

```c
void
push_kboard (struct kboard *kb)
{
    // Save current kboard
    struct kboard_stack *p = xmalloc (sizeof *p);
    p->kboard = current_kboard;
    p->next = kboard_stack;
    kboard_stack = p;

    // Switch to new kboard
    current_kboard = kb;
}

void
pop_kboard (void)
{
    struct kboard_stack *p = kboard_stack;

    // Restore previous kboard
    current_kboard = p->kboard;
    kboard_stack = p->next;
    xfree (p);
}
```

**Single vs Any-KBOARD Mode:**

```c
// In read_key_sequence():

// When entering command execution:
temporarily_switch_to_single_kboard (SELECTED_FRAME ());

// This sets:
single_kboard = true;
current_kboard = FRAME_KBOARD (frame);

// In read_char():
if (single_kboard)
{
    // Only accept input from current_kboard
    if (event_kboard != current_kboard)
    {
        // Put event back in its KBOARD's queue
        KVAR (event_kboard, kbd_queue) =
            Fcons (event, KVAR (event_kboard, kbd_queue));
        event_kboard->kbd_queue_has_data = 1;

        // Continue waiting for current_kboard input
        goto retry;
    }
}
```

## Flow Diagram: Complete Event Processing

```
┌─────────────────────────────────────────────────────────────┐
│                    Hardware Event Occurs                     │
│                  (keyboard, mouse, timer, etc.)              │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│              Terminal-Specific Input Handler                 │
│   (term.c, xterm.c, w32term.c, etc.)                        │
│   - Reads from hardware/OS                                  │
│   - Creates struct input_event                              │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│               kbd_buffer_store_event()                       │
│   - Stores in circular buffer kbd_buffer[]                  │
│   - Sets input_pending flag                                 │
│   - Stores in KBOARD's queue if needed                      │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                      command_loop()                          │
│   └─> command_loop_2() [error handling]                     │
│       └─> command_loop_1() [main loop]                      │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                  read_key_sequence()                         │
│   - Builds complete key sequence                            │
│   - Handles prefix keys                                     │
│   - Applies translation maps                                │
└────────────────────────────┬────────────────────────────────┘
                             │
          ┌──────────────────┴──────────────────┐
          │                                     │
          ▼                                     ▼
┌──────────────────────┐          ┌──────────────────────┐
│     read_char()      │  (loop)  │  Translation Maps    │
│ - Gets single event  │──────────│  - input-decode-map  │
│ - From queue/macro   │          │  - function-key-map  │
│ - Handles reread     │          │  - key-translation   │
└──────────────────────┘          └──────────────────────┘
          │
          │
          ▼
┌─────────────────────────────────────────────────────────────┐
│              Keymap Lookup (access_keymap)                   │
│   Active keymap hierarchy:                                  │
│   1. overriding-terminal-local-map                          │
│   2. overriding-local-map                                   │
│   3. char property keymaps                                  │
│   4. minor-mode maps                                        │
│   5. local-map                                              │
│   6. global-map                                             │
└────────────────────────────┬────────────────────────────────┘
                             │
          ┌──────────────────┴──────────────────┐
          ▼                                     ▼
┌──────────────────────┐          ┌──────────────────────┐
│   Prefix Key Found   │          │   Command Found      │
│   - Continue reading │          │   - Complete!        │
│   - Update keymaps   │          └──────┬───────────────┘
└──────────────────────┘                 │
                                         ▼
                         ┌───────────────────────────────┐
                         │    Command Remapping          │
                         │    (command-remapping)        │
                         └───────────┬───────────────────┘
                                     │
                                     ▼
                         ┌───────────────────────────────┐
                         │    pre-command-hook           │
                         └───────────┬───────────────────┘
                                     │
                                     ▼
                         ┌───────────────────────────────┐
                         │    call-interactively         │
                         │    - Parse interactive spec   │
                         │    - Gather arguments         │
                         │    - Call function            │
                         └───────────┬───────────────────┘
                                     │
                                     ▼
                         ┌───────────────────────────────┐
                         │    Command Executes           │
                         │    (user's function)          │
                         └───────────┬───────────────────┘
                                     │
                                     ▼
                         ┌───────────────────────────────┐
                         │    post-command-hook          │
                         └───────────┬───────────────────┘
                                     │
                                     ▼
                         ┌───────────────────────────────┐
                         │    Cleanup & Continue         │
                         │    - Update mode line         │
                         │    - Auto-save check          │
                         │    - GC check                 │
                         │    - Loop back                │
                         └───────────────────────────────┘
```

## Key Functions Reference

### Event Loop

| Function | File | Lines | Purpose |
|----------|------|-------|---------|
| `command_loop` | keyboard.c | 1113-1148 | Top-level event loop |
| `command_loop_1` | keyboard.c | 1318-1700+ | Main command loop |
| `recursive_edit_1` | keyboard.c | 708-761 | Recursive editing |

### Event Reading

| Function | File | Lines | Purpose |
|----------|------|-------|---------|
| `read_char` | keyboard.c | 2534-3200+ | Read single event |
| `read_key_sequence` | keyboard.c | 10841-12500+ | Read complete key sequence |
| `kbd_buffer_store_event` | keyboard.c | ~2000 | Store event in buffer |

### Keymap Operations

| Function | File | Lines | Purpose |
|----------|------|-------|---------|
| `access_keymap` | keymap.c | 492-496 | Look up key in keymap |
| `access_keymap_1` | keymap.c | 327-489 | Core lookup algorithm |
| `get_keymap` | keymap.c | 192-240 | Validate/dereference keymap |
| `get_keyelt` | keymap.c | 679-750 | Trace indirect definitions |
| `map_keymap` | keymap.c | 584-600 | Iterate over keymap |

### Command Execution

| Function | File | Lines | Purpose |
|----------|------|-------|---------|
| `Fcall_interactively` | callint.c | 253-900+ | Interactive command execution |
| `Finteractive` | callint.c | 37-121 | Interactive spec declaration |

### Keyboard Macros

| Function | File | Lines | Purpose |
|----------|------|-------|---------|
| `Fstart_kbd_macro` | macros.c | 42-110 | Begin macro recording |
| `Fend_kbd_macro` | macros.c | 112-140 | End macro recording |
| `Fexecute_kbd_macro` | macros.c | 240-330 | Execute macro |

## Performance Considerations

### 1. Keymap Lookup Optimization

**Problem:** Looking up keys in deep keymap hierarchies can be slow.

**Solutions:**
- **Char-tables:** O(1) lookup for character keys without modifiers
- **Caching:** `where-is-cache` caches reverse lookups
- **Early termination:** Stop at first non-nil binding

### 2. Event Queue Management

**Circular Buffer:** Fixed-size `kbd_buffer[KBD_BUFFER_SIZE]` avoids allocation

**Trade-offs:**
- Fast: No allocation during event processing
- Limited: Can overflow if 4096 events not consumed

### 3. Translation Map Application

**Three-stage translation** (indec → fkey → keytran) requires careful state management:

```c
// Each stage maintains:
struct keyremap {
    Lisp_Object parent;  // Original map
    Lisp_Object map;     // Current position
    int start, end;      // Range being translated
};
```

**Replaying** after translation uses mock_input to avoid re-reading:
```c
if (t < mock_input)
    key = keybuf[t];  // Replay
else
    key = read_char ();  // Read new
```

## Common Patterns

### 1. Reading a Key Sequence

```c
Lisp_Object keybuf[READ_KEY_ELTS];
int i = read_key_sequence (keybuf, Qnil, false, true, true, false, false);

for (int j = 0; j < i; j++)
{
    Lisp_Object key = keybuf[j];
    // Process key
}
```

### 2. Looking Up a Key

```c
Lisp_Object binding = access_keymap (current_global_map, key, 1, 0, 1);

if (NILP (binding))
    // Unbound
else if (KEYMAPP (binding))
    // Prefix key
else
    // Command
```

### 3. Defining a Key

```c
// C code:
initial_define_lispy_key (keymap, "C-x C-f", "find-file");

// Expands to:
store_in_keymap (keymap,
                intern_c_string ("C-x C-f"),
                intern_c_string ("find-file"),
                false);
```

### 4. Creating Interactive Commands

```lisp
(defun my-command (start end)
  "Do something with region."
  (interactive "r")  ; Two args: region start and end
  (message "Region: %d to %d" start end))
```

Equivalent C registration:
```c
DEFUN ("my-command", Fmy_command, Smy_command, 2, 2, "r",
       doc: /* Do something with region. */)
  (Lisp_Object start, Lisp_Object end)
{
    message ("Region: %d to %d", XFIXNUM (start), XFIXNUM (end));
    return Qnil;
}
```

## Debugging Tools

### 1. View Lossage

**Command:** `view-lossage` (C-h l)

Shows last 300 (or configured) input events:

```c
// Circular buffer of recent keys
static Lisp_Object recent_keys;
static int recent_keys_index;

void
record_char (Lisp_Object c)
{
    total_keys += total_keys < lossage_limit;
    ASET (recent_keys, recent_keys_index, c);
    if (++recent_keys_index >= lossage_limit)
        recent_keys_index = 0;
}
```

### 2. Describe Key

**Command:** `describe-key` (C-h k)

Shows what command a key sequence runs:

```lisp
(defun describe-key (key)
  (interactive "kDescribe key: ")
  (let ((binding (key-binding key)))
    (if binding
        (describe-function binding)
      (message "%s is undefined" (key-description key)))))
```

### 3. Where Is

**Command:** `where-is` (C-h w)

Shows all key bindings for a command:

```c
// Uses reverse map cache: where_is_cache
// Maps commands → key sequences
```

### 4. Event Debugging

**Variables:**
- `last-command-event` - Last key that invoked command
- `this-command-keys` - Full key sequence
- `this-command-keys-vector` - Vector form

**Functions:**
- `recent-keys` - Get recent key vector
- `this-single-command-keys` - Keys of current command only

## Conclusion

The keyboard and event handling system is a marvel of careful engineering, managing:

- **Multiple input sources** - keyboard, mouse, timers, menus, macros
- **Complex state** - multi-keyboard support, recursive editing, macro recording
- **Efficient lookup** - keymap hierarchy, translation maps, caching
- **Interactive execution** - argument gathering, hooks, command history

Understanding this system is crucial for:
- Implementing new input methods
- Adding special key handling
- Debugging input-related issues
- Optimizing command execution
- Extending Emacs's interactivity

The clean separation between event reading, key sequence processing, keymap lookup, and command execution makes the system remarkably extensible despite its complexity.
