# Emacs Terminology Glossary

A comprehensive reference of Emacs terminology and concepts, organized alphabetically with category tags.

**Categories:**
- `[Core]` - Core Emacs concepts
- `[Lisp]` - Emacs Lisp concepts
- `[Data]` - Data structures
- `[Display]` - Display system
- `[System]` - System and I/O concepts
- `[Abbrev]` - Abbreviations and jargon

---

## A

### Abbrev `[Core]` `[System]`
**Definition:** A shorthand text expansion system where a short word is automatically replaced with a longer phrase when typed.

**Context:** Used in text editing for inserting frequently-used text. Abbrevs can be mode-specific or global.

**Related Terms:** Auto-insert, Template, Skeleton

**Documentation:** See `doc/lispref/abbrevs.texi`

---

### Abstraction Barrier `[Lisp]`
**Definition:** A design principle separating interface from implementation, allowing internal changes without affecting external code.

**Context:** Used in Emacs Lisp API design to maintain compatibility across versions.

**Related Terms:** API, Interface, Encapsulation

---

### Active Keymap `[Core]`
**Definition:** A keymap currently in effect for key lookup, determined by the current major mode, active minor modes, and local keymaps.

**Context:** Multiple keymaps can be active simultaneously with precedence rules determining which binding applies.

**Related Terms:** Keymap, Key Sequence, Key Binding

**Documentation:** See `doc/lispref/keymaps.texi`

---

### Active Region `[Core]`
**Definition:** The region between point and mark when the mark is active, typically highlighted visually.

**Context:** Many commands operate on the active region. Transient Mark Mode controls region visibility.

**Related Terms:** Region, Mark, Point, Transient Mark Mode

**Documentation:** See `doc/lispref/markers.texi`

---

### Advice `[System]`
**Definition:** A mechanism to modify the behavior of existing functions by adding code before, after, or around them without changing their definition.

**Context:** Used for customization, debugging, and extending functionality. Modern advice uses `advice-add`.

**Related Terms:** Advice Combinator, nadvice, Defadvice (deprecated)

**Documentation:** See `doc/lispref/functions.texi`

---

### Advice Combinator `[Lisp]`
**Definition:** Functions like `:before`, `:after`, `:around`, `:override` that specify how advice is combined with the original function.

**Context:** Determines the execution order and relationship between advised function and advice.

**Related Terms:** Advice, advice-add, Function

**Documentation:** See `doc/lispref/functions.texi`

---

### After-Change Function `[System]`
**Definition:** A function called automatically after text is modified in a buffer, used to track or respond to changes.

**Context:** Added to `after-change-functions` hook. Receives start, end, and old length as arguments.

**Related Terms:** Before-Change Function, Hook, Modification

**Documentation:** See `doc/lispref/text.texi`

---

### After String `[Display]`
**Definition:** Text associated with an overlay or text property that is displayed after the overlay's region.

**Context:** Used for adding annotations, inline images, or supplementary text without modifying buffer contents.

**Related Terms:** Before String, Overlay, Display Property

**Documentation:** See `doc/lispref/display.texi`

---

### ANSI Escape Sequence `[Display]`
**Definition:** Terminal control codes for formatting text output, including colors, cursor movement, and text attributes.

**Context:** Processed by `ansi-color.el` in compilation buffers, shell modes, and other terminal output.

**Related Terms:** ANSI Color, Terminal, TTY

---

### Alist `[Data]`
**Definition:** Association List - a list of cons cells where each car is a key and each cdr is the associated value.

**Context:** Common data structure for key-value mappings in Emacs Lisp. Less efficient than hash tables for large datasets.

**Related Terms:** Plist, Hash Table, Cons Cell

**Documentation:** See `doc/lispref/lists.texi`

---

### Apropos `[Core]`
**Definition:** A search system for finding commands, variables, and functions matching a pattern or keyword.

**Context:** Invoked with `M-x apropos`, `apropos-command`, etc. for discovering functionality.

**Related Terms:** Help System, Documentation, Describe

---

### Arc Mode `[Core]`
**Definition:** A major mode for viewing and editing archive files (ZIP, TAR, etc.) as if they were directories.

**Context:** Allows browsing and modifying archive contents without external tools.

**Related Terms:** Major Mode, Dired, Archive

---

### Argument List `[Lisp]`
**Definition:** The list of parameters accepted by a function, specified in its definition.

**Context:** Can include required, optional (`&optional`), rest (`&rest`), and keyword (`&key` in CL) arguments.

**Related Terms:** Lambda List, Parameter, Function

**Documentation:** See `doc/lispref/functions.texi`

---

### ASCII `[System]`
**Definition:** American Standard Code for Information Interchange - a 7-bit character encoding standard.

**Context:** Subset of most character encodings used in Emacs. ASCII characters are bytes 0-127.

**Related Terms:** Character Set, Coding System, UTF-8, Unibyte

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Async Process `[System]`
**Definition:** A subprocess that runs concurrently with Emacs, allowing non-blocking I/O operations.

**Context:** Created with `start-process`. Output handled via process filters, completion via sentinels.

**Related Terms:** Process, Filter, Sentinel, Subprocess

**Documentation:** See `doc/lispref/processes.texi`

---

### Atom `[Lisp]`
**Definition:** Any Lisp object that is not a cons cell - includes symbols, numbers, strings, vectors, etc.

**Context:** Opposite of list/cons. Used in conditional logic and type checking.

**Related Terms:** Cons Cell, List, Symbol

**Documentation:** See `doc/lispref/lists.texi`

---

### Auto-Composition `[Display]`
**Definition:** Automatic character composition for complex scripts (Arabic, Indic, etc.) requiring glyph shaping.

**Context:** Controlled by composition functions and font backend. Happens during redisplay.

**Related Terms:** Composition, Font, Glyph, Complex Script

**Documentation:** See `doc/lispref/display.texi`

---

### Auto-Fill Mode `[Core]`
**Definition:** A minor mode that automatically breaks lines at the fill column while typing.

**Context:** Commonly used for writing text. Fill column defaults to 70 characters.

**Related Terms:** Fill Column, Minor Mode, Line Wrapping

---

### Auto-Revert Mode `[Core]`
**Definition:** A minor mode that automatically reverts a buffer when its file changes on disk.

**Context:** Useful for log files and files modified by external programs.

**Related Terms:** Revert Buffer, File Notification, Minor Mode

---

### Auto-Save `[Core]`
**Definition:** Automatic periodic saving of buffer contents to a backup file (typically `#filename#`).

**Context:** Protection against crashes and data loss. Controlled by `auto-save-mode`.

**Related Terms:** Backup File, Crash Recovery, Auto-Save File

**Documentation:** See `doc/lispref/backups.texi`

---

### Autoload `[Lisp]`
**Definition:** A mechanism to defer loading a function's definition until it's first called, reducing startup time.

**Context:** Declared with `;;;###autoload` magic comment or `autoload` function. Essential for package management.

**Related Terms:** Feature, Provide, Require, Lazy Loading

**Documentation:** See `doc/lispref/loading.texi`

---

### Autoload Cookie `[Lisp]`
**Definition:** The magic comment `;;;###autoload` that marks definitions for automatic autoload generation.

**Context:** Processed during package compilation to create autoload files.

**Related Terms:** Autoload, Package, Loaddefs

---

## B

### Backtrace `[Lisp]`
**Definition:** A stack trace showing the sequence of function calls leading to an error or debugger invocation.

**Context:** Displayed in `*Backtrace*` buffer during debugging. Shows call chain and arguments.

**Related Terms:** Debugger, Stack Frame, Call Stack, Edebug

**Documentation:** See `doc/lispref/debugging.texi`

---

### Backup File `[Core]`
**Definition:** A copy of a file made before saving, typically named with a tilde suffix (`filename~`).

**Context:** Controlled by `make-backup-files`. Multiple backup versions can be kept.

**Related Terms:** Auto-Save, Version Control, Numbered Backup

**Documentation:** See `doc/lispref/backups.texi`

---

### Balanced Expression `[Lisp]`
**Definition:** An s-expression with properly matched delimiters (parentheses, brackets, quotes).

**Context:** Required for valid Lisp code. Emacs provides commands for navigating and manipulating balanced expressions.

**Related Terms:** S-expression, Sexp, Paren Matching

---

### Before-Change Function `[System]`
**Definition:** A function called before text is modified in a buffer, receiving the region about to be changed.

**Context:** Added to `before-change-functions` hook. Used for validation or preparation.

**Related Terms:** After-Change Function, Hook, Modification

**Documentation:** See `doc/lispref/text.texi`

---

### Before String `[Display]`
**Definition:** Text associated with an overlay or text property displayed before the overlay's region.

**Context:** Used for annotations, line numbers, or icons without modifying buffer text.

**Related Terms:** After String, Overlay, Display Property

**Documentation:** See `doc/lispref/display.texi`

---

### BEG / BEGV `[Data]`
**Definition:** Buffer constants - BEG is position 1 (buffer beginning), BEGV is beginning of accessible region (after narrowing).

**Context:** C macros used throughout Emacs internals for buffer boundary checks.

**Related Terms:** Point, Z, ZV, Narrowing, Gap Buffer

**Source:** See `src/buffer.h`

---

### Bidirectional Text `[Display]`
**Definition:** Text containing both left-to-right (LTR) and right-to-left (RTL) scripts like Arabic or Hebrew.

**Context:** Emacs implements the Unicode Bidirectional Algorithm for correct display.

**Related Terms:** BIDI, RTL, LTR, Unicode

**Documentation:** See `doc/lispref/display.texi`

---

### Binding `[Lisp]`
**Definition:** The association between a variable name and its value, or a key sequence and its command.

**Context:** Can be global, buffer-local, let-bound, or dynamically scoped.

**Related Terms:** Variable, Key Binding, Scope, Environment

**Documentation:** See `doc/lispref/variables.texi`

---

### Bitmap `[Display]`
**Definition:** A small monochrome image used in the fringe for indicators like continuation, truncation, or debugging marks.

**Context:** Defined with `define-fringe-bitmap`. System bitmaps exist for common indicators.

**Related Terms:** Fringe, Glyph, Icon, Indicator

**Documentation:** See `doc/lispref/display.texi`

---

### Bobp / Bolp / Eobp / Eolp `[Core]`
**Definition:** Predicates testing if point is at Beginning Of Buffer, Beginning Of Line, End Of Buffer, or End Of Line.

**Context:** Common in motion and editing commands to test boundary conditions.

**Related Terms:** Point, Buffer Position, Predicate

**Documentation:** See `doc/lispref/positions.texi`

---

### Bool Vector `[Data]`
**Definition:** A compact array of boolean values, stored as bits rather than full Lisp objects.

**Context:** Memory-efficient for large boolean arrays. Used in char-tables and other internal structures.

**Related Terms:** Vector, Bit Array, Char Table

**Documentation:** See `doc/lispref/sequences.texi`

---

### Buffer `[Core]`
**Definition:** A Lisp object containing editable text, either associated with a file or existing only in memory.

**Context:** Fundamental to Emacs editing. Each buffer has its own point, mark, local variables, and major mode.

**Related Terms:** Current Buffer, Window, Point, Mode

**Documentation:** See `doc/lispref/buffers.texi`

---

### Buffer-Local Variable `[Lisp]`
**Definition:** A variable that can have different values in different buffers, overriding its global value.

**Context:** Set with `make-local-variable` or `setq-local`. Major modes typically set buffer-local variables.

**Related Terms:** Local Variable, Global Variable, Buffer

**Documentation:** See `doc/lispref/variables.texi`

---

### Buffer Gap `[Data]`
**Definition:** An empty space in a buffer's text storage that allows efficient insertion and deletion at point.

**Context:** Part of the gap buffer data structure. Moves to follow editing operations.

**Related Terms:** Gap Buffer, GPT, Point, Insertion

**Source:** See `src/buffer.h`

**Documentation:** See `doc/lispref/buffers.texi`

---

### Buffer List `[Core]`
**Definition:** The ordered collection of all live buffers, with most recently selected buffers first.

**Context:** Accessed via `buffer-list`. Modified by buffer selection and killing.

**Related Terms:** Buffer, Buried Buffer, Buffer Menu

**Documentation:** See `doc/lispref/buffers.texi`

---

### Buffer-Undo-List `[Core]`
**Definition:** A list recording changes to a buffer to enable undo operations.

**Context:** Contains entries for insertions, deletions, and property changes. Can be truncated or disabled.

**Related Terms:** Undo, Redo, Change List

**Documentation:** See `doc/lispref/text.texi`

---

### Buried Buffer `[Core]`
**Definition:** A buffer moved to the end of the buffer list, making it less likely to be displayed.

**Context:** Created by `bury-buffer`. Keeps buffers alive without showing them prominently.

**Related Terms:** Buffer List, Hidden Buffer, Buffer Switching

---

### Byte Code `[Lisp]`
**Definition:** A compact intermediate representation of compiled Lisp code executed by the byte-code interpreter.

**Context:** Produced by the byte compiler. Faster than interpreted Lisp but slower than native code.

**Related Terms:** Byte Compiler, .elc File, Native Compilation, LAP

**Documentation:** See `doc/lispref/compile.texi`

---

### Byte Compiler `[Lisp]`
**Definition:** The compiler that translates Emacs Lisp source code into byte code.

**Context:** Invoked via `byte-compile-file` or during package installation. Produces `.elc` files.

**Related Terms:** Byte Code, Compilation, .elc File, Native Compilation

**Documentation:** See `doc/lispref/compile.texi`

---

### Byte Position `[Data]`
**Definition:** A position in a buffer measured in bytes rather than characters, important for multibyte text.

**Context:** Used internally. Most Lisp code uses character positions.

**Related Terms:** Character Position, Multibyte, Point, Marker

**Documentation:** See `doc/lispref/positions.texi`

---

## C

### C-h `[Abbrev]`
**Definition:** The help prefix key in Emacs, used to access help commands.

**Context:** `C-h k` describes key, `C-h f` describes function, `C-h v` describes variable, etc.

**Related Terms:** Help, Describe, Apropos

---

### C Source `[System]`
**Definition:** The C language implementation of Emacs core, providing primitives and performance-critical functions.

**Context:** Located in `src/` directory. Provides DEFUN primitives callable from Lisp.

**Related Terms:** Primitive, DEFUN, Subr, Built-in

**Source:** See `src/` directory

---

### Call Stack `[Lisp]`
**Definition:** The runtime stack of function invocations, showing which functions called which.

**Context:** Visible in backtrace during debugging. Limited by `max-lisp-eval-depth`.

**Related Terms:** Backtrace, Stack Frame, Recursion

**Documentation:** See `doc/lispref/debugging.texi`

---

### Canonical Character `[System]`
**Definition:** The normalized form of a character used for case-insensitive comparisons and operations.

**Context:** Handles case folding and equivalence classes for various character sets.

**Related Terms:** Case Table, Character Folding, Normalization

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Case Table `[Data]`
**Definition:** A char-table defining uppercase/lowercase relationships and case folding rules for characters.

**Context:** Language-specific case tables handle different alphabets. Affects case conversion and searching.

**Related Terms:** Char Table, Case Folding, Syntax Table

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Category Table `[Data]`
**Definition:** A char-table assigning categories to characters, used by regular expressions for character class matching.

**Context:** Categories are single-character symbols. Used in `\cX` regexp syntax.

**Related Terms:** Char Table, Regexp, Character Class

**Documentation:** See `doc/lispref/syntax.texi`

---

### CEDET `[Abbrev]`
**Definition:** Collection of Emacs Development Environment Tools - an infrastructure for parsing and analyzing code.

**Context:** Provides semantic analysis, project management, and code navigation. Predecessor to modern LSP.

**Related Terms:** Semantic, EDE, LSP, IDE

**Documentation:** See `doc/misc/` for CEDET manuals

---

### Change Group `[System]`
**Definition:** A mechanism to group multiple buffer modifications into a single undoable unit.

**Context:** Used by `atomic-change-group`. All changes succeed together or are undone together.

**Related Terms:** Undo, Transaction, Atomic Operation

**Documentation:** See `doc/lispref/text.texi`

---

### Character `[Data]`
**Definition:** A Lisp integer representing a Unicode code point, the basic unit of text in Emacs.

**Context:** Emacs 23+ uses Unicode internally. Characters range from 0 to #x3FFFFF.

**Related Terms:** Character Code, Unicode, Multibyte, Codepoint

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Character Class `[Lisp]`
**Definition:** A regexp construct matching any character in a specified set, enclosed in `[...]`.

**Context:** Supports ranges `[a-z]`, negation `[^...]`, and predefined classes `[:alpha:]`.

**Related Terms:** Regexp, Pattern Matching, Syntax Class

**Documentation:** See `doc/lispref/searching.texi`

---

### Character Code `[Data]`
**Definition:** The numeric value of a character, typically a Unicode code point.

**Context:** Obtained with `char-code`. Character literals in Lisp use `?` syntax: `?A` = 65.

**Related Terms:** Character, Unicode, Code Point

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Character Position `[Data]`
**Definition:** A position in a buffer measured in characters, independent of multibyte encoding.

**Context:** Standard for Lisp programming. May differ from byte position in multibyte buffers.

**Related Terms:** Byte Position, Point, Marker, Position

**Documentation:** See `doc/lispref/positions.texi`

---

### Charset `[System]`
**Definition:** A character set defining a collection of characters with numeric codes, like ASCII, ISO-8859-1, or Unicode.

**Context:** Emacs supports multiple charsets but uses Unicode as the universal internal representation.

**Related Terms:** Coding System, Character Set, Unicode, Multibyte

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Char-Table `[Data]`
**Definition:** A specialized array indexed by character codes, used for character properties and mappings.

**Context:** Used for syntax tables, case tables, category tables, and display tables. Very memory-efficient.

**Related Terms:** Syntax Table, Case Table, Display Table, Array

**Documentation:** See `doc/lispref/sequences.texi`

---

### Circular List `[Data]`
**Definition:** A list structure containing a cycle, where a cons cell's cdr eventually points back to an earlier cell.

**Context:** Can cause infinite loops. Detected by `circular-list` error or print-circle.

**Related Terms:** List, Cons Cell, Print Circle

**Documentation:** See `doc/lispref/lists.texi`

---

### CL (Common Lisp) `[Lisp]`
**Definition:** Common Lisp - a Lisp dialect whose features are partially available in Emacs Lisp via cl-lib.

**Context:** `cl-lib` provides loop, destructuring, structures, and other CL features for Emacs Lisp.

**Related Terms:** cl-lib, CLOS, Lisp Dialect

**Documentation:** See `doc/misc/cl.texi`

---

### Closure `[Lisp]`
**Definition:** A function that captures and retains access to variables from its defining lexical environment.

**Context:** Enabled by lexical binding. Allows functional programming patterns like partial application.

**Related Terms:** Lexical Binding, Lambda, Anonymous Function, Environment

**Documentation:** See `doc/lispref/variables.texi`

---

### Coding System `[System]`
**Definition:** A specification for encoding and decoding text between internal Unicode and external byte representations.

**Context:** Examples: utf-8, iso-8859-1, euc-jp. Automatically detected or explicitly set for files and processes.

**Related Terms:** Character Encoding, Charset, EOL Convention, Multibyte

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Column `[Core]`
**Definition:** A horizontal position in a line, measured in characters or visual columns.

**Context:** `current-column` returns point's column. Tab characters and variable-width fonts complicate column calculation.

**Related Terms:** Visual Column, Goal Column, Indentation

**Documentation:** See `doc/lispref/positions.texi`

---

### Command `[Core]`
**Definition:** An interactive function that can be invoked via `M-x` or a key binding.

**Context:** Declared with `(interactive ...)` spec. Distinguishes user-callable from internal functions.

**Related Terms:** Interactive, Key Binding, M-x

**Documentation:** See `doc/lispref/commands.texi`

---

### Command Loop `[System]`
**Definition:** The main loop that reads user input, executes commands, and updates the display.

**Context:** Handles keyboard and mouse events, manages keymaps, and triggers redisplay.

**Related Terms:** Event Loop, Redisplay, Key Sequence

**Documentation:** See `doc/lispref/commands.texi`

---

### Comment Syntax `[Lisp]`
**Definition:** Syntax rules defining how comments are written in a programming language, stored in the syntax table.

**Context:** Emacs supports multiple comment styles: line comments, block comments, nested comments.

**Related Terms:** Syntax Table, Comment Delimiters, Syntax Class

**Documentation:** See `doc/lispref/syntax.texi`

---

### Compilation `[Lisp]`
**Definition:** The process of translating Emacs Lisp source code into byte code or native code for improved performance.

**Context:** Byte compilation produces `.elc` files. Native compilation produces `.eln` files.

**Related Terms:** Byte Compiler, Native Compilation, .elc, .eln

**Documentation:** See `doc/lispref/compile.texi`

---

### Composition `[Display]`
**Definition:** Combining multiple characters into a single glyph for display, used in complex scripts and emoji.

**Context:** Automatic for complex scripts (Arabic, Devanagari). Can be manual via composition functions.

**Related Terms:** Glyph, Font, Complex Script, Auto-Composition

**Documentation:** See `doc/lispref/display.texi`

---

### Cons Cell `[Data]`
**Definition:** The fundamental building block of Lisp lists - a pair of two values (car and cdr).

**Context:** Created with `cons`. Lists are chains of cons cells. Dotted pairs have non-nil cdr.

**Related Terms:** List, Car, Cdr, Pair

**Documentation:** See `doc/lispref/lists.texi`

---

### Continuation Line `[Display]`
**Definition:** A logical line that spans multiple screen lines due to line wrapping.

**Context:** Indicated in the fringe. Controlled by truncate-lines variable.

**Related Terms:** Line Wrapping, Truncation, Visual Line, Fringe

**Documentation:** See `doc/lispref/display.texi`

---

### Current Buffer `[Core]`
**Definition:** The buffer that editing commands implicitly operate on.

**Context:** Set by `set-buffer` or `with-current-buffer`. Often different from the displayed buffer.

**Related Terms:** Buffer, Selected Window, set-buffer

**Documentation:** See `doc/lispref/buffers.texi`

---

### Customization `[System]`
**Definition:** The Emacs system for declaring user options with types, defaults, and interactive editing.

**Context:** Defined with `defcustom`. Edited via Customize interface (`M-x customize`).

**Related Terms:** Defcustom, Custom, User Option, Variable

**Documentation:** See `doc/lispref/customize.texi`

---

### Custom Theme `[System]`
**Definition:** A coordinated set of face and variable customizations that can be loaded as a unit.

**Context:** Themes provide consistent color schemes and UI appearance. Multiple themes can be active.

**Related Terms:** Face, Theme, Customization, Appearance

**Documentation:** See `doc/lispref/customize.texi`

---

## D

### Daemon Mode `[System]`
**Definition:** Running Emacs as a background server process that clients can connect to.

**Context:** Started with `emacs --daemon`. Clients connect via `emacsclient`.

**Related Terms:** Server, Client, Background Process

**Documentation:** See `doc/emacs/` manual

---

### Debug On Error `[Lisp]`
**Definition:** A variable that, when non-nil, invokes the debugger automatically when an error occurs.

**Context:** Essential for debugging. Set with `M-x toggle-debug-on-error`.

**Related Terms:** Debugger, Error, Backtrace, Debugging

**Documentation:** See `doc/lispref/debugging.texi`

---

### Debugger `[Lisp]`
**Definition:** An interactive tool for inspecting Lisp execution, examining the call stack, and stepping through code.

**Context:** Invoked by errors (when `debug-on-error` is set), explicitly, or via breakpoints.

**Related Terms:** Edebug, Backtrace, Breakpoint, Debug On Error

**Documentation:** See `doc/lispref/debugging.texi`

---

### Defadvice `[Lisp]` (Deprecated)
**Definition:** Old advice system for modifying function behavior, superseded by the new advice system.

**Context:** Use `advice-add` instead. Defadvice is retained for compatibility.

**Related Terms:** Advice, advice-add, nadvice

**Documentation:** See `doc/lispref/functions.texi`

---

### Defconst `[Lisp]`
**Definition:** Defines a constant variable with documentation, though technically still mutable in Emacs Lisp.

**Context:** Convention for values that shouldn't change. Sets a special variable like defvar.

**Related Terms:** Defvar, Variable, Constant, Special Variable

**Documentation:** See `doc/lispref/variables.texi`

---

### Defcustom `[Lisp]`
**Definition:** Defines a customizable user option with type, default, and customize interface support.

**Context:** Preferred over defvar for user-facing configuration. Provides interactive editing.

**Related Terms:** Customization, User Option, Defvar, Custom

**Documentation:** See `doc/lispref/customize.texi`

---

### Defface `[Lisp]`
**Definition:** Defines a face with default attributes and customization support.

**Context:** Faces control text appearance. Defface allows theme and user customization.

**Related Terms:** Face, Customization, Theme, Display

**Documentation:** See `doc/lispref/display.texi`

---

### Defmacro `[Lisp]`
**Definition:** Defines a Lisp macro that transforms code at compile time.

**Context:** Macros receive unevaluated arguments and return code to be evaluated. Powerful but complex.

**Related Terms:** Macro, Macro Expansion, Backquote, Compile Time

**Documentation:** See `doc/lispref/macros.texi`

---

### Defsubst `[Lisp]`
**Definition:** Defines an inline function that the compiler substitutes directly at call sites for performance.

**Context:** Like C inline functions. Use for tiny, frequently-called functions.

**Related Terms:** Function, Inline, Compilation, Optimization

**Documentation:** See `doc/lispref/functions.texi`

---

### DEFUN `[Lisp]` `[System]`
**Definition:** A C macro for defining primitives (built-in functions) callable from Lisp.

**Context:** Used in Emacs C source code. Specifies Lisp name, C name, arguments, and documentation.

**Related Terms:** Primitive, Subr, Built-in Function, C Source

**Source:** See `src/lisp.h`

---

### Defun `[Abbrev]`
**Definition:** Short for "define function" - refers to function definitions or top-level forms.

**Context:** Also refers to the beginning of a top-level definition for navigation commands.

**Related Terms:** Function, Beginning-of-Defun, End-of-Defun

---

### Defvar `[Lisp]`
**Definition:** Defines a special (dynamically scoped) variable with optional initial value and documentation.

**Context:** Only sets value if variable is void. Declares dynamic scope even under lexical binding.

**Related Terms:** Variable, Special Variable, Dynamic Binding, Defconst

**Documentation:** See `doc/lispref/variables.texi`

---

### Defvaralias `[Lisp]`
**Definition:** Makes one variable an alias for another, so they share the same value.

**Context:** Used for renaming variables while maintaining backward compatibility.

**Related Terms:** Alias, Variable, Compatibility

**Documentation:** See `doc/lispref/variables.texi`

---

### Describe `[Core]`
**Definition:** Help system commands that display documentation for functions, variables, keys, modes, etc.

**Context:** `C-h f` (describe-function), `C-h v` (describe-variable), `C-h k` (describe-key).

**Related Terms:** Help, Documentation, Apropos, Info

---

### Display Engine `[Display]`
**Definition:** The subsystem responsible for converting buffer contents into screen pixels.

**Context:** Handles text rendering, faces, overlays, images, and all visual presentation.

**Related Terms:** Redisplay, Glyph Matrix, Font Backend, Rendering

**Source:** See `src/xdisp.c`

---

### Display Property `[Display]`
**Definition:** A text property or overlay property that controls how text is displayed.

**Context:** Can insert images, change text appearance, add margins, or hide text.

**Related Terms:** Text Property, Overlay, Image, Invisible Text

**Documentation:** See `doc/lispref/display.texi`

---

### Display Spec `[Display]`
**Definition:** A specification for the display property describing how to render text or insert non-text elements.

**Context:** Complex format supporting images, space specs, margins, and composed text.

**Related Terms:** Display Property, Image Spec, Space Spec

**Documentation:** See `doc/lispref/display.texi`

---

### Display Table `[Data]`
**Definition:** A char-table specifying how to display each character, supporting character substitution.

**Context:** Can display non-printing characters, control characters, or alternative glyphs.

**Related Terms:** Char Table, Glyph, Character Display

**Documentation:** See `doc/lispref/display.texi`

---

### Dotted Pair `[Data]`
**Definition:** A cons cell written as `(a . b)` where the cdr is not a list.

**Context:** Differs from proper list. Used for alist entries and simple key-value pairs.

**Related Terms:** Cons Cell, Pair, Alist, Improper List

**Documentation:** See `doc/lispref/lists.texi`

---

### DTRT `[Abbrev]`
**Definition:** "Do The Right Thing" - Emacs philosophy of automatic, intelligent default behavior.

**Context:** Features that automatically adapt to context without user configuration.

**Related Terms:** DWIM, Smart Defaults, Heuristics

---

### DWIM `[Abbrev]`
**Definition:** "Do What I Mean" - commands that infer user intention from context.

**Context:** Example: `comment-dwim` comments or uncomments depending on region state.

**Related Terms:** DTRT, Context-Aware, Smart Command

---

### Dynamic Binding `[Lisp]`
**Definition:** Variable scoping where bindings are looked up in the runtime call stack rather than lexical environment.

**Context:** Emacs Lisp's traditional scoping. Special variables use dynamic binding even under lexical-binding mode.

**Related Terms:** Lexical Binding, Scope, Special Variable, Environment

**Documentation:** See `doc/lispref/variables.texi`

---

### Dynamic Module `[System]`
**Definition:** A shared library that extends Emacs with native code, loaded at runtime.

**Context:** Provides high-performance extensions in C or other languages. Requires module support enabled.

**Related Terms:** FFI, Native Code, Shared Library, Plugin

**Documentation:** See `doc/lispref/` manual

---

## E

### Echo Area `[Core]`
**Definition:** The single-line region at the bottom of a frame for displaying messages and minibuffer input.

**Context:** Shares space with minibuffer. Shows command feedback, errors, and prompts.

**Related Terms:** Minibuffer, Mode Line, Message, Frame

**Documentation:** See `doc/lispref/display.texi`

---

### Edebug `[Lisp]`
**Definition:** A source-level debugger for Emacs Lisp supporting breakpoints, stepping, and expression evaluation.

**Context:** Instruments functions for debugging. More powerful than basic debugger.

**Related Terms:** Debugger, Breakpoint, Step, Debug

**Documentation:** See `doc/lispref/edebug.texi`

---

### Electric `[Core]`
**Definition:** Automatic behavior triggered by certain characters, like auto-indentation or paren insertion.

**Context:** Electric Pair Mode, Electric Indent Mode. "Electric" keys have special smart behavior.

**Related Terms:** Auto-Indent, Automatic, Smart Behavior

---

### ELPA `[Abbrev]`
**Definition:** Emacs Lisp Package Archive - the official package repository for Emacs.

**Context:** Accessed via package.el. Contains curated, GNU-compatible packages.

**Related Terms:** Package, MELPA, Package Manager, Repository

**Documentation:** See `doc/lispref/package.texi`

---

### .elc File `[Lisp]`
**Definition:** Byte-compiled Emacs Lisp file containing byte code.

**Context:** Produced by byte compiler from `.el` source. Faster to load and execute.

**Related Terms:** Byte Code, Compilation, .el File, .eln File

**Documentation:** See `doc/lispref/compile.texi`

---

### .eln File `[Lisp]`
**Definition:** Native-compiled Emacs Lisp file containing machine code.

**Context:** Produced by native compiler (GCC libgccjit). Significantly faster than byte code.

**Related Terms:** Native Compilation, Byte Code, .elc File

**Documentation:** See Emacs manual

---

### Emulation Mode `[Core]`
**Definition:** A minor mode that emulates key bindings and behavior of another editor (vi, CUA, etc.).

**Context:** Examples: viper-mode, cua-mode. Uses special keymap precedence.

**Related Terms:** Minor Mode, Keymap, Key Binding, Compatibility

---

### Environment Variable `[System]`
**Definition:** OS-level variables inherited by Emacs process, accessible via `getenv` and `setenv`.

**Context:** Affects PATH, locale, terminal settings, etc. Can be set per-process for subprocesses.

**Related Terms:** Process Environment, System, Shell

**Documentation:** See `doc/lispref/os.texi`

---

### EOL Convention `[System]`
**Definition:** End-of-line character convention - LF (Unix), CRLF (DOS/Windows), or CR (old Mac).

**Context:** Part of coding system. Auto-detected and preserved when editing files.

**Related Terms:** Coding System, Line Ending, Newline

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Error `[Lisp]`
**Definition:** An exceptional condition signaled during execution, interrupting normal control flow.

**Context:** Signaled by `error`, `signal`, or implicitly. Can be caught with `condition-case`.

**Related Terms:** Signal, Condition, Exception, Error Symbol

**Documentation:** See `doc/lispref/errors.texi`

---

### Error Symbol `[Lisp]`
**Definition:** A symbol representing an error type, with an error-conditions property defining its hierarchy.

**Context:** Used in `signal` and caught in `condition-case`. Examples: `error`, `file-error`, `void-variable`.

**Related Terms:** Error, Condition, Signal, Exception

**Documentation:** See `doc/lispref/errors.texi`

---

### Eval `[Lisp]`
**Definition:** The function that evaluates a Lisp form, executing code represented as data.

**Context:** Core of Lisp interpretation. Rarely needed explicitly; most code is automatically evaluated.

**Related Terms:** Evaluation, Interpreter, REPL, Read-Eval-Print Loop

**Documentation:** See `doc/lispref/eval.texi`

---

### Evaluation `[Lisp]`
**Definition:** The process of executing Lisp code by interpreting or running its compiled form.

**Context:** Self-evaluating objects (numbers, strings) return themselves. Symbols are looked up. Lists are function calls.

**Related Terms:** Eval, Interpreter, Execution, Read

**Documentation:** See `doc/lispref/eval.texi`

---

### Event `[System]`
**Definition:** A user input action like a key press, mouse click, or system notification.

**Context:** Read by command loop, processed via keymaps to invoke commands.

**Related Terms:** Key Event, Mouse Event, Command Loop, Input

**Documentation:** See `doc/lispref/commands.texi`

---

### Extent `[Data]` (XEmacs)
**Definition:** XEmacs equivalent of overlays - not used in GNU Emacs.

**Context:** Historical term. GNU Emacs uses overlays instead.

**Related Terms:** Overlay, XEmacs, Text Property

---

## F

### Face `[Display]`
**Definition:** A named collection of text display attributes like font, color, size, and weight.

**Context:** Applied via text properties or overlays. Themes customize faces.

**Related Terms:** Font, Color, Text Property, Theme, Display

**Documentation:** See `doc/lispref/display.texi`

---

### Face Attribute `[Display]`
**Definition:** A property of a face like `:foreground`, `:background`, `:weight`, `:slant`, `:height`, or `:family`.

**Context:** Set with `set-face-attribute`. Can be specified per-frame or globally.

**Related Terms:** Face, Font, Display, Theme

**Documentation:** See `doc/lispref/display.texi`

---

### Face Remapping `[Display]`
**Definition:** Buffer-local override of face definitions, changing appearance without affecting global faces.

**Context:** Used by text-scale-mode and similar features. Implemented via `face-remapping-alist`.

**Related Terms:** Face, Buffer-Local, Display, Theme

**Documentation:** See `doc/lispref/display.texi`

---

### Feature `[Lisp]`
**Definition:** A named collection of related functionality, registered when loaded via `provide`.

**Context:** Prevents redundant loading. Required via `require`. Tracked in `features` list.

**Related Terms:** Provide, Require, Library, Package

**Documentation:** See `doc/lispref/loading.texi`

---

### Field `[Core]`
**Definition:** A region of text with semantic meaning, like a form input field or completion candidate.

**Context:** Defined by `field` text property. Commands can move between fields.

**Related Terms:** Text Property, Form, Widget, Minibuffer

**Documentation:** See `doc/lispref/text.texi`

---

### File Handler `[System]`
**Definition:** A function that intercepts file operations for special file name patterns (remote files, archives, etc.).

**Context:** Registered in `file-name-handler-alist`. Enables TRAMP, compressed files, archives.

**Related Terms:** TRAMP, File Name, Remote File, Magic File Name

**Documentation:** See `doc/lispref/files.texi`

---

### File Local Variable `[Core]`
**Definition:** A variable setting specified in a file's header or footer, effective when that file is visited.

**Context:** Format: `-*- mode: emacs-lisp; -*-` or `Local Variables:` block. Security restrictions apply.

**Related Terms:** Local Variable, Directory Local Variable, Safe Local Variable

**Documentation:** See Emacs manual

---

### Fill `[Core]`
**Definition:** Reformatting text to fit within a specified column width by adjusting line breaks.

**Context:** Auto Fill Mode fills while typing. `fill-paragraph` fills existing text.

**Related Terms:** Fill Column, Auto Fill, Line Breaking, Paragraph

**Documentation:** See `doc/lispref/text.texi`

---

### Fill Column `[Core]`
**Definition:** The target column width for filling text, typically 70 characters.

**Context:** Set per-buffer. Controlled by `fill-column` variable.

**Related Terms:** Fill, Auto Fill Mode, Column, Line Width

---

### Fill Prefix `[Core]`
**Definition:** A string prepended to each line during filling, typically for maintaining indentation or comment markers.

**Context:** Set automatically in many modes. Used by fill commands.

**Related Terms:** Fill, Prefix, Indentation, Paragraph

**Documentation:** See `doc/lispref/text.texi`

---

### Filter `[System]`
**Definition:** A function called when an async process produces output, receiving the process and output string.

**Context:** Set with `set-process-filter`. Handles incremental output parsing.

**Related Terms:** Process, Sentinel, Async, Output

**Documentation:** See `doc/lispref/processes.texi`

---

### Finalizer `[Lisp]`
**Definition:** A function automatically called when an object is garbage collected.

**Context:** Used for cleanup of external resources. Created with `make-finalizer`.

**Related Terms:** Garbage Collection, Cleanup, Resource Management

**Documentation:** See `doc/lispref/` manual

---

### Font `[Display]`
**Definition:** A typeface with specific size, weight, and style used for rendering text.

**Context:** Specified in face definitions. Font backend handles font selection and rendering.

**Related Terms:** Face, Font Backend, Glyph, Typeface

**Documentation:** See `doc/lispref/display.texi`

---

### Font Backend `[Display]`
**Definition:** The low-level subsystem for font discovery, loading, and rendering (ftfont, xft, harfbuzz, etc.).

**Context:** Platform-specific. Multiple backends may be available. Handles complex text shaping.

**Related Terms:** Font, Harfbuzz, Rendering, Display Engine

**Source:** See `src/font.c`

---

### Font Lock Mode `[Display]`
**Definition:** A minor mode providing syntax highlighting through pattern matching and face application.

**Context:** Uses `font-lock-keywords` for patterns. Nearly universal in programming modes.

**Related Terms:** Syntax Highlighting, Face, Pattern, Major Mode

**Documentation:** See `doc/lispref/modes.texi`

---

### Font Lock Keywords `[Display]`
**Definition:** A list of patterns and faces defining how Font Lock Mode highlights text.

**Context:** Can be matchers, functions, or complex specs with subexpressions and anchoring.

**Related Terms:** Font Lock, Syntax Highlighting, Regexp, Face

**Documentation:** See `doc/lispref/modes.texi`

---

### Form `[Lisp]`
**Definition:** Any Lisp object that can be evaluated as code.

**Context:** Includes self-evaluating objects, symbols, and lists representing function calls or special forms.

**Related Terms:** S-expression, Expression, Evaluation

**Documentation:** See `doc/lispref/eval.texi`

---

### Frame `[Core]`
**Definition:** A graphical window (GUI) or terminal screen containing one or more Emacs windows.

**Context:** Each frame has independent window layout. Created with `make-frame`.

**Related Terms:** Window, Window-System, Terminal, Display

**Documentation:** See `doc/lispref/frames.texi`

---

### Frame Parameter `[Display]`
**Definition:** A named property of a frame controlling its appearance or behavior (size, position, font, etc.).

**Context:** Get with `frame-parameter`, set with `modify-frame-parameters`.

**Related Terms:** Frame, Window Parameter, Configuration

**Documentation:** See `doc/lispref/frames.texi`

---

### Fringe `[Display]`
**Definition:** Narrow vertical strips on the left and right edges of windows displaying indicators.

**Context:** Shows continuation, truncation, line wrapping, breakpoints, and custom bitmaps.

**Related Terms:** Margin, Bitmap, Window, Indicator

**Documentation:** See `doc/lispref/display.texi`

---

### Function `[Lisp]`
**Definition:** A callable Lisp object that performs computation and returns a value.

**Context:** Can be lambda expression, symbol naming a function, byte-code object, or primitive.

**Related Terms:** Lambda, Defun, Primitive, Call

**Documentation:** See `doc/lispref/functions.texi`

---

### Function Cell `[Lisp]`
**Definition:** The slot in a symbol that holds its function definition.

**Context:** Separate from value cell. Accessed with `symbol-function`.

**Related Terms:** Symbol, Value Cell, Namespace, Function

**Documentation:** See `doc/lispref/symbols.texi`

---

## G

### Gap Buffer `[Data]`
**Definition:** An efficient data structure for editable text using a movable gap for fast insertion/deletion at point.

**Context:** Core buffer implementation. Gap follows point to optimize editing at cursor.

**Related Terms:** Buffer, Point, Gap, Insertion, GPT

**Documentation:** See `doc/lispref/buffers.texi`

**Source:** See `src/buffer.h`, `src/insdel.c`

---

### Garbage Collection (GC) `[System]`
**Definition:** Automatic memory management that reclaims unused Lisp objects.

**Context:** Triggered when allocation exceeds threshold. Can cause brief pauses. Stats in `gc-elapsed`.

**Related Terms:** Memory Management, GC Threshold, Finalizer, Weak Reference

**Documentation:** See `doc/lispref/` manual

---

### Generic Function `[Lisp]`
**Definition:** A function that dispatches to different implementations based on argument types (polymorphism).

**Context:** Implemented via cl-generic. Supports single and multiple dispatch.

**Related Terms:** Method, CLOS, Polymorphism, Dispatch

**Documentation:** See `doc/lispref/functions.texi`

---

### Glyph `[Display]`
**Definition:** A graphical representation of a character or display element on screen.

**Context:** One character may produce multiple glyphs (ligatures) or one glyph may represent multiple characters (compositions).

**Related Terms:** Glyph Matrix, Font, Character, Display

**Source:** See `src/dispextern.h`

---

### Glyph Matrix `[Display]`
**Definition:** Internal data structure holding the glyphs to be displayed in a window, organized by rows.

**Context:** Maintained by display engine. Current and desired matrices compared for efficient redisplay.

**Related Terms:** Glyph, Redisplay, Display Engine, Window

**Source:** See `src/dispextern.h`

---

### Goal Column `[Core]`
**Definition:** The target column for vertical cursor movement, maintained across lines of different lengths.

**Context:** Preserves horizontal position when moving through short lines. Can be set explicitly.

**Related Terms:** Column, Vertical Motion, Track-EOL, Cursor

**Documentation:** See `doc/lispref/positions.texi`

---

### GPT / GPT_BYTE `[Data]`
**Definition:** Gap PosiTion - macros for the position of the buffer gap in characters and bytes.

**Context:** C internals of gap buffer. Gap moves to follow editing location.

**Related Terms:** Gap Buffer, Buffer, Point, Z

**Source:** See `src/buffer.h`

---

## H

### Hash Table `[Data]`
**Definition:** An efficient key-value mapping data structure with O(1) average lookup time.

**Context:** Created with `make-hash-table`. More efficient than alists for large datasets.

**Related Terms:** Alist, Plist, Dictionary, Map

**Documentation:** See `doc/lispref/hash.texi`

---

### Header Line `[Display]`
**Definition:** An optional first line in a window displaying persistent information, separate from buffer contents.

**Context:** Controlled by `header-line-format`. Similar to mode line but at top of window.

**Related Terms:** Mode Line, Window, Display, Format Spec

**Documentation:** See `doc/lispref/modes.texi`

---

### Help `[Core]`
**Definition:** The comprehensive documentation system providing function, variable, and key descriptions.

**Context:** Accessed via `C-h` prefix. Includes apropos, describe commands, info reader.

**Related Terms:** Describe, Apropos, Info, Documentation

**Documentation:** See Emacs manual

---

### Hook `[System]`
**Definition:** A variable holding a list of functions called at specific points in execution.

**Context:** Functions run via `run-hooks`. Normal hooks take no arguments. Abnormal hooks may take arguments or affect control flow.

**Related Terms:** Normal Hook, Abnormal Hook, Add-Hook, Callback

**Documentation:** See `doc/lispref/hooks.texi`

---

### Horizontal Scrolling `[Display]`
**Definition:** Shifting displayed text left or right within a window to view content beyond window width.

**Context:** Automatic in truncate-lines mode. Manual via `scroll-left`/`scroll-right`.

**Related Terms:** Truncation, Scroll, Window, Display

**Documentation:** See `doc/lispref/windows.texi`

---

## I

### Idle Timer `[System]`
**Definition:** A timer that fires after Emacs has been idle (no user input) for a specified duration.

**Context:** Created with `run-with-idle-timer`. Used for background tasks, auto-save, etc.

**Related Terms:** Timer, Idle, Background Task, Auto-Save

**Documentation:** See `doc/lispref/os.texi`

---

### Image `[Display]`
**Definition:** A graphical picture displayed in a buffer via display property or overlay.

**Context:** Supports various formats (PNG, JPEG, SVG, etc.). Can be inline or in margins.

**Related Terms:** Display Property, Image Spec, Icon, Graphic

**Documentation:** See `doc/lispref/display.texi`

---

### Image Descriptor `[Display]`
**Definition:** A Lisp structure specifying an image's type, source, and display properties.

**Context:** Format: `(image :type png :file "..." :scale 1.5)`. Used in display specs.

**Related Terms:** Image, Display Property, Image Spec

**Documentation:** See `doc/lispref/display.texi`

---

### Imenu `[Core]`
**Definition:** Index Menu - a system for creating navigable indices of definitions in a buffer.

**Context:** Generates menu of functions, classes, etc. Customized per major mode.

**Related Terms:** Which-Function, Index, Navigation, Menu

---

### Indentation `[Core]`
**Definition:** Horizontal spacing at the beginning of lines, typically for code structure visualization.

**Context:** Controlled by major mode. Electric Indent Mode automates indentation.

**Related Terms:** Tab, Column, Electric, SMIE, Indent Function

**Documentation:** See `doc/lispref/modes.texi`

---

### Indent Function `[Core]`
**Definition:** A function that calculates or performs indentation, typically set by major mode.

**Context:** Stored in `indent-line-function`. Called by TAB and electric indent.

**Related Terms:** Indentation, Major Mode, SMIE, Syntax

**Documentation:** See `doc/lispref/modes.texi`

---

### Indirect Buffer `[Core]`
**Definition:** A buffer that shares text with another (base) buffer but has independent point, mark, and local variables.

**Context:** Created with `make-indirect-buffer`. Useful for multiple views of same content with different modes.

**Related Terms:** Buffer, Base Buffer, Clone Buffer

**Documentation:** See `doc/lispref/buffers.texi`

---

### Info `[Core]`
**Definition:** Emacs's built-in hypertext documentation reader for Texinfo manuals.

**Context:** Accessed via `C-h i`. Contains Emacs, Elisp, and package documentation.

**Related Terms:** Manual, Documentation, Texinfo, Help

**Documentation:** See `doc/misc/info.texi`

---

### Inhibit Quit `[System]`
**Definition:** A variable that, when non-nil, prevents `C-g` from interrupting execution.

**Context:** Used for critical sections requiring atomicity. Use sparingly to avoid hanging Emacs.

**Related Terms:** Quit, C-g, Interrupt, Critical Section

**Documentation:** See `doc/lispref/commands.texi`

---

### Init File `[Core]`
**Definition:** The user's Emacs configuration file, typically `~/.emacs` or `~/.emacs.d/init.el`.

**Context:** Loaded at startup. Contains personal customizations, package configuration, etc.

**Related Terms:** Configuration, Startup, .emacs, Early Init File

**Documentation:** See Emacs manual

---

### Input Focus `[Display]`
**Definition:** The keyboard and interaction target, determining which frame and window receives input events.

**Context:** Managed by window manager. `select-frame-set-input-focus` sets focus.

**Related Terms:** Frame, Selected Window, Event, Focus

**Documentation:** See `doc/lispref/frames.texi`

---

### Input Method `[System]`
**Definition:** A system for inputting characters not directly available on the keyboard, like CJK characters or accents.

**Context:** Activated with `C-\`. Many methods available for different languages and scripts.

**Related Terms:** Multilingual, Quail, Character Input, IME

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Insertion `[Core]`
**Definition:** Adding text to a buffer, increasing buffer size and updating markers and overlays.

**Context:** Performed by `insert`, `insert-char`, `insert-file-contents`, etc. Undoable.

**Related Terms:** Deletion, Point, Gap Buffer, Modification

**Documentation:** See `doc/lispref/text.texi`

---

### Insertion Type `[Data]`
**Definition:** A marker property determining whether the marker stays before or after text inserted at its position.

**Context:** Set with `set-marker-insertion-type`. Default is before (marker advances).

**Related Terms:** Marker, Insertion, Point, Relocation

**Documentation:** See `doc/lispref/markers.texi`

---

### Interactive `[Lisp]`
**Definition:** A special form declaring a function as a command and specifying how to obtain its arguments interactively.

**Context:** Takes an interactive spec. Enables `M-x` invocation and key binding.

**Related Terms:** Command, Interactive Spec, M-x, Call Interactively

**Documentation:** See `doc/lispref/commands.texi`

---

### Interactive Spec `[Lisp]`
**Definition:** A string or form in `interactive` describing how to read command arguments from the user.

**Context:** Code characters specify argument types: `s` for string, `r` for region, `P` for prefix arg, etc.

**Related Terms:** Interactive, Command, Argument, Prompt

**Documentation:** See `doc/lispref/commands.texi`

---

### Interpreter `[Lisp]`
**Definition:** The component of Emacs that evaluates Lisp forms directly without compilation.

**Context:** Slower than byte code or native code but always available. Used for interactive evaluation.

**Related Terms:** Evaluation, Byte Code, Native Compilation, Eval

**Documentation:** See `doc/lispref/eval.texi`

---

### Interval `[Data]`
**Definition:** An internal data structure for storing text properties efficiently over ranges of text.

**Context:** Forms an interval tree. Users don't manipulate intervals directly; they work with text properties.

**Related Terms:** Interval Tree, Text Property, Data Structure

**Source:** See `src/intervals.h`

---

### Interval Tree `[Data]`
**Definition:** A balanced tree data structure for efficiently storing and querying text properties over text ranges.

**Context:** Internal implementation detail. Provides O(log n) property lookup and modification.

**Related Terms:** Interval, Text Property, Balanced Tree, itree

**Source:** See `src/intervals.h`, `src/itree.c`

---

### Invisible Text `[Display]`
**Definition:** Text marked with the `invisible` property that is not displayed but remains in the buffer.

**Context:** Used for outlining, narrowing, and hiding details. Point can skip over invisible text.

**Related Terms:** Display Property, Text Property, Outline, Ellipsis

**Documentation:** See `doc/lispref/display.texi`

---

### Isearch `[Core]`
**Definition:** Incremental Search - an interactive search mode showing matches as you type.

**Context:** Started with `C-s`. Supports regexp, word search, symbol search, and many variants.

**Related Terms:** Search, Regexp, Incremental, Replace

---

## J

### JIT Lock `[Display]`
**Definition:** Just-In-Time syntax highlighting that fontifies text as it becomes visible.

**Context:** Defers fontification for performance. Operates in chunks during redisplay.

**Related Terms:** Font Lock, Fontification, Lazy, Performance

**Documentation:** See `lisp/jit-lock.el`

---

## K

### Keyboard Macro `[Core]`
**Definition:** A recorded sequence of keystrokes that can be replayed to automate repetitive tasks.

**Context:** Record with `C-x (`, stop with `C-x )`, execute with `C-x e`. Can be named and saved.

**Related Terms:** Macro, Automation, Replay, Command

**Documentation:** See Emacs manual

---

### Key Binding `[Core]`
**Definition:** An association between a key sequence and a command in a keymap.

**Context:** Created with `define-key`, `global-set-key`, etc. Queried with `describe-key`.

**Related Terms:** Keymap, Key Sequence, Command, Binding

**Documentation:** See `doc/lispref/keymaps.texi`

---

### Key Sequence `[Core]`
**Definition:** A sequence of one or more key events that can be bound to a command.

**Context:** Examples: `C-x C-f`, `M-x`, `C-c C-c`. Can include mouse events and modifiers.

**Related Terms:** Key Binding, Event, Prefix Key, Keymap

**Documentation:** See `doc/lispref/keymaps.texi`

---

### Keymap `[Core]`
**Definition:** A data structure mapping key sequences to commands or other keymaps.

**Context:** Multiple keymaps active simultaneously with precedence rules. Can be sparse or full.

**Related Terms:** Key Binding, Key Sequence, Active Keymap, Prefix Key

**Documentation:** See `doc/lispref/keymaps.texi`

---

### Kill `[Abbrev]` `[Core]`
**Definition:** Cutting or deleting text, saving it to the kill ring for later yanking (pasting).

**Context:** Unlike most editors' "cut", killed text is added to a ring, not replacing previous kills.

**Related Terms:** Kill Ring, Yank, Cut, Delete

**Documentation:** See Emacs manual

---

### Kill Ring `[Core]`
**Definition:** A ring buffer storing previously killed text, allowing retrieval of earlier kills.

**Context:** `C-y` yanks most recent kill. `M-y` cycles through kill ring.

**Related Terms:** Kill, Yank, Clipboard, Ring Buffer

**Documentation:** See `doc/lispref/text.texi`

---

### Killing Buffers `[Core]`
**Definition:** Removing a buffer from Emacs, freeing its memory and closing any associated file.

**Context:** Done with `kill-buffer`. Unsaved changes prompt for confirmation.

**Related Terms:** Buffer, Buried Buffer, Buffer List

**Documentation:** See `doc/lispref/buffers.texi`

---

## L

### Lambda `[Lisp]`
**Definition:** An anonymous function definition created with the `lambda` special form.

**Context:** Creates a function object without naming it. Often used as arguments to higher-order functions.

**Related Terms:** Function, Anonymous Function, Closure, Defun

**Documentation:** See `doc/lispref/functions.texi`

---

### Lambda List `[Lisp]`
**Definition:** The parameter list of a lambda or defun, possibly including `&optional`, `&rest`, or `&key`.

**Context:** Specifies function arguments and their types (required, optional, rest, keyword).

**Related Terms:** Argument List, Lambda, Parameter, Function

**Documentation:** See `doc/lispref/functions.texi`

---

### LAP `[Lisp]`
**Definition:** Lisp Assembly Program - a human-readable representation of byte code.

**Context:** Intermediate format between Lisp and byte code. Used in byte compiler implementation.

**Related Terms:** Byte Code, Disassembly, Byte Compiler, Assembly

**Documentation:** See `doc/lispref/compile.texi`

---

### Lazy Loading `[Lisp]`
**Definition:** Deferring the loading of code until it's actually needed, improving startup time.

**Context:** Implemented via autoload. Essential for keeping Emacs responsive.

**Related Terms:** Autoload, Feature, Loading, Performance

**Documentation:** See `doc/lispref/loading.texi`

---

### Let Binding `[Lisp]`
**Definition:** A local variable binding created by `let` or `let*`, shadowing outer bindings in its scope.

**Context:** `let` binds in parallel, `let*` binds sequentially. Lexical or dynamic depending on `lexical-binding`.

**Related Terms:** Scope, Binding, Local Variable, Lexical Binding

**Documentation:** See `doc/lispref/variables.texi`

---

### Lexical Binding `[Lisp]`
**Definition:** Variable scoping where bindings are determined by textual structure rather than runtime call stack.

**Context:** Enabled by `lexical-binding: t` file header. Enables closures and better optimization.

**Related Terms:** Dynamic Binding, Scope, Closure, Environment

**Documentation:** See `doc/lispref/variables.texi`

---

### Library `[Lisp]`
**Definition:** A file or collection of files providing related functionality, loaded as a unit.

**Context:** Loaded with `load-library` or `require`. Provides features.

**Related Terms:** Feature, Require, Package, Load

**Documentation:** See `doc/lispref/loading.texi`

---

### Line Number `[Core]`
**Definition:** The sequential position of a line in a buffer, starting from 1.

**Context:** Display-line-numbers-mode shows line numbers in margin. `line-number-at-pos` gets number.

**Related Terms:** Line, Position, Margin, Display

**Documentation:** See `doc/lispref/positions.texi`

---

### Line Wrapping `[Display]`
**Definition:** Continuing long logical lines on multiple screen lines rather than truncating.

**Context:** Controlled by `truncate-lines`. Visual-line-mode provides word wrapping.

**Related Terms:** Continuation Line, Truncation, Visual Line, Word Wrap

**Documentation:** See `doc/lispref/display.texi`

---

### Lisp_Object `[Lisp]` `[Data]`
**Definition:** The fundamental C type representing any Emacs Lisp value.

**Context:** Tagged pointer encoding type and value. Core of C implementation.

**Related Terms:** Tagged Pointer, C Source, Type, Value

**Source:** See `src/lisp.h`

---

### List `[Data]`
**Definition:** A sequence of cons cells linked by their cdr pointers, terminated by nil.

**Context:** Fundamental data structure in Lisp. Proper lists end in nil. Improper lists end otherwise.

**Related Terms:** Cons Cell, Nil, Proper List, Car, Cdr

**Documentation:** See `doc/lispref/lists.texi`

---

### Load `[Lisp]`
**Definition:** Reading and evaluating Lisp code from a file.

**Context:** Performed by `load`, `require`, or during startup. Can load `.el`, `.elc`, or `.eln` files.

**Related Terms:** Require, Feature, Loading, Eval

**Documentation:** See `doc/lispref/loading.texi`

---

### Load Path `[Lisp]`
**Definition:** A list of directories searched when loading libraries, stored in `load-path` variable.

**Context:** Modified by packages, users, and site configuration. Order matters.

**Related Terms:** Load, Library, Require, Path

**Documentation:** See `doc/lispref/loading.texi`

---

### Local Keymap `[Core]`
**Definition:** A buffer-local or mode-specific keymap containing bindings for that context.

**Context:** Major and minor modes install local keymaps. Overrides global keymap.

**Related Terms:** Keymap, Buffer-Local, Major Mode, Minor Mode

**Documentation:** See `doc/lispref/keymaps.texi`

---

### Local Variable `[Lisp]`
**Definition:** A variable whose binding is limited to a specific scope (let binding, function parameter, or buffer-local).

**Context:** Contrasts with global/special variables visible everywhere.

**Related Terms:** Let Binding, Buffer-Local Variable, Scope, Binding

**Documentation:** See `doc/lispref/variables.texi`

---

### Locking `[System]`
**Definition:** A mechanism to prevent simultaneous editing of a file by multiple processes.

**Context:** Creates symbolic link lock file. Can be disabled with `create-lockfiles`.

**Related Terms:** File, Concurrent Editing, Lock File, Version Control

**Documentation:** See `doc/lispref/files.texi`

---

### LSP `[Abbrev]`
**Definition:** Language Server Protocol - a standard for IDE features like completion, navigation, and refactoring.

**Context:** Supported by eglot and lsp-mode packages. Modern alternative to CEDET.

**Related Terms:** Eglot, lsp-mode, IDE, Language Server

**Documentation:** See `doc/misc/eglot.texi`

---

## M

### M-x `[Core]`
**Definition:** The key sequence (Meta-x or Alt-x) for executing commands by name.

**Context:** Provides access to all interactive commands. Supports completion and history.

**Related Terms:** Execute Extended Command, Command, Interactive

---

### Macro `[Lisp]`
**Definition:** A special function that transforms code at compile/read time rather than runtime.

**Context:** Defined with `defmacro`. Receives unevaluated arguments, returns code to evaluate.

**Related Terms:** Defmacro, Macro Expansion, Backquote, Special Form

**Documentation:** See `doc/lispref/macros.texi`

---

### Macro Expansion `[Lisp]`
**Definition:** The process of applying a macro to its arguments to produce expanded code.

**Context:** Happens at compile time (byte compilation) or read time. Can be inspected with `macroexpand`.

**Related Terms:** Macro, Compile Time, Defmacro, Evaluation

**Documentation:** See `doc/lispref/macros.texi`

---

### Major Mode `[Core]`
**Definition:** A buffer-local mode defining primary editing behavior, syntax, key bindings, and commands for a file type.

**Context:** Each buffer has exactly one major mode. Examples: emacs-lisp-mode, python-mode, text-mode.

**Related Terms:** Minor Mode, Mode, Derived Mode, Mode Hook

**Documentation:** See `doc/lispref/modes.texi`

---

### Margin `[Display]`
**Definition:** White space on the left or right edge of a window, outside the text area, for displaying annotations.

**Context:** Can display text, images, or be empty. Distinct from fringe.

**Related Terms:** Fringe, Display Property, Window, Annotation

**Documentation:** See `doc/lispref/display.texi`

---

### Mark `[Core]`
**Definition:** A saved buffer position marking one end of the region, with point marking the other end.

**Context:** Set with `C-SPC`. Can be inactive (invisible) or active (visible region).

**Related Terms:** Point, Region, Mark Ring, Marker

**Documentation:** See `doc/lispref/markers.texi`

---

### Mark Ring `[Core]`
**Definition:** A buffer-local ring of previously set mark positions, allowing navigation to earlier marks.

**Context:** `C-u C-SPC` pops mark ring. Separate from global mark ring.

**Related Terms:** Mark, Ring Buffer, Navigation, Point

**Documentation:** See `doc/lispref/markers.texi`

---

### Marker `[Data]`
**Definition:** A Lisp object representing a buffer position that automatically updates when text is inserted or deleted.

**Context:** Unlike integer positions, markers track the conceptual location between characters.

**Related Terms:** Point, Position, Buffer, Relocation

**Documentation:** See `doc/lispref/markers.texi`

---

### Match Data `[Lisp]`
**Definition:** Information about the most recent successful regexp search, including matched text and subexpressions.

**Context:** Accessed via `match-beginning`, `match-end`, `match-string`. Saved/restored with `save-match-data`.

**Related Terms:** Regexp, Search, Subexpression, Capture Group

**Documentation:** See `doc/lispref/searching.texi`

---

### MELPA `[Abbrev]`
**Definition:** Milkypostman's Emacs Lisp Package Archive - a large community package repository.

**Context:** Contains thousands of packages. Updates frequently. Less curated than ELPA.

**Related Terms:** ELPA, Package, Repository, Package Manager

---

### Message `[Core]`
**Definition:** Text displayed in the echo area to inform the user.

**Context:** Created with `message` function. Appears briefly or until next event.

**Related Terms:** Echo Area, Minibuffer, Log, *Messages* Buffer

**Documentation:** See `doc/lispref/display.texi`

---

### Meta Key `[Core]`
**Definition:** A modifier key (Alt or Esc) used in Emacs key sequences, denoted M- in documentation.

**Context:** `M-x` = Alt-x or Esc x. Essential for Emacs key bindings.

**Related Terms:** Modifier, Key Sequence, Control Key, Esc

---

### Minibuffer `[Core]`
**Definition:** A special buffer appearing in the echo area for user input (commands, files, strings, etc.).

**Context:** Provides completion, history, and sophisticated input methods. Active during prompts.

**Related Terms:** Echo Area, Completion, Prompt, Read Function

**Documentation:** See `doc/lispref/minibuf.texi`

---

### Minibuffer History `[Core]`
**Definition:** Lists of previously entered minibuffer inputs, accessible via M-p/M-n during prompts.

**Context:** Separate histories for commands, files, search strings, etc.

**Related Terms:** Minibuffer, History, Completion

**Documentation:** See `doc/lispref/minibuf.texi`

---

### Minor Mode `[Core]`
**Definition:** An optional buffer-local or global feature that can be toggled independently of the major mode.

**Context:** Multiple minor modes can be active simultaneously. Examples: auto-fill-mode, font-lock-mode.

**Related Terms:** Major Mode, Mode, Global Minor Mode, Mode Line

**Documentation:** See `doc/lispref/modes.texi`

---

### Mode Hook `[System]`
**Definition:** A hook run when a major or minor mode is activated, allowing customization.

**Context:** Named `<mode>-hook`. Add functions with `add-hook`.

**Related Terms:** Hook, Major Mode, Minor Mode, Customization

**Documentation:** See `doc/lispref/modes.texi`

---

### Mode Line `[Display]`
**Definition:** The status line at the bottom of each window displaying buffer name, mode, position, etc.

**Context:** Highly customizable via `mode-line-format`. Click-sensitive.

**Related Terms:** Header Line, Window, Display, Format Spec

**Documentation:** See `doc/lispref/modes.texi`

---

### Mode Line Format `[Display]`
**Definition:** A specification describing what to display in the mode line, similar to format strings.

**Context:** Complex nested structure supporting conditionals, functions, and properties.

**Related Terms:** Mode Line, Format Spec, Display, Customization

**Documentation:** See `doc/lispref/modes.texi`

---

### Modification Time `[System]`
**Definition:** The timestamp when a file or buffer was last modified.

**Context:** Used to detect external changes. Checked before saving.

**Related Terms:** File, Buffer Modification, Timestamp, Visited File

**Documentation:** See `doc/lispref/files.texi`

---

### Mouse Event `[System]`
**Definition:** An event representing mouse movement, clicks, drags, or wheel scrolling.

**Context:** Includes position, button, modifiers, and click count. Processed by keymaps.

**Related Terms:** Event, Key Event, Click, Mouse

**Documentation:** See `doc/lispref/commands.texi`

---

### Multibyte `[System]`
**Definition:** A buffer or string encoding where characters can occupy multiple bytes (UTF-8 internally).

**Context:** Modern default. Contrasts with unibyte (byte-oriented).

**Related Terms:** Unicode, Unibyte, Coding System, Character

**Documentation:** See `doc/lispref/nonascii.texi`

---

## N

### Narrowing `[Core]`
**Definition:** Restricting buffer visibility and editability to a portion, hiding text outside the region.

**Context:** Commands like `narrow-to-region`. Use `widen` to restore full buffer.

**Related Terms:** Region, Restriction, BEGV, ZV, Widen

**Documentation:** See `doc/lispref/positions.texi`

---

### Native Compilation `[Lisp]`
**Definition:** Compilation of Emacs Lisp to native machine code using GCC's libgccjit.

**Context:** Produces `.eln` files. Significantly faster than byte code.

**Related Terms:** Byte Code, Compilation, .eln File, Performance

**Documentation:** See Emacs manual

---

### Nil `[Lisp]`
**Definition:** The symbol representing both the empty list and the boolean false value.

**Context:** Only false value in Emacs Lisp. All other values are true.

**Related Terms:** T, Boolean, Empty List, False

**Documentation:** See `doc/lispref/lists.texi`

---

### Normal Hook `[System]`
**Definition:** A hook where functions are called with no arguments and whose return values are ignored.

**Context:** Most hooks are normal hooks. Run with `run-hooks`.

**Related Terms:** Hook, Abnormal Hook, Run Hooks, Callback

**Documentation:** See `doc/lispref/hooks.texi`

---

## O

### Obarray `[Data]`
**Definition:** A hash table (vector) for interning symbols, ensuring each symbol name has one unique object.

**Context:** Default obarray contains all global symbols. Can create isolated obarrays.

**Related Terms:** Symbol, Intern, Hash Table, Namespace

**Documentation:** See `doc/lispref/symbols.texi`

---

### Overlay `[Data]`
**Definition:** An object specifying a buffer region with associated properties, independent of text properties.

**Context:** Can specify faces, invisibility, modification hooks, etc. Used for temporary highlighting.

**Related Terms:** Text Property, Face, Invisible Text, Before/After String

**Documentation:** See `doc/lispref/display.texi`

---

### Override Keymap `[Core]`
**Definition:** A keymap with highest precedence, overriding all other keymaps including minor modes.

**Context:** Set via `overriding-local-map` or `overriding-terminal-local-map`. Rarely used.

**Related Terms:** Keymap, Precedence, Local Keymap

**Documentation:** See `doc/lispref/keymaps.texi`

---

## P

### Package `[System]`
**Definition:** A bundled collection of Emacs Lisp files providing related functionality, installable via package.el.

**Context:** Distributed via ELPA, MELPA, etc. Includes metadata and dependencies.

**Related Terms:** ELPA, MELPA, package.el, Library

**Documentation:** See `doc/lispref/package.texi`

---

### Package Manager `[System]`
**Definition:** The system (package.el) for discovering, installing, and managing Emacs packages.

**Context:** `M-x list-packages` browses available packages. Handles dependencies automatically.

**Related Terms:** Package, ELPA, MELPA, Installation

**Documentation:** See `doc/lispref/package.texi`

---

### Paren Matching `[Display]`
**Definition:** Highlighting or navigation to matching delimiters (parentheses, brackets, braces).

**Context:** Show-paren-mode highlights matches. `forward-sexp` navigates by balanced expressions.

**Related Terms:** Sexp, Balanced Expression, Syntax Table, Delimiter

---

### Parse State `[Lisp]`
**Definition:** Information about syntactic context at a buffer position (comment depth, string state, paren depth, etc.).

**Context:** Returned by `parse-partial-sexp`. Critical for syntax-aware operations.

**Related Terms:** Syntax Table, Parsing, SMIE, Context

**Documentation:** See `doc/lispref/syntax.texi`

---

### Plist `[Data]`
**Definition:** Property List - a list of alternating keys and values: `(key1 val1 key2 val2 ...)`.

**Context:** Simpler than alist for small datasets. Used for symbol properties and faces.

**Related Terms:** Alist, Symbol Property, List, Key-Value

**Documentation:** See `doc/lispref/lists.texi`

---

### Point `[Core]`
**Definition:** The current buffer position where insertion and many operations occur, typically where cursor is displayed.

**Context:** An integer counting characters from buffer start (1). Each buffer has its own point.

**Related Terms:** Cursor, Mark, Position, Marker, Insertion

**Documentation:** See `doc/lispref/positions.texi`

---

### Position `[Core]`
**Definition:** A buffer location, represented as a character number (integer) or marker.

**Context:** Positions range from 1 (BEG) to (point-max). Zero is never a valid position.

**Related Terms:** Point, Marker, Character Position, Byte Position

**Documentation:** See `doc/lispref/positions.texi`

---

### Predicate `[Lisp]`
**Definition:** A function that returns a boolean value, testing a condition or type.

**Context:** Often named with `-p` suffix: `bufferp`, `integerp`, `null`, `boundp`.

**Related Terms:** Boolean, Test, Type Check, Function

**Documentation:** See `doc/lispref/` various sections

---

### Prefix Argument `[Core]`
**Definition:** A numeric or symbolic argument passed to commands via `C-u` or `M-<number>`.

**Context:** Modifies command behavior. Raw form `(4)` from one `C-u`, `(16)` from two, etc.

**Related Terms:** Universal Argument, C-u, Command, Argument

**Documentation:** See `doc/lispref/commands.texi`

---

### Prefix Key `[Core]`
**Definition:** A key sequence that is a prefix of longer key sequences, like `C-x` or `C-c`.

**Context:** Bound to a keymap rather than a command. Opens further key possibilities.

**Related Terms:** Key Sequence, Keymap, Key Binding

**Documentation:** See `doc/lispref/keymaps.texi`

---

### Primitive `[Lisp]`
**Definition:** A function implemented in C rather than Emacs Lisp, also called a subr or built-in function.

**Context:** Provides core functionality and performance-critical operations.

**Related Terms:** Subr, Built-in, DEFUN, C Source

**Documentation:** See `doc/lispref/eval.texi`

---

### Print `[Lisp]`
**Definition:** Converting Lisp objects to their textual representation.

**Context:** Opposite of read. `prin1` prints readably, `princ` prints for humans, `print` adds newline.

**Related Terms:** Read, Printer, Format, Output

**Documentation:** See `doc/lispref/streams.texi`

---

### Process `[System]`
**Definition:** A subprocess running concurrently with Emacs, with optional I/O connections.

**Context:** Created with `start-process` or `make-process`. Can be synchronous or asynchronous.

**Related Terms:** Subprocess, Filter, Sentinel, Async, Pipe

**Documentation:** See `doc/lispref/processes.texi`

---

### Property List `[Data]`
**Definition:** See Plist.

**Related Terms:** Plist, Symbol Property

---

### Provide `[Lisp]`
**Definition:** Declares that a library provides a named feature, registering it in the `features` list.

**Context:** Placed at end of library files. Paired with `require`.

**Related Terms:** Require, Feature, Library, Loading

**Documentation:** See `doc/lispref/loading.texi`

---

## Q

### Quail `[System]`
**Definition:** The Emacs input method framework for entering non-ASCII characters.

**Context:** Defines phonetic and other input methods for various languages.

**Related Terms:** Input Method, Multilingual, Character Input

**Documentation:** See leim/ directory

---

### Query-Replace `[Core]`
**Definition:** Interactive search-and-replace that prompts for confirmation at each match.

**Context:** `M-%` for string, `C-M-%` for regexp. Offers skip, replace, replace-all options.

**Related Terms:** Replace, Search, Interactive, Regexp

**Documentation:** See Emacs manual

---

### Quit `[Core]`
**Definition:** Interrupting the current command or operation, typically with `C-g`.

**Context:** Signals `quit` condition. Can be inhibited with `inhibit-quit`.

**Related Terms:** C-g, Interrupt, Signal, Inhibit Quit

**Documentation:** See `doc/lispref/commands.texi`

---

### Quote `[Lisp]`
**Definition:** A special form preventing evaluation of its argument, returning it as data.

**Context:** `'x` is shorthand for `(quote x)`. Fundamental for treating code as data.

**Related Terms:** Evaluation, Special Form, Backquote, Unquote

**Documentation:** See `doc/lispref/eval.texi`

---

## R

### Read `[Lisp]`
**Definition:** Parsing textual representation to create Lisp objects.

**Context:** Opposite of print. Used by `load`, `eval`, and REPL.

**Related Terms:** Reader, Print, Parse, S-expression

**Documentation:** See `doc/lispref/streams.texi`

---

### Read-Only Buffer `[Core]`
**Definition:** A buffer where modifications are prevented, signaling an error on edit attempts.

**Context:** Controlled by `buffer-read-only` variable. Toggle with `C-x C-q`.

**Related Terms:** Buffer, Modification, Protection

**Documentation:** See `doc/lispref/buffers.texi`

---

### Reader `[Lisp]`
**Definition:** The component that parses textual Lisp code into data structures.

**Context:** Handles syntax like quotes, backquotes, reader macros, and # syntax.

**Related Terms:** Read, Parse, S-expression, Syntax

**Documentation:** See `doc/lispref/streams.texi`

---

### Recursion `[Lisp]`
**Definition:** A function calling itself, directly or indirectly.

**Context:** Limited by `max-lisp-eval-depth`. Tail recursion not optimized in Emacs Lisp.

**Related Terms:** Stack, Call Stack, Depth, Loop

**Documentation:** See `doc/lispref/functions.texi`

---

### Redisplay `[Display]`
**Definition:** The process of updating the screen to reflect current buffer contents and state.

**Context:** Normally automatic. Can be forced with `redisplay` function. Performance-critical.

**Related Terms:** Display Engine, Glyph Matrix, Refresh, Rendering

**Documentation:** See `doc/lispref/display.texi`

---

### Regexp `[Lisp]`
**Definition:** Regular Expression - a pattern language for matching and searching text.

**Context:** Emacs uses its own regexp syntax, similar but not identical to POSIX or Perl.

**Related Terms:** Pattern, Search, Match Data, Character Class

**Documentation:** See `doc/lispref/searching.texi`

---

### Region `[Core]`
**Definition:** The text between point and mark.

**Context:** Many commands operate on the region. Visibility controlled by transient-mark-mode.

**Related Terms:** Point, Mark, Active Region, Selection

**Documentation:** See `doc/lispref/markers.texi`

---

### Register `[Core]`
**Definition:** A named storage location for positions, text, windows configurations, or other data.

**Context:** Accessed via single-character names. `C-x r` prefix for register commands.

**Related Terms:** Bookmark, Storage, Clipboard

**Documentation:** See Emacs manual

---

### REPL `[Lisp]`
**Definition:** Read-Eval-Print Loop - an interactive programming environment.

**Context:** *scratch* buffer and `ielm` mode provide REPL functionality.

**Related Terms:** Interactive, Eval, Read, Print

---

### Require `[Lisp]`
**Definition:** Loads a library if its feature has not been provided yet.

**Context:** Ensures dependencies are loaded. Idempotent unlike `load`.

**Related Terms:** Provide, Feature, Load, Library

**Documentation:** See `doc/lispref/loading.texi`

---

### Restriction `[Core]`
**Definition:** The accessible portion of a buffer, possibly limited by narrowing.

**Context:** BEGV to ZV. Many commands respect restriction.

**Related Terms:** Narrowing, BEGV, ZV, Accessible Region

**Documentation:** See `doc/lispref/positions.texi`

---

### Revert Buffer `[Core]`
**Definition:** Reloading a buffer's contents from its associated file, discarding changes.

**Context:** `M-x revert-buffer`. Auto-revert-mode does this automatically.

**Related Terms:** Reload, File, Auto-Revert, Buffer

**Documentation:** See `doc/lispref/buffers.texi`

---

### Ring Buffer `[Data]`
**Definition:** A fixed-size circular buffer where oldest entries are overwritten when full.

**Context:** Used for kill ring, mark ring, command history, etc.

**Related Terms:** Kill Ring, Mark Ring, Circular Buffer, History

**Documentation:** See `lisp/ring.el`

---

## S

### Safe Local Variable `[Core]`
**Definition:** A file-local or directory-local variable deemed safe to set without confirmation.

**Context:** Registered in `safe-local-variable-values` or with safe predicate.

**Related Terms:** File Local Variable, Directory Local Variable, Security

**Documentation:** See Emacs manual

---

### Save-Excursion `[Lisp]`
**Definition:** A special form that saves and restores point, mark, and current buffer around code execution.

**Context:** Common pattern for temporary buffer operations. Consider `save-current-buffer` if only buffer matters.

**Related Terms:** Point, Mark, Current Buffer, Unwinding

**Documentation:** See `doc/lispref/positions.texi`

---

### Scope `[Lisp]`
**Definition:** The region of code where a variable binding is visible and accessible.

**Context:** Lexical scope based on code structure, dynamic scope based on call stack.

**Related Terms:** Binding, Lexical Binding, Dynamic Binding, Visibility

**Documentation:** See `doc/lispref/variables.texi`

---

### Search `[Core]`
**Definition:** Finding text matching a string or pattern in a buffer.

**Context:** Isearch (incremental), `search-forward`, `re-search-forward` (regexp), etc.

**Related Terms:** Isearch, Regexp, Match Data, Find

**Documentation:** See `doc/lispref/searching.texi`

---

### Selected Frame `[Core]`
**Definition:** The frame with input focus, receiving keyboard and most commands.

**Context:** Queried with `selected-frame`, set with `select-frame-set-input-focus`.

**Related Terms:** Frame, Input Focus, Selected Window

**Documentation:** See `doc/lispref/frames.texi`

---

### Selected Window `[Core]`
**Definition:** The window receiving most commands and usually displaying the cursor.

**Context:** Its buffer is typically (but not always) the current buffer.

**Related Terms:** Window, Current Buffer, Cursor, Selection

**Documentation:** See `doc/lispref/windows.texi`

---

### Sentinel `[System]`
**Definition:** A function called when an asynchronous process changes state (exits, crashes, etc.).

**Context:** Set with `set-process-sentinel`. Receives process and state string.

**Related Terms:** Process, Filter, Async, Callback

**Documentation:** See `doc/lispref/processes.texi`

---

### Server Mode `[System]`
**Definition:** Running Emacs as a server that clients can connect to for editing.

**Context:** Enables `emacsclient`. Can run as daemon or in existing session.

**Related Terms:** Daemon, Client, emacsclient

**Documentation:** See Emacs manual

---

### S-expression `[Lisp]`
**Definition:** Symbolic Expression - any valid Lisp form: atom, list, or special syntax.

**Context:** Fundamental unit of Lisp code and data. Read by reader, evaluated by interpreter.

**Related Terms:** Sexp, Form, Expression, List

**Documentation:** See `doc/lispref/` introduction

---

### Sexp `[Abbrev]`
**Definition:** Abbreviation for S-expression.

**Context:** Used in function names like `forward-sexp`, `backward-sexp`.

**Related Terms:** S-expression, Form, Expression

---

### Signal `[Lisp]`
**Definition:** Throwing an error or condition, interrupting normal execution flow.

**Context:** Function `signal` or convenience `error`. Caught by `condition-case`.

**Related Terms:** Error, Condition, Exception, Throw

**Documentation:** See `doc/lispref/errors.texi`

---

### SMIE `[Lisp]`
**Definition:** Simple Minded Indentation Engine - a framework for implementing major mode indentation.

**Context:** Simpler than full parsing. Uses precedence grammar and tokens.

**Related Terms:** Indentation, Major Mode, Parser, Syntax

**Documentation:** See `doc/lispref/modes.texi`

---

### Special Form `[Lisp]`
**Definition:** A built-in syntactic construct with special evaluation rules, like `if`, `let`, `quote`.

**Context:** Arguments not automatically evaluated. Cannot be redefined. Core language constructs.

**Related Terms:** Form, Macro, Primitive, Evaluation

**Documentation:** See `doc/lispref/eval.texi`

---

### Special Variable `[Lisp]`
**Definition:** A variable using dynamic binding even under lexical-binding mode.

**Context:** Declared with `defvar` or `defconst`. Allows dynamic scoping when needed.

**Related Terms:** Dynamic Binding, Defvar, Variable, Scope

**Documentation:** See `doc/lispref/variables.texi`

---

### Subr `[Lisp]`
**Definition:** A primitive function implemented in C (short for "subroutine").

**Context:** Type name for built-in functions. `subrp` tests for this type.

**Related Terms:** Primitive, Built-in, DEFUN, C Source

**Documentation:** See `doc/lispref/eval.texi`

---

### Symbol `[Lisp]`
**Definition:** A Lisp object with a name, used for variables, functions, and as unique identifiers.

**Context:** Has value cell, function cell, property list, and name. Interned in obarray.

**Related Terms:** Variable, Function, Intern, Obarray

**Documentation:** See `doc/lispref/symbols.texi`

---

### Symbol Property `[Lisp]`
**Definition:** A key-value association attached to a symbol, stored in its property list.

**Context:** Get with `get`, set with `put`. Independent of variable/function bindings.

**Related Terms:** Plist, Symbol, Property, Metadata

**Documentation:** See `doc/lispref/symbols.texi`

---

### Syntax Class `[Lisp]`
**Definition:** A classification of characters (word, whitespace, open paren, etc.) in a syntax table.

**Context:** Determines parsing behavior. Examples: word constituent, punctuation, comment delimiter.

**Related Terms:** Syntax Table, Character Class, Parsing

**Documentation:** See `doc/lispref/syntax.texi`

---

### Syntax Table `[Data]`
**Definition:** A char-table defining the syntactic role of each character for parsing and motion.

**Context:** Each major mode typically has its own syntax table. Affects forward-word, parse-partial-sexp, etc.

**Related Terms:** Char Table, Syntax Class, Major Mode, Parsing

**Documentation:** See `doc/lispref/syntax.texi`

---

## T

### T `[Lisp]`
**Definition:** The symbol representing the canonical true value, though any non-nil value is true.

**Context:** Preferred over other values when explicit true needed.

**Related Terms:** Nil, Boolean, True, False

**Documentation:** See `doc/lispref/` introduction

---

### Tab `[Core]`
**Definition:** The TAB character or key, typically performing indentation or completion.

**Context:** Can be literal character (ASCII 9) or trigger smart behavior via keybinding.

**Related Terms:** Indentation, Completion, Whitespace, Electric

---

### Tab Stop `[Core]`
**Definition:** Column positions where TAB key moves cursor in certain modes.

**Context:** Controlled by `tab-stop-list`. Used in text modes without smart indentation.

**Related Terms:** Tab, Column, Indentation

---

### Tagged Pointer `[Data]`
**Definition:** An encoding scheme where type information is stored in unused low bits of a pointer.

**Context:** Lisp_Object uses tagged pointers for efficient type representation.

**Related Terms:** Lisp_Object, Type Tag, Pointer, C Implementation

**Source:** See `src/lisp.h`

---

### Text Property `[Data]`
**Definition:** A property attached to a character or range of characters, stored with the text itself.

**Context:** Copied/deleted with text. Examples: face, font-lock-face, invisible, help-echo.

**Related Terms:** Overlay, Face, Display Property, Interval

**Documentation:** See `doc/lispref/text.texi`

---

### Theme `[Display]`
**Definition:** See Custom Theme.

**Related Terms:** Custom Theme, Face, Customization

---

### Thread `[System]`
**Definition:** An independent strand of Lisp execution, allowing concurrent computation.

**Context:** Limited support. Created with `make-thread`. Shares most state.

**Related Terms:** Concurrency, Async, Parallel, Mutex

**Documentation:** See `doc/lispref/threads.texi`

---

### Timer `[System]`
**Definition:** An object that schedules function execution after a delay or at regular intervals.

**Context:** Created with `run-with-timer` or `run-at-time`. Can be idle timers.

**Related Terms:** Idle Timer, Scheduling, Async, Callback

**Documentation:** See `doc/lispref/os.texi`

---

### Tooltip `[Display]`
**Definition:** A small temporary window displaying help text when hovering over UI elements.

**Context:** Triggered by help-echo text property or mode-line mouse hover.

**Related Terms:** Help Echo, Mouse, Display, Popup

**Documentation:** See `doc/lispref/display.texi`

---

### TRAMP `[Abbrev]`
**Definition:** Transparent Remote Access, Multiple Protocols - editing remote files as if local.

**Context:** Syntax: `/method:user@host:/path`. Supports ssh, sudo, docker, etc.

**Related Terms:** Remote File, File Handler, SSH, Network

**Documentation:** See `doc/misc/tramp.texi`

---

### Transient Mark Mode `[Core]`
**Definition:** A mode where the region is highlighted when the mark is active.

**Context:** Default in modern Emacs. Affects region-based commands.

**Related Terms:** Region, Mark, Active Region, Selection

**Documentation:** See Emacs manual

---

### Truncation `[Display]`
**Definition:** Cutting off long lines at window edge rather than wrapping to next screen line.

**Context:** Controlled by `truncate-lines`. Indicated by symbols in fringe.

**Related Terms:** Line Wrapping, Continuation Line, Fringe, Display

**Documentation:** See `doc/lispref/display.texi`

---

### TTY `[System]`
**Definition:** Text Terminal - a character-based terminal without graphical capabilities.

**Context:** Emacs runs in terminal or GUI. TTY has fewer display features.

**Related Terms:** Terminal, Frame, Display, GUI

**Documentation:** See `doc/lispref/frames.texi`

---

### Type Predicate `[Lisp]`
**Definition:** A function testing whether an object is of a specific type.

**Context:** Examples: `stringp`, `numberp`, `listp`, `bufferp`. Usually end in `-p`.

**Related Terms:** Predicate, Type, Type Check

**Documentation:** See `doc/lispref/objects.texi`

---

## U

### Undo `[Core]`
**Definition:** Reversing previous buffer modifications, restoring earlier state.

**Context:** `C-/` or `C-x u`. Undo itself can be undone. Tracked in buffer-undo-list.

**Related Terms:** Redo, Buffer-Undo-List, Modification, Revert

**Documentation:** See `doc/lispref/text.texi`

---

### Unibyte `[System]`
**Definition:** A buffer or string encoding where each byte represents one character.

**Context:** Legacy mode. Most buffers are multibyte. Useful for binary data.

**Related Terms:** Multibyte, Binary, Coding System, Character

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Unicode `[System]`
**Definition:** Universal character encoding standard, used internally by modern Emacs.

**Context:** Supports all world scripts. Characters are code points 0 to #x10FFFF.

**Related Terms:** UTF-8, Character, Code Point, Multibyte

**Documentation:** See `doc/lispref/nonascii.texi`

---

### Universal Argument `[Core]`
**Definition:** The prefix command `C-u` for passing numeric or symbolic arguments to commands.

**Context:** `C-u` = 4, `C-u C-u` = 16, `C-u 5` = 5, etc. Raw form `(4)`, `(16)`, etc.

**Related Terms:** Prefix Argument, C-u, Command, Argument

**Documentation:** See `doc/lispref/commands.texi`

---

### Unwind-Protect `[Lisp]`
**Definition:** A special form ensuring cleanup code runs even if protected code exits abnormally.

**Context:** Like try/finally. Critical for resource cleanup.

**Related Terms:** Exception, Cleanup, Finally, Non-Local Exit

**Documentation:** See `doc/lispref/control.texi`

---

### User Option `[Lisp]`
**Definition:** A customizable variable intended for user configuration.

**Context:** Defined with `defcustom`. Editable via Customize interface.

**Related Terms:** Defcustom, Customization, Variable, Configuration

**Documentation:** See `doc/lispref/customize.texi`

---

### UTF-8 `[System]`
**Definition:** Unicode Transformation Format, 8-bit - a variable-length character encoding for Unicode.

**Context:** Emacs's internal encoding. Default external encoding for files.

**Related Terms:** Unicode, Coding System, Multibyte, Encoding

**Documentation:** See `doc/lispref/nonascii.texi`

---

## V

### Value Cell `[Lisp]`
**Definition:** The slot in a symbol holding its variable value.

**Context:** Separate from function cell. Accessed with `symbol-value`.

**Related Terms:** Symbol, Function Cell, Variable, Binding

**Documentation:** See `doc/lispref/symbols.texi`

---

### Variable `[Lisp]`
**Definition:** A named location for storing a value, represented by a symbol.

**Context:** Can be global, buffer-local, let-bound, lexical, or dynamic.

**Related Terms:** Symbol, Binding, Value Cell, Let

**Documentation:** See `doc/lispref/variables.texi`

---

### Vector `[Data]`
**Definition:** A fixed-size array of Lisp objects, indexed by integers starting at 0.

**Context:** Created with `[...]` or `make-vector`. More efficient than lists for random access.

**Related Terms:** Array, Sequence, List, String

**Documentation:** See `doc/lispref/sequences.texi`

---

### Version Control `[System]`
**Definition:** System integration for tracking file changes with Git, SVN, etc.

**Context:** VC mode provides unified interface. `C-x v` prefix for VC commands.

**Related Terms:** Git, VCS, Diff, Commit

**Documentation:** See Emacs manual

---

### Visiting `[Core]`
**Definition:** Loading a file into a buffer for editing, establishing the buffer-file association.

**Context:** `C-x C-f` visits files. Buffer becomes associated with file for saving.

**Related Terms:** Find File, Buffer, File, Open

**Documentation:** See `doc/lispref/files.texi`

---

### Visual Line Mode `[Core]`
**Definition:** A minor mode providing word-wrapped display with motion commands treating screen lines as lines.

**Context:** `C-n`/`C-p` move by visual lines rather than logical lines.

**Related Terms:** Line Wrapping, Word Wrap, Continuation Line

**Documentation:** See Emacs manual

---

## W

### Widget `[Display]`
**Definition:** An interactive UI element in a buffer, like buttons, fields, or menus in the customization interface.

**Context:** Implemented by widget.el. Used extensively in Customize.

**Related Terms:** Button, Field, Customize, UI

**Documentation:** See `lisp/wid-edit.el`

---

### Widen `[Core]`
**Definition:** Removing narrowing restrictions to make the entire buffer accessible.

**Context:** Opposite of narrow. `C-x n w`.

**Related Terms:** Narrowing, Restriction, BEGV, ZV

**Documentation:** See `doc/lispref/positions.texi`

---

### Window `[Core]`
**Definition:** A tiled area within a frame displaying a buffer.

**Context:** Frames contain one or more non-overlapping windows. Each window displays exactly one buffer.

**Related Terms:** Frame, Buffer, Split, Selected Window

**Documentation:** See `doc/lispref/windows.texi`

---

### Window Configuration `[Core]`
**Definition:** A snapshot of window layout in a frame, including which buffers are displayed where.

**Context:** Saved with `current-window-configuration`, restored with `set-window-configuration`.

**Related Terms:** Window, Layout, Frame, Configuration

**Documentation:** See `doc/lispref/windows.texi`

---

### Window Parameter `[Display]`
**Definition:** A named property attached to a window for storing metadata or controlling behavior.

**Context:** Similar to frame parameters. Get/set with `window-parameter` / `set-window-parameter`.

**Related Terms:** Window, Frame Parameter, Metadata

**Documentation:** See `doc/lispref/windows.texi`

---

### Window Point `[Core]`
**Definition:** Each window's own point position in its displayed buffer.

**Context:** Separate from buffer's point. Restored when window redisplays buffer.

**Related Terms:** Point, Window, Buffer, Cursor

**Documentation:** See `doc/lispref/windows.texi`

---

### Window System `[Display]`
**Definition:** The graphical environment (X11, Wayland, Windows, macOS) providing GUI capabilities.

**Context:** Detected with `window-system` variable. Affects available features.

**Related Terms:** GUI, X11, Display, TTY, Frame

**Documentation:** See `doc/lispref/frames.texi`

---

### Window Tree `[Core]`
**Definition:** The hierarchical structure of window splits within a frame.

**Context:** Windows organized as binary tree of horizontal/vertical splits.

**Related Terms:** Window, Split, Frame, Layout

**Documentation:** See `doc/lispref/windows.texi`

---

### Word Wrap `[Display]`
**Definition:** Breaking lines at word boundaries rather than character boundaries for readability.

**Context:** Enabled by visual-line-mode or word-wrap variable.

**Related Terms:** Line Wrapping, Visual Line Mode, Fill

**Documentation:** See Emacs manual

---

## X

### X Window System `[Display]`
**Definition:** The traditional Unix/Linux graphical windowing system, commonly called X11 or X.

**Context:** One of several window systems Emacs supports. Provides GUI features.

**Related Terms:** Window System, GUI, Display, Frame

**Documentation:** See `doc/lispref/frames.texi`

---

### Xref `[Core]`
**Definition:** Cross-reference - a system for finding definitions and references of symbols.

**Context:** `M-.` finds definitions, `M-?` finds references. Backend-agnostic.

**Related Terms:** Tags, LSP, Navigation, Definition

**Documentation:** See Emacs manual

---

## Y

### Yank `[Abbrev]` `[Core]`
**Definition:** Inserting text from the kill ring (pasting).

**Context:** `C-y` yanks most recent kill. `M-y` cycles through kill ring.

**Related Terms:** Kill, Kill Ring, Paste, Clipboard

**Documentation:** See `doc/lispref/text.texi`

---

### Yank-Pop `[Core]`
**Definition:** After yanking, replacing the yanked text with an earlier kill from the kill ring.

**Context:** `M-y` after `C-y`. Cycles through kill ring history.

**Related Terms:** Yank, Kill Ring, Ring Buffer

**Documentation:** See Emacs manual

---

## Z

### Z / ZV `[Data]`
**Definition:** Buffer constants - Z is end position of buffer, ZV is end of accessible region (after narrowing).

**Context:** C macros. Z = (point-max) without narrowing, ZV = (point-max) with narrowing.

**Related Terms:** BEG, BEGV, Point, Narrowing, Gap Buffer

**Source:** See `src/buffer.h`

**Documentation:** See `doc/lispref/buffers.texi`

---

## Appendix: Common Patterns

### BEGV-to-ZV Pattern `[Data]`
**Definition:** The accessible region of a buffer, respecting narrowing restrictions.

**Context:** Many functions operate only within BEGV to ZV.

**Related Terms:** BEG, Z, Narrowing, Restriction

---

### Car-Cdr Recursion `[Lisp]`
**Definition:** The classic Lisp pattern of processing lists by operating on first element (car) and recursing on rest (cdr).

**Context:** Fundamental to list processing in Lisp.

**Related Terms:** Cons Cell, List, Recursion, Car, Cdr

---

### Save-Match-Data Pattern `[Lisp]`
**Definition:** Protecting match data around code that might perform regexp searches.

**Context:** Prevents unintended modification of match data from outer search.

**Related Terms:** Match Data, Regexp, Search, Unwinding

---

### With-Current-Buffer Pattern `[Lisp]`
**Definition:** Temporarily switching to another buffer for operations, then restoring original buffer.

**Context:** Safer than `set-buffer` for most purposes. Macro handles unwinding.

**Related Terms:** Current Buffer, Set-Buffer, Save-Excursion

---

## Document Statistics

**Total Terms:** 230

**Categories Distribution:**
- Core Concepts: 85 terms
- Lisp Concepts: 62 terms
- Data Structures: 28 terms
- Display System: 35 terms
- System Concepts: 30 terms
- Abbreviations: 20 terms

**Primary Documentation References:**
- `doc/lispref/` - Emacs Lisp Reference Manual
- `src/` - C source code and headers
- `lisp/` - Emacs Lisp implementation
- `doc/emacs/` - User manual
- `doc/misc/` - Specialized manuals

---

**Last Updated:** 2025-11-18

**Emacs Version:** GNU Emacs (development version)

**License:** GNU General Public License v3 or later
