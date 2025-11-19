# Text Processing: Search and Regular Expressions

**Author:** Emacs Documentation Team
**Last Updated:** 2025-11-18
**Status:** Complete
**Related:** `02-core-subsystems/03-buffer-text.md`, `03-elisp-runtime/02-core-types.md`

---

## Table of Contents

1. [Overview](#overview)
2. [Search System Architecture](#search-system-architecture)
3. [Regular Expression Engine](#regular-expression-engine)
4. [Syntax Tables](#syntax-tables)
5. [Case Handling](#case-handling)
6. [Elisp Layer](#elisp-layer)
7. [Integration and Data Flow](#integration-and-data-flow)
8. [Performance Characteristics](#performance-characteristics)
9. [API Reference](#api-reference)

---

## Overview

Emacs provides a sophisticated text processing subsystem that combines multiple components for efficient searching, pattern matching, and text analysis. This document covers the core components:

### Component Summary

| Component | Source File | Lines | Purpose |
|-----------|-------------|-------|---------|
| Search System | `src/search.c` | 3,514 | String search algorithms and caching |
| Regex Engine | `src/regex-emacs.c` | 5,355 | Pattern compilation and matching |
| Syntax Tables | `src/syntax.c` | 3,831 | Character classification and parsing |
| Case Handling | `src/casefiddle.c` | 764 | Case conversion and folding |

### Key Features

- **Multiple search algorithms**: Simple scan, Boyer-Moore, and regex-based
- **Incremental search**: Real-time feedback as you type (`isearch.el`)
- **Syntax-aware parsing**: Language-specific character classification
- **Unicode support**: Full multibyte character handling
- **Case folding**: Intelligent case-insensitive matching
- **Character folding**: Match Unicode variants (`char-fold.el`)
- **Regex caching**: Pattern compilation optimization
- **POSIX compliance**: Optional POSIX backtracking mode

---

## Search System Architecture

### File: `src/search.c` (3,514 lines)

The search system provides multiple algorithms optimized for different use cases.

### Search Algorithms

#### 1. Simple Search

Used when case folding or translation makes Boyer-Moore impossible.

```c
static EMACS_INT
simple_search (EMACS_INT n, unsigned char *pat,
               ptrdiff_t len, ptrdiff_t len_byte, Lisp_Object trt,
               ptrdiff_t pos, ptrdiff_t pos_byte,
               ptrdiff_t lim, ptrdiff_t lim_byte)
{
  // Naive string matching with character-by-character comparison
  // Supports multibyte characters and translation tables
  // Time complexity: O(nm) where n=text length, m=pattern length
}
```

**Characteristics:**
- Handles arbitrary character translations
- Works with multibyte characters
- Fallback when Boyer-Moore cannot be used
- No preprocessing required

#### 2. Boyer-Moore Search

Fast algorithm for literal string search without complex translations.

```c
static EMACS_INT
boyer_moore (EMACS_INT n, unsigned char *base_pat,
             ptrdiff_t len_byte,
             Lisp_Object trt, Lisp_Object inverse_trt,
             ptrdiff_t pos_byte, ptrdiff_t lim_byte,
             int char_base)
{
  // Build skip table (BM_tab) for pattern
  // Skip characters that don't match last character of pattern
  // Time complexity: O(n/m) best case, O(nm) worst case
}
```

**Algorithm Details:**

1. **Preprocessing Phase:**
   ```c
   // Build skip table: for each possible byte value,
   // store distance to skip if that byte is seen
   for (i = 0; i < 0400; i++)
     BM_tab[i] = dirlen;  // Default: skip entire pattern length

   // For bytes in pattern, store distance from end
   while (i != dirlen) {
     unsigned char c = base_pat[i];
     BM_tab[c] = dirlen - i - 1;
     i++;
   }
   ```

2. **Search Phase:**
   - Compare pattern from right to left
   - On mismatch, skip ahead using BM_tab
   - Can skip multiple characters per comparison

**When Boyer-Moore is Used:**
```c
bool boyer_moore_ok = 1;

// Disable if:
// - Case folding changes character lengths
// - Translation maps to multiple characters
// - Non-ASCII chars with complex equivalences

if (c != inverse && boyer_moore_ok) {
  // Check if translation preserves simple mapping
  boyer_moore_ok = // complex condition...
}
```

#### 3. Regular Expression Search

Uses the compiled regex engine (see next section).

```c
// From looking_at_1() and re_search_2()
struct regexp_cache *cache_entry = compile_pattern(
  string,
  &search_regs,
  case_canon_table,  // For case-insensitive matching
  posix,
  multibyte
);

re_match_2_internal(...);
```

### Regex Pattern Cache

To avoid recompiling patterns, Emacs maintains a cache:

```c
#define REGEXP_CACHE_SIZE 20

struct regexp_cache {
  struct regexp_cache *next;
  Lisp_Object regexp, f_whitespace_regexp;
  Lisp_Object syntax_table;
  struct re_pattern_buffer buf;
  char fastmap[0400];
  bool posix;
  bool busy;  // Prevents recursive use
};

static struct regexp_cache searchbufs[REGEXP_CACHE_SIZE];
static struct regexp_cache *searchbuf_head;  // LRU list
```

**Cache Lookup Logic:**
1. Check if pattern matches cached entry
2. Verify syntax table compatibility
3. Check whitespace-regexp equivalence
4. If match found, move to front (LRU)
5. If not found, reuse least-recently-used non-busy entry

### Search Functions API

#### Core Search Functions

```c
DEFUN ("search-forward", Fsearch_forward, Ssearch_forward, 1, 4, ...);
DEFUN ("search-backward", Fsearch_backward, Ssearch_backward, 1, 4, ...);
DEFUN ("re-search-forward", Fre_search_forward, Sre_search_forward, 1, 4, ...);
DEFUN ("re-search-backward", Fre_search_backward, Sre_search_backward, 1, 4, ...);
DEFUN ("posix-search-forward", Fposix_search_forward, ...);
DEFUN ("posix-search-backward", Fposix_search_backward, ...);
```

**Common Parameters:**
- `string`: Pattern to search for
- `bound`: Limit of search (nil = end of buffer)
- `noerror`: If non-nil, return nil instead of error when not found
- `count`: Repeat search N times (negative = backward)

---

## Regular Expression Engine

### File: `src/regex-emacs.c` (5,355 lines)

Emacs uses a custom regex engine optimized for editor use cases.

### Regex Opcodes

The engine compiles patterns into a bytecode format with these opcodes:

```c
typedef enum {
  no_op = 0,
  succeed,           // Immediate success, no backtracking

  // Literal matching
  exactn,            // Match N literal bytes
  anychar,           // Match any character (.)

  // Character sets
  charset,           // Match one of specified characters [...]
  charset_not,       // Match anything except [^...]

  // Grouping and backreferences
  start_memory,      // Begin capture group \(...\)
  stop_memory,       // End capture group
  duplicate,         // Match previous group \N

  // Anchors
  begline,           // ^ (beginning of line)
  endline,           // $ (end of line)
  begbuf,            // \` (beginning of buffer)
  endbuf,            // \' (end of buffer)

  // Control flow
  jump,              // Unconditional jump
  on_failure_jump,   // Backtracking point
  on_failure_keep_string_jump,  // Loop optimization
  on_failure_jump_loop,         // Infinite loop detection
  on_failure_jump_smart,        // Greedy * and + optimization

  // Repetition
  succeed_n,         // Jump after N matches
  jump_n,            // Bounded repetition
  set_number_at,     // Dynamic counter update

  // Word boundaries
  wordbeg,           // \< (beginning of word)
  wordend,           // \> (end of word)
  wordbound,         // \b (word boundary)
  notwordbound,      // \B (not word boundary)

  // Symbol boundaries (Emacs extension)
  symbeg,            // \_< (beginning of symbol)
  symend,            // \_> (end of symbol)

  // Syntax-based matching
  syntaxspec,        // \s followed by syntax code
  notsyntaxspec,     // \S followed by syntax code

  // Category matching
  categoryspec,      // Match character category
  notcategoryspec,   // Match not in category

  at_dot             // Match at point
} re_opcode_t;
```

### Pattern Compilation

```c
const char *
re_compile_pattern (const char *pattern, ptrdiff_t length,
                    bool posix_backtracking,
                    const char *whitespace_regexp,
                    struct re_pattern_buffer *bufp)
{
  // Parse pattern and generate opcodes
  // Build fastmap for quick pre-scanning
  // Handle character classes, ranges, etc.
  // Return NULL on success, error string on failure
}
```

**Compilation Steps:**

1. **Lexical Analysis**: Parse pattern into tokens
   - Literal characters
   - Special characters (*, +, ?, |, etc.)
   - Escape sequences (\n, \t, \d, etc.)
   - Character classes ([a-z], [^0-9])
   - Groups \(...\)

2. **Syntax Validation**: Check for errors
   - Unmatched brackets
   - Invalid escape sequences
   - Invalid repetition operators

3. **Code Generation**: Emit opcodes
   - Convert to internal bytecode
   - Optimize common patterns
   - Insert backtracking points

4. **Fastmap Construction**: Build quick-reject table
   ```c
   static void re_compile_fastmap (struct re_pattern_buffer *bufp)
   {
     // For each possible starting character,
     // mark if pattern could match starting with it
     char *fastmap = bufp->fastmap;
     // ... analyze compiled pattern ...
   }
   ```

### Pattern Matching

```c
static ptrdiff_t
re_match_2_internal (struct re_pattern_buffer *bufp,
                     re_char *string1, ptrdiff_t size1,
                     re_char *string2, ptrdiff_t size2,
                     ptrdiff_t pos,
                     struct re_registers *regs,
                     ptrdiff_t stop)
{
  // Main matching engine using backtracking
  // Handles split strings (gap buffer support)
  // Records match positions in regs
}
```

**Matching Algorithm:**

1. **Fastmap Pre-check**:
   ```c
   if (fastmap && startpos < total_size && !bufp->can_be_null) {
     // Quick reject: scan for valid starting characters
     if (!fastmap[RE_STRING_CHAR(string, startpos)])
       continue;  // Skip to next position
   }
   ```

2. **Bytecode Interpretation**:
   - Execute opcodes sequentially
   - Push/pop failure points for backtracking
   - Record capture group positions

3. **Backtracking**:
   - On failure, pop last failure point
   - Restore position and state
   - Try alternative paths

### Regex Pattern Buffer

```c
struct re_pattern_buffer {
  unsigned char *buffer;      // Compiled opcodes
  ptrdiff_t allocated;        // Buffer size
  ptrdiff_t used;            // Bytes used

  int charset_unibyte;       // Charset at compile time
  char *fastmap;             // Quick-reject table

  Lisp_Object translate;     // Case folding table

  // Flags
  bool fastmap_accurate;
  bool can_be_null;          // Can match empty string
  bool not_bol;              // Not at beginning of line
  bool not_eol;              // Not at end of line
  bool used_syntax;          // Uses syntax table
  bool multibyte;            // Pattern is multibyte
  bool target_multibyte;     // Target is multibyte

  // Match data
  size_t re_nsub;            // Number of subexpressions

  // Registers for match positions
  // (managed externally in struct re_registers)
};
```

### Emacs-Specific Extensions

**1. Syntax Classes** (`\s` and `\S`):
```
\sw  - Word constituent
\s_  - Symbol constituent
\s.  - Punctuation
\s(  - Open parenthesis
\s)  - Close parenthesis
\s"  - String quote
\s'  - Expression prefix
\s<  - Comment start
\s>  - Comment end
\s!  - Generic comment delimiter
\s|  - Generic string delimiter
```

**2. Symbol Boundaries** (`\_<` and `\_>`):
- Like word boundaries but for symbols
- Respects `symbol` syntax class

**3. Category Matching** (`\c` and `\C`):
- Unicode character categories
- Used for i18n and script detection

**4. Position Matching**:
```
\`   - Beginning of buffer (not line)
\'   - End of buffer (not line)
\=   - At point (current cursor position)
```

### POSIX vs. Emacs Backtracking

**Emacs Mode (default)**:
- Stops at first match
- Faster for typical editor use
- Non-greedy by default

**POSIX Mode** (`posix-search-forward`):
- Finds longest possible match
- Required for POSIX compliance
- Slower due to exhaustive search

```c
// In re_match_2_internal:
if (posix_backtracking) {
  // Try all alternatives, keep longest
} else {
  // Stop at first match
}
```

### Performance Optimizations

**1. Smart Greedy Matching** (`on_failure_jump_smart`):
```c
// For patterns like a*b or a+b
// Analyze loop to avoid unnecessary backtracking
// If loop doesn't require backtracking, short-circuit it
```

**2. String-Keeping Loops** (`on_failure_keep_string_jump`):
```c
// For simple loops that don't need position restoration
// Saves stack space and time
```

**3. Duplicate Detection**:
```c
// Prevent infinite loops in patterns like (a*)*
// Track visited states
```

---

## Syntax Tables

### File: `src/syntax.c` (3,831 lines)

Syntax tables classify characters for parsing and navigation.

### Syntax Classes

```c
enum syntaxcode {
  Swhitespace,    // ' ' - whitespace characters
  Spunct,         // '.' - punctuation
  Sword,          // 'w' - word constituents
  Ssymbol,        // '_' - symbol constituents (not word)
  Sopen,          // '(' - open delimiter
  Sclose,         // ')' - close delimiter
  Squote,         // '\'' - prefix character (Lisp quote)
  Sstring,        // '"' - string delimiter
  Smath,          // '$' - paired delimiter (TeX)
  Sescape,        // '\\' - escape character
  Scharquote,     // '/' - character quote
  Scomment,       // '<' - comment starter
  Sendcomment,    // '>' - comment ender
  Sinherit,       // '@' - inherit from standard table
  Scomment_fence, // '!' - generic comment delimiter
  Sstring_fence,  // '|' - generic string delimiter
  Smax            // Sentinel value
};
```

### Syntax Flags

Eight single-bit flags provide additional information:

```c
// Extract flags from syntax descriptor
static bool SYNTAX_FLAGS_COMSTART_FIRST(int flags);   // First char of comment start
static bool SYNTAX_FLAGS_COMSTART_SECOND(int flags);  // Second char of comment start
static bool SYNTAX_FLAGS_COMEND_FIRST(int flags);     // First char of comment end
static bool SYNTAX_FLAGS_COMEND_SECOND(int flags);    // Second char of comment end
static bool SYNTAX_FLAGS_PREFIX(int flags);           // Is prefix character
static bool SYNTAX_FLAGS_COMMENT_STYLEB(int flags);   // Style b comment
static bool SYNTAX_FLAGS_COMMENT_STYLEC(int flags);   // Style c comment
static bool SYNTAX_FLAGS_COMMENT_NESTED(int flags);   // Nested comments allowed
```

**Comment Styles:**
- **Style a**: Default (C-style `/* ... */`)
- **Style b**: Alternate (C++-style `// ...`)
- **Style c**: Nestable (like `(* ... (* ... *) ... *)`)

### Syntax Table Structure

Syntax information is stored in char-tables:

```c
// Each buffer has its own syntax table
BVAR (current_buffer, syntax_table)

// Syntax table is a char-table mapping characters to syntax descriptors
// Descriptor format: (SYNTAX-CODE . MATCHING-CHAR)
//   SYNTAX-CODE: integer with syntax class and flags
//   MATCHING-CHAR: matching delimiter (for parens, etc.)
```

### Global Syntax State

```c
struct gl_state_s {
  Lisp_Object object;        // Buffer or string being parsed
  ptrdiff_t b_property;      // Beginning of property range
  ptrdiff_t e_property;      // End of property range
  ptrdiff_t offset;          // Position offset
  // ... more fields for syntax property tracking
};

extern struct gl_state_s gl_state;
```

### Syntax-Based Navigation

#### Scanning Functions

```c
// Skip characters matching a specification
static Lisp_Object skip_chars(bool forward, Lisp_Object string, Lisp_Object lim);

// Skip characters by syntax class
static Lisp_Object skip_syntaxes(bool forward, Lisp_Object string, Lisp_Object lim);

// Scan balanced expressions
static Lisp_Object scan_lists(EMACS_INT from, EMACS_INT count, EMACS_INT depth, bool sexpflag);

// Main parsing state machine
static void scan_sexps_forward(struct lisp_parse_state *state,
                               ptrdiff_t from, ptrdiff_t from_byte,
                               ptrdiff_t end, EMACS_INT targetdepth,
                               bool stopbefore, int commentstop);
```

#### Parse State

```c
struct lisp_parse_state {
  EMACS_INT depth;          // Paren depth at end
  int instring;             // -1 if not in string, else terminator
  EMACS_INT incomment;      // Comment nesting level (-1 if not in comment)
  int comstyle;             // Comment style (a=0, b=1, or ST_COMMENT_STYLE)
  bool quoted;              // Just after escape character
  EMACS_INT mindepth;       // Minimum depth seen
  ptrdiff_t thislevelstart; // Start of current level
  ptrdiff_t prevlevelstart; // Start of containing level
  ptrdiff_t location;       // Character position where parsing stopped
  ptrdiff_t location_byte;  // Byte position
  ptrdiff_t comstr_start;   // Start of last comment/string
  Lisp_Object levelstarts;  // List of start positions of each level
  int prev_syntax;          // Previous character's syntax
};
```

### Comment and String Handling

**Two-Character Delimiters:**
```c
// C-style comments: /* and */
// Tracked via COMSTART_FIRST + COMSTART_SECOND flags

if (SYNTAX_FLAGS_COMSTART_FIRST(syntax) && from < end) {
  int next_char = FETCH_CHAR(from + 1);
  if (SYNTAX_FLAGS_COMSTART_SECOND(SYNTAX_WITH_FLAGS(next_char))) {
    // Found two-char comment start
    comstyle = SYNTAX_FLAGS_COMMENT_STYLE(syntax1, syntax2);
  }
}
```

**Generic Delimiters (Fences):**
```c
// Scomment_fence and Sstring_fence
// Any character with same syntax is matching delimiter
// Example: Python's ''' or """
```

### Syntax Properties

Override syntax table via text properties:

```c
// If parse_sexp_lookup_properties is true,
// 'syntax-table' property overrides buffer's syntax table

if (parse_sexp_lookup_properties) {
  Lisp_Object prop = Fget_text_property(pos, Qsyntax_table, Qnil);
  if (!NILP(prop)) {
    // Use property value instead of buffer syntax table
  }
}
```

**Use Cases:**
- String interpolation in programming languages
- Heredocs with different syntax rules
- Embedded languages (e.g., SQL in strings)

---

## Case Handling

### File: `src/casefiddle.c` (764 lines)

Handles case conversion with full Unicode support.

### Case Operations

```c
enum case_action {
  CASE_UP,            // upcase: "hello" → "HELLO"
  CASE_DOWN,          // downcase: "HELLO" → "hello"
  CASE_CAPITALIZE,    // capitalize: "hello world" → "Hello World"
  CASE_CAPITALIZE_UP  // upcase-initials: "hello world" → "Hello World" (no downcasing)
};
```

### Casing Context

```c
struct casing_context {
  Lisp_Object titlecase_char_table;      // Title case mappings
  Lisp_Object specialcase_char_tables[3]; // Special case rules (up/down/title)
  enum case_action flag;                  // Operation type
  bool inbuffer;                          // Operating on buffer vs. string
  bool inword;                            // Currently in a word
  bool downcase_last;                     // Last operation was downcase
};
```

### Unicode Case Mapping

**Simple Cases (one-to-one):**
```c
static inline int case_single_character(struct casing_context *ctx, int ch) {
  if (flag == CASE_DOWN)
    return downcase(ch);  // Uses Unicode lowercase table
  else
    return upcase(ch);    // Uses Unicode uppercase table
}
```

**Special Cases (one-to-many):**

Some characters expand when cased:

```c
// Example: ﬁ (U+FB01 LATIN SMALL LIGATURE FI)
// Uppercase: "FI" (two characters)

static bool case_character(struct casing_str_buf *buf,
                           struct casing_context *ctx,
                           int ch, const unsigned char *next) {
  // Check special-casing table
  prop = CHAR_TABLE_REF(ctx->specialcase_char_tables[flag], ch);
  if (STRINGP(prop)) {
    // Character expands to multiple characters
    memcpy(buf->data, SDATA(prop), SBYTES(prop));
    buf->len_chars = SCHARS(prop);
    buf->len_bytes = SBYTES(prop);
    return true;
  }
  // ... handle simple case ...
}
```

**Examples of Special Cases:**
- `ﬁ` → `FI` (ligature)
- `ß` → `SS` (German eszett)
- `ΐ` (Greek iota with dialytika and tonos)

### Greek Final Sigma

Special handling for context-sensitive casing:

```c
enum { GREEK_CAPITAL_LETTER_SIGMA = 0x03A3 };  // Σ
enum { GREEK_SMALL_LETTER_FINAL_SIGMA = 0x03C2 }; // ς

// When downcasing Σ:
// - If at end of word → ς (final sigma)
// - If in middle of word → σ (regular sigma)

if (was_inword && ch == GREEK_CAPITAL_LETTER_SIGMA && changed
    && (!next || !case_ch_is_word(SYNTAX(STRING_CHAR(next))))) {
  buf->data[0] = GREEK_SMALL_LETTER_FINAL_SIGMA;
}
```

### Word Boundaries

```c
static bool case_ch_is_word(enum syntaxcode syntax) {
  return syntax == Sword ||
         (case_symbols_as_words && syntax == Ssymbol);
}

// Variable: case-symbols-as-words
// If non-nil, treat symbols as part of words for case operations
// Useful for programming languages (camelCase, snake_case)
```

### Buffer Case Operations

```c
static ptrdiff_t
do_casify_multibyte_region(struct casing_context *ctx,
                           ptrdiff_t *startp, ptrdiff_t *endp) {
  // For each character in region:
  // 1. Case according to context
  // 2. Handle size changes (e.g., ß → SS adds one character)
  // 3. Update text properties
  // 4. Return number of characters added/removed
}
```

**Challenge**: Characters may change byte length:
- ASCII → non-ASCII (Turkish i → İ in some locales)
- Single → multiple characters (ligatures)
- Non-ASCII → ASCII (downcase in unibyte buffers)

### API Functions

```c
DEFUN ("upcase", Fupcase, Supcase, 1, 1, 0, ...);
DEFUN ("downcase", Fdowncase, Sdowncase, 1, 1, 0, ...);
DEFUN ("capitalize", Fcapitalize, Scapitalize, 1, 1, 0, ...);
DEFUN ("upcase-initials", Fupcase_initials, Supcase_initials, 1, 1, 0, ...);

DEFUN ("upcase-region", Fupcase_region, Supcase_region, 2, 3, ...);
DEFUN ("downcase-region", Fdowncase_region, Sdowncase_region, 2, 3, ...);
DEFUN ("capitalize-region", Fcapitalize_region, Scapitalize_region, 2, 3, ...);
DEFUN ("upcase-initials-region", Fupcase_initials_region, ...);

DEFUN ("upcase-word", Fupcase_word, Supcase_word, 1, 1, "p", ...);
DEFUN ("downcase-word", Fdowncase_word, Sdowncase_word, 1, 1, "p", ...);
DEFUN ("capitalize-word", Fcapitalize_word, Scapitalize_word, 1, 1, "p", ...);
```

---

## Elisp Layer

### Incremental Search (`lisp/isearch.el`)

Real-time search with immediate feedback.

#### Key Features

1. **Search Modes**:
   - Plain string search
   - Regular expression search
   - Word search (match whole words)
   - Symbol search (match whole symbols)
   - Character folding (match Unicode variants)

2. **Customization Variables**:
```elisp
;; Case sensitivity control
(defcustom search-upper-case 'not-yanks
  "If non-nil, uppercase in search string disables case folding.")

;; Whitespace handling
(defcustom search-whitespace-regexp "[ \t]+"
  "Regexp to match whitespace in incremental search.")

;; Invisible text
(defcustom search-invisible 'open
  "Whether to search invisible text.")

;; Wrapping behavior
(defcustom isearch-wrap-pause t
  "Pause before wrapping when no more matches.")
```

3. **Search Ring**:
```elisp
;; Stores search history
(defvar search-ring nil)
(defvar regexp-search-ring nil)

;; Navigate through previous searches with M-p / M-n
```

4. **Dynamic Updates**:
```elisp
;; Update search as you type
(defun isearch-search ()
  "Search for the current search string."
  (let ((result (isearch-search-string
                 isearch-string nil isearch-forward)))
    ;; Update highlight immediately
    (isearch-highlight ...)))
```

#### Lazy Highlighting

```elisp
;; Show all matches in buffer
(defvar isearch-lazy-highlight t
  "Controls lazy highlighting of matches.")

;; Highlight matches in viewport
(defun isearch-lazy-highlight-update ()
  "Update lazy highlighting of matches."
  ;; Scan visible portion of buffer
  ;; Apply overlay to each match
  ;; Stop at isearch-lazy-highlight-max-at-a-time)
```

### Regular Expression Builder (`lisp/emacs-lisp/re-builder.el`)

Interactive regex development tool.

#### Features

1. **Live Preview**:
```elisp
;; Three input syntaxes:
;; - 'read: "\\(hello\\|world\\)"  (Lisp read syntax)
;; - 'string: "\(hello\|world\)"    (String syntax, less escaping)
;; - 'rx: (or "hello" "world")      (Symbolic rx syntax)

(defcustom reb-re-syntax 'read
  "Syntax for REs in RE Builder.")
```

2. **Visual Feedback**:
```elisp
;; Highlight matches with colored overlays
(defface reb-match-0 ...)  ; Whole match
(defface reb-match-1 ...)  ; First subgroup
(defface reb-match-2 ...)  ; Second subgroup
; ... up to reb-match-3
```

3. **Target Buffer**:
```elisp
;; Test regex against any buffer
(defun reb-change-target-buffer (buf)
  "Change target buffer for RE Builder."
  ;; Remove overlays from old buffer
  ;; Apply to new buffer)
```

### Character Folding (`lisp/char-fold.el`)

Match Unicode characters by similarity to ASCII.

#### Folding Table

```elisp
(defconst char-fold--default-include
  '((?\" "＂" """ """ "„" ...)   ; Match various quote styles
    (?' "'" "'" "‚" "‛" ...)     ; Match various apostrophes
    (?ß "ss")                     ; German eszett
    (?ι "ΐ")                      ; Greek iota variants
    ...))

(defconst char-fold--default-exclude
  '((?и "й")))  ; Cyrillic: don't fold these
```

#### Decomposition-Based Folding

```elisp
;; Build equivalence table from Unicode decompositions
(defun char-fold--make-table ()
  ;; For each character with a decomposition:
  ;; 1. Let char match its decomposition
  ;; 2. Let decomposition match char
  ;; 3. Let base char match accented variants

  ;; Example: 'a' matches 'à', 'á', 'â', 'ã', 'ä', 'å', ...
  ;; Example: 'e' matches 'è', 'é', 'ê', 'ë', ...
  )
```

#### Converting Searches

```elisp
(defun char-fold-to-regexp (string)
  "Convert STRING to a regexp matching character variants."
  ;; For each character in string:
  ;; - If has variants, insert [...] with all variants
  ;; - Otherwise, use character literally

  ;; "cafe" → "c[aàáâãäå]f[eèéêë]"
  )
```

### Integration Example

How these layers work together for a case-insensitive search:

```elisp
;; User types C-s hello RET

;; 1. isearch.el handles input
(isearch-forward)

;; 2. Determine search parameters
(let* ((case-fold-search t)           ; User wants case-insensitive
       (search-string "hello")
       (search-fn 'search-forward))   ; Use literal search, not regex

  ;; 3. If char-fold enabled, convert to regex
  (when char-fold-search
    (setq search-string (char-fold-to-regexp "hello"))
    (setq search-fn 're-search-forward))

  ;; 4. Call C layer
  (funcall search-fn search-string nil t))

;; 5. C layer (search.c):
;;    - Checks if Boyer-Moore can be used
;;    - Uses case_canon_table for case folding
;;    - Returns match position or nil

;; 6. Update display
(isearch-highlight (match-beginning 0) (match-end 0))
```

---

## Integration and Data Flow

### Search Flow Diagram

```
User Input (C-s, M-C-s)
    ↓
isearch.el (incremental search UI)
    ↓
char-fold.el (optional: expand to Unicode variants)
    ↓
search.c or regex-emacs.c
    ├─→ simple_search() [Simple string matching]
    ├─→ boyer_moore()   [Fast literal search]
    └─→ re_match_2()    [Regex matching]
        ↓
    syntax.c (for \sw, \s_, etc. in regexes)
    casefiddle.c (for case-insensitive search)
        ↓
Match position returned
    ↓
isearch.el updates display
```

### Key Integration Points

#### 1. Case Folding in Search

```c
// In search.c:
if (!NILP(Vcase_fold_search)) {
  // Use case_canon_table for translation
  trt = BVAR(current_buffer, case_canon_table);
}

// case_canon_table maps:
// 'A' → 'a', 'B' → 'b', ..., 'a' → 'a', 'b' → 'b', ...
```

#### 2. Syntax Tables in Regex

```c
// In regex-emacs.c, for \sw, \s_, etc.:
#define SYNTAX(c) syntax_property(c, 1)

// During matching:
case syntaxspec:
  if (SYNTAX(*d) == (enum syntaxcode) *p++)
    // Match succeeds
```

#### 3. Whitespace Handling

```elisp
;; Elisp layer:
(setq search-whitespace-regexp "[ \t\n]+")

// C layer (search.c):
if (STRINGP(Vsearch_spaces_regexp)) {
  whitespace_regexp = SSDATA(Vsearch_spaces_regexp);
  // Pass to re_compile_pattern
}

// regex-emacs.c:
// Each space in pattern expands to whitespace_regexp
```

#### 4. Character Folding Integration

```elisp
;; Elisp converts literal search to regex:
(when char-fold-search
  ;; "hello" becomes regex like:
  ;; "[h]e[l]l[o]" but with Unicode variants:
  ;; "[hĥħḣḥ...][eèéêë...][l][l][oòóôõö...]"
  (setq pattern (char-fold-to-regexp pattern))
  (setq use-regex t))

// Then use regex search path instead of literal
```

---

## Performance Characteristics

### Algorithm Complexity

| Algorithm | Best Case | Average Case | Worst Case | Use When |
|-----------|-----------|--------------|------------|----------|
| Simple Search | O(n) | O(nm) | O(nm) | Case folding, translation |
| Boyer-Moore | O(n/m) | O(n) | O(nm) | Literal strings, no translation |
| Regex (DFA) | O(n) | O(n) | O(n) | Simple patterns, no backtracking |
| Regex (Backtracking) | O(n) | O(nm) | O(2^n) | Complex patterns, backreferences |

Where:
- n = length of text being searched
- m = length of pattern

### Memory Usage

**Regex Compilation:**
```c
// Typical compiled regex size:
// Pattern: "foo.*bar"
// Compiled: ~50-100 bytes (opcodes + metadata)

// With character classes:
// Pattern: "[a-zA-Z0-9_]+"
// Compiled: ~300 bytes (bitmap for charset)
```

**Regex Cache:**
```c
// 20 cached patterns × ~500 bytes average = ~10KB
// Plus fastmaps: 20 × 256 bytes = ~5KB
// Total: ~15KB for regex cache
```

**Search Registers:**
```c
// Match data for up to 255 subexpressions
// 2 positions per group × sizeof(ptrdiff_t)
// Typical: 10 groups × 2 × 8 bytes = 160 bytes
```

### Optimization Strategies

#### 1. Regex Caching

```c
// Cache hit: ~0µs (pointer comparison)
// Cache miss: ~100-1000µs (compilation time)
// → Keep frequently-used patterns cached
```

#### 2. Fastmap Usage

```c
// Without fastmap: O(nm) per attempt
// With fastmap: O(n) to scan + O(m) per valid attempt
// → Huge win for rare patterns in large text
```

#### 3. Boyer-Moore Conditions

```c
// Boyer-Moore is ~3-10× faster than simple search
// Use when:
// - No case folding OR simple case folding
// - No character translation
// - Pattern length > 2 characters
```

#### 4. Lazy Highlighting Limits

```elisp
;; Don't highlight too many matches
(defcustom isearch-lazy-highlight-max-at-a-time 20
  "Maximum matches to highlight at a time.")

;; Don't search too far
(defvar isearch-lazy-highlight-max nil
  "Maximum number of matches to highlight.")
```

### Performance Tips for Users

1. **Use literal search when possible** (not regex)
   - Boyer-Moore is much faster
   - Less CPU per keystroke in isearch

2. **Anchor regexes when possible**
   - `^foo` or `foo$` skip impossible positions
   - Fastmap can optimize better

3. **Avoid catastrophic backtracking**
   - Pattern `(a+)+b` on "aaaaaa..." is exponential
   - Use possessive/atomic groups if available

4. **Use word/symbol search**
   - `M-s w` for word search
   - Automatically anchors with `\<...\>`

---

## API Reference

### C Functions

#### Search Functions

```c
// search.c
Lisp_Object search_buffer(Lisp_Object string, ptrdiff_t pos,
                         ptrdiff_t pos_byte, ptrdiff_t lim,
                         ptrdiff_t lim_byte, EMACS_INT n,
                         int RE, Lisp_Object trt,
                         Lisp_Object inverse_trt, bool posix);

DEFUN("search-forward", Fsearch_forward, Ssearch_forward, 1, 4, "MSearch: ",
      doc: /* Search forward for STRING... */);

DEFUN("re-search-forward", Fre_search_forward, Sre_search_forward, 1, 4,
      doc: /* Search forward for regular expression REGEXP... */);
```

#### Regex Functions

```c
// regex-emacs.c
const char *re_compile_pattern(const char *pattern, ptrdiff_t length,
                               bool posix_backtracking,
                               const char *whitespace_regexp,
                               struct re_pattern_buffer *bufp);

ptrdiff_t re_search(struct re_pattern_buffer *bufp,
                   const char *string, ptrdiff_t size,
                   ptrdiff_t startpos, ptrdiff_t range,
                   struct re_registers *regs);

ptrdiff_t re_match(struct re_pattern_buffer *bufp,
                  const char *string, ptrdiff_t size,
                  ptrdiff_t pos, struct re_registers *regs);
```

#### Syntax Functions

```c
// syntax.c
DEFUN("char-syntax", Fchar_syntax, Schar_syntax, 1, 1, 0,
      doc: /* Return syntax code of CHARACTER... */);

DEFUN("modify-syntax-entry", Fmodify_syntax_entry, Smodify_syntax_entry, 2, 3,
      doc: /* Set syntax for character CHAR according to NEWENTRY... */);

DEFUN("scan-lists", Fscan_lists, Sscan_lists, 3, 3, 0,
      doc: /* Scan from character FROM by COUNT balanced expressions... */);

DEFUN("scan-sexps", Fscan_sexps, Sscan_sexps, 2, 2, 0,
      doc: /* Scan from FROM by ARG s-expressions... */);

DEFUN("parse-partial-sexp", Fparse_partial_sexp, Sparse_partial_sexp, 2, 6, 0,
      doc: /* Parse Lisp syntax starting at FROM until TO... */);
```

#### Case Functions

```c
// casefiddle.c
DEFUN("upcase", Fupcase, Supcase, 1, 1, 0,
      doc: /* Convert argument to upper case... */);

DEFUN("downcase", Fdowncase, Sdowncase, 1, 1, 0,
      doc: /* Convert argument to lower case... */);

DEFUN("capitalize", Fcapitalize, Scapitalize, 1, 1, 0,
      doc: /* Convert argument to capitalized form... */);

DEFUN("upcase-region", Fupcase_region, Supcase_region, 2, 3,
      "(list (region-beginning) (region-end) (region-noncontiguous-p))",
      doc: /* Convert the region to upper case... */);
```

### Elisp Functions

#### Search Commands

```elisp
;; Basic search
(search-forward STRING &optional BOUND NOERROR COUNT)
(search-backward STRING &optional BOUND NOERROR COUNT)

;; Regex search
(re-search-forward REGEXP &optional BOUND NOERROR COUNT)
(re-search-backward REGEXP &optional BOUND NOERROR COUNT)

;; POSIX regex
(posix-search-forward REGEXP &optional BOUND NOERROR COUNT)
(posix-search-backward REGEXP &optional BOUND NOERROR COUNT)

;; String matching (no buffer movement)
(string-match REGEXP STRING &optional START)
(string-match-p REGEXP STRING &optional START)  ; No match data
(looking-at REGEXP)
(looking-at-p REGEXP)  ; No match data
```

#### Match Data

```elisp
;; Access match results
(match-beginning SUBEXP)  ; Start of match/group
(match-end SUBEXP)        ; End of match/group
(match-string SUBEXP &optional STRING)  ; Extract matched text
(match-data)              ; All match positions
(set-match-data LIST)     ; Restore match positions

;; Replacement
(replace-match NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)
```

#### Syntax Functions

```elisp
;; Syntax table operations
(char-syntax CHAR)
(modify-syntax-entry CHAR NEWENTRY &optional SYNTAX-TABLE)
(set-syntax-table TABLE)

;; Parsing
(scan-lists FROM COUNT DEPTH)
(scan-sexps FROM COUNT)
(parse-partial-sexp FROM TO &optional TARGETDEPTH STOPBEFORE OLDSTATE COMMENTSTOP)

;; Navigation
(forward-word &optional ARG)
(backward-word &optional ARG)
(forward-sexp &optional ARG)
(backward-sexp &optional ARG)
```

#### Case Functions

```elisp
;; String/character casing
(upcase OBJ)
(downcase OBJ)
(capitalize OBJ)
(upcase-initials OBJ)

;; Region casing
(upcase-region START END)
(downcase-region START END)
(capitalize-region START END)

;; Word casing
(upcase-word ARG)
(downcase-word ARG)
(capitalize-word ARG)
```

#### Interactive Search

```elisp
;; Incremental search
(isearch-forward &optional REGEXP-P NO-RECURSIVE-EDIT)
(isearch-backward &optional REGEXP-P NO-RECURSIVE-EDIT)
(isearch-forward-regexp &optional NOT-REGEXP NO-RECURSIVE-EDIT)
(isearch-backward-regexp &optional NOT-REGEXP NO-RECURSIVE-EDIT)

;; Search modes
(isearch-toggle-case-fold)
(isearch-toggle-regexp)
(isearch-toggle-word)
(isearch-toggle-symbol)
(isearch-toggle-char-fold)
```

---

## Related Documentation

- **Buffer Management**: `04-buffer-management/01-buffer-core.md` - Gap buffer structure
- **Display Engine**: `05-display-engine/01-redisplay.md` - Highlighting matches
- **Elisp Runtime**: `03-elisp-runtime/02-core-types.md` - String and character types
- **Character Handling**: `15-internationalization/01-character-sets.md` - Unicode support
- **Syntax Tables**: Detailed syntax table documentation (if separate doc exists)

---

## References

### Source Files

- `src/search.c` - String search implementation (3,514 lines)
- `src/regex-emacs.c` - Regular expression engine (5,355 lines)
- `src/syntax.c` - Syntax table implementation (3,831 lines)
- `src/casefiddle.c` - Case conversion (764 lines)
- `src/regex-emacs.h` - Regex API and structures
- `src/syntax.h` - Syntax classes and macros

### Elisp Files

- `lisp/isearch.el` - Incremental search
- `lisp/emacs-lisp/re-builder.el` - Interactive regex development
- `lisp/char-fold.el` - Character folding for Unicode matching
- `lisp/replace.el` - Search and replace commands
- `lisp/emacs-lisp/rx.el` - Symbolic regex syntax

### Documentation

- Emacs Lisp Manual: (elisp) Searching and Matching
- Emacs Lisp Manual: (elisp) Syntax Tables
- Emacs Manual: (emacs) Search
- Emacs Manual: (emacs) Regexps

### External References

- Boyer-Moore Algorithm: Boyer, R.S., and Moore, J.S. (1977)
- Unicode Case Mapping: Unicode Standard Annex #21
- POSIX Regular Expressions: POSIX.2 (IEEE Std 1003.2)
- Regular Expression Matching: Thompson, K. (1968), "Regular Expression Search Algorithm"

---

**Document History:**
- 2025-11-18: Initial comprehensive documentation of text processing subsystem
- Covers search algorithms, regex engine, syntax tables, and case handling
- Includes performance characteristics and API reference
