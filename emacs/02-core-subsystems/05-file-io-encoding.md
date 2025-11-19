# File I/O and Character Encoding System

**Core Files:**
- File I/O: `src/fileio.c` (7,062 lines), `src/filelock.c` (840 lines), `src/dired.c` (1,213 lines)
- Encoding: `src/coding.c` (12,337 lines - third largest!), `src/charset.c` (2,456 lines), `src/character.c` (1,164 lines)
- CCL Interpreter: `src/ccl.c`
- Elisp Layer: `lisp/files.el` (9,391 lines), `lisp/international/mule.el` (2,618 lines)

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [File I/O Subsystem](#file-io-subsystem)
3. [Character Encoding Subsystem](#character-encoding-subsystem)
4. [Coding System Framework](#coding-system-framework)
5. [EOL Conversion and BOM Handling](#eol-conversion-and-bom-handling)
6. [Charset System](#charset-system)
7. [CCL Interpreter](#ccl-interpreter)
8. [File Operations Pipeline](#file-operations-pipeline)
9. [Backup and Auto-Save](#backup-and-auto-save)
10. [Elisp Interface](#elisp-interface)

---

## Architecture Overview

Emacs' file I/O and character encoding system is one of its most sophisticated subsystems, handling the complex task of reading and writing files across different character encodings, line ending conventions, and file systems.

### Design Philosophy

```
┌─────────────────────────────────────────────────────────────┐
│                    Elisp User Interface                      │
│         (files.el, mule.el, find-file-hook, etc.)           │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                File I/O Layer (fileio.c)                     │
│  • insert-file-contents    • write-region                   │
│  • expand-file-name        • directory-files                │
│  • file-attributes         • file-locks                      │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│              Character Encoding Pipeline                     │
│                     (coding.c)                               │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │   Detection  │─▶│   Decoding   │─▶│   Encoding   │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                 Charset/Character Layer                      │
│           (charset.c, character.c, composite.c)              │
│  • Unicode mapping         • Character composition          │
│  • Charset definitions     • Width calculation               │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Operating System                          │
│              (POSIX, Windows, Android APIs)                  │
└─────────────────────────────────────────────────────────────┘
```

### Key Concepts

1. **Emacs Internal Format**: UTF-8 based representation (`emacs-utf-8`)
2. **Coding Systems**: Pluggable encoders/decoders for different character encodings
3. **File Handlers**: Virtual file system abstraction for remote files, archives, etc.
4. **Atomic Operations**: File locking and safe writing strategies
5. **Encoding Detection**: Heuristic-based automatic detection of file encodings

---

## File I/O Subsystem

### Core Data Structures

#### File Descriptor Abstraction

From `/home/user/emacs/src/fileio.c:117-149`:

```c
/* Type describing a file descriptor used by functions such as
   `insert-file-contents'.  */

#if !defined HAVE_ANDROID || defined ANDROID_STUBIFY
typedef int emacs_fd;

/* Function used to read and open from such a file descriptor.  */
#define emacs_fd_open		emacs_open
#define emacs_fd_close		emacs_close
#define emacs_fd_read		emacs_read_quit
#define emacs_fd_lseek		lseek
#define emacs_fd_fstat		sys_fstat
#define emacs_fd_valid_p(fd)	((fd) >= 0)

#else /* HAVE_ANDROID && !defined ANDROID_STUBIFY */
typedef struct android_fd_or_asset emacs_fd;

#define emacs_fd_open		android_open_asset
#define emacs_fd_close		android_close_asset
#define emacs_fd_read		android_asset_read_quit
#define emacs_fd_lseek		android_asset_lseek
#define emacs_fd_fstat		android_asset_fstat
#define emacs_fd_valid_p(fd)	((fd).asset != ((void *) -1))
#endif
```

**Key insight**: Emacs abstracts file descriptors to support special file systems like Android assets and content URIs. This allows the same code to handle regular files and virtual files.

#### Global State Variables

From `/home/user/emacs/src/fileio.c:151-174`:

```c
/* True during writing of auto-save files.  */
static bool auto_saving;

/* Emacs's real umask.  */
static mode_t realmask;

/* Nonzero umask during creation of auto-save directories.  */
static mode_t auto_saving_dir_umask;

/* Set by auto_save_1 to mode of original file so Fwrite_region will create
   a new file with the same mode as the original.  */
static mode_t auto_save_mode_bits;

/* Set by auto_save_1 if an error occurred during the last auto-save.  */
static bool auto_save_error_occurred;

/* If VALID_TIMESTAMP_FILE_SYSTEM, then TIMESTAMP_FILE_SYSTEM is the device
   number of a file system where time stamps were observed to work.  */
static bool valid_timestamp_file_system;
static dev_t timestamp_file_system;

/* Each time an annotation function changes the buffer, the new buffer
   is added here.  */
static Lisp_Object Vwrite_region_annotation_buffers;
```

### File Reading: insert-file-contents

The `insert-file-contents` function is the core of Emacs' file reading. It's defined at `/home/user/emacs/src/fileio.c:4055`.

#### Function Signature

```c
DEFUN ("insert-file-contents", Finsert_file_contents, Sinsert_file_contents,
       1, 5, 0,
       doc: /* Insert contents of file FILENAME after point.
Returns list of absolute file name and number of characters inserted.
...
This function does code conversion according to the value of
`coding-system-for-read' or `file-coding-system-alist', and sets the
variable `last-coding-system-used' to the coding system actually used. */)
  (Lisp_Object filename, Lisp_Object visit, Lisp_Object beg,
   Lisp_Object end, Lisp_Object replace)
```

#### Key Parameters

- **filename**: File to read
- **visit**: If non-nil, set buffer's visited file and mark as unmodified
- **beg/end**: Byte range to read (not character range!)
- **replace**: If non-nil, replace buffer contents with file contents

#### Read Buffer Size Strategy

From `/home/user/emacs/src/fileio.c:4096-4102`:

```c
/* A good read blocksize for insert-file-contents.
   It is for reading a big chunk of a file into memory,
   as opposed to coreutils IO_BUFSIZE which is for 'cat'-like stream reads.
   If too small, insert-file-contents has more syscall overhead.
   If too large, insert-file-contents might take too long respond to a quit.
   1 MiB should be reasonable even for older, slower devices circa 2025.  */
enum { INSERT_READ_SIZE_MAX = min (1024 * 1024, SYS_BUFSIZE_MAX) };
```

**Design decision**: 1 MiB buffer balances syscall overhead with quit responsiveness. This is much larger than traditional Unix buffer sizes but appropriate for modern systems.

#### The Reading Pipeline

```
1. File Name Handler Check
   ↓
2. File Opening (with encoding)
   ↓
3. File Size Detection
   ↓
4. Coding System Selection
   ↓
5. Read Loop (1 MiB chunks)
   ↓
6. Decode to Internal Format
   ↓
7. Insert into Buffer
   ↓
8. EOL Conversion
   ↓
9. Format Decoding (format-decode)
```

### File Writing: write-region

The `write-region` function handles file writing with sophisticated atomic update strategies.

From `/home/user/emacs/src/fileio.c:5459-5503`:

```c
DEFUN ("write-region", Fwrite_region, Swrite_region, 3, 7,
       "r\nFWrite region to file: \ni\ni\ni\np",
       doc: /* Write current region into specified file.
...
This does code conversion according to the value of
`coding-system-for-write', `buffer-file-coding-system', or
`file-coding-system-alist', and sets the variable
`last-coding-system-used' to the coding system actually used. */)
```

#### Atomic Write Strategy

Emacs uses several strategies to ensure atomic file updates:

1. **Write to temporary file, then rename** (most common)
2. **Write directly** (for append mode or special files)
3. **Write through file handlers** (for remote/virtual files)

#### The Writing Pipeline

```
1. File Name Handler Check
   ↓
2. File Locking (if visiting)
   ↓
3. Annotation Functions (write-region-annotate-functions)
   ↓
4. Coding System Selection
   ↓
5. Encode from Internal Format
   ↓
6. EOL Conversion
   ↓
7. Write Loop
   ↓
8. fsync (if appropriate)
   ↓
9. Rename/Close
   ↓
10. Update modtime
```

### File Locking

From `/home/user/emacs/src/filelock.c:58-99`:

```c
/* Normally use a symbolic link to represent a lock.
   The strategy: to lock a file FN, create a symlink .#FN in FN's
   directory, with link data USER@HOST.PID:BOOT.  This avoids a single
   mount (== failure) point for lock files.  The :BOOT is omitted if
   the boot time is not available.

   When the host in the lock data is the current host, we can check if
   the pid is valid with kill.

   ...

   We use symlinks instead of normal files because (1) they can be
   stored more efficiently on the filesystem, since the kernel knows
   they will be small, and (2) all the info about the lock can be read
   in a single system call (readlink).

   ...

   On some file systems, notably those of MS-Windows, symbolic links
   do not work well, so instead of a symlink .#FN -> USER@HOST.PID:BOOT,
   the lock is a regular file .#FN with contents USER@HOST.PID:BOOT.
*/
```

**Lock file format**: `.#filename` → `user@host.pid:boottime`

This distributed locking scheme allows:
- Detection of stale locks (check if PID exists)
- Detection of locks from different machines
- No central lock server required
- Atomic lock creation (symlink creation is atomic)

### Directory Operations

From `/home/user/emacs/src/dired.c`, Emacs provides sophisticated directory listing functionality with support for:

1. **Multiple platforms**: Unix, Windows, Android (including `/assets` special directory)
2. **File attributes**: Permissions, ownership, timestamps, size
3. **Symbolic link handling**: Following or preserving links
4. **Wildcard matching**: Shell-style pattern matching

---

## Character Encoding Subsystem

The character encoding subsystem (`src/coding.c`) is the third-largest source file in Emacs at 12,337 lines. It implements a sophisticated framework for converting between different character encodings.

### Coding System Architecture

From `/home/user/emacs/src/coding.c:43-138`:

```
CODING SYSTEM

  A coding system is an object for an encoding mechanism that contains
  information about how to convert byte sequences to character
  sequences and vice versa.  When we say "decode", it means converting
  a byte sequence of a specific coding system into a character
  sequence that is represented by Emacs's internal coding system
  `emacs-utf-8', and when we say "encode", it means converting a
  character sequence of emacs-utf-8 to a byte sequence of a specific
  coding system.

  In Emacs Lisp, a coding system is represented by a Lisp symbol.  On
  the C level, a coding system is represented by a vector of attributes
  stored in the hash table Vcharset_hash_table.
```

### The struct coding_system

From `/home/user/emacs/src/coding.h:396-502`:

```c
struct coding_system
{
  /* ID number of the coding system.  This is an index to
     Vcoding_system_hash_table.  */
  ptrdiff_t id;

  /* Flag bits of the coding system.  The meaning of each bit is common
     to all types of coding systems.  */
  unsigned common_flags : 14;

  /* Mode bits of the coding system.  */
  unsigned mode : 5;

  /* The following two members specify how binary 8-bit code 128..255
     are represented in source and destination text respectively.  */
  bool_bf src_multibyte : 1;
  bool_bf dst_multibyte : 1;

  /* True if the source of conversion is not in the member
     `charbuf', but at `src_object'.  */
  bool_bf chars_at_source : 1;

  /* Nonzero if the result of conversion is in `destination'
     buffer rather than in `dst_object'.  */
  bool_bf raw_destination : 1;

  /* Set to true if charbuf contains an annotation.  */
  bool_bf annotated : 1;

  /* Used internally in coding.c.  See the comment of detect_ascii.  */
  unsigned eol_seen : 3;

  /* Finish status of code conversion.  */
  ENUM_BF (coding_result_code) result : 3;

  int max_charset_id;

  /* Detailed information specific to each type of coding system.  */
  union
    {
      struct iso_2022_spec iso_2022;
      struct ccl_spec *ccl;	/* Defined in ccl.h.  */
      struct utf_16_spec utf_16;
      enum utf_bom_type utf_8_bom;
      struct emacs_mule_spec emacs_mule;
      struct undecided_spec undecided;
    } spec;

  unsigned char *safe_charsets;
  ptrdiff_t head_ascii;
  ptrdiff_t detected_utf8_bytes, detected_utf8_chars;

  /* The following members are set by encoding/decoding routine.  */
  ptrdiff_t produced, produced_char, consumed, consumed_char;

  ptrdiff_t src_pos, src_pos_byte, src_chars, src_bytes;
  Lisp_Object src_object;
  const unsigned char *source;

  ptrdiff_t dst_pos, dst_pos_byte, dst_bytes;
  Lisp_Object dst_object;
  unsigned char *destination;

  /* Character buffer for intermediate results.
     If an element is non-negative, it is a character code.
     If it is in the range -128..-1, it is a 8-bit character code minus 256.
     If it is less than -128, it specifies the start of an annotation chunk. */
  int *charbuf;
  int charbuf_size, charbuf_used;

  unsigned char carryover[64];
  int carryover_bytes;

  int default_char;

  bool (*detector) (struct coding_system *, struct coding_detection_info *);
  void (*decoder) (struct coding_system *);
  bool (*encoder) (struct coding_system *);
};
```

**Key design features**:

1. **Polymorphic design**: Function pointers for detector/decoder/encoder
2. **Efficient buffering**: Character buffer for intermediate results
3. **Annotation support**: Allows metadata in the conversion stream
4. **Carryover handling**: Manages incomplete multibyte sequences
5. **Type-specific specs**: Union for different coding system types

### Coding Categories

From `/home/user/emacs/src/coding.c:473-498`:

```c
enum coding_category
  {
    coding_category_iso_7,
    coding_category_iso_7_tight,
    coding_category_iso_8_1,
    coding_category_iso_8_2,
    coding_category_iso_7_else,
    coding_category_iso_8_else,
    coding_category_utf_8_auto,
    coding_category_utf_8_nosig,
    coding_category_utf_8_sig,
    coding_category_utf_16_auto,
    coding_category_utf_16_be,
    coding_category_utf_16_le,
    coding_category_utf_16_be_nosig,
    coding_category_utf_16_le_nosig,
    coding_category_charset,
    coding_category_sjis,
    coding_category_big5,
    coding_category_ccl,
    coding_category_emacs_mule,
    /* All above are targets of code detection.  */
    coding_category_raw_text,
    coding_category_undecided,
    coding_category_max
  };
```

**Detection priority**: The order matters! UTF-8 variants come before legacy encodings, ensuring modern formats are detected first.

---

## Coding System Framework

### Decoding Pipeline

The generic decoding template from `/home/user/emacs/src/coding.c:204-239`:

```c
static void
decode_coding_XXXX (struct coding_system *coding)
{
  const unsigned char *src = coding->source + coding->consumed;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  /* SRC_BASE remembers the start position in source in each loop.
     The loop will be exited when there's not enough source code, or
     when there's no room in CHARBUF for a decoded character.  */
  const unsigned char *src_base;
  /* A buffer to produce decoded characters.  */
  int *charbuf = coding->charbuf + coding->charbuf_used;
  int *charbuf_end = coding->charbuf + coding->charbuf_size;
  bool multibytep = coding->src_multibyte;

  while (1)
    {
      src_base = src;
      if (charbuf < charbuf_end)
	/* No more room to produce a decoded character.  */
	break;
      ONE_MORE_BYTE (c);
      /* Decode it. */
    }

 no_more_source:
  if (src_base < src_end
      && coding->mode & CODING_MODE_LAST_BLOCK)
    /* If the source ends by partial bytes to construct a character,
       treat them as eight-bit raw data.  */
    while (src_base < src_end && charbuf < charbuf_end)
      *charbuf++ = *src_base++;
  /* Remember how many bytes and characters we consumed.  */
  coding->consumed = coding->consumed_char = src_base - coding->source;
  /* Remember how many characters we produced.  */
  coding->charbuf_used = charbuf - coding->charbuf;
}
```

**Design patterns**:
- **Restart capability**: Tracks position for resuming after buffer fills
- **Graceful degradation**: Treats invalid sequences as raw bytes
- **Separation of concerns**: Character buffer isolates decode from output

### Encoding Pipeline

The generic encoding template from `/home/user/emacs/src/coding.c:260-281`:

```c
static void
encode_coding_XXX (struct coding_system *coding)
{
  bool multibytep = coding->dst_multibyte;
  int *charbuf = coding->charbuf;
  int *charbuf_end = charbuf->charbuf + coding->charbuf_used;
  unsigned char *dst = coding->destination + coding->produced;
  unsigned char *dst_end = coding->destination + coding->dst_bytes;
  unsigned char *adjusted_dst_end = dst_end - _MAX_BYTES_PRODUCED_IN_LOOP_;
  ptrdiff_t produced_chars = 0;

  for (; charbuf < charbuf_end && dst < adjusted_dst_end; charbuf++)
    {
      int c = *charbuf;
      /* Encode C into DST, and increment DST.  */
    }
 label_no_more_destination:
  /* How many chars and bytes we produced.  */
  coding->produced_char += produced_chars;
  coding->produced = dst - coding->destination;
}
```

### Setup and Configuration

From `/home/user/emacs/src/coding.c:5666-5815`:

```c
void
setup_coding_system (Lisp_Object coding_system, struct coding_system *coding)
{
  Lisp_Object attrs;
  Lisp_Object eol_type;
  Lisp_Object coding_type;

  if (NILP (coding_system))
    coding_system = Qundecided;

  CHECK_CODING_SYSTEM_GET_ID (coding_system, coding->id);
  attrs = CODING_ID_ATTRS (coding->id);
  eol_type = inhibit_eol_conversion ? Qunix : CODING_ID_EOL_TYPE (coding->id);

  coding_type = CODING_ATTR_TYPE (attrs);

  if (EQ (coding_type, Qutf_8))
    {
      val = AREF (attrs, coding_attr_utf_bom);
      CODING_UTF_8_BOM (coding) = (CONSP (val) ? utf_detect_bom
				   : EQ (val, Qt) ? utf_with_bom
				   : utf_without_bom);
      coding->detector = detect_coding_utf_8;
      coding->decoder = decode_coding_utf_8;
      coding->encoder = encode_coding_utf_8;
      // ...
    }
  else if (EQ (coding_type, Qutf_16))
    {
      // UTF-16 setup...
    }
  else if (EQ (coding_type, Qiso_2022))
    {
      // ISO-2022 setup...
    }
  // ... other coding systems
}
```

**Polymorphic dispatch**: Each coding system type gets its own detector/decoder/encoder functions assigned.

---

## EOL Conversion and BOM Handling

### End-of-Line Detection

From `/home/user/emacs/src/coding.c:1101-1104`:

```c
#define EOL_SEEN_NONE	0
#define EOL_SEEN_LF	1
#define EOL_SEEN_CR	2
#define EOL_SEEN_CRLF	4
```

EOL detection is stateful and cumulative using bit flags. A file can contain multiple EOL types, and Emacs tracks all of them:

```c
// During detection:
if (c == '\n')
  eol_seen |= EOL_SEEN_LF;
else if (c == '\r')
  {
    if (next == '\n')
      eol_seen |= EOL_SEEN_CRLF;
    else
      eol_seen |= EOL_SEEN_CR;
  }
```

**Heuristic decision**: After scanning, the most common EOL type is chosen. If `CRLF` is seen, it takes precedence as it's most specific.

### BOM (Byte Order Mark) Handling

#### UTF-8 BOM

From `/home/user/emacs/src/coding.c:1124-1155`:

```c
#define UTF_8_BOM_1 0xEF
#define UTF_8_BOM_2 0xBB
#define UTF_8_BOM_3 0xBF

static bool
detect_coding_utf_8 (struct coding_system *coding,
		     struct coding_detection_info *detect_info)
{
  // ...
  if (src == coding->source	/* BOM should be at the head.  */
      && src + 3 < src_end	/* BOM is 3-byte long.  */
      && src[0] == UTF_8_BOM_1
      && src[1] == UTF_8_BOM_2
      && src[2] == UTF_8_BOM_3)
    {
      bom_found = 1;
      src += 3;
      nchars++;
    }
```

**BOM policy**:
- **Detection**: BOM must be at file start
- **Preservation**: BOM is consumed during decode, not passed to buffer
- **Generation**: Controlled by coding system attributes

#### UTF-16 BOM

UTF-16 has more complex BOM handling because it determines byte order:

```c
enum utf_bom_type
  {
    utf_detect_bom,      // Auto-detect based on BOM
    utf_without_bom,     // No BOM expected
    utf_with_bom         // BOM required
  };

enum utf_16_endian_type
  {
    utf_16_big_endian,
    utf_16_little_endian
  };
```

The BOM `0xFEFF` appears as:
- `FE FF` in big-endian
- `FF FE` in little-endian

**Detection strategy**: If BOM present, use it to determine endianness. Otherwise, use statistical analysis of the byte stream.

---

## Charset System

### Charset Architecture

From `/home/user/emacs/src/charset.c:43-56`:

```
/*** GENERAL NOTES on CODED CHARACTER SETS (CHARSETS) ***

  A coded character set ("charset" hereafter) is a meaningful
  collection (i.e. language, culture, functionality, etc.) of
  characters.  Emacs handles multiple charsets at once.  In Emacs Lisp
  code, a charset is represented by a symbol.  In C code, a charset is
  represented by its ID number or by a pointer to a struct charset.

  The actual information about each charset is stored in two places.
  Lispy information is stored in the hash table Vcharset_hash_table as
  a vector (charset attributes).  The other information is stored in
  charset_table as a struct charset.
```

### Dual Representation

```
Lisp Level:              C Level:
┌──────────────┐        ┌──────────────────┐
│ Symbol       │        │ struct charset   │
│ 'iso-8859-1  │───────▶│ {                │
│              │        │   id: 42         │
│              │        │   dimension: 1   │
│              │        │   code_space[8]  │
│              │        │   min_code       │
│              │        │   max_code       │
│              │        │   ...            │
│              │        │ }                │
└──────────────┘        └──────────────────┘
        │                       │
        ▼                       ▼
┌──────────────────────────────────────┐
│    Vcharset_hash_table               │
│    (Symbol → Attribute Vector)       │
└──────────────────────────────────────┘
```

### Code Point Mapping

From `/home/user/emacs/src/charset.c:106-141`:

```c
#define CODE_POINT_TO_INDEX(charset, code)				\
  ((charset)->code_linear_p						\
   ? (int) ((code) - (charset)->min_code)				\
   : (((charset)->code_space_mask[(code) >> 24] & 0x8)			\
      && ((charset)->code_space_mask[((code) >> 16) & 0xFF] & 0x4)	\
      && ((charset)->code_space_mask[((code) >> 8) & 0xFF] & 0x2)	\
      && ((charset)->code_space_mask[(code) & 0xFF] & 0x1))		\
   ? (int) (((((code) >> 24) - (charset)->code_space[12])		\
	     * (charset)->code_space[11])				\
	    + (((((code) >> 16) & 0xFF) - (charset)->code_space[8])	\
	       * (charset)->code_space[7])				\
	    + (((((code) >> 8) & 0xFF) - (charset)->code_space[4])	\
	       * (charset)->code_space[3])				\
	    + (((code) & 0xFF) - (charset)->code_space[0])		\
	    - ((charset)->char_index_offset))				\
   : -1)
```

**Two strategies**:
1. **Linear charsets**: Simple offset calculation (ASCII, ISO-8859-*)
2. **Non-linear charsets**: Multi-dimensional mapping (CJK ideographs)

### Important Charsets

From `/home/user/emacs/src/charset.c:67-81`:

```c
/* Special charsets corresponding to symbols.  */
int charset_ascii;
int charset_eight_bit;
static int charset_iso_8859_1;
int charset_unicode;
static int charset_emacs;

/* The other special charsets.  */
int charset_jisx0201_roman;
int charset_jisx0208_1978;
int charset_jisx0208;
int charset_ksc5601;

/* Charset of unibyte characters.  */
int charset_unibyte;
```

### Character Composition

Emacs supports complex character composition for languages like Thai, Arabic, and Indic scripts. Characters can be composed using composition rules:

From `/home/user/emacs/src/coding.c:1084-1090`:

```c
#define ADD_COMPOSITION_DATA(buf, nchars, nbytes, method)		    \
  do {									    \
    ADD_ANNOTATION_DATA (buf, 5, CODING_ANNOTATE_COMPOSITION_MASK, nchars); \
    *buf++ = nbytes;							    \
    *buf++ = method;							    \
  } while (0)
```

**Composition methods**:
- **Relative**: Characters overlap
- **Base + combining**: Base character with combining marks
- **Rule-based**: Complex composition rules (from language-specific tables)

---

## CCL Interpreter

The CCL (Code Conversion Language) interpreter provides a way to define custom character encoding/decoding without writing C code.

From `/home/user/emacs/src/ccl.c:50-84`:

```c
/* CCL (Code Conversion Language) is a simple language which has
   operations on one input buffer, one output buffer, and 7 registers.
   The syntax of CCL is described in `ccl.el'.  Emacs Lisp function
   `ccl-compile' compiles a CCL program and produces a CCL code which
   is a vector of integers.  The structure of this vector is as
   follows: The 1st element: buffer-magnification, a factor for the
   size of output buffer compared with the size of input buffer.  The
   2nd element: address of CCL code to be executed when encountered
   with end of input stream.  The 3rd and the remaining elements: CCL
   codes.  */

/* CCL code is a sequence of 28-bit integers.  Each contains a CCL
   command and/or arguments in the following format:

	|----------------- integer (28-bit) ------------------|
	|------- 17-bit ------|- 3-bit --|- 3-bit --|- 5-bit -|
	|--constant argument--|-register-|-register-|-command-|
	   ccccccccccccccccc      RRR        rrr       XXXXX
  or
	|------- relative address -------|-register-|-command-|
	       cccccccccccccccccccc          rrr       XXXXX
  or
	|------------- constant or other args ----------------|
                     cccccccccccccccccccccccccccc
```

### CCL Architecture

```
┌─────────────────────────────────────────┐
│          Lisp CCL Program               │
│      (define-ccl-program ...)           │
└─────────────────────────────────────────┘
                 │
                 │ ccl-compile
                 ▼
┌─────────────────────────────────────────┐
│         CCL Code Vector                 │
│  [buf-mag, eof-addr, code1, code2, ...] │
└─────────────────────────────────────────┘
                 │
                 │ ccl_driver
                 ▼
┌─────────────────────────────────────────┐
│         CCL Virtual Machine             │
│  • 7 registers (r0-r6)                  │
│  • Input buffer + pointer               │
│  • Output buffer + pointer              │
│  • Instruction counter                  │
└─────────────────────────────────────────┘
```

### CCL Commands

Basic commands include:
- `CCL_SetRegister`: Copy register to register
- `CCL_SetConst`: Load constant into register
- `CCL_ReadWriteReadJump`: Read, write, read again, then jump
- `CCL_Branch`: Conditional branching
- `CCL_Translate`: Table-based character translation
- `CCL_End`: Terminate CCL program

**Use cases**:
- Legacy encodings not built into Emacs
- Custom encoding schemes
- Character transliteration tables
- Special text transformations during I/O

---

## File Operations Pipeline

### Complete Read Pipeline

```
find-file
   │
   ├──▶ File name expansion
   │    └─ expand-file-name (handles ~, .., symlinks)
   │
   ├──▶ File name handler check
   │    └─ Ffind_file_name_handler (TRAMP, ange-ftp, archives)
   │
   ├──▶ File locking
   │    └─ lock_file (create .#filename symlink)
   │
   ├──▶ insert-file-contents
   │    │
   │    ├──▶ Open file
   │    │    └─ emacs_fd_open (platform abstracted)
   │    │
   │    ├──▶ Determine coding system
   │    │    ├─ coding-system-for-read (highest priority)
   │    │    ├─ file-coding-system-alist
   │    │    ├─ Auto-detection
   │    │    └─ default-buffer-file-coding-system
   │    │
   │    ├──▶ Setup coding system
   │    │    └─ setup_coding_system
   │    │
   │    ├──▶ Read loop (1 MiB chunks)
   │    │    ├─ emacs_fd_read
   │    │    ├─ decode_coding (bytes → chars)
   │    │    └─ insert_from_gap (chars → buffer)
   │    │
   │    └──▶ EOL conversion
   │         └─ decode_eol (CR/LF/CRLF → LF)
   │
   ├──▶ Format decoding
   │    └─ format-decode (handles enriched text, etc.)
   │
   ├──▶ Run hooks
   │    ├─ find-file-hook
   │    └─ after-find-file
   │
   └──▶ Update buffer state
        ├─ Set buffer-file-name
        ├─ Set buffer-file-coding-system
        ├─ Clear modified flag
        └─ Record modtime
```

### Complete Write Pipeline

```
save-buffer
   │
   ├──▶ Backup creation (if first save)
   │    └─ backup-buffer
   │         ├─ Find backup file name
   │         └─ Copy or rename
   │
   ├──▶ write-region
   │    │
   │    ├──▶ File locking check
   │    │    └─ Verify we still own the lock
   │    │
   │    ├──▶ Annotation functions
   │    │    └─ write-region-annotate-functions
   │    │
   │    ├──▶ Determine coding system
   │    │    ├─ coding-system-for-write
   │    │    ├─ buffer-file-coding-system
   │    │    └─ select-safe-coding-system
   │    │
   │    ├──▶ Temporary file creation
   │    │    └─ Open with O_EXCL | O_CREAT
   │    │
   │    ├──▶ Encode and write
   │    │    ├─ encode_coding (chars → bytes)
   │    │    ├─ encode_eol (LF → CR/LF/CRLF)
   │    │    └─ e_write (write bytes)
   │    │
   │    ├──▶ Sync to disk
   │    │    └─ fsync (if write-region-inhibit-fsync is nil)
   │    │
   │    └──▶ Atomic rename
   │         └─ rename(temp, target)
   │
   ├──▶ Update buffer state
   │    ├─ Clear modified flag
   │    ├─ Update modtime
   │    └─ Set buffer-file-coding-system
   │
   └──▶ Unlock file
        └─ unlock_file (remove .#filename)
```

### Error Handling Strategy

```c
// From write_region implementation:
// 1. Write to temporary file
// 2. If error occurs, temp file is cleaned up
// 3. Original file remains untouched
// 4. Only on successful write + fsync do we rename
// 5. Rename is atomic on most file systems
```

**Crash safety**: Even if Emacs crashes during write, the original file is preserved.

---

## Backup and Auto-Save

### Backup Strategy

From `/home/user/emacs/lisp/files.el:5356-5435`:

```elisp
(defun backup-buffer ()
  "Make a backup of the disk file visited by the current buffer, if appropriate.
This is normally done before saving the buffer the first time.

A backup may be done by renaming or by copying; see documentation of
variable `make-backup-files'.  If it's done by renaming, then the file is
no longer accessible under its old name."
  (when (and make-backup-files (not backup-inhibited) (not buffer-backed-up))
    ;; Determine whether to copy or rename
    (let ((make-copy
	    (or file-precious-flag backup-by-copying
		;; Don't rename a suid or sgid file.
		(and modes (< 0 (logand modes #o6000)))
		(not (file-writable-p (file-name-directory real-file-name)))
		(and backup-by-copying-when-linked
		     (< 1 (file-nlinks real-file-name)))
		;; Preserve ownership/group
		(and backup-by-copying-when-mismatch
		     (not (file-ownership-preserved-p real-file-name t))))))
      ;; Actually make the backup file.
      (if make-copy
	  (backup-buffer-copy real-file-name backupname modes extended-attributes)
	;; rename-file should delete old backup.
	(rename-file real-file-name backupname t))
```

**Decision tree for backup method**:

```
Should use copying if:
├─ file-precious-flag is set
├─ backup-by-copying is set
├─ File has setuid/setgid bits
├─ Directory not writable
├─ File has multiple hard links (backup-by-copying-when-linked)
└─ Ownership would change (backup-by-copying-when-mismatch)

Otherwise use renaming (faster)
```

### Backup File Naming

```elisp
;; Simple backup
file.txt → file.txt~

;; Numbered backup
file.txt → file.txt.~1~
file.txt → file.txt.~2~
file.txt → file.txt.~3~
```

### Auto-Save Mechanism

From `/home/user/emacs/src/fileio.c:6313-6412`:

```c
DEFUN ("do-auto-save", Fdo_auto_save, Sdo_auto_save, 0, 2, "",
       doc: /* Auto-save all buffers that need it.
This auto-saves all buffers that have auto-saving enabled and
were changed since last auto-saved.

Auto-saving writes the buffer into a file so that your edits are
not lost if the system crashes.

The auto-save file is not the file you visited; that changes only
when you save.  */)
```

#### Auto-Save File Names

```
Regular file:        /path/to/file.txt
Auto-save file:      /path/to/#file.txt#

Unsaved buffer:      <buffer-name>
Auto-save file:      ~/.emacs.d/auto-save-list/.saves-PID-hostname~
```

#### Auto-Save List File

Emacs maintains a list of all auto-save files in `auto-save-list-file-name`:

```
/path/to/file.txt
/path/to/#file.txt#
/other/file.el
/other/#file.el#
```

This allows recovery tools to:
1. Find all auto-saved files
2. Match them to original files
3. Offer batch recovery after a crash

#### Auto-Save Trigger Conditions

```elisp
;; Triggered by:
auto-save-interval          ; Number of input events (default 300)
auto-save-timeout           ; Idle time in seconds (default 30)
kill-emacs-hook             ; When exiting Emacs
```

---

## Elisp Interface

### File I/O Functions

From `/home/user/emacs/lisp/files.el`:

#### Core Reading

```elisp
(defun find-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME, creating one if none exists."
  ;; Implementation delegates to find-file-noselect + switch-to-buffer
  )

(defun insert-file-contents (filename &optional visit beg end replace)
  ;; C implementation in fileio.c
  )
```

#### Core Writing

```elisp
(defun save-buffer (&optional args)
  "Save current buffer in visited file if modified.
Calls backup-buffer first time, then write-region."
  ;; Handles backup creation
  ;; Calls write-region (C function)
  ;; Updates buffer state
  )

(defun write-file (filename &optional confirm)
  "Write current buffer into file FILENAME.
Makes buffer visit that file and marks it not modified."
  )
```

#### Directory Operations

```elisp
(defun directory-files (directory &optional full match nosort count)
  ;; C implementation in dired.c
  )

(defun directory-files-and-attributes (directory &optional full match nosort id-format count)
  ;; Returns file list with attributes
  )
```

### Coding System Functions

From `/home/user/emacs/lisp/international/mule.el`:

```elisp
(defun define-charset (name docstring &rest props)
  "Define NAME (symbol) as a charset with DOCSTRING.
Properties:
  :dimension        - Number of bytes per character
  :code-space       - Valid byte ranges
  :min-code, :max-code - Code point range
  :iso-final-char   - ISO-2022 final character
  :emacs-mule-id    - ID in emacs-mule encoding
  :code-offset      - Base offset for code points
  :map              - Mapping table file
  :subset, :superset - Inheritance relationships"
  )

(defun detect-coding-region (start end &optional highest)
  "Detect coding system of the text in the region between START and END.
Return a list of possible coding systems ordered by priority."
  ;; C implementation in coding.c
  )

(defun decode-coding-region (start end coding-system &optional destination)
  "Decode the current region from CODING-SYSTEM.
Decodes the text between START and END."
  ;; C implementation in coding.c
  )

(defun encode-coding-region (start end coding-system &optional destination)
  "Encode the current region to CODING-SYSTEM."
  ;; C implementation in coding.c
  )
```

### Important Variables

```elisp
;; File coding
buffer-file-coding-system         ; Coding system for current buffer
file-coding-system-alist          ; Filename patterns → coding systems
auto-coding-alist                 ; File patterns for auto-detection
auto-coding-functions             ; Functions to determine coding
coding-system-for-read            ; Override for next read
coding-system-for-write           ; Override for next write
last-coding-system-used           ; What was actually used

;; File backups
make-backup-files                 ; Enable backups
backup-by-copying                 ; Copy vs rename
backup-directory-alist            ; Where to put backups
kept-new-versions                 ; Number of newest to keep
kept-old-versions                 ; Number of oldest to keep
delete-old-versions               ; Auto-delete excess versions
version-control                   ; nil, never, t (numbered)

;; Auto-save
auto-save-default                 ; Enable auto-save for new buffers
auto-save-interval                ; Events between auto-saves
auto-save-timeout                 ; Idle seconds before auto-save
auto-save-list-file-prefix        ; Where to record auto-saves
```

### Hooks

```elisp
;; File finding
find-file-hook                    ; After file found
find-file-not-found-functions     ; When file doesn't exist
after-find-file                   ; After find-file completes

;; File saving
before-save-hook                  ; Before saving
after-save-hook                   ; After saving
write-file-functions              ; Override save mechanism
write-contents-functions          ; Alternative save functions

;; Encoding
auto-coding-functions             ; Determine coding system
```

---

## Implementation Deep Dives

### UTF-8 Detection and Decoding

From `/home/user/emacs/src/coding.c:1131-1199`:

```c
static bool
detect_coding_utf_8 (struct coding_system *coding,
		     struct coding_detection_info *detect_info)
{
  const unsigned char *src = coding->source, *src_base;
  const unsigned char *src_end = coding->source + coding->src_bytes;
  bool multibytep = coding->src_multibyte;
  ptrdiff_t consumed_chars = 0;
  bool bom_found = 0;
  ptrdiff_t nchars = coding->head_ascii;

  detect_info->checked |= CATEGORY_MASK_UTF_8;
  /* A coding system of this category is always ASCII compatible.  */
  src += nchars;

  // Check for UTF-8 BOM
  if (src == coding->source	/* BOM should be at the head.  */
      && src + 3 < src_end	/* BOM is 3-byte long.  */
      && src[0] == UTF_8_BOM_1  /* 0xEF */
      && src[1] == UTF_8_BOM_2  /* 0xBB */
      && src[2] == UTF_8_BOM_3) /* 0xBF */
    {
      bom_found = 1;
      src += 3;
      nchars++;
    }

  while (1)
    {
      int c, c1, c2, c3, c4;
      src_base = src;
      ONE_MORE_BYTE (c);

      if (c < 0 || UTF_8_1_OCTET_P (c))  // ASCII character
	{
	  nchars++;
	  if (c == '\r')  // Track EOL
	    {
	      if (src < src_end && *src == '\n')
		{
		  src++;
		  nchars++;
		}
	    }
	  continue;
	}

      // Multi-byte UTF-8 sequence
      ONE_MORE_BYTE (c1);
      if (c1 < 0 || ! UTF_8_EXTRA_OCTET_P (c1))
	break;  // Invalid UTF-8

      if (UTF_8_2_OCTET_LEADING_P (c))
	{
	  nchars++;
	  continue;
	}

      // 3-byte sequence...
      // 4-byte sequence...
      // Similar validation for longer sequences
    }

  // If we found invalid UTF-8, reject this coding
  detect_info->rejected |= CATEGORY_MASK_UTF_8;
  return 0;

 no_more_source:
  // Successfully scanned entire file
  detect_info->found |= found;
  coding->detected_utf8_chars = nchars;
  coding->detected_utf8_bytes = src_base - coding->source;
  return 1;
}
```

**Detection strategy**:
1. Check for BOM at file start
2. Validate UTF-8 byte sequences
3. Track character count for efficiency
4. Detect EOL style simultaneously
5. Reject on first invalid sequence

### ISO-2022 State Machine

ISO-2022 is one of the most complex encoding systems, requiring a state machine to track:

- **4 character sets** (G0, G1, G2, G3)
- **2 graphic planes** (GL, GR)
- **Designation sequences**: Escape sequences that load charsets into G0-G3
- **Invocation sequences**: Shift codes that map G0-G3 to GL/GR
- **Single-shift**: Temporary one-character invocation

```c
// State tracking
#define CODING_ISO_DESIGNATION(coding, reg)    \
  ((coding)->spec.iso_2022.current_designation[reg])

#define CODING_ISO_INVOCATION(coding, plane)   \
  ((coding)->spec.iso_2022.current_invocation[plane])

// Example designation sequence:
// ESC $ B  → Designate JISX0208 to G0
// ESC ( B  → Designate ASCII to G0
// ESC ) I  → Designate JISX0201-KANA to G1
```

This complexity is why ISO-2022 code is so large and why modern systems prefer UTF-8.

### File Name Expansion

File name expansion is complex due to:
1. Platform differences (Unix vs Windows vs Android)
2. Remote file access (TRAMP)
3. Symbolic links
4. Tilde expansion
5. Environment variables
6. Relative path resolution

From `/home/user/emacs/src/fileio.c:992`:

```c
DEFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
       doc: /* Convert filename NAME to absolute, and canonicalize it.
Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
\(does not start with slash or tilde); both the directory name and
a directory's file name are accepted.  If DEFAULT-DIRECTORY is nil or
missing, the current buffer's value of `default-directory' is used.
File name components that are `.' are removed, and so are file name
components followed by `..', along with the `..' itself; note that
these simplifications are done without checking the resulting file
names in the file system.  Multiple consecutive slashes are collapsed
into a single slash, except at the beginning of the file name when
they are significant (e.g., UNC file names on MS-Windows.)
An initial `~/' expands to your home directory.
An initial `~USER/' expands to USER's home directory.
See also the function `substitute-in-file-name'. */)
```

The implementation handles:
- `~` → home directory
- `~/` → current user's home
- `~user/` → specific user's home
- `.` removal
- `..` resolution
- Multiple slash collapsing
- UNC path preservation (Windows)
- Drive letters (DOS/Windows)

---

## Performance Characteristics

### Read Performance

| Operation | Time Complexity | Notes |
|-----------|----------------|-------|
| File open | O(1) | System call |
| Coding detection | O(n) | Scans file prefix |
| UTF-8 decode | O(n) | Linear scan |
| ISO-2022 decode | O(n × s) | State machine transitions |
| Buffer insertion | O(m) | Move gap, m = insertion point |

### Write Performance

| Operation | Time Complexity | Notes |
|-----------|----------------|-------|
| Backup creation | O(n) | Copy or rename |
| Encoding | O(n) | Linear |
| Temp file write | O(n) | Linear |
| fsync | Variable | Depends on disk cache |
| Atomic rename | O(1) | Filesystem operation |

### Memory Usage

```
Reading a file of size N bytes:

Minimum:
  - Read buffer: 1 MiB
  - Charbuf: ~256 KB (for decoding)
  - Buffer gap: ~2N (worst case for multibyte)
  Total: ~2N + 1.25 MB

Maximum (many multibyte chars):
  - Read buffer: 1 MiB
  - Charbuf: ~1 MB (worst case)
  - Buffer: up to 4N (max expansion: 1 byte → 4 bytes UTF-8)
  - Gap: 2N
  Total: ~6N + 2 MB
```

### Optimization Strategies

1. **ASCII Fast Path**:
   - Most files are ASCII-compatible
   - Skip decoding for ASCII prefix
   - `head_ascii` tracks how far we can skip

2. **Detection Caching**:
   - Once coding detected, skip re-detection
   - Cache in `buffer-file-coding-system`

3. **Gap Buffer Reuse**:
   - Insert at gap to avoid moving text
   - REPLACE mode reuses existing buffer space

4. **Lazy EOL Conversion**:
   - If file is all LF, no conversion needed
   - Detected during initial scan

---

## Security Considerations

### Path Traversal Prevention

```c
// Emacs validates file paths to prevent:
// - Directory traversal (../..)
// - Symlink attacks
// - Access outside allowed directories
```

### File Lock Race Conditions

The file locking mechanism prevents:
1. **Double-write**: Two Emacs instances modifying same file
2. **Lost updates**: Second write overwrites first
3. **Stale locks**: Boot time detection identifies stale locks

### Encoding Security

1. **Invalid UTF-8**: Treated as raw bytes, not error
2. **BOM injection**: BOM only recognized at file start
3. **Null byte handling**: Special detection to avoid encoding issues
4. **Overlong sequences**: UTF-8 decoder rejects overlong encodings

### Temporary File Security

```c
// Temporary files created with:
// - O_EXCL: Fail if file exists (prevents symlink attacks)
// - O_CREAT: Atomic creation
// - Restrictive permissions (0600)
// - Random component in name
```

---

## Testing and Validation

### Coding System Test Coverage

Emacs includes extensive tests for:
- All major coding systems (UTF-8, UTF-16, ISO-2022, Shift-JIS, Big5, etc.)
- EOL conversion (LF, CRLF, CR)
- BOM handling
- Encoding detection
- Round-trip conversion (encode → decode = identity)
- Edge cases (partial sequences, invalid bytes, etc.)

### File I/O Test Coverage

- Large files (> 2 GB)
- Empty files
- Files with no final newline
- Read-only files
- Special files (/dev/null, /dev/urandom)
- Remote files (TRAMP)
- Archives (tar, zip)
- Symbolic links
- Hard links
- Named pipes (FIFOs)

---

## Related Subsystems

### Buffer Management
- **Buffer gap**: Efficient insertion/deletion
- **Multibyte representation**: Internal character encoding
- **Markers**: Position tracking across modifications

### Display Engine
- **Character width**: Unicode width properties
- **Composition**: Combining characters for display
- **Font selection**: Based on charset

### Process I/O
- **Encoding pipes**: stdin/stdout encoding for subprocesses
- **PTY encoding**: Terminal encoding for interactive processes
- **Network encoding**: Socket I/O encoding

---

## Historical Notes

### Evolution of Internal Encoding

1. **Emacs 19**: Mixed multibyte (emacs-mule)
2. **Emacs 20-21**: Mule-UCS (partial Unicode)
3. **Emacs 22+**: Full Unicode support
4. **Emacs 23+**: UTF-8 based internal representation

### Why Not Pure UTF-8?

Emacs' internal representation is "UTF-8 based" but not pure UTF-8 because:

1. **Eight-bit bytes**: Raw bytes (128-255) represented as special characters
2. **Unibyte buffers**: Some buffers remain unibyte for efficiency
3. **Composition**: Complex character composition metadata
4. **Charset information**: Preserved for round-trip conversion

### Backward Compatibility

Emacs maintains compatibility with:
- Old Mule encodings (emacs-mule)
- Legacy Japanese encodings (iso-2022-jp variants)
- Platform-specific encodings (cp1252, shift-jis, etc.)
- Ancient systems (no Unicode support)

This accounts for much of coding.c's size and complexity.

---

## Conclusion

The file I/O and character encoding system is one of Emacs' most mature and battle-tested subsystems. Its design reflects decades of evolution handling:

- **Dozens of character encodings** from around the world
- **Multiple operating systems** with different file semantics
- **Billions of files** in every encoding imaginable
- **Mission-critical data** that must not be corrupted

The key architectural principles are:

1. **Robustness**: Never lose user data
2. **Flexibility**: Support any encoding via CCL
3. **Performance**: Optimize common cases (ASCII, UTF-8)
4. **Compatibility**: Handle legacy formats correctly
5. **Safety**: Atomic operations, file locking, crash recovery

Understanding this subsystem provides insight into how Emacs maintains its reputation for reliability and internationalization support.

---

## Further Reading

### Source Code Entry Points

- File I/O: `/home/user/emacs/src/fileio.c:4055` (insert-file-contents)
- Encoding: `/home/user/emacs/src/coding.c:5666` (setup_coding_system)
- Charsets: `/home/user/emacs/src/charset.c:43` (charset overview)
- CCL: `/home/user/emacs/src/ccl.c:50` (CCL overview)
- Elisp: `/home/user/emacs/lisp/files.el:5356` (backup-buffer)

### Documentation

- Info node: `(elisp) Files`
- Info node: `(elisp) Coding Systems`
- Info node: `(emacs) International`
- Source: `src/coding.c:0` (extensive comments)

### Related Subsystems

- [Buffer Management](./01-buffer-management.md)
- [Display Engine](./02-display-engine.md)
- Process I/O (not yet documented)
- Network I/O (not yet documented)
