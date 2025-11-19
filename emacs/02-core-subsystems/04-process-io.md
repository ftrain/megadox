# Process Management and I/O System

This document provides comprehensive literate programming documentation for Emacs's process management and I/O system, one of the most sophisticated components of the editor.

## Table of Contents

1. [Overview and Architecture](#overview-and-architecture)
2. [Core Data Structures](#core-data-structures)
3. [Process Creation and Execution](#process-creation-and-execution)
4. [I/O System](#io-system)
5. [Process Filters and Sentinels](#process-filters-and-sentinels)
6. [Network Processes](#network-processes)
7. [Serial Port Communication](#serial-port-communication)
8. [PTY Allocation](#pty-allocation)
9. [Signal Handling](#signal-handling)
10. [Elisp Layer](#elisp-layer)
11. [Advanced Topics](#advanced-topics)
12. [Cross-Platform Considerations](#cross-platform-considerations)

## Overview and Architecture

Emacs's process management system provides a unified interface for:
- Asynchronous subprocess execution
- Network connections (TCP/UDP clients and servers)
- Serial port communication
- Pipe processes

### Core Files

| File | Lines | Purpose |
|------|-------|---------|
| `src/process.c` | 9,096 | Main process management (64 DEFUN declarations) |
| `src/sysdep.c` | 4,714 | System-dependent process operations |
| `src/callproc.c` | 2,247 | Synchronous subprocess invocation |
| `src/process.h` | 318 | Process data structures and interfaces |
| `lisp/comint.el` | 4,370 | Command interpreter in a buffer |
| `lisp/progmodes/compile.el` | ~3,000 | Compilation mode |

### Design Philosophy

The process system is designed with several key principles:

1. **Unified Abstraction**: All process types (subprocess, network, serial) share a common interface
2. **Asynchronous I/O**: Non-blocking operations with event-driven processing
3. **Extensibility**: Filters and sentinels provide hooks for custom processing
4. **Thread Safety**: Careful handling of signals and concurrent access
5. **Cross-Platform**: Abstractions over Unix/Windows differences

## Core Data Structures

### The Lisp_Process Structure

The heart of process management is `struct Lisp_Process`, defined in `src/process.h`:

```c
/* File: src/process.h, Lines: 42-214 */

struct Lisp_Process
  {
    union vectorlike_header header;

    /* Name of subprocess terminal.  */
    Lisp_Object tty_name;

    /* Name of this process.  */
    Lisp_Object name;

    /* List of command arguments that this process was run with.
       Is set to t for a stopped network process; nil otherwise.  */
    Lisp_Object command;

    /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
       to dispose of a bunch of chars from the process all at once.  */
    Lisp_Object filter;

    /* (funcall SENTINEL PROCESS) when process state changes.  */
    Lisp_Object sentinel;

    /* (funcall LOG SERVER CLIENT MESSAGE) when a server process
       accepts a connection from a client.  */
    Lisp_Object log;

    /* Buffer that output is going to.  */
    Lisp_Object buffer;

    /* t if this is a real child process.  For a network or serial
       connection, it is a plist based on the arguments to
       make-network-process or make-serial-process.  */
    Lisp_Object childp;

    /* Plist for programs to keep per-process state information, parameters, etc.  */
    Lisp_Object plist;

    /* Symbol indicating the type of process: real, network, serial.  */
    Lisp_Object type;

    /* Marker set to end of last buffer-inserted output from this process.  */
    Lisp_Object mark;

    /* Symbol indicating status of process.
       This may be a symbol: run, listen, or failed.
       Or it may be a pair (connect . ADDRINFOS) where ADDRINFOS is
       a list of remaining (PROTOCOL . ADDRINFO) pairs to try.
       Or it may be (failed ERR) where ERR is an integer, string or symbol.
       Or it may be a list, whose car is stop, exit or signal
       and whose cdr is a pair (EXIT_CODE . COREDUMP_FLAG)
       or (SIGNAL_NUMBER . COREDUMP_FLAG).  */
    Lisp_Object status;

    /* Coding-system for decoding the input from this process.  */
    Lisp_Object decode_coding_system;

    /* Working buffer for decoding.  */
    Lisp_Object decoding_buf;

    /* Coding-system for encoding the output to this process.  */
    Lisp_Object encode_coding_system;

    /* Working buffer for encoding.  */
    Lisp_Object encoding_buf;

    /* Queue for storing waiting writes.  */
    Lisp_Object write_queue;

    /* Pipe process attached to the standard error of this process.  */
    Lisp_Object stderrproc;

    /* The thread a process is linked to, or nil for any thread.  */
    Lisp_Object thread;

    /* After this point, there are no Lisp_Objects.  */

    /* Process ID.  A positive value is a child process ID.
       Zero is for pseudo-processes such as network or serial connections,
       or for processes that have not been fully created yet.
       -1 is for a process that was not created successfully.
       -2 is for a pty with no process, e.g., for GDB.  */
    pid_t pid;

    /* Descriptor by which we read from this process.  */
    int infd;

    /* Byte-count modulo (UINTMAX_MAX + 1) for process output read from `infd'.  */
    uintmax_t nbytes_read;

    /* Descriptor by which we write to this process.  */
    int outfd;

    /* Descriptors that were created for this process and that need
       closing.  Unused entries are negative.  */
    int open_fd[PROCESS_OPEN_FDS];

    /* Event-count of last event in which this process changed status.  */
    EMACS_INT tick;

    /* Event-count of last such event reported.  */
    EMACS_INT update_tick;

    /* Size of carryover in decoding.  */
    int decoding_carryover;

    /* Hysteresis to try to read process output in larger blocks.
       On some systems, e.g. GNU/Linux, Emacs is seen as
       an interactive app also when reading process output, meaning
       that process output can be read in as little as 1 byte at a
       time.  Value is nanoseconds to delay reading output from
       this process.  Range is 0 .. 50 * 1000 * 1000.  */
    int read_output_delay;

    /* Should we delay reading output from this process.
       Initialized from `Vprocess_adaptive_read_buffering'.
       0 = nil, 1 = t, 2 = other.  */
    unsigned int adaptive_read_buffering : 2;

    /* Skip reading this process on next read.  */
    bool_bf read_output_skip : 1;

    /* Maximum number of bytes to read in a single chunk. */
    ptrdiff_t readmax;

    /* True means kill silently if Emacs is exited.
       This is the inverse of the `query-on-exit' flag.  */
    bool_bf kill_without_query : 1;

    /* True if communicating through a pty for input or output.  */
    bool_bf pty_in : 1;
    bool_bf pty_out : 1;

    /* Flag to set coding-system of the process buffer from the
       coding_system used to decode process output.  */
    bool_bf inherit_coding_system_flag : 1;

    /* Whether the process is alive, i.e., can be waited for.  Running
       processes can be waited for, but exited and fake processes cannot.  */
    bool_bf alive : 1;

    /* Record the process status in the raw form in which it comes from `wait'.
       This is to avoid consing in a signal handler.  The `raw_status_new'
       flag indicates that `raw_status' contains a new status that still
       needs to be synced to `status'.  */
    bool_bf raw_status_new : 1;

    /* Whether this is a nonblocking socket. */
    bool_bf is_non_blocking_client : 1;

    /* Whether this is a server or a client socket. */
    bool_bf is_server : 1;

    int raw_status;

    /* The length of the socket backlog. */
    int backlog;

    /* The port number. */
    int port;

    /* The socket type. */
    int socktype;

#ifdef HAVE_GNUTLS
    gnutls_initstage_t gnutls_initstage;
    gnutls_session_t gnutls_state;
    gnutls_certificate_client_credentials gnutls_x509_cred;
    gnutls_anon_client_credentials_t gnutls_anon_cred;
    gnutls_x509_crt_t *gnutls_certificates;
    int gnutls_certificates_length;
    unsigned int gnutls_peer_verification;
    unsigned int gnutls_extra_peer_verification;
    int gnutls_log_level;
    int gnutls_handshakes_tried;
    bool_bf gnutls_p : 1;
    bool_bf gnutls_complete_negotiation_p : 1;
#endif
  } GCALIGNED_STRUCT;
```

**Key Design Points:**

1. **GC Alignment**: The structure is marked with `GCALIGNED_STRUCT` for proper garbage collection
2. **Lisp Objects First**: All Lisp_Object fields come before C types (required for GC marking)
3. **File Descriptors**: Separate `infd` and `outfd` for bidirectional communication
4. **Status Tracking**: Both symbolic (`status`) and raw (`raw_status`) forms
5. **Adaptive Buffering**: Fields for optimizing read performance
6. **Encoding Support**: Separate coding systems for input and output

### Process Type Predicates

```c
/* File: src/process.h, Lines: 216-233 */

INLINE bool
PROCESSP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_PROCESS);
}

INLINE void
CHECK_PROCESS (Lisp_Object x)
{
  CHECK_TYPE (PROCESSP (x), Qprocessp, x);
}

INLINE struct Lisp_Process *
XPROCESS (Lisp_Object a)
{
  eassert (PROCESSP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Process);
}
```

## Process Creation and Execution

### The make-process Function

`make-process` is the primary interface for creating asynchronous subprocesses:

```c
/* File: src/process.c, Lines: 1767-1849 */

DEFUN ("make-process", Fmake_process, Smake_process, 0, MANY, 0,
       doc: /* Start a program in a subprocess.  Return the process object for it.

This is similar to `start-process', but arguments are specified as
keyword/argument pairs.  The following arguments are defined:

:name NAME -- NAME is name for process.  It is modified if necessary
to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at end of that buffer, unless
you specify a filter function to handle the output.  BUFFER may be
also nil, meaning that this process is not associated with any buffer.

:command COMMAND -- COMMAND is a list starting with the program file
name, followed by strings to give to the program as arguments.  If the
program file name is not an absolute file name, `make-process' will
look for the program file name in `exec-path' (which is a list of
directories).

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and
the process is running.  If BOOL is not given, query before exiting.

:stop BOOL -- BOOL must be nil.  The `:stop' key is ignored otherwise
and is retained for compatibility with other process types such as
pipe processes.

:connection-type TYPE -- TYPE is control type of device used to
communicate with subprocesses.  Values are `pipe' to use a pipe, `pty'
to use a pty, or nil to use the default specified through
`process-connection-type'.  If TYPE is a cons (INPUT . OUTPUT), then
INPUT will be used for standard input and OUTPUT for standard output
(and standard error if `:stderr' is nil).

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:stderr STDERR -- STDERR is either a buffer or a pipe process attached
to the standard error of subprocess.

:file-handler FILE-HANDLER -- If FILE-HANDLER is non-nil, then look
for a file name handler for the current buffer's `default-directory'
and invoke that file name handler to make the process.

usage: (make-process &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object buffer, command, program, proc, contact, current_dir, tem;
  Lisp_Object xstderr, stderrproc;
  specpdl_ref count = SPECPDL_INDEX ();

  if (nargs == 0)
    return Qnil;
  CHECK_KEYWORD_ARGS (nargs);

  /* Save arguments for process-contact and clone-process.  */
  contact = Flist (nargs, args);

  if (!NILP (plist_get (contact, QCfile_handler)))
    {
      Lisp_Object file_handler
        = Ffind_file_name_handler (BVAR (current_buffer, directory),
                                   Qmake_process);
      if (!NILP (file_handler))
        return CALLN (Fapply, file_handler, Qmake_process, contact);
    }

  buffer = plist_get (contact, QCbuffer);
  /* ... continued ... */
```

**Process Creation Flow:**

1. Parse keyword arguments
2. Check for file handlers (TRAMP support)
3. Validate buffer and command
4. Set up encoding/decoding
5. Allocate PTY if needed
6. Fork and exec subprocess
7. Set up I/O descriptors
8. Install filter and sentinel
9. Add to process list

### Synchronous vs. Asynchronous Processes

Emacs supports two models:

**Asynchronous (process.c):**
```elisp
;; Non-blocking, returns immediately
(make-process :name "async"
              :buffer "*output*"
              :command '("long-running-command"))
```

**Synchronous (callproc.c):**
```elisp
;; Blocks until completion
(call-process "command" nil t nil "arg1" "arg2")
```

### Fork/Exec Model

The traditional Unix process creation follows this pattern:

```c
/* Conceptual flow - actual implementation in src/process.c */

1. allocate_pty() - if PTY requested
2. Setup file descriptors (pipes or PTY)
3. block_child_signal() - prevent race conditions
4. fork() - create child process
5. In child:
   - dup2() to redirect stdin/stdout/stderr
   - close unused file descriptors
   - set process group (setsid)
   - execvp() to run program
6. In parent:
   - Store process information
   - Setup read/write descriptors
   - Add to process list
7. unblock_child_signal()
```

### Modern Alternative: posix_spawn

On systems that support it, Emacs can use `posix_spawn` for better performance:

```c
/* File: src/callproc.c, Lines: 34-49 */

/* In order to be able to use `posix_spawn', it needs to support some
   variant of `chdir' as well as `setsid'.  */
#if defined HAVE_SPAWN_H && defined HAVE_POSIX_SPAWN        \
  && defined HAVE_POSIX_SPAWNATTR_SETFLAGS                  \
  && (defined HAVE_POSIX_SPAWN_FILE_ACTIONS_ADDCHDIR        \
      || defined HAVE_POSIX_SPAWN_FILE_ACTIONS_ADDCHDIR_NP) \
  && defined HAVE_DECL_POSIX_SPAWN_SETSID                   \
  && HAVE_DECL_POSIX_SPAWN_SETSID == 1
# include <spawn.h>
# define USABLE_POSIX_SPAWN 1
#else
# define USABLE_POSIX_SPAWN 0
#endif
```

Benefits of `posix_spawn`:
- Faster than fork/exec on some systems
- Better memory efficiency
- Avoids vfork issues
- Atomic setup of file descriptors and environment

## I/O System

### Non-Blocking I/O Architecture

Emacs uses non-blocking I/O for all asynchronous processes:

```c
/* File: src/process.c, Lines: 184-192 */

/* True if ERRNUM represents an error where the system call would
   block if a blocking variant were used.  */
static bool
would_block (int errnum)
{
#ifdef EWOULDBLOCK
  if (EWOULDBLOCK != EAGAIN && errnum == EWOULDBLOCK)
    return true;
#endif
  return errnum == EAGAIN;
}
```

### The Main Event Loop: wait_reading_process_output

This is the heart of Emacs's I/O system:

```c
/* File: src/process.c - Conceptual Overview */

wait_reading_process_output (
    intmax_t time_limit,     /* Maximum time to wait */
    int nsecs,               /* Nanoseconds component */
    int read_kbd,            /* Also check for keyboard input */
    bool do_display,         /* Update display while waiting */
    Lisp_Object wait_for_cell,
    struct Lisp_Process *wait_proc,
    int just_wait_proc)
{
    /* Key responsibilities:
       1. Use select()/pselect() to wait for I/O
       2. Handle process output when available
       3. Check for keyboard input if requested
       4. Handle SIGCHLD (process status changes)
       5. Respect timeouts
       6. Update display if requested
    */
}
```

**Event Loop Flow:**

```
┌─────────────────────────────────────┐
│ wait_reading_process_output         │
└───────────┬─────────────────────────┘
            │
            ├─► Setup file descriptor sets (FD_SET)
            │
            ├─► Call select()/pselect() - Wait for events
            │
            ├─► Process available input:
            │   │
            │   ├─► read_process_output()
            │   │   ├─► Read from file descriptor
            │   │   ├─► Decode using coding system
            │   │   └─► Call filter or insert in buffer
            │   │
            │   └─► Check keyboard input
            │
            ├─► Handle SIGCHLD:
            │   └─► Update process status
            │       └─► Call sentinel if status changed
            │
            └─► Check timeout and continue or return
```

### Reading Process Output

```c
/* File: src/process.c, Line: 274 */
static int read_process_output (Lisp_Object proc, int wait_proc_fd);

/* This function:
   1. Reads available data from the process
   2. Handles encoding/decoding
   3. Either calls the filter function or inserts into buffer
   4. Manages adaptive read buffering
   5. Updates process markers
*/
```

**Adaptive Read Buffering:**

```c
/* File: src/process.h, Lines: 150-164 */

/* Hysteresis to try to read process output in larger blocks.
   On some systems, e.g. GNU/Linux, Emacs is seen as
   an interactive app also when reading process output, meaning
   that process output can be read in as little as 1 byte at a
   time.  Value is nanoseconds to delay reading output from
   this process.  Range is 0 .. 50 * 1000 * 1000.  */
int read_output_delay;

/* Should we delay reading output from this process.
   Initialized from `Vprocess_adaptive_read_buffering'.
   0 = nil, 1 = t, 2 = other.  */
unsigned int adaptive_read_buffering : 2;
```

This clever optimization delays reading by a small amount to allow the OS to buffer more data, reducing the number of small reads.

### Encoding and Decoding on the Fly

All process I/O goes through Emacs's coding system:

```c
/* File: src/process.c, Lines: 6500-6518 */

/* Decoding input from process */
decode_coding_c_string (process_coding,
                        (unsigned char *) buf, nread, curbuf);

/* After decoding, insert into buffer */
TEMP_SET_PT_BOTH (PT + process_coding->produced_char,
                  PT_BYTE + process_coding->produced);
```

**Encoding output to process:**

```c
/* File: src/process.c, Lines: 6714+ */

send_process (Lisp_Object proc, const char *buf, ptrdiff_t len,
              Lisp_Object object)
{
  struct Lisp_Process *p = XPROCESS (proc);
  ssize_t rv;
  struct coding_system *coding;

  /* ... encoding happens here ... */
}
```

**Key Points:**

1. Input is decoded from process's character set to Emacs's internal format
2. Output is encoded from internal format to process's character set
3. Partial character sequences are handled across read boundaries
4. `decoding_carryover` field stores incomplete multibyte sequences

### Process Output Buffering

Output can be handled two ways:

**1. Direct insertion (default):**
```c
/* File: src/process.c, Lines: 6589-6599 */

DEFUN ("internal-default-process-filter", Finternal_default_process_filter,
       Sinternal_default_process_filter, 2, 2, 0,
       doc: /* Function used as default process filter.
This inserts the process's output into its buffer, if there is one.
Otherwise it discards the output.  */)
  (Lisp_Object proc, Lisp_Object text)
{
  struct Lisp_Process *p;

  CHECK_PROCESS (proc);
  p = XPROCESS (proc);
  /* Insert text at process mark... */
}
```

**2. Custom filter function:**
```c
/* File: src/process.c, Lines: 6521-6587 */

static void
read_and_dispose_of_process_output (struct Lisp_Process *p, char *chars,
                                    ssize_t nbytes,
                                    struct coding_system *coding)
{
  Lisp_Object outstream = p->filter;

  /* ... setup ... */

  if (fast_read_process_output
      && EQ (p->filter, Qinternal_default_process_filter))
    read_and_insert_process_output (p, chars, nbytes, coding);
  else
    {
      decode_coding_c_string (coding, (unsigned char *) chars, nbytes, Qt);
      text = coding->dst_object;

      if (SBYTES (text) > 0)
        internal_condition_case_1 (read_process_output_call,
                                   list3 (outstream, make_lisp_proc (p), text),
                                   !NILP (Vdebug_on_error) ? Qnil : Qerror,
                                   read_process_output_error_handler);
    }
}
```

### Write Queue for Output

When a process can't accept all data immediately, Emacs queues it:

```c
/* File: src/process.h, Line: 115 */

/* Queue for storing waiting writes.  */
Lisp_Object write_queue;
```

This allows non-blocking writes and prevents data loss when the pipe/socket is full.

## Process Filters and Sentinels

### Process Filters

Filters are the primary mechanism for handling process output:

```elisp
;; Install a filter
(set-process-filter proc
  (lambda (proc string)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert (format "Received: %s" string)))))
```

**C Implementation:**

```c
/* File: src/process.c, Lines: 1359-1407 */

DEFUN ("set-process-filter", Fset_process_filter, Sset_process_filter,
       2, 2, 0,
       doc: /* Give PROCESS the filter function FILTER; nil means default.
A value of t means stop accepting output from the process.

When a process has a non-default filter, its buffer is not used for output.
Instead, each time it does output, the entire string of output is
passed to the filter.

The filter gets two arguments: the process and the string of output.
The string argument is normally a multibyte string, except:
- if the process's input coding system is no-conversion or raw-text,
  it is a unibyte string (the non-converted input).  */)
  (Lisp_Object process, Lisp_Object filter)
{
  CHECK_PROCESS (process);
  struct Lisp_Process *p = XPROCESS (process);

  /* Don't signal an error if the process's input file descriptor
     is closed.  This could make debugging Lisp code difficult.  */
  if (NETCONN_P (process) || p->infd >= 0)
    {
      if (EQ (filter, Qt) && !EQ (p->status, Qlisten))
        {
          FD_CLR (p->infd, &input_wait_mask);
          FD_CLR (p->infd, &non_keyboard_wait_mask);
        }
      else if (EQ (p->filter, Qt)
               && !EQ (p->command, Qt)) /* Network process not stopped.  */
        {
          FD_SET (p->infd, &input_wait_mask);
          FD_SET (p->infd, &non_keyboard_wait_mask);
        }
    }

  pset_filter (p, filter);

  if (NETCONN_P (process) || SERIALCONN_P (process))
    pset_childp (p, plist_put (p->childp, QCfilter, filter));
  return filter;
}
```

**Filter Function Characteristics:**

1. Called asynchronously when output is available
2. Receives process object and output string
3. Can modify any buffer, not just the process buffer
4. Must handle partial lines
5. Can be set to `t` to stop accepting output

### Process Sentinels

Sentinels are called when a process changes state:

```elisp
;; Install a sentinel
(set-process-sentinel proc
  (lambda (proc event)
    (message "Process %s %s" (process-name proc) event)))
```

**C Implementation:**

```c
/* File: src/process.c, Lines: 7796-7861 */

static void
exec_sentinel (Lisp_Object proc, Lisp_Object reason)
{
  Lisp_Object sentinel, odeactivate;
  struct Lisp_Process *p = XPROCESS (proc);
  specpdl_ref count = SPECPDL_INDEX ();

  /* Inhibit quit so that random quits don't screw up a running filter.  */
  specbind (Qinhibit_quit, Qt);

  sentinel = p->sentinel;

  if (!NILP (sentinel))
    {
      /* We used to bind `inhibit-quit' to t here, but that's not
         needed now that we don't call Lisp code from
         handle_child_signal.  */

      Lisp_Object obuffer, okeymap;
      ptrdiff_t count1 = SPECPDL_INDEX ();

      /* Running the sentinel might delete the process, so save the
         buffer and the keymap now.  */
      XSETBUFFER (obuffer, current_buffer);
      okeymap = BVAR (current_buffer, keymap);

      /* Inhibit quit so that random quits don't screw up a running filter.  */
      specbind (Qinhibit_quit, Qt);
      specbind (Qlast_nonmenu_event, Qt);

      /* There's no good reason to let sentinels change the current
         buffer, and many callers of accept-process-output don't expect it.  */
      record_unwind_current_buffer ();

      sentinel = p->sentinel;
      if (NILP (sentinel))
        goto unlock;

      /* Zilch the sentinel while it's running, to avoid recursive invocations;
         assure that it gets restored no matter how the sentinel exits.  */
      pset_sentinel (p, Qnil);
      record_unwind_protect (exec_sentinel_restore, Fcons (proc, sentinel));

      internal_condition_case_1 (exec_sentinel_call, list2 (sentinel, proc, reason),
                                 Qt, exec_sentinel_error_handler);

    unlock:
      unbind_to (count1, Qnil);
    }

  unbind_to (count, Qnil);
}
```

**Sentinel Characteristics:**

1. Called when process status changes (exits, signals, etc.)
2. Receives process object and string describing the change
3. Sentinel is temporarily cleared during execution to prevent recursion
4. Errors in sentinels are caught and reported
5. Can examine exit status with `process-exit-status`

### Signal Handling and Sentinels

Process status changes are detected via SIGCHLD:

```c
/* File: src/process.c, Lines: 7687-7720 */

handle_child_signal (int sig)
{
  Lisp_Object tail, proc;
  bool changed = false;

  /* Find the process that signaled us, and record its status.  */

  /* The process can have been deleted by Fdelete_process, or have
     been started asynchronously by Fcall_process.  */
  for (tail = deleted_pid_list; CONSP (tail); tail = XCDR (tail))
    {
      /* ... check deleted processes ... */
    }

  for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
    {
      proc = XCDR (XCAR (tail));
      p = XPROCESS (proc);

      /* ... check if this process changed status ... */

      if (p->pid > 0)
        {
          pid_t pid;
          int status;

          /* Use waitpid to get status */
          pid = waitpid (p->pid, &status, WNOHANG | WUNTRACED);

          if (pid > 0)
            {
              /* Process status changed */
              p->raw_status = status;
              p->raw_status_new = 1;
              changed = true;
            }
        }
    }

  /* If any process changed status, call the sentinel */
  if (changed)
    status_notify (NULL, NULL);
}
```

## Network Processes

### Creating Network Processes

```c
/* File: src/process.c, Lines: 3804-3823 */

DEFUN ("make-network-process", Fmake_network_process, Smake_network_process,
       0, MANY, 0,
       doc: /* Create and return a network server or client process.

In Emacs, network connections are represented by process objects, so
input and output work as for subprocesses and `delete-process' closes
a network connection.  However, a network process has no process id,
it cannot be signaled, and the status codes are different from normal
processes.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:name NAME -- NAME is name for process.  It is modified if necessary
to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at end of that buffer, unless
you specify a filter function to handle the output.  BUFFER may be
also nil, meaning that this process is not associated with any buffer.
```

**Network Process Features:**

1. **Client connections**: TCP, UDP, local sockets
2. **Server sockets**: Listen and accept connections
3. **Non-blocking connects**: Asynchronous connection establishment
4. **TLS/SSL support**: Via GnuTLS integration
5. **Async DNS**: Non-blocking hostname resolution
6. **IPv4 and IPv6**: Full protocol support

### Network Server Example

```elisp
;; Create a TCP server on port 8080
(make-network-process
 :name "my-server"
 :server t
 :service 8080
 :sentinel 'my-server-sentinel
 :filter 'my-server-filter
 :log 'my-server-log)

;; Log function called when client connects
(defun my-server-log (server client message)
  (message "Connection from %s: %s" client message))
```

### Async DNS Resolution

Modern Emacs supports non-blocking DNS lookups:

```c
/* File: src/process.c, Lines: 5200-5228 */

#ifdef HAVE_GETADDRINFO_A

/* Check if a DNS lookup is complete */
if (p->dns_request)
  {
    int ret = gai_error (p->dns_request);

    if (ret == EAI_INPROGRESS)
      return Qnil;  /* Still waiting */

    /* We got a response. */
    if (ret == 0)
      {
        struct addrinfo *res;

        for (res = p->dns_request->ar_result; res; res = res->ai_next)
          addrinfos = Fcons (conv_addrinfo_to_lisp (res), addrinfos);

        addrinfos = Fnreverse (addrinfos);
      }
    /* The DNS lookup failed. */
    else if (connecting_status (p->status))
      {
        deactivate_process (proc);
        pset_status (p, (list2
                         (Qfailed,
                          concat3 (build_string ("Name lookup of "),
                                   build_string (p->dns_request->ar_name),
                                   build_string (" failed")))));
      }

    free_dns_request (proc);
  }
#endif
```

This prevents the entire Emacs process from blocking during DNS lookups.

## Serial Port Communication

Emacs can communicate with serial ports for embedded systems, Arduinos, etc.:

```c
/* File: src/process.c, Lines: 3112-3142 */

DEFUN ("make-serial-process", Fmake_serial_process, Smake_serial_process,
       0, MANY, 0,
       doc: /* Create and return a serial port process.

In Emacs, serial port connections are represented by process objects,
so input and output work as for subprocesses, and `delete-process'
closes a serial port connection.  However, a serial process has no
process id, it cannot be signaled, and the status codes are different
from normal processes.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:port PORT -- (mandatory) PORT is the path or name of the serial port.
For example, this could be "/dev/ttyS0" on Unix.  On Windows, this
could be "COM1", or "\\\\.\\COM10" for ports higher than COM9.

:speed SPEED -- (mandatory) SPEED is the terminal speed.
Possible values: 1200, 1800, 2400, 4800, 9600, 14400, 19200,
28800, 38400, 57600, 115200, 230400.

:stopbits STOPBITS -- STOPBITS is the number of stopbits.
STOPBITS = 1 or 2 (default 1).

:bytesize BYTESIZE -- BYTESIZE is the number of bits per byte.
BYTESIZE = 7 or 8 (default 8).

:parity PARITY -- PARITY can be nil (don't use parity), the symbol
`odd' (use odd parity), or the symbol `even' (use even parity).
```

**Example Usage:**

```elisp
;; Connect to Arduino on /dev/ttyUSB0
(setq arduino
      (make-serial-process
       :port "/dev/ttyUSB0"
       :speed 9600
       :coding 'no-conversion
       :filter 'arduino-filter))

;; Send commands
(process-send-string arduino "LED ON\n")
```

## PTY Allocation

PTY (pseudo-terminal) allocation is crucial for interactive programs:

```c
/* File: src/process.c, Lines: 841-891 */

allocate_pty (char pty_name[PTY_NAME_SIZE])
{
#ifdef HAVE_PTYS
  int fd;

#ifdef PTY_ITERATION
  PTY_ITERATION
#else
  register int c, i;
  for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
    for (i = 0; i < 16; i++)
#endif
      {
#ifdef PTY_NAME_SPRINTF
        PTY_NAME_SPRINTF
#else
        sprintf (pty_name, "/dev/pty%c%x", c, i);
#endif

#ifdef PTY_OPEN
        PTY_OPEN;
#else
        fd = emacs_open (pty_name, O_RDWR | O_NONBLOCK, 0);
#endif

        if (fd >= 0)
          {
#ifdef PTY_TTY_NAME_SPRINTF
            PTY_TTY_NAME_SPRINTF
#else
            /* ... get slave name ... */
#endif

            /* Check permissions */
            if (faccessat (AT_FDCWD, pty_name, R_OK | W_OK, AT_EACCESS) != 0)
              {
                emacs_close (fd);
                continue;
              }

            setup_pty (fd);
            return fd;
          }
      }
#endif /* HAVE_PTYS */
  return -1;
}
```

**Why PTYs Matter:**

1. **Line editing**: Programs like shells need PTY for line editing
2. **Job control**: PTYs support process groups and job control signals
3. **Terminal emulation**: Programs can detect they're running in a terminal
4. **Character-at-a-time I/O**: For interactive programs

**PTY vs. Pipe:**

| Feature | PTY | Pipe |
|---------|-----|------|
| Buffering | Line buffering | Block buffering |
| Job Control | Yes | No |
| Terminal Detection | isatty() returns true | isatty() returns false |
| Overhead | Higher | Lower |
| Use Case | Interactive shells | Non-interactive commands |

## Signal Handling

### Child Process Signals

Emacs uses a clever self-pipe trick to handle SIGCHLD safely:

```c
/* File: src/process.c, Lines: 297-302 */

/* File descriptor that becomes readable when we receive SIGCHLD.  */
static int child_signal_write_fd = -1;

#ifndef WINDOWSNT
static void child_signal_read (int, void *);
#endif
```

**The Self-Pipe Pattern:**

```
SIGCHLD arrives
     ↓
Signal handler writes byte to pipe
     ↓
Main event loop detects readable pipe
     ↓
Calls waitpid() to get child status
     ↓
Updates process object
     ↓
Calls sentinel if needed
```

This avoids calling non-async-signal-safe functions in the signal handler.

### Sending Signals to Processes

Users can send various signals:

```elisp
;; Send SIGINT (Ctrl-C)
(interrupt-process proc)

;; Send SIGTERM
(kill-process proc)

;; Send SIGSTOP
(stop-process proc)

;; Send SIGCONT
(continue-process proc)

;; Send arbitrary signal
(signal-process proc 'SIGUSR1)
```

## Elisp Layer

### comint.el - Command Interpreter

The `comint` package provides a framework for process interaction:

```elisp
/* File: lisp/comint.el, Lines: 27-54 */

;;; Commentary:

;; This file defines a general command-interpreter-in-a-buffer package
;; (comint mode).  The idea is that you can build specific process-in-a-buffer
;; modes on top of comint mode -- e.g., Lisp, shell, scheme, T, soar, ....
;; This way, all these specific packages share a common base functionality,
;; and a common set of bindings, which makes them easier to use (and
;; saves code, implementation time, etc., etc.).

;; Several packages are already defined using comint mode:
;; - shell.el defines a shell-in-a-buffer mode.
;; - cmulisp.el defines a simple lisp-in-a-buffer mode.
;;
;; - The file cmuscheme.el defines a scheme-in-a-buffer mode.
;; - The file tea.el tunes scheme and inferior-scheme modes for T.
;; - The file soar.el tunes Lisp and inferior-lisp modes for Soar.
;; - cmutex.el defines TeX and LaTeX modes that invoke TeX, LaTeX, BibTeX,
;;   previewers, and printers from within Emacs.
;; - background.el allows csh-like job control inside Emacs.
;; It is pretty easy to make new derived modes for other processes.
```

**Key comint Features:**

1. **Input History**: Cycle through previous commands with M-p/M-n
2. **Output Handling**: Smart handling of prompts and output
3. **Completion**: Filename and command completion
4. **Password Input**: Detect password prompts and disable echoing
5. **ANSI Color**: Process terminal escape sequences

**Major comint-based modes:**
- `shell-mode` - Interactive shell
- `ielm-mode` - Interactive Emacs Lisp
- `inferior-python-mode` - Python REPL
- `sql-interactive-mode` - Database shells

### compile.el - Compilation Mode

The compilation mode for running compilers and parsing errors:

```elisp
/* File: lisp/progmodes/compile.el, Lines: 25-40 */

;;; Commentary:

;; This package provides the compile facilities documented in the Emacs user's
;; manual.

;;; Key Features:

;; 1. Error Parsing: Automatically parse compiler output
;; 2. Navigation: Jump to errors with next-error/previous-error
;; 3. Highlighting: Colorize errors, warnings, info messages
;; 4. Recompilation: Rerun with the same command
;; 5. Multiple Formats: Support many compiler output formats
```

**Error Parsing Example:**

```elisp
;; Run make
(compile "make")

;; In the *compilation* buffer:
;; foo.c:42:10: error: undeclared identifier 'bar'

;; Press RET or next-error to jump to foo.c line 42
```

### Process API Summary

**Creation:**
- `make-process` - Asynchronous subprocess
- `start-process` - Simplified async process
- `call-process` - Synchronous subprocess
- `call-process-region` - Sync with region as input
- `make-network-process` - Network connection
- `make-serial-process` - Serial port

**Querying:**
- `process-status` - Current status (run, exit, signal, etc.)
- `process-exit-status` - Exit code
- `process-id` - Process ID
- `process-command` - Command that started it
- `process-buffer` - Associated buffer
- `process-mark` - Output insertion point

**I/O:**
- `process-send-string` - Send string to process
- `process-send-region` - Send buffer region
- `process-send-eof` - Send EOF
- `set-process-filter` - Install output handler
- `set-process-sentinel` - Install status change handler

**Control:**
- `delete-process` - Kill process
- `interrupt-process` - Send SIGINT
- `kill-process` - Send SIGKILL
- `quit-process` - Send SIGQUIT
- `stop-process` - Send SIGSTOP
- `continue-process` - Send SIGCONT

**Properties:**
- `process-get` / `process-put` - Get/set plist values
- `process-plist` - Get full property list
- `set-process-query-on-exit-flag` - Control exit query

## Advanced Topics

### Process Environment

Each process inherits or can customize its environment:

```elisp
;; Set environment for a process
(let ((process-environment (copy-sequence process-environment)))
  (setenv "PATH" "/custom/path")
  (setenv "LANG" "en_US.UTF-8")
  (make-process :name "custom-env"
                :command '("program")))
```

The environment is copied at process creation time.

### Subprocess Queries

```c
/* Get list of all processes */
DEFUN ("process-list", Fprocess_list, Sprocess_list, 0, 0, 0,
       doc: /* Return a list of all processes that are Emacs sub-processes.  */)
  (void)
{
  return Fmapcar (Qcdr, Vprocess_alist);
}
```

### Process Connections

The `:use-external-socket` feature allows using externally created sockets:

```elisp
;; Accept pre-created socket (systemd socket activation, etc.)
(make-network-process
 :name "external"
 :use-external-socket t
 :service socket-fd)
```

### Pipe Processes

Create a pipe between two processes:

```elisp
;; Pipe stderr to a separate buffer
(let* ((stderr-buf (generate-new-buffer "*stderr*"))
       (stderr-proc (make-pipe-process
                     :name "stderr-pipe"
                     :buffer stderr-buf))
       (main-proc (make-process
                   :name "main"
                   :buffer "*output*"
                   :command '("command")
                   :stderr stderr-proc)))
  main-proc)
```

### Thread Affinity

Processes can be bound to specific threads:

```c
/* File: src/process.h, Line: 126 */

/* The thread a process is linked to, or nil for any thread.  */
Lisp_Object thread;
```

```elisp
;; Bind process to current thread
(set-process-thread proc (current-thread))
```

### Adaptive Read Buffering

Control read performance:

```elisp
;; Enable adaptive buffering (default)
(setq process-adaptive-read-buffering t)

;; Disable for low latency
(setq process-adaptive-read-buffering nil)

;; Set maximum read size
(setq read-process-output-max (* 1024 1024)) ; 1MB
```

### File Handlers and TRAMP

Process creation respects file handlers:

```elisp
;; This works over TRAMP
(let ((default-directory "/ssh:remote:/path"))
  (make-process :name "remote"
                :command '("ls" "-la")))
```

The `:file-handler` keyword controls this:

```elisp
;; Explicitly disable file handler
(make-process :name "local-only"
              :command '("ls")
              :file-handler nil)
```

## Cross-Platform Considerations

### Unix vs. Windows

| Feature | Unix | Windows |
|---------|------|---------|
| PTY Support | Yes | Limited |
| Fork/Exec | Native | Emulated |
| Signal Delivery | POSIX signals | Limited |
| Process Groups | Full support | Partial |
| Select/Poll | Native | Emulated |
| Local Sockets | Unix domain | Named pipes |

### Platform-Specific Code

The process system has many conditional compilation sections:

```c
#ifdef subprocesses
/* Full process support */
#else
/* MS-DOS: No subprocess support */
#define PIPECONN_P(p) false
#endif

#ifdef WINDOWSNT
/* Windows-specific implementations */
extern int sys_select (...);
#endif

#ifdef HAVE_PTYS
/* PTY allocation code */
#endif

#ifdef HAVE_GNUTLS
/* TLS/SSL support */
#endif
```

### macOS Specifics

- Uses `kqueue` for efficient event notification
- Special handling for framework integration
- Different PTY naming conventions

### Android

Special support for Android:

```c
#ifdef HAVE_ANDROID
#include "android.h"
#include "androidterm.h"
#endif
```

Handles Android's unique process model and restrictions.

## Performance Considerations

### Optimizing Process I/O

1. **Increase read buffer size:**
```elisp
(setq read-process-output-max (* 1024 1024)) ; 1MB chunks
```

2. **Use binary I/O when possible:**
```elisp
(make-process :name "binary"
              :command '("cat" "file.bin")
              :coding 'no-conversion)
```

3. **Batch writes:**
```elisp
;; Bad: Multiple small writes
(dotimes (i 1000)
  (process-send-string proc (format "%d\n" i)))

;; Good: One large write
(process-send-string proc
  (mapconcat (lambda (i) (format "%d" i))
             (number-sequence 0 999)
             "\n"))
```

4. **Disable adaptive buffering for low latency:**
```elisp
(setq process-adaptive-read-buffering nil)
```

### Memory Usage

- Process buffers grow unbounded by default
- Use filters to limit buffer size
- Consider circular buffers for logs

```elisp
(defun limit-buffer-size (proc string)
  "Insert STRING but keep buffer under 100KB."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (when (> (buffer-size) 100000)
      (delete-region (point-min)
                     (- (point-max) 100000)))))
```

## Debugging Process Issues

### Useful Debug Variables

```elisp
;; Log all process events
(setq process-adaptive-read-buffering 'debug)

;; Show process output in real-time
(setq debug-on-error t)

;; Inspect process state
(process-attributes (process-id proc))
```

### Common Issues

**1. Process Not Producing Output**
- Check if program is buffering stdout
- Try using PTY instead of pipe
- Verify encoding settings

**2. "Process Not Running" Errors**
- Check process status: `(process-status proc)`
- Examine exit status: `(process-exit-status proc)`
- Review sentinel for clues

**3. High CPU Usage**
- Check for rapid output
- Increase `read-process-output-max`
- Optimize filter function

**4. Encoding Issues**
- Verify `:coding` parameter
- Check `process-coding-system`
- Use `set-process-coding-system`

## Summary

Emacs's process management and I/O system is a sophisticated piece of engineering that provides:

1. **Unified Interface**: Subprocesses, network, and serial all use the same API
2. **Asynchronous Operation**: Non-blocking I/O throughout
3. **Rich Features**: Filters, sentinels, encoding, signals
4. **Cross-Platform**: Works on Unix, Windows, macOS, Android
5. **Performance**: Adaptive buffering, efficient event loops
6. **Extensibility**: Elisp can customize every aspect

The system balances power with usability, allowing both simple process creation:

```elisp
(start-process "ls" "*ls*" "ls" "-la")
```

And sophisticated network servers:

```elisp
(make-network-process
 :name "http-server"
 :server t
 :service 8080
 :sentinel 'http-sentinel
 :filter 'http-filter
 :log 'http-log
 :plist '(:clients nil))
```

Understanding this subsystem is crucial for:
- Writing modes that interact with external programs
- Building network clients and servers
- Implementing REPL modes
- Creating build systems
- Developing remote editing capabilities

The process system truly makes Emacs an operating system within an operating system.

## References

**Source Files:**
- `/home/user/emacs/src/process.c` - Main process implementation
- `/home/user/emacs/src/process.h` - Process structures
- `/home/user/emacs/src/callproc.c` - Synchronous processes
- `/home/user/emacs/src/sysdep.c` - System-dependent operations
- `/home/user/emacs/lisp/comint.el` - Command interpreter framework
- `/home/user/emacs/lisp/progmodes/compile.el` - Compilation mode

**Documentation:**
- Info node `(elisp) Processes`
- Info node `(elisp) Asynchronous Processes`
- Info node `(elisp) Network`
- Info node `(elisp) Serial Ports`

**Key Functions (DEFUN count: 64 in process.c):**
- Process creation: `make-process`, `make-network-process`, `make-serial-process`
- Process control: `delete-process`, `interrupt-process`, `signal-process`
- I/O: `process-send-string`, `process-send-region`
- Filters/Sentinels: `set-process-filter`, `set-process-sentinel`
- Queries: `process-status`, `process-list`, `process-attributes`
