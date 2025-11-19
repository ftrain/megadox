# Emacs Lisp Standard Library

*A literate programming guide to the core libraries that power Emacs*

---

## Table of Contents

1. [Introduction](#introduction)
2. [Core Utilities](#core-utilities)
   - [subr.el - Fundamental Subroutines](#subrel---fundamental-subroutines)
   - [simple.el - Basic Editing Commands](#simpleel---basic-editing-commands)
   - [files.el - File Operations](#filesel---file-operations)
   - [window.el - Window Management](#windowel---window-management)
3. [Data Structures](#data-structures)
   - [seq.el - Sequence Manipulation](#seqel---sequence-manipulation)
   - [map.el - Map/Dictionary Operations](#mapel---mapdictionary-operations)
   - [ring.el - Ring Buffers](#ringel---ring-buffers)
   - [avl-tree.el - Balanced Binary Trees](#avl-treeel---balanced-binary-trees)
4. [Completion Framework](#completion-framework)
   - [minibuffer.el - Minibuffer and Completion](#minibufferel---minibuffer-and-completion)
5. [Search and Replace](#search-and-replace)
   - [isearch.el - Incremental Search](#isearchel---incremental-search)
6. [Help System](#help-system)
   - [help.el - Help Commands](#helpel---help-commands)
7. [Customization](#customization)
   - [custom.el - Customization Framework](#customel---customization-framework)

---

## Introduction

The Emacs Lisp standard library comprises the foundational Elisp code that all other Emacs functionality depends on. These libraries, residing in the `lisp/` directory, provide everything from fundamental data structures and control flow to file operations, window management, and user interaction.

This document follows the **literate programming** philosophy: we'll explore not just what the code does, but *why* it exists, how it's structured, and how the pieces fit together. Each section combines narrative explanation with concrete code examples and API documentation.

### Design Philosophy

The standard library reflects several key design principles:

1. **Progressive Enhancement**: Simple APIs for common cases, with extensible mechanisms for complex scenarios
2. **Interactivity First**: Most functions work both programmatically and interactively
3. **Buffer-Centric**: Operations typically apply to the current buffer unless specified otherwise
4. **Customizable by Default**: Extensive use of hooks, variables, and customization groups

---

## Core Utilities

### subr.el - Fundamental Subroutines

**Location**: `/home/user/emacs/lisp/subr.el` (7,876 lines)

`subr.el` contains the fundamental building blocks of Elisp - the subroutines that are loaded before almost anything else. These functions are so fundamental that most Elisp programmers use them without thinking about where they come from.

#### Philosophy and Structure

The file deliberately avoids dependencies - it can't even use backquotes in its macro definitions because backquote.el hasn't loaded yet! This constraint forces extreme simplicity and elegance.

#### Basic Macros and Control Flow

##### Lambda Functions

```elisp
(defmacro lambda (&rest cdr)
  "Return an anonymous function.
Under lexical binding, the result is a closure."
  (list 'function (cons 'lambda cdr)))
```

The `lambda` macro is foundational - it creates anonymous functions. Under lexical binding (now the default), it produces closures that capture their environment.

**Example**:
```elisp
;; Create a counter using closure
(let ((count 0))
  (lambda () (setq count (1+ count))))
```

##### Conditional Execution

```elisp
(defmacro when (cond &rest body)
  "If COND yields non-nil, do BODY, else return nil."
  (declare (indent 1) (debug t))
  `(if ,cond (progn ,@body)))

(defmacro unless (cond &rest body)
  "If COND yields nil, do BODY, else return nil."
  (declare (indent 1) (debug t))
  `(if ,cond nil (progn ,@body)))
```

These macros provide more readable alternatives to `if` when you only care about one branch:

```elisp
;; Instead of: (if (buffer-modified-p) (save-buffer) nil)
(when (buffer-modified-p)
  (save-buffer))

;; Instead of: (if (not (file-exists-p "~/.emacs")) (create-file "~/.emacs"))
(unless (file-exists-p "~/.emacs")
  (create-file "~/.emacs"))
```

#### Variable Manipulation

##### Buffer-Local Variables

```elisp
(defmacro setq-local (&rest pairs)
  "Make each VARIABLE local to current buffer and set it to corresponding VALUE."
  (declare (debug setq))
  (unless (evenp (length pairs))
    (error "PAIRS must have an even number of variable/value members"))
  (let ((expr nil))
    (while pairs
      (unless (symbolp (car pairs))
        (error "Attempting to set a non-symbol: %s" (car pairs)))
      (setq expr
            (cons
             (list 'setq (car pairs)
                   (list 'prog1
                    (car (cdr pairs))
                    (list 'make-local-variable (list 'quote (car pairs)))))
             expr))
      (setq pairs (cdr (cdr pairs))))
    (macroexp-progn (nreverse expr))))
```

This powerful macro makes variables buffer-local and sets them in one operation. It's used extensively throughout Emacs for mode-specific configuration:

```elisp
;; In a major mode's setup
(defun my-mode ()
  (setq-local comment-start "# "
              comment-end ""
              indent-tabs-mode nil))
```

##### Default Values

```elisp
(defmacro setq-default (&rest args)
  "Set the default value of variable VAR to VALUE.
More generally, you can use multiple variables and values, as in
  (setq-default VAR VALUE VAR VALUE...)"
  (declare (debug setq))
  (let ((exps nil))
    (while args
      (push `(set-default ',(pop args) ,(pop args)) exps))
    `(progn . ,(nreverse exps))))
```

Sets the global default value of a variable, used as a fallback when buffers don't have buffer-local values.

#### List Operations

##### List Traversal

```elisp
(defmacro dolist (spec &rest body)
  "Loop over a list, evaluating BODY with VAR bound to each element.
\(fn (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  ;; Implementation uses while loops internally
  ...)

(defmacro dotimes (spec &rest body)
  "Loop a certain number of times, evaluating BODY with VAR bound to each integer.
\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent 1) (debug dolist))
  ...)
```

The workhorses of iteration in Elisp:

```elisp
;; Iterate over a list
(dolist (file '("foo.el" "bar.el" "baz.el"))
  (load file))

;; Count from 0 to 9
(dotimes (i 10)
  (insert (format "Line %d\n" i)))
```

##### List Manipulation

```elisp
(defun last (list &optional n)
  "Return the last link of LIST.  Its car is the last element.
If N is non-nil, return the Nth-to-last link of LIST."
  ...)

(defun butlast (list &optional n)
  "Return a copy of LIST with the last N elements removed.
If N is omitted or nil, the last element is removed."
  ...)

(defun delete-dups (list)
  "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list."
  ...)
```

These functions provide essential list processing:

```elisp
(last '(1 2 3 4 5))           ; => (5)
(butlast '(1 2 3 4 5))        ; => (1 2 3 4)
(delete-dups '(1 2 2 3 3 3))  ; => (1 2 3)
```

#### Association Lists (alists)

```elisp
(defun alist-get (key alist &optional default remove testfn)
  "Return the value associated with KEY in ALIST.
If KEY is not found, return DEFAULT.
TESTFN defaults to `eq' when comparing keys."
  ...)

(defun assoc-default (key alist &optional test default)
  "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects. Each element
 (or the element's car, if it is a cons) is compared with KEY by
 calling TEST, with two arguments: (i) the element or its car,
 and (ii) KEY."
  ...)
```

Association lists are one of Elisp's primary data structures:

```elisp
(let ((config '((indent . 4)
                (width . 80)
                (style . "gnu"))))
  (alist-get 'indent config))  ; => 4
```

#### Numeric Predicates

```elisp
(defun zerop (number)
  "Return t if NUMBER is zero."
  (= number 0))

(defun plusp (number)
  "Return t if NUMBER is positive."
  (> number 0))

(defun minusp (number)
  "Return t if NUMBER is negative."
  (< number 0))

(defun oddp (number)
  "Return t if INTEGER is odd."
  (= (logand number 1) 1))

(defun evenp (number)
  "Return t if INTEGER is even."
  (= (logand number 1) 0))
```

These predicates make numeric code more readable:

```elisp
(if (zerop count)
    (message "No items")
  (message "%d items" count))
```

#### Key Functions Reference

| Function | Purpose | Example |
|----------|---------|---------|
| `lambda` | Create anonymous function | `(lambda (x) (* x 2))` |
| `when` | Conditional execution (true branch only) | `(when test (do-something))` |
| `unless` | Conditional execution (false branch only) | `(unless ready (wait))` |
| `dolist` | Iterate over list elements | `(dolist (x list) (print x))` |
| `dotimes` | Iterate N times | `(dotimes (i 10) (insert "*"))` |
| `push` | Add element to list | `(push item stack)` |
| `pop` | Remove and return first element | `(pop stack)` |
| `setq-local` | Set buffer-local variable | `(setq-local indent-tabs-mode nil)` |
| `alist-get` | Retrieve from association list | `(alist-get 'key alist)` |

---

### simple.el - Basic Editing Commands

**Location**: `/home/user/emacs/lisp/simple.el` (11,712 lines)

`simple.el` contains the basic editing commands that users interact with daily - commands for moving point, inserting text, deleting text, and manipulating buffers. Despite the name "simple," this file is one of the largest in the standard library!

#### The Next-Error Framework

A powerful but often overlooked feature is the next-error framework, which provides a generic interface for navigating through lists of locations (compilation errors, grep matches, etc.):

```elisp
(defun next-error (&optional arg reset)
  "Visit next compilation error and return buffer.
This function operates on a buffer with the most recent compilation,
grep, occur, etc. output."
  ...)

(defun previous-error (&optional n)
  "Visit previous compilation error and return buffer."
  (interactive "p")
  (next-error (- (or n 1))))
```

**Usage Example**:
```elisp
;; After running M-x grep
;; C-x ` (next-error) jumps to first match
;; Subsequent C-x ` jumps to next matches
```

#### Movement Commands

```elisp
(defun beginning-of-buffer (&optional arg)
  "Move point to the beginning of the buffer."
  (interactive "^P")
  (or (consp arg)
      (region-active-p)
      (push-mark))
  (let ((size (- (point-max) (point-min))))
    (goto-char (if (and arg (not (consp arg)))
                   (+ (point-min)
                      (if (> size 10000)
                          ;; Avoid overflow for large buffer sizes!
                          (* (prefix-numeric-value arg)
                             (/ size 10))
                        (/ (+ 10 (* size (prefix-numeric-value arg)))
                           10)))
                 (point-min))))
  (if (and arg (not (consp arg)))
      (forward-line 1)))

(defun end-of-buffer (&optional arg)
  "Move point to the end of the buffer."
  ...)
```

These commands demonstrate Emacs's philosophy: even "simple" movement commands handle edge cases (large buffers, numeric arguments, mark management) gracefully.

#### Text Insertion and Deletion

```elisp
(defun newline (&optional arg interactive)
  "Insert a newline, and move to left margin of the new line if it's blank.
If option `use-hard-newlines' is non-nil, the newline is marked with
the text-property `hard'."
  (interactive "*P\np")
  ...)

(defun delete-blank-lines ()
  "On blank line, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete any immediately following blank lines."
  (interactive "*")
  ...)

(defun just-one-space (&optional n)
  "Delete all spaces and tabs around point, leaving one space (or N spaces)."
  (interactive "*p")
  (cycle-spacing n nil 'single-shot))
```

**Interactive Usage**:
- `C-o` (`open-line`) - Insert newline without moving point
- `C-x C-o` (`delete-blank-lines`) - Clean up excess blank lines
- `M-SPC` (`just-one-space`) - Collapse whitespace to single space

#### Counting and Position Information

```elisp
(defun count-words-region (start end &optional arg)
  "Count the number of words in the region."
  ...)

(defun count-lines (start end &optional ignore-invisible-lines)
  "Return number of lines between START and END."
  ...)

(defun what-line ()
  "Print the current buffer line number and narrowing status of point."
  (interactive)
  (let ((start (point-min))
        (n (line-number-at-pos)))
    (message "Line %d" n)))

(defun what-cursor-position (&optional detail)
  "Print info on cursor position (on screen and within buffer)."
  (interactive "P")
  ;; Displays: character, encoding, point position, total size, column
  ...)
```

These introspective commands help users understand their position in a buffer. They're extensively used in mode lines and status displays.

#### The Mark and Region

```elisp
(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))
```

The mark-ring system is fundamental to Emacs's editing model, allowing users to mark positions and return to them:

```elisp
;; Mark current position
(push-mark)

;; Jump back to previous mark
(set-mark-command t)  ; C-u C-SPC
```

#### Key Functions Reference

| Function | Purpose | Key Binding |
|----------|---------|-------------|
| `next-error` | Jump to next error/match | `C-x `` |
| `beginning-of-buffer` | Move to buffer start | `M-<` |
| `end-of-buffer` | Move to buffer end | `M->` |
| `newline` | Insert newline | `RET` |
| `delete-blank-lines` | Clean up blank lines | `C-x C-o` |
| `just-one-space` | Collapse whitespace | `M-SPC` |
| `count-words-region` | Count words in region | `M-=` |
| `what-cursor-position` | Show position info | `C-x =` |
| `mark-whole-buffer` | Select entire buffer | `C-x h` |

---

### files.el - File Operations

**Location**: `/home/user/emacs/lisp/files.el` (9,391 lines)

`files.el` handles all file-related operations: visiting files, saving buffers, backups, auto-saves, file-name handling, and directory navigation. It's the interface between Emacs buffers and the filesystem.

#### File Name Manipulation

```elisp
(defun abbreviate-file-name (filename)
  "Return a version of FILENAME shortened using `directory-abbrev-alist'.
Also replaces home directory with ~ if applicable."
  ...)

(defun directory-abbrev-apply (filename)
  "Apply the abbreviations in `directory-abbrev-alist' to FILENAME."
  (dolist (dir-abbrev directory-abbrev-alist filename)
    (when (string-match (car dir-abbrev) filename)
      (setq filename (concat (cdr dir-abbrev)
                             (substring filename (match-end 0)))))))
```

**Practical Use**:
```elisp
;; Configure abbreviations
(setq directory-abbrev-alist
      '(("\\`/home/user/projects/" . "~/proj/")
        ("\\`/very/long/path/to/src/" . "/src/")))

;; Now file names are displayed more concisely
(abbreviate-file-name "/home/user/file.txt")  ; => "~/file.txt"
```

#### Finding and Visiting Files

```elisp
(defun find-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  ...)

(defun find-file-noselect (filename &optional nowarn rawfile wildcards)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but verify
that the file has not changed since visited or saved."
  ...)
```

The distinction is important:
- `find-file` - Visit file and display its buffer (interactive)
- `find-file-noselect` - Load file into buffer but don't display (programmatic)

**Example**:
```elisp
;; Load a file without displaying it
(with-current-buffer (find-file-noselect "config.el")
  (goto-char (point-min))
  (search-forward "setting")
  (buffer-substring-no-properties (point) (line-end-position)))
```

#### Temporary Files

```elisp
(defun make-temp-file (prefix &optional dir-flag suffix text)
  "Create a temporary file.
PREFIX is a string to be used in generating the file name.
If DIR-FLAG is non-nil, create a directory instead of a file.
SUFFIX, if non-nil, is added to the end of the file name.
TEXT, if non-nil, is written to the file initially."
  ...)
```

**Usage**:
```elisp
;; Create temporary file for processing
(let ((temp-file (make-temp-file "emacs-data-" nil ".json")))
  (with-temp-file temp-file
    (insert (json-encode data)))
  ;; Process temp-file
  (delete-file temp-file))
```

#### Backup and Auto-Save Configuration

```elisp
(defcustom make-backup-files t
  "Non-nil means make a backup of a file the first time it is saved."
  :type 'boolean
  :group 'backup)

(defcustom backup-by-copying nil
  "Non-nil means always use copying to create backup files."
  :type 'boolean
  :group 'backup)

(defcustom backup-directory-alist nil
  "Alist of filename patterns and backup directory names."
  :type '(repeat (cons (regexp :tag "Regexp matching filename")
                       (directory :tag "Backup directory name")))
  :group 'backup)
```

**Configuration Example**:
```elisp
;; Store all backups in one directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups"))))

;; Keep multiple versions
(setq version-control t
      kept-new-versions 10
      kept-old-versions 5
      delete-old-versions t)
```

#### Directory Operations

```elisp
(defun directory-files-recursively (dir regexp &optional include-directories
                                        predicate follow-symlinks)
  "Return list of all files under DIR that have file names matching REGEXP.
This function works recursively."
  ...)

(defun locate-dominating-file (file name)
  "Look up the directory hierarchy from FILE for a directory containing NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found."
  ...)
```

The `locate-dominating-file` function is crucial for project-aware features:

```elisp
;; Find project root (directory containing .git)
(locate-dominating-file default-directory ".git")

;; Find configuration file in parent directories
(locate-dominating-file buffer-file-name ".editorconfig")
```

#### Key Functions Reference

| Function | Purpose | Use Case |
|----------|---------|----------|
| `find-file` | Visit file interactively | User opens file |
| `find-file-noselect` | Load file programmatically | Background processing |
| `save-buffer` | Save current buffer | Persist changes |
| `write-file` | Save with new name | "Save As" operation |
| `make-temp-file` | Create temporary file | Processing scratch space |
| `directory-files-recursively` | List files recursively | Build file lists |
| `locate-dominating-file` | Find project root | Project detection |
| `abbreviate-file-name` | Shorten file paths | Display optimization |

---

### window.el - Window Management

**Location**: `/home/user/emacs/lisp/window.el` (11,465 lines)

`window.el` manages the window system - splitting, displaying buffers, managing window configurations, and controlling how Emacs decides where to show things. Windows in Emacs are viewport regions showing buffers, distinct from GUI frames.

#### The Window Selection State

```elisp
(defmacro save-selected-window (&rest body)
  "Execute BODY, then select the previously selected window.
This macro saves and restores the selected window, as well as the
selected window in each frame."
  (declare (indent 0) (debug t))
  `(let ((save-selected-window--state (internal--before-save-selected-window)))
     (save-current-buffer
       (unwind-protect
           (progn ,@body)
         (internal--after-save-selected-window save-selected-window--state)))))
```

This macro is fundamental for operations that temporarily switch windows:

```elisp
(defun my-peek-other-window ()
  "Temporarily show another window's content."
  (save-selected-window
    (other-window 1)
    (message "Other window shows: %s" (buffer-name))
    ;; Window selection automatically restored
    ))
```

#### Temporary Buffer Display

```elisp
(defmacro with-temp-buffer-window (buffer-or-name action quit-function &rest body)
  "Bind `standard-output' to BUFFER-OR-NAME, eval BODY, show the buffer.
BUFFER-OR-NAME must specify either a live buffer, or the name of
a buffer."
  (declare (debug t))
  ...)

(defun temp-buffer-window-show (buffer &optional action)
  "Show temporary buffer BUFFER in a window.
Return the window showing BUFFER."
  ...)
```

This pattern is used throughout Emacs for help buffers, completions, and other transient displays:

```elisp
(with-temp-buffer-window "*My Output*" nil nil
  (princ "Temporary output here\n")
  (princ "Will be displayed in a window"))
```

#### Display Actions

The display-buffer system is Emacs's sophisticated mechanism for controlling where buffers appear:

```elisp
;; Display buffer in specific location
(display-buffer buffer
  '((display-buffer-reuse-window
     display-buffer-below-selected)
    (window-height . 10)))

;; Display but don't select
(display-buffer-no-window buffer
  '((side . bottom)
    (slot . 0)))
```

**Action Functions**:
- `display-buffer-same-window` - Reuse selected window
- `display-buffer-below-selected` - Split and show below
- `display-buffer-at-bottom` - Use bottom of frame
- `display-buffer-reuse-window` - Find existing window showing buffer
- `display-buffer-pop-up-window` - Create new window
- `display-buffer-pop-up-frame` - Create new frame

#### Window Configuration

```elisp
(defun current-window-configuration (&optional frame)
  "Return an object representing the current window configuration of FRAME.
If FRAME is nil or omitted, use the selected frame."
  ...)

(defun set-window-configuration (configuration &optional dont-set-frame)
  "Restore window configuration CONFIGURATION."
  ...)
```

**Usage Pattern**:
```elisp
(let ((config (current-window-configuration)))
  ;; Do something that changes windows
  (other-window 1)
  (delete-other-windows)
  ;; Restore original layout
  (set-window-configuration config))
```

#### Window Splitting

```elisp
(defun split-window-below (&optional size)
  "Split the selected window into two windows, one above the other."
  (interactive "P")
  ...)

(defun split-window-right (&optional size)
  "Split the selected window into two side-by-side windows."
  (interactive "P")
  ...)
```

#### Key Functions Reference

| Function | Purpose | Typical Use |
|----------|---------|-------------|
| `save-selected-window` | Preserve window selection | Temporary window switches |
| `with-temp-buffer-window` | Display temporary content | Help buffers, output |
| `display-buffer` | Show buffer with control | Generic buffer display |
| `split-window-below` | Horizontal split | Create layout |
| `split-window-right` | Vertical split | Create layout |
| `delete-window` | Close window | Clean up layout |
| `delete-other-windows` | Keep only selected | Focus on one buffer |
| `current-window-configuration` | Save layout | Layout restoration |

---

## Data Structures

### seq.el - Sequence Manipulation

**Location**: `/home/user/emacs/lisp/emacs-lisp/seq.el`

`seq.el` provides a unified, generic API for working with sequences (lists, vectors, strings). It uses cl-generic to dispatch to the appropriate implementation based on sequence type.

#### Core Philosophy

The key insight of `seq.el` is that many operations apply to any ordered collection:

```elisp
;; Same API works on lists, vectors, and strings!
(seq-filter #'oddp [1 2 3 4 5])      ; => [1 3 5]
(seq-filter #'oddp '(1 2 3 4 5))     ; => (1 3 5)
(seq-map #'upcase "hello")           ; => "HELLO"
```

#### Iteration

```elisp
(defmacro seq-doseq (spec &rest body)
  "Loop over a SEQUENCE, evaluating BODY with VAR bound to each element.
Similar to `dolist' but works on lists, strings, and vectors.
\(fn (VAR SEQUENCE) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  `(seq-do (lambda (,(car spec))
             ,@body)
           ,(cadr spec)))

(defmacro seq-let (args sequence &rest body)
  "Bind the variables in ARGS to the elements of SEQUENCE, then evaluate BODY.
ARGS can also include the `&rest' marker."
  (declare (indent 2) (debug (sexp form body)))
  ...)
```

**Examples**:
```elisp
;; Iterate over any sequence
(seq-doseq (word ["apple" "banana" "cherry"])
  (insert word "\n"))

;; Destructure sequences
(seq-let [first second &rest others] [1 2 3 4 5]
  (message "First: %s, Second: %s, Rest: %s" first second others))
;; => "First: 1, Second: 2, Rest: (3 4 5)"
```

#### Filtering and Mapping

```elisp
(cl-defgeneric seq-filter (pred sequence)
  "Return a list of all elements for which PRED returns non-nil in SEQUENCE."
  ...)

(cl-defgeneric seq-map (function sequence)
  "Return the result of applying FUNCTION to each element of SEQUENCE."
  ...)

(defun seq-remove (pred sequence)
  "Return a list of all elements for which PRED returns nil in SEQUENCE."
  (seq-filter (lambda (elt) (not (funcall pred elt))) sequence))
```

**Practical Examples**:
```elisp
;; Filter files by extension
(seq-filter (lambda (f) (string-suffix-p ".el" f))
            (directory-files "/path/to/dir"))

;; Transform data
(seq-map (lambda (x) (* x x))
         [1 2 3 4 5])  ; => [1 4 9 16 25]

;; Remove empty strings
(seq-remove #'string-empty-p '("a" "" "b" "" "c"))  ; => ("a" "b" "c")
```

#### Subsequences and Access

```elisp
(cl-defgeneric seq-subseq (sequence start &optional end)
  "Return the sequence of elements of SEQUENCE from START to END.
END is exclusive."
  ...)

(defun seq-take (sequence n)
  "Return the first N elements of SEQUENCE."
  (seq-subseq sequence 0 n))

(defun seq-drop (sequence n)
  "Return SEQUENCE without its first N elements."
  (seq-subseq sequence n))
```

**Usage**:
```elisp
(seq-take [1 2 3 4 5] 3)        ; => [1 2 3]
(seq-drop "hello world" 6)      ; => "world"
(seq-subseq '(a b c d e) 1 4)   ; => (b c d)
```

#### Searching and Testing

```elisp
(defun seq-find (pred sequence &optional default)
  "Return the first element for which PRED returns non-nil in SEQUENCE."
  ...)

(defun seq-contains-p (sequence elt &optional testfn)
  "Return non-nil if SEQUENCE contains an element equal to ELT."
  ...)

(defun seq-every-p (pred sequence)
  "Return non-nil if PRED returns non-nil for all elements of SEQUENCE."
  ...)

(defun seq-some (pred sequence)
  "Return non-nil if PRED returns non-nil for any element of SEQUENCE."
  ...)
```

**Examples**:
```elisp
;; Find first even number
(seq-find #'evenp [1 3 5 6 7])  ; => 6

;; Check if sequence contains element
(seq-contains-p '(a b c) 'b)    ; => t

;; Test all elements
(seq-every-p #'numberp [1 2 3])  ; => t
(seq-every-p #'numberp [1 'a 3]) ; => nil

;; Test any element
(seq-some #'stringp '(1 2 "three" 4))  ; => t
```

#### Reduction

```elisp
(defun seq-reduce (function sequence initial-value)
  "Reduce SEQUENCE to a single value by successively applying FUNCTION.
Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQUENCE, then calling FUNCTION with that result
and the second element, etc."
  ...)
```

**Examples**:
```elisp
;; Sum numbers
(seq-reduce #'+ [1 2 3 4 5] 0)  ; => 15

;; Concatenate strings
(seq-reduce (lambda (acc s) (concat acc " " s))
            ["Hello" "from" "seq.el"]
            "")  ; => " Hello from seq.el"

;; Build alist
(seq-reduce (lambda (acc pair)
              (cons pair acc))
            '((:a . 1) (:b . 2))
            nil)
```

#### Key Functions Reference

| Function | Purpose | Example |
|----------|---------|---------|
| `seq-map` | Transform each element | `(seq-map #'1+ [1 2 3])` |
| `seq-filter` | Keep matching elements | `(seq-filter #'oddp [1 2 3])` |
| `seq-remove` | Remove matching elements | `(seq-remove #'oddp [1 2 3])` |
| `seq-reduce` | Fold/accumulate | `(seq-reduce #'+ [1 2 3] 0)` |
| `seq-find` | Find first match | `(seq-find #'evenp [1 2 3])` |
| `seq-take` | First N elements | `(seq-take [1 2 3 4] 2)` |
| `seq-drop` | All but first N | `(seq-drop [1 2 3 4] 2)` |
| `seq-contains-p` | Test membership | `(seq-contains-p [1 2 3] 2)` |

---

### map.el - Map/Dictionary Operations

**Location**: `/home/user/emacs/lisp/emacs-lisp/map.el`

`map.el` provides a generic API for associative data structures: alists, plists, and hash tables. Like `seq.el`, it uses cl-generic for polymorphic dispatch.

#### Universal Map Access

```elisp
(cl-defgeneric map-elt (map key &optional default testfn)
  "Look up KEY in MAP and return its associated value.
If KEY is not found, return DEFAULT which defaults to nil."
  ...)

;; Works with different map types:
(map-elt '((a . 1) (b . 2)) 'a)              ; alist => 1
(map-elt '(:a 1 :b 2) :a)                    ; plist => 1
(map-elt #s(hash-table data (a 1 b 2)) 'a)  ; hash => 1
```

#### Pattern Matching

```elisp
(pcase-defmacro map (&rest args)
  "Build a `pcase' pattern matching map elements.
Each element of ARGS can be (KEY PAT [DEFAULT])."
  ...)

(defmacro map-let (keys map &rest body)
  "Bind the variables in KEYS to the elements of MAP, then evaluate BODY."
  (declare (indent 2))
  ...)
```

**Destructuring Example**:
```elisp
;; Extract values from maps
(map-let (name age city)
    '((name . "Alice")
      (age . 30)
      (city . "NYC"))
  (message "%s is %d years old and lives in %s" name age city))

;; Pattern matching in pcase
(pcase my-config
  ((map (:host host) (:port port) (:ssl ssl))
   (message "Connecting to %s:%d (SSL: %s)" host port ssl)))
```

#### Map Manipulation

```elisp
(cl-defgeneric map-put! (map key value &optional testfn)
  "Associate KEY with VALUE in MAP.
This mutates the map if possible."
  ...)

(defun map-insert (map key value)
  "Return a new map based on MAP with KEY associated with VALUE."
  ...)

(defun map-delete (map key)
  "Return a new map based on MAP without KEY."
  ...)
```

**Examples**:
```elisp
;; Add to alist (immutable)
(setq config (map-insert config 'timeout 30))

;; Mutate hash table
(let ((hash (make-hash-table)))
  (map-put! hash 'key "value")
  hash)

;; Remove key
(setq config (map-delete config 'old-setting))
```

#### Iteration

```elisp
(cl-defgeneric map-do (function map)
  "Apply FUNCTION to each key-value pair in MAP.
FUNCTION is called with two arguments: the key and the value."
  ...)

(defmacro map-let (keys map &rest body)
  "Bind variables in KEYS to values in MAP, then eval BODY."
  ...)
```

**Examples**:
```elisp
;; Iterate over map entries
(map-do (lambda (key value)
          (message "%s => %s" key value))
        '((a . 1) (b . 2) (c . 3)))

;; Convert alist to hash table
(let ((hash (make-hash-table)))
  (map-do (lambda (k v) (puthash k v hash))
          my-alist)
  hash)
```

#### Conversions

```elisp
(defun map-keys (map)
  "Return the list of keys in MAP."
  ...)

(defun map-values (map)
  "Return the list of values in MAP."
  ...)

(defun map-pairs (map)
  "Return the key-value pairs in MAP as a list of conses."
  ...)
```

**Usage**:
```elisp
(map-keys '((a . 1) (b . 2)))      ; => (a b)
(map-values '((a . 1) (b . 2)))    ; => (1 2)
(map-pairs '(:a 1 :b 2))           ; => ((‫:a . 1) (:b . 2))
```

#### Key Functions Reference

| Function | Purpose | Example |
|----------|---------|---------|
| `map-elt` | Get value by key | `(map-elt map 'key)` |
| `map-put!` | Set value (mutating) | `(map-put! map 'k 'v)` |
| `map-insert` | Add entry (immutable) | `(map-insert map 'k 'v)` |
| `map-delete` | Remove entry | `(map-delete map 'key)` |
| `map-keys` | List all keys | `(map-keys map)` |
| `map-values` | List all values | `(map-values map)` |
| `map-do` | Iterate over entries | `(map-do fn map)` |
| `map-let` | Destructure map | `(map-let (k1 k2) map ...)` |

---

### ring.el - Ring Buffers

**Location**: `/home/user/emacs/lisp/emacs-lisp/ring.el`

A ring is a fixed-size circular buffer that automatically overwrites the oldest elements when full. Rings are used throughout Emacs for history mechanisms (kill ring, command history, search history).

#### Ring Structure

```elisp
;; A ring is represented as: (hd-index length . vector)
;; - hd-index: vector index of oldest element
;; - length: current number of elements
;; - vector: the storage array

(defun make-ring (size)
  "Make a ring that can contain SIZE elements."
  (cons 0 (cons 0 (make-vector size nil))))

(defun ring-p (x)
  "Return t if X is a ring; nil otherwise."
  (and (consp x) (integerp (car x))
       (consp (cdr x)) (integerp (cadr x))
       (vectorp (cddr x))))
```

#### Ring Operations

```elisp
(defun ring-insert (ring item)
  "Insert onto RING the item ITEM, as the newest (last) item.
If the ring is full, dump the oldest item to make room."
  ...)

(defun ring-remove (ring &optional index)
  "Remove an item from RING and return it.
If optional INDEX is nil, remove the oldest item."
  ...)

(defun ring-ref (ring index)
  "Return RING's INDEX element.
INDEX = 0 is the most recently inserted; higher indices
correspond to older elements."
  ...)
```

#### Practical Example: Command History

```elisp
;; Create a command history ring
(defvar my-command-history (make-ring 50)
  "History of recent commands.")

;; Add command to history
(defun my-record-command (command)
  (ring-insert my-command-history command))

;; Retrieve last N commands
(defun my-recent-commands (n)
  (cl-loop for i from 0 below (min n (ring-length my-command-history))
           collect (ring-ref my-command-history i)))

;; Usage
(my-record-command 'find-file)
(my-record-command 'save-buffer)
(my-recent-commands 2)  ; => (save-buffer find-file)
```

#### Ring Traversal

```elisp
(defun ring-elements (ring)
  "Return a list of the elements of RING, in order from newest to oldest."
  ...)

(defun ring-empty-p (ring)
  "Return t if RING is empty; nil otherwise."
  (zerop (cadr ring)))

(defun ring-size (ring)
  "Return the size of RING, the maximum number of elements it can contain."
  (length (cddr ring)))
```

#### Real-World Usage: Kill Ring

The kill ring is Emacs's clipboard history, implemented as a ring:

```elisp
;; The kill ring stores clipboard history
(defvar kill-ring (make-ring 60))

;; Recent kills
(ring-ref kill-ring 0)  ; Most recent kill
(ring-ref kill-ring 1)  ; Previous kill

;; Yank (paste) cycles through the ring with M-y
```

#### Key Functions Reference

| Function | Purpose | Example |
|----------|---------|---------|
| `make-ring` | Create ring of size N | `(make-ring 10)` |
| `ring-insert` | Add newest element | `(ring-insert ring item)` |
| `ring-remove` | Remove element | `(ring-remove ring 0)` |
| `ring-ref` | Access by index | `(ring-ref ring 0)` |
| `ring-length` | Current size | `(ring-length ring)` |
| `ring-empty-p` | Test if empty | `(ring-empty-p ring)` |
| `ring-elements` | Convert to list | `(ring-elements ring)` |

---

### avl-tree.el - Balanced Binary Trees

**Location**: `/home/user/emacs/lisp/emacs-lisp/avl-tree.el`

AVL trees are self-balancing binary search trees providing O(log n) insertion, deletion, and retrieval. They're used when you need sorted data with efficient operations.

#### Tree Structure

```elisp
(cl-defstruct (avl-tree-
            :named
            (:constructor avl-tree--create (cmpfun))
            (:predicate avl-tree-p))
  (dummyroot (avl-tree--node-create nil nil nil 0))
  cmpfun)

;; Nodes: [left right data balance]
(cl-defstruct (avl-tree--node
            (:type vector)
            (:constructor avl-tree--node-create (left right data balance)))
  left right data balance)
```

#### Creation and Basic Operations

```elisp
(defun avl-tree-create (compare-function)
  "Create an empty AVL tree.
COMPARE-FUNCTION is a function which takes two arguments, A and B,
and returns non-nil if A is less than B, and nil otherwise."
  (avl-tree--create compare-function))

(defun avl-tree-enter (tree data)
  "Insert DATA into the AVL TREE."
  ...)

(defun avl-tree-delete (tree data)
  "Delete DATA from the AVL TREE."
  ...)

(defun avl-tree-member (tree data)
  "Return non-nil if DATA is in TREE."
  ...)
```

#### Example: Sorted Set

```elisp
;; Create a sorted set of numbers
(defvar my-numbers (avl-tree-create #'<))

;; Insert elements (automatically sorted)
(avl-tree-enter my-numbers 5)
(avl-tree-enter my-numbers 2)
(avl-tree-enter my-numbers 8)
(avl-tree-enter my-numbers 1)

;; Check membership: O(log n)
(avl-tree-member my-numbers 2)  ; => t
(avl-tree-member my-numbers 7)  ; => nil

;; Iterate in sorted order
(avl-tree-mapc (lambda (x) (message "Number: %d" x))
               my-numbers)
;; Prints: 1, 2, 5, 8 (in order!)
```

#### Traversal

```elisp
(defun avl-tree-map (map-function tree)
  "Apply MAP-FUNCTION to all elements in TREE.
The function is applied in ascending order."
  ...)

(defun avl-tree-mapc (map-function tree)
  "Apply MAP-FUNCTION to all elements in TREE for side effects."
  ...)

(defun avl-tree-mapcar (map-function tree)
  "Apply MAP-FUNCTION to all elements in TREE.
Return a list of the results, in ascending order."
  ...)
```

#### When to Use AVL Trees

**Use AVL trees when:**
- You need sorted data with efficient insertion/deletion
- Lookups are more common than modifications
- You need to maintain a sorted collection dynamically

**Don't use when:**
- Simple list is sufficient (< 100 elements)
- Hash tables would work (unordered data)
- Read-only sorted data (use sorted vector)

#### Key Functions Reference

| Function | Purpose | Complexity |
|----------|---------|------------|
| `avl-tree-create` | Create empty tree | O(1) |
| `avl-tree-enter` | Insert element | O(log n) |
| `avl-tree-delete` | Remove element | O(log n) |
| `avl-tree-member` | Check membership | O(log n) |
| `avl-tree-mapcar` | Map to list | O(n) |
| `avl-tree-empty` | Check if empty | O(1) |

---

## Completion Framework

### minibuffer.el - Minibuffer and Completion

**Location**: `/home/user/emacs/lisp/minibuffer.el` (5,763 lines)

The minibuffer is Emacs's command-line interface, and `minibuffer.el` implements its sophisticated completion system. This is one of Emacs's most powerful subsystems.

#### Completion Tables

Completion tables are the heart of the system. They can be lists, hash tables, functions, or alists:

```elisp
(defun completion-boundaries (string collection pred suffix)
  "Return the boundaries of text on which COLLECTION will operate.
STRING is the string on which completion will be performed.
SUFFIX is the string after point."
  ...)

(defun completion-metadata (string table pred)
  "Return the metadata of elements to complete at the end of STRING.
Metadata includes:
- `category': the kind of objects
- `annotation-function': function to add annotations
- `affixation-function': function to prepend/append prefix/suffix
- `group-function': function for grouping candidates
- `display-sort-function': function to sort in *Completions*
- `cycle-sort-function': function to sort when cycling"
  ...)
```

#### Completion Metadata

Metadata controls how completion behaves:

```elisp
;; Example: Define completion with metadata
(defun my-completion-table (string pred action)
  (if (eq action 'metadata)
      '(metadata
        (category . my-category)
        (annotation-function . my-annotate)
        (display-sort-function . my-sort))
    ;; Normal completion logic
    (all-completions string my-candidates pred)))

(defun my-annotate (candidate)
  "Add annotation to CANDIDATE."
  (concat " " (get-text-property 0 'info candidate)))
```

#### Reading with Completion

```elisp
(completing-read "Choose: " '("apple" "banana" "cherry"))

;; With custom metadata
(completing-read "Select file: "
                 (completion-table-dynamic
                  (lambda (prefix)
                    (file-name-all-completions prefix "~/")))
                 nil nil nil 'file-name-history)
```

#### Completion Styles

Emacs supports multiple completion styles that can be mixed:

- **basic**: Prefix matching (abc matches "abc...")
- **partial**: Wildcards (a*c matches "abc", "axc")
- **substring**: Substring matching (bc matches "abc")
- **flex**: Flexible matching (fnd matches "find")
- **initials**: Initials (fb matches "foo-bar")

```elisp
;; Configure completion styles
(setq completion-styles '(basic partial-completion emacs22))

;; Different styles for different categories
(setq completion-category-overrides
      '((file (styles basic partial-completion))
        (buffer (styles flex basic))))
```

#### Completion UI

```elisp
;; Completion in region (at point)
(completion-in-region start end collection predicate)

;; Programmatic completion
(completion-all-completions
 string                  ; Input string
 collection             ; Completion table
 predicate              ; Filter function
 point)                 ; Position in string
```

#### Real-World Example: Custom Completion

```elisp
;; Define a completion command
(defun my-choose-project ()
  "Choose a project with completion."
  (interactive)
  (let* ((projects '(("emacs" . "~/src/emacs")
                     ("website" . "~/projects/website")
                     ("notes" . "~/notes")))
         (choice (completing-read "Project: "
                                  (lambda (string pred action)
                                    (if (eq action 'metadata)
                                        '(metadata (category . project))
                                      (complete-with-action
                                       action projects string pred))))))
    (message "Selected: %s -> %s"
             choice
             (alist-get choice projects nil nil #'equal))))
```

#### Key Functions Reference

| Function | Purpose | Use Case |
|----------|---------|----------|
| `completing-read` | Read with completion | Interactive input |
| `completion-boundaries` | Determine completion scope | Multi-part completion |
| `completion-metadata` | Get completion metadata | Custom completion |
| `completion-all-completions` | Get all matches | Programmatic access |
| `completion-try-completion` | Test completion | Validation |

---

## Search and Replace

### isearch.el - Incremental Search

**Location**: `/home/user/emacs/lisp/isearch.el`

Incremental search (isearch) is Emacs's signature search feature - searching happens as you type, with immediate visual feedback.

#### Search Modes

```elisp
(defgroup isearch nil
  "Incremental search minor mode."
  :group 'matching)

(defcustom search-upper-case 'not-yanks
  "If non-nil, upper case chars disable case fold searching.
That is, upper and lower case chars must match exactly."
  :type '(choice (const :tag "Case-sensitive when upper case used" not-yanks)
                 (const :tag "Always case-sensitive" t)
                 (const :tag "Never case-sensitive" nil)))
```

#### Search State

Isearch maintains rich state during searching:
- Search string and position
- Direction (forward/backward)
- Regexp vs literal
- Case sensitivity
- Wrapped status
- Match history

#### Customization

```elisp
;; Configure search behavior
(setq search-upper-case t)           ; Smart case
(setq isearch-lazy-count t)          ; Show match count
(setq isearch-allow-scroll t)        ; Allow scrolling during search
(setq isearch-wrap-pause 'no-ding)   ; Don't beep when wrapping
```

#### Search Extensions

```elisp
;; Add custom search behavior
(defun my-isearch-word-at-point ()
  "Start isearch with word at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (isearch-mode t nil nil nil)
    (isearch-yank-string word)))

;; Bind it
(define-key global-map (kbd "C-*") 'my-isearch-word-at-point)
```

#### Key Functions Reference

| Function | Purpose | Default Binding |
|----------|---------|-----------------|
| `isearch-forward` | Search forward | `C-s` |
| `isearch-backward` | Search backward | `C-r` |
| `isearch-forward-regexp` | Regexp search | `C-M-s` |
| `isearch-yank-word` | Yank word into search | `C-s C-w` |

---

## Help System

### help.el - Help Commands

**Location**: `/home/user/emacs/lisp/help.el`

The help system makes Emacs self-documenting. Every function, variable, and key binding can be queried interactively.

#### Help Map

```elisp
(defvar-keymap help-map
  :doc "Keymap for characters following the Help key."
  "a"    #'apropos-command          ; Search commands
  "b"    #'describe-bindings        ; Show all key bindings
  "c"    #'describe-key-briefly     ; What does key do (brief)
  "f"    #'describe-function        ; Describe function
  "k"    #'describe-key             ; What does key do (detailed)
  "m"    #'describe-mode            ; Describe current modes
  "o"    #'describe-symbol          ; Describe symbol
  "v"    #'describe-variable        ; Describe variable
  "w"    #'where-is)                ; Where is command bound
```

#### Interactive Help

All help commands follow a pattern: they read input, look up documentation, and display it in a `*Help*` buffer:

```elisp
;; C-h f RET describe-function RET
;; Shows: signature, documentation, source location

;; C-h v RET completion-styles RET
;; Shows: value, documentation, customization info

;; C-h k C-x C-f
;; Shows: what find-file does
```

#### Programmatic Help Access

```elisp
;; Get function documentation
(documentation 'car)
;; => "Return the car of LIST..."

;; Check if function is interactive
(commandp 'save-buffer)  ; => t

;; Find where function is defined
(find-function-noselect 'car)
;; => buffer visiting src/data.c
```

#### Help System Extensibility

```elisp
;; Add custom help
(defun my-help-mode-hook ()
  "Customize help buffer."
  ;; Add custom key bindings
  (local-set-key (kbd "q") 'quit-window))

(add-hook 'help-mode-hook 'my-help-mode-hook)
```

---

## Customization

### custom.el - Customization Framework

**Location**: `/home/user/emacs/lisp/custom.el`

The customization system provides a structured way to define and set user options, with type checking, persistence, and UI support.

#### Defining Custom Variables

```elisp
(defcustom user-option value
  "Documentation string."
  :type 'type-specification
  :group 'group-name
  :options '(list of options))
```

**Example**:
```elisp
(defcustom my-indentation-width 4
  "Number of spaces for indentation."
  :type 'integer
  :group 'my-mode
  :safe #'integerp)

(defcustom my-completion-backend 'company
  "Which completion backend to use."
  :type '(choice (const :tag "Company" company)
                 (const :tag "Auto-complete" auto-complete)
                 (const :tag "Built-in" completion-at-point))
  :group 'my-mode)
```

#### Custom Groups

```elisp
(defgroup my-mode nil
  "Settings for my-mode."
  :group 'programming
  :prefix "my-")
```

#### Initialization Functions

```elisp
(defun custom-initialize-default (symbol exp)
  "Initialize SYMBOL with EXP if it doesn't have a default binding."
  ...)

(defun custom-initialize-set (symbol exp)
  "Initialize SYMBOL using its :set function."
  ...)

(defun custom-initialize-reset (symbol exp)
  "Initialize SYMBOL, running :set function."
  ...)
```

#### Type Specifications

The `:type` keyword accepts sophisticated type descriptions:

```elisp
:type 'boolean                    ; t or nil
:type 'integer                    ; Any integer
:type 'string                     ; Any string
:type 'file                       ; File name
:type 'directory                  ; Directory name
:type '(repeat string)            ; List of strings
:type '(choice (const :tag "A" a)
               (const :tag "B" b)) ; One of several options
:type '(alist :key-type string
              :value-type integer) ; Association list
```

#### Custom Setters

```elisp
(defcustom my-variable value
  "Documentation."
  :type 'type
  :set (lambda (symbol value)
         ;; Validate or transform value
         (set-default symbol value)
         ;; Trigger side effects
         (my-update-configuration)))
```

---

## Conclusion

The Emacs Lisp standard library is a masterclass in API design:

1. **Progressive Enhancement**: Simple things are simple, complex things are possible
2. **Consistency**: Common patterns (predicates ending in `-p`, destructive functions ending in `!`)
3. **Discoverability**: Self-documenting code with excellent docstrings
4. **Extensibility**: Hooks, generic functions, and customization at every level

These libraries are not just utility code - they embody decades of refinement in creating a programmable text editor. Understanding them deeply enables you to:

- Write idiomatic Elisp code
- Leverage existing abstractions instead of reinventing
- Extend Emacs in ways consistent with its design philosophy
- Contribute to Emacs development

The standard library is meant to be read, understood, and learned from. Every function has a story, every abstraction solves real problems, and the whole system fits together into something greater than its parts.

---

## Further Reading

- **Emacs Lisp Reference Manual**: The definitive guide
- **Source Code**: Read `lisp/*.el` files directly
- **`M-x apropos`**: Discover related functions
- **`C-h f`, `C-h v`**: Learn by exploring

**Happy hacking!**
