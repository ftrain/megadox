# Version Control (VC) System

**Location**: `/lisp/vc/`
**Files**: 39 files, 52,964 lines of code
**Purpose**: Unified interface for interacting with multiple version control systems

## Overview

The Emacs Version Control (VC) system provides a consistent, backend-agnostic interface for working with various version control systems including Git, Mercurial, Subversion, Bazaar, CVS, RCS, SCCS, and SRC. It abstracts the differences between these systems behind a common API, allowing users to perform version control operations without needing to know system-specific commands.

## Architecture

### Core Components

The VC system is organized into several architectural layers:

```
┌─────────────────────────────────────────────────────────────┐
│                    User Interface Layer                      │
│  vc.el: High-level commands (commit, log, diff, etc.)       │
│  vc-dir.el: Directory status browser                         │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                  Backend Abstraction Layer                   │
│  vc-hooks.el: Initialization & property caching             │
│  vc-dispatcher.el: Async command execution                  │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                       Backend Layer                          │
│  vc-git.el, vc-hg.el, vc-svn.el, vc-bzr.el, vc-cvs.el      │
│  vc-rcs.el, vc-sccs.el, vc-src.el                           │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Related Tools                           │
│  diff-mode.el, log-view.el, log-edit.el                     │
│  smerge-mode.el, ediff (10 files), vc-annotate.el          │
└─────────────────────────────────────────────────────────────┘
```

### File Organization

#### Core Files (5 files)
- **`vc-hooks.el`** (1,164 lines): Preloaded initialization, property caching, find-file hooks
- **`vc.el`** (5,283 lines): Main user interface, backend dispatch, high-level operations
- **`vc-dispatcher.el`** (1,073 lines): Command execution framework, async operations
- **`vc-dir.el`** (1,744 lines): Directory-level status browser using ewoc
- **`vc-filewise.el`** (86 lines): Helper for file-based VCS operations

#### Backend Implementations (8 files)
- **`vc-git.el`** (2,846 lines): Git backend - most feature-complete
- **`vc-hg.el`** (1,941 lines): Mercurial backend
- **`vc-svn.el`** (840 lines): Subversion backend
- **`vc-bzr.el`** (1,378 lines): Bazaar backend
- **`vc-cvs.el`** (1,350 lines): CVS backend
- **`vc-rcs.el`** (1,470 lines): RCS backend
- **`vc-sccs.el`** (532 lines): SCCS backend
- **`vc-src.el`** (337 lines): SRC backend (RCS wrapper)

#### Related Tools (15 files)
- **`diff-mode.el`** (3,505 lines): Major mode for viewing/editing diffs
- **`log-view.el`** (956 lines): Revision log browser
- **`log-edit.el`** (1,466 lines): Commit message editor
- **`vc-annotate.el`** (835 lines): Blame/annotate visualization
- **`smerge-mode.el`** (1,720 lines): Merge conflict resolution
- **Ediff suite** (10 files, ~18,000 lines): Advanced diff/merge/patch tool
- **Emerge** (3,064 lines): Older merge tool
- **PCL-CVS** (5 files): CVS-specific interface

#### Supporting Files (11 files)
- **`add-log.el`** (1,398 lines): ChangeLog integration
- **`compare-w.el`** (427 lines): Window comparison
- **`cvs-status.el`** (533 lines): CVS status parsing
- **`diff.el`** (300 lines): Diff utilities
- **`pcvs-*.el`** (4 files): PCL-CVS components

## Backend Abstraction Layer

### The vc-call Dispatch Mechanism

The heart of VC's abstraction is the `vc-call` macro and `vc-call-backend` function, which dynamically dispatch operations to backend-specific implementations:

```elisp
;; Location: /lisp/vc/vc-hooks.el:303-308

(defmacro vc-call (fun file &rest args)
  "A convenience macro for calling VC backend functions.
Functions called by this macro must accept FILE as the first argument.
ARGS specifies any additional arguments. FUN should be unquoted."
  (macroexp-let2 nil file file
    `(vc-call-backend (vc-backend ,file) ',fun ,file ,@args)))
```

This mechanism:
1. Determines the backend for a file via `vc-backend`
2. Constructs the backend-specific function name (e.g., `vc-git-state`)
3. Calls the function, or falls back to `vc-default-*` if not implemented
4. Caches function lookups in the backend's `vc-functions` property

### Backend Function Discovery

```elisp
;; Location: /lisp/vc/vc-hooks.el:264-279

(defun vc-make-backend-sym (backend sym)
  "Return BACKEND-specific version of VC symbol SYM."
  (intern (concat "vc-" (downcase (symbol-name backend))
                  "-" (symbol-name sym))))

(defun vc-find-backend-function (backend fun)
  "Return BACKEND-specific implementation of FUN.
If there is no such implementation, return the default implementation;
if that doesn't exist either, return nil."
  (let ((f (vc-make-backend-sym backend fun)))
    (if (fboundp f) f
      ;; Load vc-BACKEND.el if needed.
      (require (intern (concat "vc-" (downcase (symbol-name backend)))))
      (if (fboundp f) f
        (let ((def (vc-make-backend-sym 'default fun)))
          (if (fboundp def) (cons def backend) nil))))))
```

**Auto-loading Pattern**: Backend files use `;;;###autoload` directives to register their presence without loading the entire backend:

```elisp
;; Location: /lisp/vc/vc-git.el:285-290

;;;###autoload (defun vc-git-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with git."
;;;###autoload   (if (vc-find-root file ".git")       ; Short cut.
;;;###autoload       (progn
;;;###autoload         (load "vc-git" nil t)
;;;###autoload         (vc-git-registered file))))
```

### Backend API Contract

Backends implement a standard set of functions documented in `/lisp/vc/vc.el:108-755`. The API is divided into several categories:

#### 1. Backend Properties

```elisp
;; Required (*)
(defun vc-BACKEND-revision-granularity ()
  ;; Return 'file or 'repository

;; Optional (-)
(defun vc-BACKEND-update-on-retrieve-tag () ...)
(defun vc-BACKEND-async-checkins () ...)
(defun vc-BACKEND-working-revision-symbol () ...)
```

**Example from Git**:
```elisp
;; Location: /lisp/vc/vc-git.el:279-281

(defun vc-git-revision-granularity () 'repository)
(defun vc-git-checkout-model (_files) 'implicit)
(defun vc-git-update-on-retrieve-tag () nil)
```

#### 2. State-Querying Functions

```elisp
;; * registered (file)
;;   Return non-nil if FILE is registered in this backend

;; * state (file)
;;   Return the current version control state:
;;   - 'up-to-date, 'edited, 'added, 'removed, 'missing
;;   - 'needs-update, 'needs-merge, 'unlocked-changes
;;   - 'conflict, 'unregistered, 'ignored

;; - dir-status-files (dir files update-function)
;;   Asynchronously produce status for FILES in DIR

;; * working-revision (file)
;;   Return the working revision (current checkout)

;; * checkout-model (files)
;;   Return 'implicit, 'explicit, or 'locking
```

**Git State Implementation**:
```elisp
;; Location: /lisp/vc/vc-git.el:402-428

(defun vc-git-state (file)
  "Git-specific version of `vc-state'."
  (let* ((args
          `("status" "--porcelain" "-z"
            "--untracked-files"
            ,@(when (version<= "1.7.6.3" (vc-git--program-version))
                '("--ignored"))
            "--"))
        (status (apply #'vc-git--run-command-string file args)))
    (if (null status)
        'unregistered
      (vc-git--git-status-to-vc-state
       (mapcar (lambda (s) (substring s 0 2))
               (split-string status "\0" t))))))
```

The state conversion logic handles Git's two-character status codes:

```elisp
;; Location: /lisp/vc/vc-git.el:369-400

(defun vc-git--git-status-to-vc-state (code-list)
  "Convert CODE-LIST to a VC status."
  (pcase code-list
    ('nil 'up-to-date)
    (`(,code)
     (pcase code
       ("!!" 'ignored)
       ("??" 'unregistered)
       ("D " 'removed)
       (_ (cond
           ((string-match-p "^.D$" code) 'missing)
           ((string-match-p "^[ M]+$" code) 'edited)
           ((string-match-p "^[ A]+$" code) 'added)
           ((string-match-p "^[ U]+$" code) 'conflict)
           (t 'edited)))))
    ('("D " "??") 'unregistered)
    (_ 'edited)))
```

#### 3. State-Changing Functions

```elisp
;; * create-repo ()
;;   Initialize a new repository

;; * register (files &optional comment)
;;   Register FILES in version control

;; - responsible-p (file)
;;   Return non-nil if backend should handle FILE

;; * checkin (files comment &optional rev)
;;   Commit changes with COMMENT

;; - checkin-patch (patch-string comment)
;;   Commit a patch without touching working tree

;; * find-revision (file rev buffer)
;;   Retrieve revision REV of FILE into BUFFER

;; * checkout (file &optional rev)
;;   Check out revision REV of FILE

;; * revert (file &optional contents-done)
;;   Revert FILE to working revision

;; - merge-branch ()
;;   Merge another branch into current

;; - pull (prompt)
;;   Pull upstream changes
```

#### 4. History Functions

```elisp
;; * print-log (files buffer &optional shortlog start-revision limit)
;;   Insert revision log into BUFFER

;; * incoming-revision (&optional upstream-location refresh)
;;   Return revision at head of upstream branch

;; - log-search (buffer pattern)
;;   Search for PATTERN in revision log

;; - log-view-mode ()
;;   Mode for displaying print-log output

;; * diff (files &optional rev1 rev2 buffer async)
;;   Generate diff between revisions

;; - annotate-command (file buf &optional rev)
;;   Generate annotated (blame) view

;; - region-history (file buffer lfrom lto)
;;   Show history of region between lines

;; - mergebase (rev1 &optional rev2)
;;   Return common ancestor of revisions
```

#### 5. Tag/Branch System

```elisp
;; - create-tag (dir name branchp)
;;   Create tag NAME, or branch if BRANCHP

;; - retrieve-tag (dir name update)
;;   Switch to tag/branch NAME
```

#### 6. Miscellaneous

```elisp
;; - root (file)
;;   Return root of VC hierarchy

;; - ignore (file &optional directory remove)
;;   Add/remove FILE to ignore list

;; - find-ignore-file (file)
;;   Return ignore file (.gitignore, etc.)

;; - previous-revision (file rev)
;;   Return revision before REV

;; - next-revision (file rev)
;;   Return revision after REV

;; - delete-file (file)
;;   Delete FILE from repository

;; - rename-file (old new)
;;   Rename file in repository

;; - conflicted-files (dir)
;;   Return list of conflicted files

;; - repository-url (file-or-dir &optional remote-name)
;;   Return repository URL
```

### Backend Registration

Backends are registered via the `vc-handled-backends` customization variable:

```elisp
;; Location: /lisp/vc/vc-hooks.el:112-124

(defcustom vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg)
  "List of version control backends for which VC will be used.
Entries in this list will be tried in order to determine whether a
file is under that sort of version control.
Removing an entry from the list prevents VC from being activated
when visiting a file managed by that backend.
An empty list disables VC altogether."
  :type '(repeat symbol)
  :version "25.1"
  :group 'vc)
```

**Backend Discovery Process**:
1. When a file is opened, `vc-refresh-state` (in `find-file-hook`) is called
2. `vc-registered` iterates through `vc-handled-backends`
3. For each backend, calls `vc-BACKEND-registered` (auto-loaded)
4. First backend that returns non-nil "claims" the file
5. Backend is cached in file property `vc-backend`

## Property Caching System

VC maintains a per-file property cache to avoid repeated expensive operations:

```elisp
;; Location: /lisp/vc/vc-hooks.el:229-252

(defvar vc-file-prop-obarray (make-hash-table :test 'equal)
  "Obarray for per-file properties.")

(defun vc-file-setprop (file property value)
  "Set per-file VC PROPERTY for FILE to VALUE."
  (if (and vc-touched-properties
           (not (memq property vc-touched-properties)))
      (setq vc-touched-properties (append (list property)
                                          vc-touched-properties)))
  (put (intern (expand-file-name file) vc-file-prop-obarray)
       property value))

(defun vc-file-getprop (file property)
  "Get per-file VC PROPERTY for FILE."
  (get (intern (expand-file-name file) vc-file-prop-obarray) property))
```

**Cached Properties**:
- `vc-backend`: Which backend manages this file
- `vc-state`: Current state (up-to-date, edited, etc.)
- `vc-working-revision`: Current revision/commit
- `vc-checkout-time`: When file was last checked out
- `vc-git-symbolic-ref`: Git branch name
- Plus backend-specific properties

**Cache Invalidation**: The `with-vc-properties` macro coordinates cache updates:

```elisp
;; When a backend function returns a value, it's automatically cached
;; Example usage:
(vc-file-setprop file 'vc-state 'edited)
(vc-file-getprop file 'vc-state)  ; => 'edited
```

## Core Features

### 1. File Status Queries

The state machine is central to VC's operation:

```
            ┌──────────────┐
            │ unregistered │ ← File not in VC
            └──────┬───────┘
                   │ vc-register
                   ↓
            ┌──────────────┐
            │    added     │ ← Staged for first commit
            └──────┬───────┘
                   │ vc-checkin
                   ↓
            ┌──────────────┐
       ┌───→│ up-to-date   │←──┐
       │    └──────┬───────┘   │
       │           │ edit       │
       │           ↓            │
       │    ┌──────────────┐   │
       │    │    edited    │   │
       │    └──────┬───────┘   │
       │           │ vc-checkin │
       │           └────────────┘
       │
       │    ┌──────────────┐
       ├───→│ needs-update │ ← Remote has changes
       │    └──────────────┘
       │
       │    ┌──────────────┐
       └───→│ needs-merge  │ ← Both local and remote changes
            └──────────────┘
```

**State Query Implementation**:
```elisp
;; High-level state query (with caching)
(vc-state file)  ; Returns state symbol

;; Backend-specific implementation
(vc-call state file)  ; Dispatches to vc-BACKEND-state

;; Directory-level status
(vc-dir default-directory)  ; Opens status browser
```

### 2. Diff Generation

VC provides multiple diff interfaces:

**Buffer Diff** (`vc-diff`):
```elisp
;; Compare working file with repository
C-x v =  → vc-diff

;; Implementation dispatches to backend
(vc-call diff files rev1 rev2 buffer async)
```

**Revision Range Diff**:
```elisp
C-u C-x v =  → Prompts for two revisions
```

**Diff Modes**:
- **`diff-mode`** (3,505 lines): Rich major mode for viewing diffs
  - Syntax highlighting for hunks
  - Navigation between hunks (`n`, `p`)
  - Apply/revert hunks (`C-c C-a`, `C-c C-r`)
  - Refine hunks to show word-level changes
  - Jump to source (`C-c C-c`)
  - Edit diffs and update line numbers

```elisp
;; Location: /lisp/vc/diff-mode.el

;; Key features:
;; - Font-lock with syntax highlighting from source
;; - Hunk refinement (word-level diffs)
;; - Fringe indicators for +/- lines
;; - Integration with VC for applying patches
```

### 3. Commit Interface

The commit workflow uses a specialized log-edit buffer:

```elisp
;; Initiate commit
C-x v v  → vc-next-action (context-aware)

;; For edited files, opens log-edit buffer
;; User types commit message
C-c C-c  → log-edit-done (commits changes)
```

**log-edit-mode** provides:
- Commit message history (`M-p`, `M-n`)
- ChangeLog integration (`C-c C-a`)
- Diff preview (`C-c C-d`)
- File list (`C-c C-f`)
- Comment search (`M-r`, `M-s`)

```elisp
;; Location: /lisp/vc/log-edit.el

(defvar-keymap log-edit-mode-map
  "C-c C-c" #'log-edit-done
  "C-c C-a" #'log-edit-insert-changelog
  "C-c C-w" #'log-edit-generate-changelog-from-diff
  "C-c C-d" #'log-edit-show-diff
  "C-c C-f" #'log-edit-show-files
  "M-n"     #'log-edit-next-comment
  "M-p"     #'log-edit-previous-comment)
```

### 4. Log Viewing

**log-view-mode** displays revision history:

```elisp
C-x v l  → vc-print-log

;; Navigation
n, p     → Next/previous revision
d, =     → Show diff for revision
D        → Show changeset diff
f        → Visit revision
a        → Annotate at revision
```

**Backend-Specific Log Formats**:

Git uses custom format strings:
```elisp
;; Location: /lisp/vc/vc-git.el:195-213

(defcustom vc-git-root-log-format
  '("%d%h..: %an %ad %s"
    "^\\(?:[*/\\| ]+ \\)?\\(?2: ([^)]+)\\)?\\(?1:[0-9a-z]+\\)\\.\\.: \
\\(?3:.*?\\)[ \t]+\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
    ((1 'log-view-message)
     (2 'change-log-list nil lax)
     (3 'change-log-name)
     (4 'change-log-date)))
  "Git log format for `vc-print-root-log'.")
```

### 5. Branch Management

Branch operations vary by backend capability:

```elisp
;; Create branch
C-x v s  → vc-create-tag (with prefix arg for branch)

;; Switch branch
C-x v r  → vc-retrieve-tag

;; Merge branch (Git/Hg/Bzr)
C-x v m  → vc-merge-branch
```

**Git Branch Implementation**:
```elisp
;; Branches are stored in refs/heads/
;; Current branch tracked via symbolic-ref

(defun vc-git--symbolic-ref (file)
  (or (vc-file-getprop file 'vc-git-symbolic-ref)
      (let ((str (vc-git--run-command-string nil "symbolic-ref" "HEAD")))
        (vc-file-setprop file 'vc-git-symbolic-ref
                         (if str
                             (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
                                 (match-string 2 str)
                               str))))))
```

### 6. Merging and Conflict Resolution

**smerge-mode** handles merge conflicts:

```elisp
;; Location: /lisp/vc/smerge-mode.el

;; Automatically activated on files with conflict markers:
<<<<<<< HEAD
version 1
=======
version 2
>>>>>>> branch

;; Commands:
n, p     → Navigate conflicts
RET      → Keep current version
a        → Keep all versions
l, u     → Keep lower/upper version
E        → Invoke ediff
```

**Conflict Marker Recognition**:
```elisp
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)
```

**Three-Way Merge Structure**:
```
<<<<<<< upper (or "mine")
Your changes
||||||| base (optional)
Common ancestor
=======
Their changes
>>>>>>> lower (or "theirs")
```

## Related Tools

### diff-mode.el (3,505 lines)

Comprehensive diff viewing and editing:

**Key Features**:
- **Syntax Highlighting**: Full source code syntax in hunks
- **Hunk Refinement**: Word-level change highlighting
- **Navigation**: Jump between files, hunks
- **Application**: Apply/reverse individual hunks
- **Editing**: Modify diffs, auto-update line numbers
- **Fringe Indicators**: Visual +/- markers

**Refinement Algorithm**:
```elisp
;; Compares old/new versions at character level
;; Highlights exact changed words/characters
;; Can be automatic (font-lock) or on-demand
(defcustom diff-refine 'font-lock
  "If non-nil, enable hunk refinement.
The value `font-lock' means to refine during font-lock.
The value `navigation' means to refine each hunk as you visit it.")
```

### log-view.el (956 lines)

Revision log browser supporting multiple VCS formats:

**Supported Formats**:
- RCS/CVS: Classic `---` separator format
- Subversion: `r4622 | author | date` format
- Git: Customizable via `--pretty` format
- Mercurial: `changeset: 11:8ff1a4166444` format
- Darcs: Patch-oriented format

**Operations**:
- View diffs for revisions
- Annotate at revision
- Cherry-pick commits
- Modify commit messages
- Mark/unmark revisions

### ediff Suite (10 files, ~18,000 lines)

Advanced visual diff/merge tool:

**Components**:
- **`ediff.el`** (1,655 lines): Main entry points
- **`ediff-util.el`** (4,098 lines): Core functionality
- **`ediff-mult.el`** (2,427 lines): Directory comparison
- **`ediff-wind.el`** (1,299 lines): Window management
- **`ediff-diff.el`** (1,474 lines): Diff engine integration
- **`ediff-init.el`** (1,536 lines): Initialization
- **`ediff-merg.el`** (383 lines): Merge operations
- **`ediff-ptch.el`** (860 lines): Patch application
- **`ediff-help.el`** (305 lines): Help system
- **`ediff-vers.el`** (193 lines): VC integration

**Ediff Modes**:
- 2-way file comparison
- 3-way file comparison
- 2-way buffer comparison
- 3-way merge with ancestor
- Directory comparison
- Patch application
- Revision comparison (VC integration)

**Window Layouts**:
```
┌────────────────────────────────────┐
│         Control Panel              │  ← Small help/command buffer
├─────────────────┬──────────────────┤
│     Buffer A    │    Buffer B      │  ← 2-way comparison
│  (original)     │  (modified)      │
│                 │                  │
├─────────────────┴──────────────────┤
│     Buffer C (optional)            │  ← 3-way: ancestor or output
└────────────────────────────────────┘
```

### vc-annotate.el (835 lines)

Blame/annotate visualization with color-coded age:

**Features**:
- Color-codes lines by age (recent → old)
- Multiple color schemes (fullscale, scale, fixed days)
- Navigate to revision at line
- Show diff at revision
- Background/foreground coloring modes

**Color Map**:
```elisp
;; Default: HSV gradient from red (new) to blue (old)
;; TTY: Optimized color sequence for 8-color terminals
;; Customizable time scales (days, weeks, months)
```

### vc-dir.el (1,744 lines)

Directory-level status browser:

**Display Format** (using `ewoc` - Emacs Widget for Object Collections):
```
VC Backend : Git
Working dir: /home/user/project
Branch     : main

                   ./
  edited           M  file1.el
  up-to-date          file2.el
  unregistered     ?? newfile.el
  ignored          !! temp.txt
```

**Features**:
- Mark/unmark files
- Mass operations (commit, revert, etc.)
- Asynchronous status updates
- Backend-specific extra info
- Directory folding
- Integration with VC commands

**Status Collection** (async pattern):
```elisp
;; Backend calls update-function incrementally
(defun vc-BACKEND-dir-status-files (dir files update-function)
  ;; Start async process
  ;; As results arrive:
  (funcall update-function partial-results t)
  ;; When complete:
  (funcall update-function final-results nil))
```

## Design Patterns

### 1. Backend Registration and Discovery

**Registration**:
```elisp
;; Backends declare themselves via:
;; 1. Entry in vc-handled-backends
;; 2. Autoload for vc-BACKEND-registered
;; 3. Backend file named vc-BACKEND.el

;; Example: vc-git.el
(put 'Git 'vc-functions nil)  ; Clear cache on reload

;;;###autoload
(defun vc-git-registered (file)
  (if (vc-find-root file ".git")
      (progn
        (load "vc-git" nil t)
        (vc-git-registered file))))
```

**Discovery Process**:
```elisp
;; 1. File opened → find-file-hook → vc-refresh-state
;; 2. vc-registered called
;; 3. Iterate vc-handled-backends
;; 4. For each backend:
;;    - Check if vc-BACKEND-registered autoload exists
;;    - Call it with short-circuit check (e.g., .git directory)
;;    - If true, load backend and call full function
;; 5. First successful backend "wins"
;; 6. Result cached in vc-backend property
```

**Optimization - Root Caching**:
```elisp
;; vc-find-root used by most backends
(defun vc-find-root (file witness)
  "Find the root of a checked out project.
The function walks up the directory tree from FILE looking for WITNESS."
  (let ((locate-dominating-stop-dir-regexp
         (or vc-ignore-dir-regexp locate-dominating-stop-dir-regexp)))
    (locate-dominating-file file witness)))

;; Git example:
(defun vc-git-root (file)
  (vc-find-root file ".git"))
```

### 2. Asynchronous Operations

**vc-dispatcher.el** provides the async framework:

```elisp
;; Location: /lisp/vc/vc-dispatcher.el

;; Core async execution
(defun vc-do-command (buffer okstatus command file-or-list &rest flags)
  "Execute a VC command, notifying user and checking for errors.
Output from COMMAND goes to BUFFER, or the current buffer if nil.
OKSTATUS is a list of acceptable exit statuses.
COMMAND is the name of the command to run.
FILE-OR-LIST is the name of a working file; it may be a list of files.
FLAGS are arguments to pass to COMMAND."
  ...)

;; Async with callback
(defun vc-start-logentry (files comment initial-contents msg action &optional after-hook)
  "Accept a comment for an operation on FILES.
Opens a log-edit buffer and calls ACTION when user confirms."
  ...)
```

**Async Dir-Status Pattern**:
```elisp
;; Backend starts async process, calls update function as results arrive

(defun vc-git-dir-status-files (dir files update-function)
  "Asynchronously update vc-dir for FILES in DIR."
  (let ((buffer (get-buffer-create " *vc-git-status*")))
    (with-current-buffer buffer
      ;; Start git status --porcelain
      (vc-git-command buffer 'async files "status" "--porcelain" "-z")
      ;; Set process filter
      (vc-set-async-update
       buffer
       (lambda ()
         ;; Parse partial output
         (let ((results (parse-git-status)))
           ;; Update UI incrementally
           (funcall update-function results t)))
       (lambda ()
         ;; Parse final output
         (let ((results (parse-git-status)))
           ;; Final update
           (funcall update-function results nil)))))))
```

**Process Filter**:
```elisp
(defun vc-process-filter (p s)
  "An alternative output filter for async process P.
One difference with the default filter is that this inserts S after markers.
Another is that undo information is not kept."
  (let ((buffer (process-buffer p)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let ((buffer-undo-list t)
                (inhibit-read-only t))
            (goto-char (process-mark p))
            (insert s)
            (set-marker (process-mark p) (point))))))))
```

### 3. State Caching and Invalidation

**Two-Level Caching**:

1. **File Properties** (short-term, in-memory):
```elisp
(defvar vc-file-prop-obarray (make-hash-table :test 'equal)
  "Obarray for per-file properties.")

;; Cache backend and state
(vc-file-setprop file 'vc-backend 'Git)
(vc-file-setprop file 'vc-state 'edited)
(vc-file-setprop file 'vc-working-revision "abc123")
```

2. **Backend-Specific Cache** (persistent across sessions):
```elisp
;; Git stores branch name, stash count, etc.
(vc-file-setprop file 'vc-git-symbolic-ref "main")
```

**Invalidation Strategy**:

```elisp
;; Explicit invalidation after state-changing operations
(defun vc-resynch-buffer (file &optional keep noquery reset-vc-info)
  "Resync buffer visiting FILE with its on-disk state.
If RESET-VC-INFO is non-nil, forget cached VC information."
  (when reset-vc-info
    (vc-file-clearprops file))
  ...)

;; Called after: commit, revert, update, merge

;; Automatic invalidation on file modification
(defun vc-after-save ()
  "Called from `basic-save-buffer' after saving a file."
  (when (vc-backend buffer-file-name)
    ;; State may have changed (conflict resolved, etc.)
    (vc-file-setprop buffer-file-name 'vc-state nil)))
```

**Cache-Aware Property Access**:
```elisp
(defun vc-state (file)
  "Return the VC state of FILE."
  (or (vc-file-getprop file 'vc-state)
      (let ((state (vc-call state file)))
        (vc-file-setprop file 'vc-state state)
        state)))
```

### 4. Hook System

VC integrates deeply with Emacs via hooks:

```elisp
;; Find-file integration
(add-hook 'find-file-hook 'vc-refresh-state)

;; Save integration
;; (Called from basic-save-buffer in files.el)
(defun vc-after-save ()
  "Check VC state after saving."
  (when (vc-backend buffer-file-name)
    (vc-state-refresh buffer-file-name)
    (when (and (vc-state buffer-file-name)
               (eq (vc-state buffer-file-name) 'conflict)
               (not (vc-find-conflict-markers)))
      ;; Conflict markers removed, mark resolved
      (when vc-resolve-conflicts
        (vc-call mark-resolved (list buffer-file-name))))))

;; Kill-buffer hook
(add-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;; After-revert hook
(add-hook 'after-revert-hook 'vc-after-revert)
```

### 5. Mode-Line Integration

VC updates the mode line to show file status:

```elisp
;; Mode line format: "Git-main:abc123"
;;                    ^^^ ^^^^ ^^^^^^
;;                    |   |    +- revision/commit
;;                    |   +- branch (if applicable)
;;                    +- backend

(defun vc-mode-line (file)
  "Set `vc-mode' to display the VC status of FILE."
  (let* ((backend (vc-backend file))
         (state (vc-state file))
         (state-echo (cdr (assoc state vc-state-heuristic-alist)))
         (face (vc-mode-line-face state))
         (string (vc-call-backend backend 'mode-line-string file)))
    (setq vc-mode
          (concat " " (propertize string 'face face
                                  'help-echo state-echo)))))
```

**State Faces**:
```elisp
;; Location: /lisp/vc/vc-hooks.el:48-98

(defface vc-up-to-date-state ...)
(defface vc-needs-update-state ...)
(defface vc-locked-state ...)
(defface vc-locally-added-state ...)
(defface vc-conflict-state ...)
(defface vc-removed-state ...)
(defface vc-missing-state ...)
(defface vc-edited-state ...)
(defface vc-ignored-state ...)
```

## Implementation Deep Dives

### Git Backend (vc-git.el)

The Git backend is the most feature-complete and serves as a reference implementation:

**Key Implementation Details**:

1. **Command Execution**:
```elisp
(defun vc-git--run-command-string (file &rest args)
  "Run git command with ARGS on FILE, return output string."
  (let ((default-directory (or (vc-git-root file) default-directory)))
    (apply 'vc-git--run-command-string-1 nil args)))

(defun vc-git-command (buffer okstatus file-or-list &rest flags)
  "Wrapper for `vc-do-command' that uses vc-git-program."
  (apply 'vc-do-command buffer okstatus vc-git-program
         file-or-list flags))
```

2. **Literal Pathspecs** (for special characters):
```elisp
(defvar vc-git-use-literal-pathspecs t
  "Non-nil to treat pathspecs literally.
Good example: \"test[56].xx\"")

;; Sets GIT_LITERAL_PATHSPECS=1 environment variable
```

3. **Dir-Status Implementation**:
```elisp
(defun vc-git-dir-status-files (dir files update-function)
  (let ((args '("status" "--porcelain" "-z" "--untracked-files")))
    ;; Add --ignored if supported
    (when (version<= "1.7.6.3" (vc-git--program-version))
      (push "--ignored" args))
    ;; Execute asynchronously
    (vc-git-dir-status-goto-stage 'update-index dir files
                                  update-function)))
```

4. **Stash Integration**:
```elisp
(defun vc-git-dir-extra-headers (dir)
  "Git-specific extra headers for vc-dir."
  (concat
   (propertize "Branch     : " 'face 'vc-dir-header)
   (propertize (vc-git--symbolic-ref dir) 'face 'vc-dir-header-value)
   "\n"
   (when (vc-git-stash-list)
     (concat
      (propertize "Stash      : " 'face 'vc-dir-header)
      (vc-git-stash-summary)
      "\n"))))
```

### Dispatcher Architecture (vc-dispatcher.el)

The dispatcher provides infrastructure for directory buffers and command execution:

**EWOC-Based Display**:
```elisp
;; EWOC = Emacs Widget for Object Collections
;; Efficiently manages large lists with per-item rendering

(defvar vc-ewoc nil
  "The ewoc data structure for the directory buffer.")

(defun vc-dir-refresh ()
  "Refresh the directory buffer."
  (ewoc-filter vc-ewoc 'identity)  ; Keep all items
  (vc-call-backend vc-dir-backend 'dir-status-files
                   default-directory nil
                   #'vc-dir-status-update-function))
```

**Command Log Buffer**:
```elisp
(defcustom vc-command-messages nil
  "If non-nil, display messages about running back-end commands.")

;; All backend commands log to *vc-cmd* buffer
;; Useful for debugging and understanding what VC is doing
```

### Diff Mode Features (diff-mode.el)

**Hunk Navigation and Application**:
```elisp
;; Find next/previous hunk
(defun diff-hunk-next (&optional arg)
  "Move to next hunk."
  (interactive "p")
  (diff-hunk-move arg))

;; Apply hunk to source file
(defun diff-apply-hunk (&optional reverse)
  "Apply current hunk to source file.
With prefix arg, reverse the hunk."
  (interactive "P")
  (let* ((hunk (diff-hunk-text))
         (file (diff-find-file-name))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (goto-char (diff-find-hunk-line-number))
      (patch-buffer hunk reverse))))
```

**Syntax Highlighting in Hunks**:
```elisp
(defcustom diff-font-lock-syntax t
  "If non-nil, diff hunk font-lock includes source language syntax."
  :type '(choice (const :tag "Automatic" t)
                 (const :tag "Hunk-only" hunk-only)
                 (const :tag "Disabled" nil)))

;; Detects language from file extension
;; Applies appropriate major-mode font-lock
;; Overlays diff highlighting on top
```

### Merge Conflict Resolution (smerge-mode.el)

**Conflict Detection and Parsing**:
```elisp
(defconst smerge-begin-re "^<<<<<<< \\(.*\\)\n"
  "Regexp matching the start of a conflict.")

(defconst smerge-end-re "^>>>>>>> \\(.*\\)\n"
  "Regexp matching the end of a conflict.")

(defconst smerge-base-re "^||||||| \\(.*\\)\n"
  "Regexp matching the base-revision marker.")

(defconst smerge-lower-re "^=======\n"
  "Regexp matching the lower-revision marker.")

(defun smerge-find-conflict ()
  "Find next merge conflict."
  (re-search-forward smerge-begin-re nil t))
```

**Resolution Commands**:
```elisp
(defun smerge-keep-upper ()
  "Keep upper (mine) version."
  (smerge-keep-n 1))

(defun smerge-keep-lower ()
  "Keep lower (theirs) version."
  (smerge-keep-n 3))

(defun smerge-keep-all ()
  "Keep all versions."
  (smerge-keep-n 0))

(defun smerge-ediff ()
  "Invoke ediff to resolve conflict."
  (let* ((buf (current-buffer))
         (upper (smerge-get-upper))
         (lower (smerge-get-lower))
         (base (smerge-get-base)))
    (ediff-merge-buffers-with-ancestor upper lower base)))
```

## User Interaction Patterns

### Context-Aware vc-next-action

The `C-x v v` command (`vc-next-action`) adapts based on file state:

```elisp
(defun vc-next-action (verbose)
  "Do the next logical VC operation on file(s).
State     | Action
----------|--------------------------------------------------
unregistered | Register file
added     | Commit (if repository supports staging)
edited    | Commit changes
up-to-date| Check out for editing (locking VCS) or do nothing
needs-update | Pull/update from repository
needs-merge | Merge with upstream
conflict  | Mark resolved"
  (interactive "P")
  (let ((state (vc-state file)))
    (pcase state
      ('unregistered (vc-register))
      ('edited (vc-checkin))
      ('needs-update (vc-update))
      ...)))
```

### Prefix Arguments

Many VC commands use prefix arguments for variants:

```elisp
C-x v v      ; Next action
C-u C-x v v  ; Next action with prompts

C-x v =      ; Diff working vs. repository
C-u C-x v =  ; Diff between two revisions

C-x v l      ; Short log
C-u C-x v l  ; Long log with full messages

C-x v ~      ; Retrieve specific revision
```

### File Set Operations

Modern VC operates on filesets, not individual files:

```elisp
;; In vc-dir buffer:
;; - Mark files (m, u, M, U)
;; - Operate on marked files (v, =, l, etc.)

;; From dired:
;; - Mark files in dired
;; - VC commands operate on marked files

;; Example: Commit multiple files
(vc-checkin files comment)  ; files is a list
```

## Configuration and Customization

### Key Customization Variables

```elisp
;; Backend selection
(setq vc-handled-backends '(Git Hg SVN))

;; Suppress prompts for experienced users
(setq vc-suppress-confirm t)

;; Follow symlinks without asking
(setq vc-follow-symlinks t)

;; Display in mode line
(setq vc-display-status t)  ; or 'no-backend or nil

;; Git-specific
(setq vc-git-diff-switches '("-b" "-w"))  ; Ignore whitespace
(setq vc-git-annotate-switches "-w")       ; Blame ignores whitespace
(setq vc-git-log-switches '("--graph" "--decorate"))

;; Diff mode
(setq diff-refine 'font-lock)  ; Auto-refine hunks
(setq diff-font-lock-syntax t)  ; Syntax highlight in diffs

;; Auto-resolve conflicts when markers removed
(setq vc-resolve-conflicts t)
(setq vc-git-resolve-conflicts 'unstage-maybe)
```

### Backend Precedence

When multiple backends could handle a file:

```elisp
;; First match wins
(setq vc-handled-backends '(Git Hg SVN))

;; For nested repositories, inner takes precedence
;; Example: Git repo inside SVN checkout
;; .git found first → Git backend used
```

### Performance Tuning

```elisp
;; Ignore slow network mounts
(setq vc-ignore-dir-regexp
      (concat vc-ignore-dir-regexp "\\|^/mnt/slow-nfs"))

;; Async operations (Git 2.28+)
(setq vc-git-async-checkins t)

;; Disable VC for certain backends
(setq vc-handled-backends (delq 'RCS vc-handled-backends))
```

## Advanced Features

### 1. Annotate/Blame

```elisp
C-x v g  → vc-annotate

;; Shows each line with:
;; - Revision/commit that last changed it
;; - Author
;; - Date
;; - Color-coded by age

;; Commands in annotate buffer:
n, p     → Next/previous revision
d        → Show diff for revision
f        → Visit revision
a        → Re-annotate at revision
```

### 2. Region History

```elisp
C-x v h  → vc-region-history

;; Shows log and diffs for selected region
;; Tracks history through renames and line movements
;; Git uses git log -L
```

### 3. Shelve/Stash

```elisp
;; Git stash shown in vc-dir header
;; Can apply, pop, drop stashes from vc-dir
```

### 4. Working Trees (Git Worktrees)

```elisp
;; List other working trees
(vc-call known-other-working-trees)

;; Add working tree
(vc-call add-working-tree directory)

;; Delete working tree
(vc-call delete-working-tree directory)
```

### 5. Cherry-Pick

```elisp
;; In log-view:
C  → log-view-cherry-pick

;; Applies commit to current branch
;; Uses backend-specific cherry-pick
```

### 6. Retrieve Revisions

```elisp
C-x v ~  → vc-retrieve-revision

;; Opens specific revision in new buffer
;; Read-only, not in working tree
;; Can diff, annotate, etc.
```

## Error Handling and Edge Cases

### Missing Backend Executable

```elisp
;; vc-git-registered checks for git executable
(defun vc-git-registered (file)
  (and (vc-git-root file)
       (executable-find vc-git-program)
       ...))

;; Avoids noisy errors if VCS not installed
```

### Corrupted Repository

```elisp
;; Backends should handle gracefully
(with-demoted-errors "VC error: %S"
  (vc-git--run-command-string file "status"))

;; Returns nil if command fails
;; VC treats as unregistered
```

### Nested Repositories

```elisp
;; Inner repository takes precedence
;; vc-find-root stops at first match

;; Example:
;; /project/.git        ← Git repo
;; /project/vendor/.hg  ← Hg subrepo
;; /project/vendor/file.c → Handled by Hg
```

### Remote Files (TRAMP)

```elisp
;; VC works over TRAMP
;; Backend commands executed on remote host
;; May be slow; caching especially important

;; Connection-local variables for remote Git
(connection-local-set-profile-variables
 'vc-git-connection-default-profile
 '((vc-git--program-version . nil)))
```

## Testing and Debugging

### Debugging VC Operations

```elisp
;; Enable command logging
(setq vc-command-messages t)

;; Check *vc-cmd* buffer for backend commands
;; Shows exact git/hg/svn commands executed

;; Trace backend calls
(trace-function 'vc-call-backend)
(trace-function 'vc-git-state)

;; Check file properties
(vc-file-getprop "file.el" 'vc-backend)  ; => Git
(vc-file-getprop "file.el" 'vc-state)     ; => edited
```

### Test Files

```elisp
;; VC has extensive test suite
;; /test/lisp/vc/

;; Test backends:
;; - vc-tests.el: Generic backend tests
;; - vc-git-tests.el: Git-specific tests
;; - ediff-*-tests.el: Ediff test suite
```

## Migration and Compatibility

### Supporting New VCS

To add support for a new VCS named "FOO":

1. **Create `/lisp/vc/vc-foo.el`**:
```elisp
;;; vc-foo.el --- VC backend for FOO

(require 'vc-dispatcher)

;; Backend properties
(defun vc-foo-revision-granularity () 'repository)
(defun vc-foo-checkout-model (_files) 'implicit)

;; State-querying
;;;###autoload
(defun vc-foo-registered (file)
  (if (vc-find-root file ".foo")
      (progn (load "vc-foo" nil t)
             (vc-foo-registered file))))

(defun vc-foo-registered (file)
  "Real implementation..."
  (vc-find-root file ".foo"))

(defun vc-foo-state (file)
  "Return state..."
  ...)

;; State-changing
(defun vc-foo-register (files &optional comment) ...)
(defun vc-foo-checkin (files comment &optional rev) ...)

;; History
(defun vc-foo-print-log (files buffer &optional shortlog start-revision limit) ...)
(defun vc-foo-diff (files &optional rev1 rev2 buffer async) ...)

(provide 'vc-foo)
```

2. **Add to `vc-handled-backends`**:
```elisp
(add-to-list 'vc-handled-backends 'FOO)
```

3. **Implement mandatory functions** (marked with `*` in API contract)

4. **Implement optional functions** as needed

### Backward Compatibility

VC maintains compatibility with older backend implementations:

```elisp
;; Default implementations for optional functions
(defun vc-default-find-ignore-file (backend file)
  "Default implementation finds .gitignore-style file."
  ...)

;; Fallback for missing functions
(vc-call-backend backend 'function args)
;; → vc-BACKEND-function if exists
;; → vc-default-function otherwise
;; → error if neither exists
```

## Performance Characteristics

### Backend Speed Comparison

| Backend | State Query | Dir Status | Log | Diff | Notes |
|---------|-------------|------------|-----|------|-------|
| Git     | Fast        | Fast       | Fast | Fast | All operations local |
| Hg      | Fast        | Fast       | Fast | Fast | All operations local |
| SVN     | Medium      | Slow       | Medium | Medium | Network operations |
| Bzr     | Medium      | Medium     | Medium | Medium | Hybrid model |
| CVS     | Slow        | Very Slow  | Slow | Slow | File-by-file, network |
| RCS     | Fast        | Medium     | Fast | Fast | Local, file-based |

### Optimization Strategies

1. **Property Caching**: Avoid redundant state queries
2. **Async Status**: Don't block on directory scanning
3. **Lazy Loading**: Backends loaded only when needed
4. **Root Caching**: Remember repository roots
5. **Batch Operations**: Group file operations when possible

## Future Directions

From `/lisp/vc/vc.el:820-849` (Todo section):

### Planned Features

```elisp
;; New Primitives:
;; - uncommit: undo last checkin, leave changes in place
;; - deal with push operations

;; Primitives that need changing:
;; - vc-update/vc-merge should work on whole repository
;; - Make sure *vc-dir* buffer updated after operations

;; Improved branch and tag handling:
;; - Generic mechanism for branch name display in mode-line
;; - Ability to list tags and branches
```

### Modern VCS Trends

Recent developments requiring VC evolution:

1. **Monorepo Support**: Handle very large repositories
2. **Sparse Checkouts**: Git sparse-checkout, Hg narrow
3. **Cloud Hosting**: GitHub, GitLab, Bitbucket integration
4. **Code Review**: Pull request workflows
5. **CI/CD Integration**: Show build status in VC buffers

## Conclusion

The Emacs VC system demonstrates exceptional software architecture:

**Key Strengths**:
1. **Clean Abstraction**: Backend dispatch system is elegant and extensible
2. **Comprehensive**: Supports 8+ VCS with unified interface
3. **Performance**: Property caching and async operations keep it responsive
4. **Integration**: Deep hooks into Emacs (find-file, save, mode-line)
5. **Maturity**: 30+ years of refinement shows in edge case handling

**Design Lessons**:
1. **Dispatch Pattern**: `vc-call` macro demonstrates dynamic dispatch in Lisp
2. **Caching Strategy**: Two-level cache (file properties + backend cache)
3. **Async Design**: Callback-based async predates modern async/await
4. **Hook System**: Multiple integration points for seamless UX
5. **Graceful Degradation**: Missing backends/features handled cleanly

**Code Organization**:
- **39 files**, **52,964 lines** well-organized by concern
- Clear separation: core → abstraction → backends → tools
- Consistent naming: `vc-BACKEND-FUNCTION` convention
- Extensive documentation in function contracts

The VC system remains one of Emacs's most sophisticated subsystems, providing a blueprint for building extensible, backend-agnostic interfaces in Lisp.

## References

### Primary Source Files
- `/lisp/vc/vc.el` - Main interface and backend API contract (lines 108-755)
- `/lisp/vc/vc-hooks.el` - Initialization and property system
- `/lisp/vc/vc-dispatcher.el` - Async execution framework
- `/lisp/vc/vc-git.el` - Reference backend implementation

### Related Documentation
- Info manual: `(emacs) Version Control`
- Backend API: `/lisp/vc/vc.el` commentary section
- Ediff manual: `(ediff) Top`

### Key Data Structures
- File property obarray: `vc-file-prop-obarray`
- Backend function cache: `(get 'BACKEND 'vc-functions)`
- Dir fileinfo: `vc-dir-fileinfo` struct (ewoc elements)

### Important Variables
- `vc-handled-backends` - Registered backends
- `vc-state` - File state symbols
- `vc-mode` - Mode line string
- `vc-directory-exclusion-list` - Ignored directories
