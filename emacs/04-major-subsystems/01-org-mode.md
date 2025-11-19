# Org Mode: Literate Programming and Organization

## Overview

Org mode is one of Emacs's most significant and comprehensive subsystems, providing a complete environment for notes, task management, literate programming, and document preparation. The codebase comprises **127 files with 146,533 lines** of code.

**Location:** `/home/user/emacs/lisp/org/`

**Key Statistics:**
- Core file: `org.el` (22,373 lines)
- Parser: `org-element.el` (8,730 lines)
- Agenda: `org-agenda.el` (11,211 lines)
- Tables: `org-table.el` (6,438 lines)
- Export core: `ox.el` (7,450 lines)
- Babel core: `ob-core.el` (3,677 lines)
- 48 Babel language files (ob-*.el)
- 12 export backends (ox-*.el)

## Core Architecture

### 1. Foundation: Building on Outline Mode

Org mode is fundamentally built on top of Emacs's `outline-mode`, extending it with rich functionality for task management, literate programming, and document export.

```elisp
;; From org.el (lines 1-50)
;;; org.el --- Outline-based notes management and organizer -*- lexical-binding: t; -*-

;; Org is a mode for keeping notes, maintaining ToDo lists, and doing
;; project planning with a fast and effective plain-text system.
;;
;; Org mode develops organizational tasks around NOTES files that
;; contain information about projects as plain text.  Org mode is
;; implemented on top of outline-mode, which makes it possible to keep
;; the content of large files well structured.

;; Core outline integration
(defvar org-outline-regexp "\\*+ "
  "Regexp to match Org headlines.")

(defvar org-outline-regexp-bol "^\\*+ "
  "Regexp to match Org headlines.
This is similar to `org-outline-regexp' but additionally makes
sure that we are at the beginning of the line.")

(defvar org-heading-regexp "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Matches a headline, putting stars and text into groups.
Stars are put in group 1 and the trimmed body in group 2.")
```

**Key Architecture Principle:** Org mode headlines are outline headings denoted by asterisks (*), with the number of asterisks determining the heading level. This simple syntax enables the entire hierarchy system.

### 2. The org.el Core (22,373 lines)

The main `org.el` file serves as the entry point and orchestrator for the entire system.

**Key Responsibilities:**

```elisp
;; From org.el (lines 72-104)
;;;; Require other packages

(require 'org-compat)
(org-assert-version)

(require 'cl-lib)
(require 'calendar)
(require 'find-func)
(require 'format-spec)
(require 'thingatpt)

;; Load org subsystems
(eval-and-compile (require 'org-macs))
(require 'org-compat)
(require 'org-keys)
(require 'ol)              ; Links
(require 'oc)              ; Citations
(require 'org-table)       ; Tables
(require 'org-fold)        ; Folding
(require 'org-cycle)       ; Visibility cycling
```

**Module Organization:**
1. **Core utilities** - `org-macs.el`, `org-compat.el`
2. **Syntax layer** - `org-element.el` (parser)
3. **UI layer** - `org-cycle.el`, `org-fold.el`, `org-keys.el`
4. **Feature modules** - Links, tables, agenda, capture
5. **Babel** - `ob-core.el` + language files
6. **Export** - `ox.el` + backend files

### 3. The Element Parser (org-element.el, 8,730 lines)

The `org-element.el` parser provides a complete abstract syntax tree (AST) representation of Org documents.

**Parser Architecture:**

```elisp
;; From org-element.el (lines 24-57)
;;; Commentary:
;;
;; See <https://orgmode.org/worg/dev/org-syntax.html> for details about
;; Org syntax.
;;
;; Lisp-wise, a syntax object can be represented as a list.
;; It follows the pattern (TYPE PROPERTIES CONTENTS), where:
;;   TYPE is a symbol describing the object.
;;   PROPERTIES is the property list attached to it.  See docstring of
;;              appropriate parsing function to get an exhaustive list.
;;   CONTENTS is a list of syntax objects or raw strings contained
;;            in the current object, when applicable.
;;
;; For the whole document, TYPE is `org-data' and PROPERTIES is nil.
```

**Element Types Defined:**

```elisp
;; From org-element.el (lines 103-200)

;; Constant definitions for various element types
(defconst org-element-archive-tag "ARCHIVE"
  "Tag marking a subtree as archived.")

(defconst org-element-citation-key-re
  (rx "@" (group (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%~"))))
  "Regexp matching a citation key.")

(defconst org-element-clock-line-re
  ;; Regex for CLOCK: lines
  "Regexp matching a clock line.")

(defconst org-element-comment-string "COMMENT"
  "String marker for commented headlines.")

(defconst org-element-closed-keyword "CLOSED:"
  "Keyword used to close TODO entries.")

(defconst org-element-deadline-keyword "DEADLINE:"
  "Keyword used to mark deadline entries.")

(defconst org-element-scheduled-keyword "SCHEDULED:"
  "Keyword used to mark scheduled entries.")

(defconst org-element-drawer-re
  (rx line-start (0+ (any ?\s ?\t))
      ":" (group (1+ (any ?- ?_ word))) ":"
      (0+ (any ?\s ?\t)) line-end)
  "Regexp matching opening or closing line of a drawer.")

(defconst org-element-dynamic-block-open-re
  ;; Regex for #+BEGIN: blocks
  "Regexp matching the opening line of a dynamic block.")
```

**Parser API:**

The element parser provides several key functions:

1. **`org-element-parse-buffer`** - Parse entire buffer into AST
2. **`org-element-at-point`** - Get element at current position
3. **`org-element-context`** - Get detailed context (including objects within elements)
4. **`org-element-map`** - Walk the parse tree
5. **`org-element-interpret-data`** - Convert AST back to Org syntax

**Cache System:**

The parser includes a sophisticated caching mechanism to avoid re-parsing unchanged portions of the buffer, critical for performance on large files.

### 4. Visibility Cycling (org-cycle.el, 947 lines)

Org mode's signature feature is TAB-based visibility cycling through outline levels.

```elisp
;; From org-cycle.el (lines 1-30)
;;; org-cycle.el --- Visibility cycling of Org entries -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains code controlling global folding state in buffer
;; and TAB-cycling.

(defvar-local org-cycle-global-status nil)
(put 'org-cycle-global-status 'org-state t)
(defvar-local org-cycle-subtree-status nil)
(put 'org-cycle-subtree-status 'org-state t)

(defcustom org-cycle-skip-children-state-if-no-children t
  "Non-nil means skip CHILDREN state in entries that don't have any."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-max-level nil
  "Maximum level which should still be subject to visibility cycling.
Levels higher than this will, for cycling, be treated as text, not a headline."
  :group 'org-cycle
  :type '(choice
          (const :tag "No limit" nil)
          (integer :tag "Maximum level")))
```

**Cycling States:**
1. **FOLDED** - Only headlines visible
2. **CHILDREN** - Direct children visible
3. **SUBTREE** - All descendants visible

The cycling system integrates with the `org-fold.el` folding backend, which provides efficient text hiding.

## Babel: Literate Programming System

Org-Babel is Org mode's literate programming subsystem, enabling executable code blocks in 40+ languages.

### 1. Babel Core (ob-core.el, 3,677 lines)

The core provides the execution engine and infrastructure.

```elisp
;; From ob-core.el (lines 1-27)
;;; ob-core.el --- Working with Code Blocks          -*- lexical-binding: t; -*-

;; Authors: Eric Schulte
;;	Dan Davison
;; Keywords: literate programming, reproducible research

;;; Commentary:

;; Security and confirmation
(defcustom org-confirm-babel-evaluate t
  "Confirm before evaluation.
Require confirmation before interactively evaluating code
blocks in Org buffers.  The default value of this variable is t,
meaning confirmation is required for any code block evaluation.
This variable can be set to nil to inhibit any future
confirmation requests.  This variable can also be set to a
function which takes two arguments the language of the code block
and the body of the code block.

Warning: Disabling confirmation may result in accidental
evaluation of potentially harmful code."
  :group 'org-babel
  :version "24.1"
  :type '(choice boolean function))

(defcustom org-babel-results-keyword "RESULTS"
  "Keyword used to name results generated by code blocks."
  :group 'org-babel
  :version "24.4"
  :type 'string)
```

**Code Block Structure:**

```org
#+begin_src language :header-args
  code here
#+end_src

#+RESULTS:
: output here
```

**Header Arguments Control:**
- `:results` - How to handle output (value, output, silent, replace, append)
- `:session` - Named session for persistent state
- `:exports` - What to export (code, results, both, none)
- `:file` - Output to file
- `:var` - Variable bindings
- `:noweb` - Literate programming references

### 2. Language Support (48 language files)

Each language has an `ob-LANG.el` file implementing the language interface.

**Example: Python Support (ob-python.el)**

```elisp
;; From ob-python.el (lines 1-124)
;;; ob-python.el --- Babel Functions for Python      -*- lexical-binding: t; -*-

;; Authors: Eric Schulte
;;	 Dan Davison
;; Maintainer: Jack Kamm <jackkamm@gmail.com>
;; Keywords: literate programming, reproducible research

(require 'ob)
(require 'org-macs)
(require 'python)

;; Register file extension for tangling
(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("python" . "py"))

;; Default header arguments
(defvar org-babel-default-header-args:python '())

;; Language-specific header arguments
(defconst org-babel-header-args:python
  '((return . :any)
    (python . :any)
    (async . ((yes no))))
  "Python-specific header arguments.")

;; Main execution function
(defun org-babel-execute:python (body params)
  "Execute Python BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-python-initiate-session
                   (cdr (assq :session params))))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (full-body
          (concat
           (org-babel-expand-body:generic
            body params
            (org-babel-variable-assignments:python params))
           (when return-val
             (format (if session "\n%s" "\nreturn %s") return-val))))
         (result (org-babel-python-evaluate
                  session full-body result-type
                  result-params preamble async graphics-file)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
                          (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
                          (cdr (assq :rownames params))))))
```

**Language Interface Contract:**

Each language file must implement:
1. **`org-babel-execute:LANG`** - Execute code block
2. **`org-babel-expand-body:LANG`** - Expand noweb references
3. **`org-babel-variable-assignments:LANG`** - Convert variables to language syntax
4. **`org-babel-prep-session:LANG`** - Initialize session (optional)

**Supported Languages (48 total):**
```
awk, C/C++, R, calc, clojure, css, ditaa, dot, emacs-lisp, eshell,
forth, fortran, gnuplot, groovy, haskell, java, js, julia, latex,
lilypond, lisp, lua, makefile, matlab, maxima, ocaml, octave, org,
perl, plantuml, processing, python, ruby, sass, scheme, screen, sed,
shell, sql, sqlite, and more...
```

### 3. Tangling (ob-tangle.el, 736 lines)

Tangling extracts code blocks to source files for execution.

```elisp
;; From ob-tangle.el (lines 1-150)
;;; ob-tangle.el --- Extract Source Code From Org Files -*- lexical-binding: t; -*-

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research

;;; Commentary:

;; Extract the code from source blocks out into raw source-code files.

(defcustom org-babel-tangle-lang-exts
  '(("emacs-lisp" . "el")
    ("elisp" . "el"))
  "Alist mapping languages to their file extensions.
The key is the language name, the value is the string that should
be inserted as the extension commonly used to identify files
written in this language."
  :group 'org-babel-tangle
  :type '(repeat
          (cons
           (string "Language name")
           (string "File Extension"))))

(defcustom org-babel-post-tangle-hook nil
  "Hook run in code files tangled by `org-babel-tangle'."
  :group 'org-babel-tangle
  :type 'hook)

(defcustom org-babel-tangle-comment-format-beg "[[%link][%source-name]]"
  "Format of inserted comments in tangled code files.
The following format strings can be used to insert special
information into the output using `org-fill-template'.
%start-line --- the line number at the start of the code block
%file --------- the file from which the code block was tangled
%link --------- Org style link to the code block
%source-name -- name of the code block"
  :group 'org-babel-tangle
  :type 'string)
```

**Tangle Process:**

1. Parse buffer for all code blocks with `:tangle` header
2. Group blocks by target file
3. Sort by `:tangle` order or buffer position
4. Write blocks to files with optional comments
5. Set file permissions (for scripts)
6. Run `org-babel-post-tangle-hook`

**Tangle Headers:**
```org
#+begin_src emacs-lisp :tangle init.el
  ;; This code will be written to init.el
#+end_src

#+begin_src python :tangle script.py :shebang #!/usr/bin/env python
  # This becomes an executable Python script
#+end_src
```

### 4. Noweb Reference System

Babel supports literate programming through noweb-style references.

```elisp
;; From ob-core.el (lines 199-200)
(defcustom org-babel-noweb-wrap-start "<<"
  "String used to begin a noweb reference in a code block.")

(defcustom org-babel-noweb-wrap-end ">>"
  "String used to end a noweb reference in a code block.")
```

**Usage Example:**

```org
#+name: setup
#+begin_src python
  import numpy as np
  import matplotlib.pyplot as plt
#+end_src

#+name: analysis
#+begin_src python :noweb yes
  <<setup>>

  # Analysis code using the imports
  data = np.random.randn(1000)
  plt.hist(data)
#+end_src
```

## Export System

The export system provides a pluggable architecture for converting Org documents to various formats.

### 1. Export Core (ox.el, 7,450 lines)

The generic export engine built on the element parser.

```elisp
;; From ox.el (lines 24-71)
;;; Commentary:
;;
;; This library implements a generic export engine for Org, built on
;; its syntactical parser: Org Elements.
;;
;; Besides that parser, the generic exporter is made of three distinct
;; parts:
;;
;; - The communication channel consists of a property list, which is
;;   created and updated during the process.  Its use is to offer
;;   every piece of information, would it be about initial environment
;;   or contextual data, all in a single place.
;;
;; - The transcoder walks the parse tree, ignores or treat as plain
;;   text elements and objects according to export options, and
;;   eventually calls backend specific functions to do the real
;;   transcoding, concatenating their return value along the way.
;;
;; - The filter system is activated at the very beginning and the very
;;   end of the export process, and each time an element or an object
;;   has been converted.  It is the entry point to fine-tune standard
;;   output from backend transcoders.
;;
;; The core functions is `org-export-as'.  It returns the transcoded
;; buffer as a string.  Its derivatives are `org-export-to-buffer' and
;; `org-export-to-file'.
;;
;; An export backend is defined with `org-export-define-backend'.
```

**Export Options (Global):**

```elisp
;; From ox.el (lines 111-190)
(defconst org-export-options-alist
  '((:title "TITLE" nil nil parse)
    (:date "DATE" nil nil parse)
    (:author "AUTHOR" nil user-full-name parse)
    (:email "EMAIL" nil user-mail-address t)
    (:language "LANGUAGE" nil org-export-default-language t)
    (:select-tags "SELECT_TAGS" nil org-export-select-tags split)
    (:exclude-tags "EXCLUDE_TAGS" nil org-export-exclude-tags split)
    (:creator "CREATOR" nil org-export-creator-string)
    (:headline-levels nil "H" org-export-headline-levels)
    (:preserve-breaks nil "\\n" org-export-preserve-breaks)
    (:section-numbers nil "num" org-export-with-section-numbers)
    (:time-stamp-file nil "timestamp" org-export-timestamp-file)
    (:with-archived-trees nil "arch" org-export-with-archived-trees)
    (:with-author nil "author" org-export-with-author)
    (:with-broken-links nil "broken-links" org-export-with-broken-links)
    (:with-clocks nil "c" org-export-with-clocks)
    (:with-creator nil "creator" org-export-with-creator)
    (:with-date nil "date" org-export-with-date)
    (:with-drawers nil "d" org-export-with-drawers)
    (:with-email nil "email" org-export-with-email)
    (:with-emphasize nil "*" org-export-with-emphasize)
    (:with-entities nil "e" org-export-with-entities)
    (:with-footnotes nil "f" org-export-with-footnotes)
    (:with-latex nil "tex" org-export-with-latex)
    (:with-planning nil "p" org-export-with-planning)
    (:with-priority nil "pri" org-export-with-priority)
    (:with-properties nil "prop" org-export-with-properties)
    (:with-smart-quotes nil "'" org-export-with-smart-quotes)
    (:with-sub-superscript nil "^" org-export-with-sub-superscripts)
    (:with-toc nil "toc" org-export-with-toc)
    (:with-tables nil "|" org-export-with-tables)
    (:with-tags nil "tags" org-export-with-tags)
    (:with-tasks nil "tasks" org-export-with-tasks)
    (:with-timestamps nil "<" org-export-with-timestamps)
    (:with-todo-keywords nil "todo" org-export-with-todo-keywords)
    ;; Citations processing
    (:with-cite-processors nil nil org-export-process-citations))
  "Alist between export properties and ways to set them.")
```

### 2. Backend Architecture

Backends are defined using `org-export-define-backend` macro.

**Example: HTML Backend (ox-html.el, 4,089 lines)**

```elisp
;; From ox-html.el (lines 58-119)
;;; Define Backend

(org-export-define-backend 'html
  '((bold . org-html-bold)
    (center-block . org-html-center-block)
    (clock . org-html-clock)
    (code . org-html-code)
    (drawer . org-html-drawer)
    (dynamic-block . org-html-dynamic-block)
    (entity . org-html-entity)
    (example-block . org-html-example-block)
    (export-block . org-html-export-block)
    (export-snippet . org-html-export-snippet)
    (fixed-width . org-html-fixed-width)
    (footnote-reference . org-html-footnote-reference)
    (headline . org-html-headline)
    (horizontal-rule . org-html-horizontal-rule)
    (inline-src-block . org-html-inline-src-block)
    (inlinetask . org-html-inlinetask)
    (inner-template . org-html-inner-template)
    (italic . org-html-italic)
    (item . org-html-item)
    (keyword . org-html-keyword)
    (latex-environment . org-html-latex-environment)
    (latex-fragment . org-html-latex-fragment)
    (line-break . org-html-line-break)
    (link . org-html-link)
    (node-property . org-html-node-property)
    (paragraph . org-html-paragraph)
    (plain-list . org-html-plain-list)
    (plain-text . org-html-plain-text)
    (planning . org-html-planning)
    (property-drawer . org-html-property-drawer)
    (quote-block . org-html-quote-block)
    (radio-target . org-html-radio-target)
    (section . org-html-section)
    (special-block . org-html-special-block)
    (src-block . org-html-src-block)
    (statistics-cookie . org-html-statistics-cookie)
    (strike-through . org-html-strike-through)
    (subscript . org-html-subscript)
    (superscript . org-html-superscript)
    (table . org-html-table)
    (table-cell . org-html-table-cell)
    (table-row . org-html-table-row)
    (target . org-html-target)
    (template . org-html-template)
    (timestamp . org-html-timestamp)
    (underline . org-html-underline)
    (verbatim . org-html-verbatim)
    (verse-block . org-html-verse-block))
  :filters-alist '((:filter-options . org-html-infojs-install-script)
                   (:filter-parse-tree . org-html-image-link-filter)
                   (:filter-final-output . org-html-final-function))
  :menu-entry
  '(?h "Export to HTML"
       ((?H "As HTML buffer" org-html-export-as-html)
        (?h "As HTML file" org-html-export-to-html)
        (?o "As HTML file and open"
            (lambda (a s v b)
              (if a (org-html-export-to-html t s v b)
                (org-open-file (org-html-export-to-html nil s v b)))))))
  :options-alist
  '((:html-doctype "HTML_DOCTYPE" nil org-html-doctype)
    (:html-container "HTML_CONTAINER" nil org-html-container-element)
    ;; ... many more options
    ))
```

**Backend Components:**

1. **Transcoders** - Functions that convert each element type to target format
2. **Filters** - Hooks to modify output at various stages
3. **Options** - Backend-specific export settings
4. **Menu entry** - Interactive export commands

### 3. Available Export Backends (12 total)

```
ox-ascii.el      (2,235 lines) - Plain text export
ox-beamer.el     (1,092 lines) - Beamer presentations (LaTeX)
ox-html.el       (4,089 lines) - HTML export
ox-icalendar.el    (937 lines) - iCalendar format
ox-koma-letter.el  (867 lines) - KOMA-Script letters
ox-latex.el      (4,512 lines) - LaTeX export
ox-man.el          (728 lines) - Unix man pages
ox-md.el           (650 lines) - Markdown export
ox-odt.el        (4,376 lines) - OpenDocument Text
ox-org.el          (369 lines) - Org to Org (normalization)
ox-publish.el    (1,368 lines) - Website publishing
ox-texinfo.el    (2,070 lines) - Texinfo documentation
```

### 4. Export Process Flow

```
1. Parse buffer with org-element-parse-buffer
   └─> Produces AST (Abstract Syntax Tree)

2. Initialize communication channel (plist with options)
   └─> Merge file options, buffer options, defaults

3. Run :filter-parse-tree filters
   └─> Modify AST before transcoding

4. Walk AST and call transcoders
   └─> Each element/object converted via backend function
   └─> Results concatenated into output string

5. Run :filter-final-output filters
   └─> Final modifications to complete output

6. Write to buffer or file
   └─> org-export-to-buffer or org-export-to-file
```

## Key Features

### 1. Agenda System (org-agenda.el, 11,211 lines)

The agenda provides a dynamic view of tasks across multiple Org files.

```elisp
;; From org-agenda.el (lines 1-45)
;;; org-agenda.el --- Dynamic task and appointment lists for Org

;;; Commentary:

;; This file contains the code for creating and using the Agenda for Org.
;;
;; The functions `org-batch-agenda', `org-batch-agenda-csv', and
;; `org-batch-store-agenda-views' are implemented as macros to provide
;; a convenient way for extracting agenda information from the command
;; line.

(defvar org-agenda-buffer-name "*Org Agenda*")

(defcustom org-agenda-confirm-kill 1
  "When set, remote killing from the agenda buffer needs confirmation.
When t, a confirmation is always needed.  When a number N, confirmation is
only needed when the text to be killed contains more than N non-white lines."
  :group 'org-agenda
  :type '(choice
          (const :tag "Never" nil)
          (const :tag "Always" t)
          (integer :tag "When more than N lines")))
```

**Agenda Views:**

1. **Daily/Weekly Agenda** - Scheduled items and deadlines
2. **TODO Lists** - Tasks by state
3. **Tags/Properties Search** - Query-based views
4. **Stuck Projects** - Projects without next actions
5. **Custom Views** - User-defined combinations

**Agenda Features:**
- Multi-file aggregation
- Custom commands and filters
- Bulk operations on entries
- Time grid display
- Habit tracking integration
- Export to various formats

### 2. Table System (org-table.el, 6,438 lines)

Org tables are a full spreadsheet system embedded in Org mode.

```elisp
;; From org-table.el (lines 1-34)
;;; org-table.el --- The Table Editor for Org        -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains the table editor and spreadsheet for Org mode.

;; Watch out:  Here we are talking about two different kind of tables.
;; Most of the code is for the tables created with the Org mode table editor.
;; Sometimes, we talk about tables created and edited with the table.el
;; Emacs package.  We call the former org-type tables, and the latter
;; table.el-type tables.

(defcustom org-table-default-size "5x2"
  "The default size for newly created tables, Columns x Rows."
  :group 'org-table-settings
  :type 'string)

(defcustom org-table-number-regexp
  "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*\\|...)$"
  "Regular expression for recognizing numbers in table columns.
If a table column contains mostly numbers, it will be aligned to the
right.  If not, it will be aligned to the left."
  :group 'org-table-settings
  :type 'regexp)
```

**Table Features:**

1. **Automatic formatting** - Columns auto-align on TAB
2. **Spreadsheet formulas** - Calc integration for cell calculations
3. **Column formulas** - Apply to entire columns
4. **Field references** - `@row$col` notation
5. **Named fields** - Use `$name` references
6. **Table ranges** - `@2$3..@5$7` range notation
7. **Remote references** - Reference other tables
8. **Plotting** - Integration with gnuplot
9. **Radio tables** - Embed in other modes

**Table Example:**

```org
| Name    | Hours | Rate | Total |
|---------+-------+------+-------|
| Alice   |    40 |   50 |  2000 |
| Bob     |    35 |   60 |  2100 |
|---------+-------+------+-------|
| Totals  |    75 |      |  4100 |
#+TBLFM: $4=$2*$3::@5$2=vsum(@2..@3)::@5$4=vsum(@2..@3)
```

### 3. Link System (ol.el, 2,311 lines)

Org's extensible link system supports internal and external links.

```elisp
;; From ol.el (lines 1-150)
;;; ol.el --- Org links library                      -*- lexical-binding: t; -*-

;;; Commentary:

;; This library provides tooling to handle both external and internal
;; links.

(defcustom org-link-parameters nil
  "Alist of properties that defines all the links in Org mode.

The key in each association is a string of the link type.
Subsequent optional elements make up a property list for that
type.

All properties are optional.  However, the most important ones
are, in this order, `:follow', `:export', and `:store'.

`:follow'
  Function used to follow the link, when the `org-open-at-point'
  command runs on it.

`:export'
  Function that accepts four arguments:
  - the path, as a string,
  - the description as a string, or nil,
  - the export backend,
  - the export communication channel, as a plist.

`:store'
  Function responsible for storing the link."
  :group 'org-link
  :type 'alist)
```

**Link Types:**

1. **Internal links** - `[[*Headline]]`, `[[#custom-id]]`
2. **File links** - `[[file:path/to/file.org]]`
3. **URL links** - `[[https://example.com][Description]]`
4. **Email links** - `[[mailto:user@example.com]]`
5. **ID links** - `[[id:UUID]]` (persistent across file moves)
6. **Code references** - `[[elisp:(function)]]`
7. **Custom link types** - Extensible via `org-link-parameters`

**Additional Link Modules:**
- `ol-docview.el` - DocView integration
- `ol-gnus.el` - Gnus email links
- `ol-man.el` - Man page links
- `ol-w3m.el` - w3m browser links

### 4. Capture System (org-capture.el, 2,024 lines)

Quick capture of notes and tasks from anywhere in Emacs.

**Capture Templates:**

```elisp
(setq org-capture-templates
  '(("t" "Todo" entry (file+headline "tasks.org" "Tasks")
     "* TODO %?\n  %i\n  %a")
    ("n" "Note" entry (file+datetree "notes.org")
     "* %?\nEntered on %U\n  %i\n  %a")
    ("m" "Meeting" entry (file+headline "meetings.org" "Meetings")
     "* MEETING with %? :meeting:\n  %U")))
```

**Template Expansion:**
- `%?` - Cursor position after expansion
- `%i` - Initial content (from selection)
- `%a` - Link to current location
- `%U` - Inactive timestamp
- `%t` - Active timestamp
- `%^{prompt}` - Interactive prompt

### 5. TODO and Scheduling

**TODO States:**

```elisp
(setq org-todo-keywords
  '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
    (sequence "WAITING(w)" "|" "CANCELLED(c)")))
```

**Scheduling Keywords:**
- `SCHEDULED:` - When you plan to work on item
- `DEADLINE:` - When item must be completed
- `CLOSED:` - When item was marked DONE

**Timestamps:**
- `<2025-01-15 Wed>` - Active timestamp (shows in agenda)
- `[2025-01-15 Wed]` - Inactive timestamp (doesn't show)
- `<2025-01-15 Wed 10:00-11:00>` - With time range
- `<2025-01-15 Wed +1w>` - Repeating task

### 6. Tags and Properties

**Tags:**

```org
* Headline :tag1:tag2:tag3:
* Project Alpha :work:important:
```

**Properties:**

```org
* Task
  :PROPERTIES:
  :CUSTOM_ID: unique-id
  :CREATED:  [2025-01-15 Wed]
  :EFFORT:   2:00
  :END:
```

### 7. Folding System

The folding system has been modernized with `org-fold.el` and `org-fold-core.el`:

- Text-properties based (not overlays for performance)
- Maintains fold state through edits
- Integration with isearch
- Preserves folds on save/load

## Integration with Emacs Systems

### 1. Calendar and Diary Integration

- Org timestamps integrate with Emacs calendar
- Can show Org agenda items in diary
- Export to iCalendar format

### 2. Narrowing and Indirect Buffers

- `org-narrow-to-subtree` - Focus on single subtree
- `org-tree-to-indirect-buffer` - Work on subtree in separate buffer

### 3. Refile System

- Move entries between files and headlines
- Completion on all headlines across agenda files
- Preserve or update metadata

### 4. Archive System

- Archive completed tasks
- Archive to separate file or subtree
- Archive with date tree

### 5. Clock System (org-clock.el, 3,336 lines)

- Clock in/out on tasks
- Time tracking reports
- Effort estimates
- Clock tables (dynamic blocks)

## Module Dependencies

```
org.el (core)
├── org-macs.el (macros and utilities)
├── org-compat.el (compatibility)
├── org-element.el (parser)
│   └── org-element-ast.el (AST utilities)
├── org-fold.el (folding)
│   └── org-fold-core.el (folding primitives)
├── org-cycle.el (visibility cycling)
├── org-keys.el (key bindings)
├── ol.el (links)
│   ├── ol-docview.el
│   ├── ol-gnus.el
│   ├── ol-man.el
│   └── ol-w3m.el
├── oc.el (citations)
├── org-table.el (tables and spreadsheet)
├── org-list.el (lists)
├── org-agenda.el (agenda views)
│   └── org-agenda-property.el
├── org-capture.el (quick capture)
├── org-refile.el (refiling)
├── org-archive.el (archiving)
├── org-clock.el (time tracking)
├── org-timer.el (timers)
├── org-id.el (unique IDs)
├── org-attach.el (attachments)
├── org-src.el (source code editing)
├── org-colview.el (column view)
├── org-duration.el (duration parsing)
├── org-macro.el (macro expansion)
├── org-indent.el (indentation)
├── org-plot.el (plotting)
├── org-num.el (heading numbers)
├── org-tempo.el (template expansion)
├── Babel subsystem
│   ├── ob-core.el (Babel core)
│   ├── ob-eval.el (evaluation)
│   ├── ob-exp.el (export)
│   ├── ob-tangle.el (tangling)
│   ├── ob-lob.el (library of babel)
│   ├── ob-ref.el (references)
│   ├── ob-comint.el (comint sessions)
│   ├── ob-table.el (table results)
│   └── ob-*.el (48 language files)
└── Export subsystem
    ├── ox.el (export core)
    └── ox-*.el (12 export backends)
```

## Performance Considerations

### 1. Element Cache

The element parser maintains a sophisticated cache to avoid re-parsing unchanged portions of large buffers. The cache uses:

- AVL trees for efficient lookups
- Invalidation on buffer changes
- Persistence across sessions

### 2. Lazy Loading

Many Org subsystems are autoloaded:
- Export backends loaded on demand
- Babel languages loaded when first used
- Agenda only loads when invoked

### 3. Deferred Parsing

The parser can defer parsing of certain elements until needed, improving initial buffer load time.

## Configuration Points

### 1. Startup Options

```elisp
(setq org-startup-folded t)              ; Start with all folded
(setq org-startup-indented t)            ; Indented view
(setq org-startup-with-inline-images t)  ; Show images
(setq org-hide-emphasis-markers t)       ; Hide */= markers
```

### 2. Babel Configuration

```elisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (shell . t)
   (R . t)))

(setq org-confirm-babel-evaluate nil)  ; Disable confirmation
```

### 3. Export Configuration

```elisp
(setq org-export-with-toc t)            ; Include table of contents
(setq org-export-with-section-numbers t) ; Number sections
(setq org-html-head-include-default-style nil) ; Custom styles
```

### 4. Agenda Configuration

```elisp
(setq org-agenda-files '("~/org/"))     ; Where to look for files
(setq org-agenda-span 7)                 ; Show week view
(setq org-agenda-start-on-weekday 1)     ; Start on Monday
```

## Key Innovations

### 1. Plain Text Format

Org uses a simple, readable plain text format that's human-editable without Emacs, making it future-proof and tool-independent.

### 2. Parse Tree Architecture

The `org-element.el` parser provides a clean separation between syntax and semantics, enabling:
- Robust export to multiple formats
- Consistent behavior across features
- Easy addition of new export backends

### 3. Extensible Link System

The link system is fully extensible, allowing new link types to be added with custom following and export behavior.

### 4. Babel's Language-Agnostic Design

Babel's architecture allows easy addition of new languages through a simple interface contract, without modifying core code.

### 5. Backend Transcoder Pattern

The export system's transcoder pattern cleanly separates the export process from backend-specific rendering.

## Documentation and Resources

- **Org Manual:** Comprehensive documentation built with Texinfo
- **Worg:** Community wiki at <https://orgmode.org/worg/>
- **Syntax specification:** <https://orgmode.org/worg/dev/org-syntax.html>
- **Export reference:** <https://orgmode.org/worg/dev/org-export-reference.html>

## Historical Context

Org mode was created by Carsten Dominik in 2003 as a personal organization system. It has grown into one of Emacs's most powerful and widely-used subsystems, influencing the development of similar tools in other editors.

The codebase demonstrates excellent software engineering:
- Clean module boundaries
- Consistent naming conventions
- Comprehensive documentation strings
- Extensive customization options
- Backward compatibility maintenance

## Conclusion

Org mode exemplifies literate programming as both a tool and a philosophy. Its architecture demonstrates how to build a large, complex system through:

1. **Layered abstraction** - Parser, core, features, UI
2. **Pluggable components** - Babel languages, export backends
3. **Extensibility** - Hooks, customization, link types
4. **Integration** - Works with Emacs calendar, diary, and other systems
5. **Performance** - Caching, lazy loading, deferred parsing

The system handles 146,533 lines of code across 127 files while remaining maintainable, extensible, and performant. It serves as an excellent example of how to architect a major subsystem within Emacs.
