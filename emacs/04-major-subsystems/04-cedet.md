# CEDET: Collection of Emacs Development Environment Tools

**Location:** `/lisp/cedet/`
**Files:** 143 files, 70,084 lines of code
**Author:** Eric M. Ludlam (primary)
**Version:** 2.0 (integrated into Emacs core)

## Executive Summary

CEDET is a comprehensive development environment toolkit that provides infrastructure for parsing, analyzing, and manipulating source code. It consists of three major components: **Semantic** (parser framework), **EDE** (project management), and **SRecode** (code generation). While historically important, CEDET has been largely superseded by modern Language Server Protocol (LSP) implementations via Eglot for many use cases, though it remains valuable for languages without LSP servers and for understanding Emacs's parser infrastructure.

## 1. Major Components Overview

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                         CEDET 2.0                            │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │  SEMANTIC   │  │     EDE     │  │   SRECODE   │         │
│  │   (Parser)  │  │  (Projects) │  │ (Templates) │         │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘         │
│         │                │                │                  │
│  ┌──────▼────────────────▼────────────────▼──────┐          │
│  │         MODE-LOCAL (infrastructure)            │          │
│  └────────────────────────────────────────────────┘          │
│                                                               │
└───────────────────────────────────────────────────────────────┘
```

### Component Breakdown

| Component | Files | Purpose | Key Features |
|-----------|-------|---------|--------------|
| **Semantic** | 74 | Code parsing & analysis | Parser generators (Bovine/Wisent), tag database, smart completion |
| **EDE** | 36 | Project management | Build system integration, multi-language support, compilation |
| **SRecode** | 23 | Code generation | Template system, context-aware insertion |
| **Common** | 10 | Shared infrastructure | mode-local, data-debug, utilities |

## 2. Semantic: Parser Framework and Code Analysis

### 2.1 The Tag System: Heart of Semantic

Semantic represents all parsed code as **tags** - structured data about code symbols. A tag is a 5-element list:

```elisp
;; From semantic/tag.el (lines 69-91)
;; Tag Structure: (NAME CLASS ATTRIBUTES PROPERTIES OVERLAY)
;;
;; Where:
;;   - NAME: string representing the tag name
;;   - CLASS: symbol like 'type, 'function, 'variable
;;   - ATTRIBUTES: public plist of language-specific data
;;   - PROPERTIES: private plist for internal use
;;   - OVERLAY: location data (overlay or [START END] vector)

(defsubst semantic-tag-name (tag)
  "Return the name of TAG."
  (car tag))

(defsubst semantic-tag-class (tag)
  "Return the class of TAG (e.g., 'function, 'variable, 'type)."
  (nth 1 tag))

(defsubst semantic-tag-attributes (tag)
  "Return the list of public attributes of TAG."
  (nth 2 tag))

(defsubst semantic-tag-properties (tag)
  "Return the list of private properties of TAG."
  (nth 3 tag))

(defsubst semantic-tag-overlay (tag)
  "Return the OVERLAY part of TAG."
  (nth 4 tag))
```

**Example Tag Creation:**

```elisp
;; Creating a tag for: int add(int a, int b);
(semantic-tag
 "add"                          ; name
 'function                       ; class
 '(:arguments (("a" variable "int")
               ("b" variable "int"))
   :type "int")                 ; attributes
 nil                            ; properties (internal)
 (vector 100 150))              ; position [start end]
```

### 2.2 Parser Infrastructure: Bovine vs. Wisent

Semantic supports two parser generator approaches:

#### Bovine Parser (LL - Left-to-right, Leftmost)

```elisp
;; From semantic/bovine.el (lines 1-34)
;; Semantic 1.x uses an LL parser named the "bovinator". This parser
;; had several conveniences which made parsing tags out of languages
;; with list characters easy. This parser lives on as one of many
;; available parsers for semantic the tool.
;;
;; Use when the language is simple, such as makefiles or other
;; data-declarative languages.

(defun semantic-bovinate-stream (stream &optional nonterminal)
  "Bovinate STREAM, starting at the first NONTERMINAL rule.
Use `bovine-toplevel' if NONTERMINAL is not provided.
This is the core routine for converting a stream into a table.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used."
  ;; Core parsing loop...
  )
```

**Best for:** Makefiles, simple declarative languages, configuration files

#### Wisent Parser (LALR - Look-Ahead LR)

```elisp
;; From semantic/wisent.el (lines 1-32)
;; Here are functions necessary to use the Wisent LALR parser from
;; Semantic environment.

(defvar wisent-lex-istream nil
  "Input stream of `semantic-lex' syntactic tokens.")

(define-wisent-lexer wisent-lex
  "Return the next available lexical token in Wisent's form.
The variable `wisent-lex-istream' contains the list of lexical tokens
produced by `semantic-lex'. Pop the next token available and convert
it to a form suitable for the Wisent's parser."
  (let* ((tk (car wisent-lex-istream)))
    (setq wisent-lex-istream (cdr wisent-lex-istream))
    (cons (semantic-lex-token-class tk)
          (cons (semantic-lex-token-text tk)
                (semantic-lex-token-bounds tk)))))
```

**Best for:** Complex languages with context-dependent grammars (C++, Java, Python)

### 2.3 Language Parsers

Semantic includes parsers for multiple languages:

**Bovine-based parsers:**
- C (`semantic/bovine/c.el`)
- Emacs Lisp (`semantic/bovine/el.el`)
- Make (`semantic/bovine/make.el`)
- Scheme (`semantic/bovine/scm.el`)

**Wisent-based parsers:**
- Java (`semantic/wisent/java-tags.el`)
- JavaScript (`semantic/wisent/javascript.el`)
- Python (`semantic/wisent/python.el`)

**Parser Setup Example:**

```elisp
;; From semantic.el (lines 234-257)
(defcustom semantic-new-buffer-setup-functions
  '((c-mode . semantic-default-c-setup)
    (c++-mode . semantic-default-c-setup)
    (html-mode . semantic-default-html-setup)
    (java-mode . wisent-java-default-setup)
    (js-mode . wisent-javascript-setup-parser)
    (python-mode . wisent-python-default-setup)
    (scheme-mode . semantic-default-scheme-setup)
    (srecode-template-mode . srecode-template-setup-parser)
    (texinfo-mode . semantic-default-texi-setup)
    (makefile-automake-mode . semantic-default-make-setup)
    (makefile-gmake-mode . semantic-default-make-setup)
    ;; ... more modes
    )
  "Alist of functions to call to set up Semantic parsing in the buffer.")
```

### 2.4 The Semantic Database (SemanticDB)

SemanticDB caches parsed tags to disk for fast access across sessions:

```elisp
;; From semantic/db.el (lines 1-112)
;; Maintain a database of tags for a group of files and enable
;; queries into the database.
;;
;; By default, assume one database per directory.

(defclass semanticdb-abstract-table ()
  ((parent-db :documentation "Database Object containing this table.")
   (major-mode :initarg :major-mode
               :documentation "Major mode this table belongs to.")
   (tags :initarg :tags
         :accessor semanticdb-get-tags
         :documentation "The tags belonging to this table.")
   (db-refs :initform nil
            :documentation "List of `semanticdb-table' objects referring to this one.")
   (index :type semanticdb-abstract-search-index
          :documentation "The search index for fast lookups.")
   (cache :type list
          :documentation "List of cache information for tools."))
  "A simple table for semantic tags.")
```

**Database Features:**
1. **Persistent storage**: Tags saved to `~/.emacs.d/semanticdb/`
2. **Cross-file references**: Track dependencies between files
3. **Fast symbol lookup**: Indexed search across entire codebase
4. **Lazy loading**: Load tag data only when needed

### 2.5 Code Analysis and Completion

```elisp
;; From semantic/analyze.el (lines 1-124)
;; Semantic, as a tool, provides a nice list of searchable tags.
;; That information can provide some very accurate answers if the current
;; context of a position is known.

(defclass semantic-analyze-context ()
  ((bounds :initarg :bounds
           :documentation "The bounds of this context.")
   (prefix :initarg :prefix
           :documentation "List of tags defining local text.
This can be nil, or a list where the last element can be a string
representing text that may be incomplete.")
   (prefixclass :initarg :prefixclass
                :documentation "Tag classes expected at this context.")
   (prefixtypes :initarg :prefixtypes
                :documentation "List of tags defining types for :prefix.")
   (scope :initarg :scope
          :type semantic-scope-cache
          :documentation "List of tags available in scopetype.")
   (buffer :initarg :buffer
           :type buffer)
   (errors :initarg :errors))
  "Base analysis data for any context.")
```

**Analysis Types:**

1. **Context Analysis** (`semantic-analyze-context`):
   - Determines what's valid at point
   - Type inference
   - Scope resolution

2. **Completion Analysis** (`semantic-analyze-completion`):
   - Smart completion based on context
   - Type-aware suggestions
   - Local variable tracking

3. **Reference Analysis** (`semantic-symref`):
   - Find symbol references
   - Call hierarchy
   - Cross-file navigation

## 3. EDE: Emacs Development Environment (Project Management)

### 3.1 Project Architecture

EDE provides object-oriented project management:

```elisp
;; From ede.el (lines 1-82)
;; EDE is the top level Lisp interface to a project management scheme
;; for Emacs. Emacs does many things well, including editing,
;; building, and debugging. Folks migrating from other IDEs don't
;; seem to think this qualifies, however, because they still have to
;; write the makefiles, and specify parameters to programs.
;;
;; This EDE mode will attempt to link these diverse programs together
;; into a comprehensive single interface, instead of a bunch of
;; different ones.

(defvar-local ede-object-root-project nil
  "The current buffer's current root project.")

(defvar-local ede-object-project nil
  "The current buffer's current project at that level.")

(defvar-local ede-object nil
  "The current buffer's target object.")
```

**Project Hierarchy:**

```
Project Root (ede-project)
  ├── Subproject 1
  │     ├── Target A (ede-target)
  │     │     ├── file1.c
  │     │     └── file2.c
  │     └── Target B
  │           └── file3.c
  └── Subproject 2
        └── Target C
              ├── file4.c
              └── file5.c
```

### 3.2 Project Types

EDE supports multiple project types through autodetection:

| Project Type | File Marker | Use Case |
|-------------|-------------|----------|
| **ede-proj** | `Project.ede` | EDE native projects with Makefile generation |
| **ede-cpp-root** | `.git`, `.svn` | C++ projects with existing build system |
| **ede-linux** | `Kconfig`, `Makefile` | Linux kernel source tree |
| **ede-maven** | `pom.xml` | Java Maven projects |
| **ede-emacs** | `configure.ac` | Emacs itself (special handling) |
| **ede-simple** | Auto-detect | Generic projects without specific structure |

**Project Class Hierarchy:**

```elisp
;; From ede/proj.el (lines 89-195)
(defclass ede-proj-target (ede-target)
  ((auxsource :initarg :auxsource
              :type list
              :documentation "Auxiliary source files.")
   (dirty :initform nil
          :type boolean)
   (compiler :initarg :compiler
             :type (or null symbol))
   (linker :initarg :linker
           :type (or null symbol)))
  "Abstract class for ede-proj targets.")

(defclass ede-proj-target-makefile (ede-proj-target)
  ((makefile :initarg :makefile
             :initform "Makefile"
             :type string)
   (partofall :initarg :partofall
              :initform t
              :type boolean)
   (configuration-variables :initarg :configuration-variables
                            :type list)
   (rules :initarg :rules
          :type (list-of ede-makefile-rule)))
  "Abstract class for Makefile based targets.")
```

### 3.3 Build System Integration

```elisp
;; Compilation commands from ede.el (lines 932-967)
(defun ede-compile-project ()
  "Compile the current project."
  (interactive)
  (let ((cp (ede-current-project)))
    (while (ede-parent-project cp)
      (setq cp (ede-parent-project cp)))
    (let ((ede-object cp))
      (ede-invoke-method 'project-compile-project))))

(defun ede-compile-target ()
  "Compile the current buffer's associated target."
  (interactive)
  (ede-invoke-method 'project-compile-target))

(defun ede-debug-target ()
  "Debug the current buffer's associated target."
  (interactive)
  (ede-invoke-method 'project-debug-target))

(defun ede-run-target ()
  "Run the current buffer's associated target."
  (interactive)
  (ede-invoke-method 'project-run-target'))
```

**Key Bindings:**

- `C-c . C` - Compile project
- `C-c . c` - Compile current target
- `C-c . D` - Debug target
- `C-c . R` - Run target
- `C-c . t` - Create new target
- `C-c . a` - Add file to target

### 3.4 Project Configuration

```elisp
;; Example Project.ede file
(ede-proj-project "MyProject"
  :name "MyProject"
  :file "Project.ede"
  :targets (list
    (ede-proj-target-makefile-program "main"
      :name "main"
      :path ""
      :source '("main.c" "utils.c")
      :compiler 'cc-compiler
      :linker 'ld-linker)
    (ede-proj-target-makefile-shared-object "libmylib"
      :name "libmylib"
      :path "lib"
      :source '("mylib.c"))))
```

## 4. SRecode: Semantic Recoder (Template System)

### 4.1 Template Language

SRecode provides a powerful template system for code generation:

```elisp
;; From srecode/template.el (lines 1-69)
;; Semantic does the job of converting source code into useful tag
;; information. The set of `semantic-format-tag' functions has one
;; function that will create a prototype of a tag, which has severe
;; issues of complexity (in the format tag file itself) and inaccuracy
;; (for the purpose of C++ code.)
;;
;; Contemplation of the simplistic problem within the scope of
;; semantic showed that the solution was more complex than could
;; possibly be handled in semantic/format.el. Semantic Recoder, or
;; srecode is a rich API for generating code out of semantic tags, or
;; recoding the tags.
```

### 4.2 Template Files

SRecode templates use `.srt` files with special syntax:

```
;; Example from /etc/srecode/c.srt

template function :blank
----
{{?TYPE}} {{NAME}}({{#ARGS}}{{TYPE}} {{NAME}}{{#NOTLAST}}, {{/NOTLAST}}{{/ARGS}})
{
  {{^}}
}
----

template class :blank
----
class {{NAME}} {
public:
  {{NAME}}();
  virtual ~{{NAME}}();

  {{^}}
};
----
```

**Template Directory Structure:**

```bash
$ ls -la /home/user/emacs/etc/srecode/
c.srt              # C templates
cpp.srt            # C++ templates
default.srt        # Language-agnostic
doc-cpp.srt        # C++ documentation
doc-default.srt    # Default documentation
doc-java.srt       # Java documentation
ede-autoconf.srt   # Autoconf templates
ede-make.srt       # Makefile templates
el.srt             # Emacs Lisp templates
getset-cpp.srt     # C++ getter/setter
java.srt           # Java templates
make.srt           # Make templates
template.srt       # Template meta-templates
texi.srt           # Texinfo templates
wisent.srt         # Wisent grammar templates
```

### 4.3 Template Variables and Context

```elisp
;; Template variables come from multiple sources:
;; 1. Current semantic tag (function, class, etc.)
;; 2. User input (prompts)
;; 3. Dictionary context (project, file, etc.)

;; Dictionary structure:
;; {{NAME}}        - Simple variable insertion
;; {{?NAME}}       - Optional (empty string if unset)
;; {{#NAME}}...{{/NAME}}  - Section (loop if list)
;; {{#NOTLAST}}...{{/NOTLAST}} - Conditional
;; {{^}}           - Cursor position after insertion
```

### 4.4 Template Maps

```elisp
;; From srecode/map.el (lines 1-96)
;; Read template files, and build a map of where they can be found.
;; Save the map to disk, and refer to it when bootstrapping a new
;; Emacs session with srecode.

(defclass srecode-map (eieio-persistent)
  ((fileheaderline :initform ";; SRECODE TEMPLATE MAP")
   (files :initarg :files
          :initform nil
          :type list
          :documentation "An alist of files and the major-mode that they cover.")
   (apps :initarg :apps
         :initform nil
         :type list
         :documentation "An alist of applications."))
  "A map of srecode templates.")

(cl-defmethod srecode-map-entries-for-mode ((map srecode-map) mode)
  "Return the entries in MAP for major MODE."
  (let ((ans nil))
    (dolist (f (oref map files))
      (when (provided-mode-derived-p mode (cdr f))
        (setq ans (cons f ans))))
    ans))
```

## 5. Integration and Architecture

### 5.1 Mode-Local Infrastructure

CEDET uses a sophisticated mode-local system for extensibility:

```elisp
;; From mode-local.el (lines 1-50)
;; Each major mode will want to support a specific set of behaviors.
;; Usually generic behaviors that need just a little bit of local
;; specifics.
;;
;; This library permits the setting of override functions for tasks of
;; that nature, and also provides reasonable defaults.
;;
;; There are buffer local variables (and there were frame local variables).
;; This library gives the illusion of mode specific variables.

(defun mode-local-map-mode-buffers (function modes)
  "Run FUNCTION on every file buffer with major mode in MODES."
  (setq modes (ensure-list modes))
  (mode-local-map-file-buffers
   function (lambda () (derived-mode-p modes))))

;; Allows mode-specific overrides:
;; - semantic-parse-region (parser implementation)
;; - semantic-tag-components (tag decomposition)
;; - ede-system-include-path (include paths)
;; - srecode-template-setup-parser (template parsing)
```

### 5.2 Component Interaction Flow

```
User Action (e.g., M-TAB for completion)
    │
    ▼
semantic-complete-analyze-inline
    │
    ├─► semantic-analyze-current-context
    │       ├─► semantic-fetch-tags (ensure buffer parsed)
    │       │       ├─► semantic-parse-region (mode-local)
    │       │       └─► semanticdb-cache
    │       ├─► semantic-scope-cache (determine scope)
    │       └─► semantic-analyze-scoped-tags
    │
    ├─► semantic-complete-inline-analyzer
    │       └─► semanticdb-find-tags-by-name-regexp
    │
    └─► Display completions to user
```

### 5.3 Idle Time Services

Semantic provides intelligent background processing:

```elisp
;; From semantic/idle.el (lines 1-150)
;; Originally, `semantic-auto-parse-mode' handled refreshing the
;; tags in a buffer in idle time. Other activities can be scheduled
;; in idle time, all of which require up-to-date tag tables.

(defcustom semantic-idle-scheduler-idle-time 1
  "Time in seconds of idle before scheduling events."
  :type 'number)

(defcustom semantic-idle-scheduler-work-idle-time 60
  "Time in seconds of idle before scheduling big work."
  :type 'number)

;; Idle services include:
;; - semantic-idle-scheduler-mode: Re-parse on idle
;; - semantic-idle-summary-mode: Show tag summary at point
;; - semantic-idle-completions-mode: Automatic completion popup
;; - semantic-idle-local-symbol-highlight-mode: Highlight references
```

**Idle Mode Services:**

1. **Fast Services** (1s idle):
   - Incremental parsing
   - Tag cache updates
   - Symbol highlighting

2. **Slow Services** (60s idle):
   - Database save
   - Cross-reference updates
   - Full buffer analysis

## 6. Modern Context: LSP vs. CEDET

### 6.1 The LSP Advantage

**Language Server Protocol (via Eglot) provides:**

```elisp
;; Modern LSP approach with Eglot
(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config
  ;; LSP provides:
  ;; - Company backend (completion)
  ;; - Xref backend (navigation)
  ;; - Flymake backend (diagnostics)
  ;; - Eldoc backend (documentation)
  ;; All with much better accuracy than CEDET
  )
```

**LSP Benefits over CEDET:**
- ✅ Language-specific expertise (maintained by language communities)
- ✅ Full compiler integration (accurate type information)
- ✅ IDE-quality features (refactoring, renaming, etc.)
- ✅ Cross-editor compatibility
- ✅ Active development and support

### 6.2 When CEDET Still Makes Sense

**Use CEDET when:**

1. **No LSP server available**: Some languages lack LSP implementations
2. **Offline development**: CEDET works without external processes
3. **Simple projects**: For quick scripts, CEDET's lighter weight may be preferable
4. **Educational purposes**: Understanding parser design and implementation
5. **Legacy codebases**: Existing CEDET configurations
6. **Resource-constrained environments**: CEDET uses less memory than LSP servers

### 6.3 Hybrid Approach

```elisp
;; Use LSP where available, CEDET as fallback
(defun my-setup-completion ()
  "Set up completion based on available tools."
  (cond
   ;; Prefer LSP if available
   ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (setq-local completion-at-point-functions
                (list (cape-capf-buster #'eglot-completion-at-point))))

   ;; Fall back to Semantic
   ((and (featurep 'semantic) (semantic-active-p))
    (setq-local completion-at-point-functions
                (list #'semantic-analyze-completion-at-point-function)))

   ;; Otherwise, use basic completion
   (t
    (setq-local completion-at-point-functions
                (list #'elisp-completion-at-point)))))
```

## 7. Historical Context and Evolution

### 7.1 CEDET History

**Timeline:**

- **1997-2000**: Original development by Eric Ludlam
- **2.0 (2009)**: Major rewrite, integration into Emacs
- **Emacs 23.2 (2010)**: First bundled version
- **Emacs 24+**: Incremental improvements
- **2016+**: LSP emerges as alternative
- **Present**: Maintained but not actively developed

### 7.2 Architectural Lessons

CEDET pioneered several concepts now standard in IDEs:

1. **Tag-based navigation**: Jump to definition, find references
2. **Incremental parsing**: Parse only changed regions
3. **Context-aware completion**: Type inference for suggestions
4. **Project awareness**: Multi-file understanding
5. **Extensible architecture**: Mode-local overrides

### 7.3 Why LSP Won

**Technical reasons:**

1. **Separation of concerns**: Language logic in dedicated servers
2. **Compiler integration**: Direct access to compiler internals
3. **Community distribution**: Language experts maintain servers
4. **Protocol standardization**: One protocol, many implementations
5. **Resource pooling**: One server serves multiple editors

**Example comparison:**

```elisp
;; CEDET approach: Emacs must understand the language
;; - Maintain grammar files (.by, .wy)
;; - Keep up with language evolution
;; - Limited to what parser can express

;; LSP approach: Delegate to language experts
;; - Language server knows language intimately
;; - Compiler-level accuracy
;; - Full language feature support
```

## 8. Code Examples and Recipes

### 8.1 Basic Semantic Usage

```elisp
;;; Enable Semantic mode
(semantic-mode 1)

;; Enable idle services
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)

;; Enable database for persistent tags
(global-semanticdb-minor-mode 1)

;; Enable decoration mode (adds visual indicators)
(global-semantic-decoration-mode 1)

;; Navigate tags
(global-set-key (kbd "C-c , j") 'semantic-complete-jump-local)
(global-set-key (kbd "C-c , J") 'semantic-complete-jump)
(global-set-key (kbd "C-c , n") 'senator-next-tag)
(global-set-key (kbd "C-c , p") 'senator-previous-tag)

;; Symbol reference searching
(global-set-key (kbd "C-c , g") 'semantic-symref-symbol)
```

### 8.2 EDE Project Setup

```elisp
;;; Enable EDE
(global-ede-mode 1)

;; Define a C++ project
(ede-cpp-root-project "MyProject"
  :name "My C++ Project"
  :file "~/projects/myproject/README"
  :include-path '("/include"
                  "/src/utils")
  :system-include-path '("/usr/include/boost"
                         "/usr/local/include")
  :spp-table '(("DEBUG" . "1")
               ("VERSION" . "\"1.0\"")))

;; Add custom compilation command
(defun my-project-compile ()
  "Compile my project."
  (interactive)
  (compile "make -C ~/projects/myproject"))
```

### 8.3 Creating Custom Templates

```
;; File: ~/.emacs.d/templates/my-templates.srt

template file-header
----
/* {{FILENAME}}
 *
 * Author: {{AUTHOR}}
 * Date: {{DATE}}
 *
 * {{PROJECT}}
 */
----

template cpp-class
----
class {{NAME}} {
public:
    {{NAME}}();
    virtual ~{{NAME}}();

    // Copy and assignment
    {{NAME}}(const {{NAME}}&) = delete;
    {{NAME}}& operator=(const {{NAME}}&) = delete;

private:
    {{^}}
};
----

template test-function
----
TEST({{TEST_SUITE}}, {{TEST_NAME}}) {
    // Arrange
    {{^}}

    // Act

    // Assert
}
----
```

### 8.4 Advanced Semantic Analysis

```elisp
;;; Query the semantic database
(defun my-find-callers (function-name)
  "Find all callers of FUNCTION-NAME."
  (interactive "sFunction name: ")
  (let* ((refs (semantic-symref-find-references-by-name
                function-name 'function))
         (matches (semantic-symref-result-get-tags refs)))
    (pop-to-buffer "*Function Callers*")
    (erase-buffer)
    (dolist (match matches)
      (insert (format "%s:%d: %s\n"
                      (semantic-tag-file-name match)
                      (semantic-tag-start match)
                      (semantic-tag-name match))))))

;;; Analyze context at point
(defun my-analyze-point ()
  "Show analysis of current point."
  (interactive)
  (let ((ctxt (semantic-analyze-current-context)))
    (if ctxt
        (progn
          (message "Prefix: %S" (oref ctxt prefix))
          (message "Scope: %S" (semantic-scope-find 'function))
          (message "Type constraint: %S"
                   (semantic-analyze-type-constraint ctxt)))
      (message "No context available"))))
```

## 9. Performance Considerations

### 9.1 Optimization Strategies

```elisp
;;; Limit Semantic to certain modes
(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)
        (emacs-lisp-mode . semantic-default-elisp-setup)))

;;; Set maximum buffer size for idle parsing
(setq semantic-idle-scheduler-max-buffer-size 100000) ; 100KB

;;; Reduce idle delay for faster response
(setq semantic-idle-scheduler-idle-time 0.5)

;;; Disable expensive features
(setq semantic-idle-scheduler-verbose-flag nil)
(global-semantic-highlight-edits-mode -1)
```

### 9.2 Database Management

```elisp
;;; Control database location
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))

;;; Periodic cleanup
(defun my-clean-old-semantic-caches ()
  "Remove semantic caches older than 30 days."
  (interactive)
  (let ((cutoff (- (float-time) (* 30 24 60 60))))
    (dolist (file (directory-files semanticdb-default-save-directory t "\\.semanticdb$"))
      (when (< (float-time (nth 5 (file-attributes file))) cutoff)
        (delete-file file)))))
```

## 10. Debugging and Troubleshooting

### 10.1 Common Issues

**Problem: Semantic not parsing buffer**

```elisp
;; Check if semantic is active
(semantic-active-p)  ; Should return t

;; Check parse state
(semantic-parse-tree-state)  ; Should return nil if up-to-date

;; Force reparse
(semantic-force-refresh)

;; Check for errors
semantic-parser-warnings
```

**Problem: Incomplete or wrong completions**

```elisp
;; Check if tags are being found
(semantic-fetch-tags)

;; Check database
(semanticdb-dump-current-table)

;; Verify include paths
(semantic-gcc-get-include-paths "c++")
```

### 10.2 Debug Tools

```elisp
;;; Enable verbose mode
(setq semantic-idle-scheduler-verbose-flag t)

;;; Use data-debug to inspect structures
(require 'data-debug)
(data-debug-new-buffer "*TAG DEBUG*")
(data-debug-insert-thing (semantic-current-tag) ">" "")

;;; Bovination output
(bovinate t)  ; Parse and show output

;;; Check what's in scope
(semantic-calculate-scope)
```

## 11. Comparison Matrix

### CEDET vs. LSP Feature Comparison

| Feature | CEDET/Semantic | LSP/Eglot | Winner |
|---------|---------------|-----------|--------|
| **Completion Accuracy** | Context-based, limited | Compiler-accurate | LSP ✓ |
| **Jump to Definition** | Tag-based | AST-precise | LSP ✓ |
| **Find References** | Text/tag search | Semantic search | LSP ✓ |
| **Refactoring** | Limited | Full IDE support | LSP ✓ |
| **Diagnostics** | Basic | Real-time compiler | LSP ✓ |
| **Memory Usage** | Lower | Higher | CEDET ✓ |
| **Startup Time** | Instant | Server startup delay | CEDET ✓ |
| **Offline Work** | Full support | Limited | CEDET ✓ |
| **Language Coverage** | ~15 languages | 100+ languages | LSP ✓ |
| **Maintenance** | Low | Active | LSP ✓ |
| **Emacs Integration** | Native | Via protocol | CEDET ✓ |
| **Learning Curve** | Steep | Moderate | LSP ✓ |

## 12. Migration Guide: CEDET to LSP

### 12.1 Equivalent Features

```elisp
;;; OLD: CEDET/Semantic approach
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-summary-mode 1)

;;; NEW: LSP/Eglot approach
(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format))
  :config
  ;; Eglot automatically provides:
  ;; - completion-at-point (M-TAB)
  ;; - xref-find-definitions (M-.)
  ;; - xref-find-references (M-?)
  ;; - eldoc-mode (automatic documentation)
  ;; - flymake-mode (diagnostics)
  )
```

### 12.2 Feature Mapping

| CEDET Function | LSP/Eglot Equivalent |
|---------------|---------------------|
| `semantic-complete-jump` | `xref-find-definitions` (M-.) |
| `semantic-symref-symbol` | `xref-find-references` (M-?) |
| `semantic-analyze-completion` | `completion-at-point` (M-TAB) |
| `semantic-idle-summary-mode` | `eldoc-mode` (built-in) |
| `semantic-decoration-mode` | `flymake-mode` (diagnostics) |
| `semantic-ia-show-doc` | `eglot-help-at-point` |
| `semantic-force-refresh` | `eglot-reconnect` |
| `ede-compile-target` | `compile` + project.el |

## 13. Future and Recommendations

### 13.1 Current Status (2025)

- **Maintenance mode**: Bugs fixed but no new features
- **Still functional**: Works for supported languages
- **Declining usage**: Most users migrated to LSP
- **Educational value**: Good for learning parser design

### 13.2 Recommendations

**For new projects:**
- ✅ Use LSP (Eglot) if language server available
- ✅ Use Tree-sitter for syntax highlighting
- ⚠️ Consider CEDET only for unsupported languages

**For existing CEDET users:**
- Evaluate migration to LSP for each language
- Keep CEDET for languages without LSP servers
- Gradually transition as LSP servers mature

**For Emacs Lisp development:**
- CEDET still relevant (no LSP server yet)
- Consider combination of CEDET + static analysis tools

### 13.3 Learning Resources

**Documentation:**
- CEDET Manual: `C-h i m CEDET RET`
- Semantic Manual: `C-h i m Semantic RET`
- EDE Manual: `C-h i m EDE RET`

**Key Files to Study:**
```
/lisp/cedet/semantic/tag.el      - Tag system fundamentals
/lisp/cedet/semantic/db.el       - Database architecture
/lisp/cedet/semantic/analyze.el  - Code analysis
/lisp/cedet/ede/proj.el          - Project structure
/lisp/cedet/mode-local.el        - Mode-local system
```

## Conclusion

CEDET represents a heroic effort to bring IDE-like features to Emacs through pure Elisp. While largely superseded by LSP for most languages, it remains architecturally interesting and historically important. Its tag-based approach, incremental parsing, and mode-local system influenced modern development tools.

For modern Emacs users, CEDET serves as:
1. **Fallback** for languages without LSP
2. **Educational resource** for understanding parsers
3. **Historical artifact** of Emacs development
4. **Proof of concept** that Emacs can be a full IDE

The future belongs to LSP, but CEDET's legacy lives on in the patterns and approaches it pioneered.
