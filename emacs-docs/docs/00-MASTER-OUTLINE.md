# Emacs Encyclopedic Guide: Master Outline

**Version:** 1.0.0
**Date:** 2025-11-18
**Status:** Architecture Design Document

---

## Table of Contents

- [Overview](#overview)
- [Documentation Philosophy](#documentation-philosophy)
- [Literate Programming Conventions](#literate-programming-conventions)
- [Cross-Reference Format](#cross-reference-format)
- [Chapter Organization](#chapter-organization)
- [Detailed Chapter Outlines](#detailed-chapter-outlines)
- [Dependencies and Reading Order](#dependencies-and-reading-order)
- [Scope Estimates](#scope-estimates)
- [Index Structure](#index-structure)
- [Compilation and Build](#compilation-and-build)

---

## Overview

This encyclopedic guide provides comprehensive documentation of the GNU Emacs source code, architecture, and implementation. It is designed for:

- **Emacs Core Developers**: Understanding the full architecture and implementation details
- **Extension Developers**: Learning how to write sophisticated extensions and packages
- **Computer Science Students**: Studying a mature, production-quality editor implementation
- **Programming Language Researchers**: Examining Lisp interpreter and runtime design
- **System Architects**: Understanding large-scale software system design

### Scope

- **Codebase Size**: ~152 C source files, ~1,576 Elisp files
- **Coverage**: Complete architectural documentation from bootstrapping to advanced features
- **Approach**: Literate programming with interwoven source code and explanatory text
- **Target Length**: Estimated 2,000-3,000 pages when compiled

### Key Features

1. **Comprehensive Coverage**: Every major subsystem documented in detail
2. **Literate Programming**: Source code excerpts with detailed explanations
3. **Cross-Referenced**: Extensive internal and external references
4. **Executable Examples**: All code examples are tested and runnable
5. **Historical Context**: Evolution of features and design decisions
6. **Performance Analysis**: Memory usage, optimization techniques, and profiling
7. **Platform Coverage**: Unix/Linux, macOS, Windows, and Android implementations

---

## Documentation Philosophy

### Principles

1. **Clarity Over Brevity**: Detailed explanations preferred over terse descriptions
2. **Code-First**: Show actual implementation, not pseudocode
3. **Progressive Disclosure**: Basic concepts before advanced topics
4. **Practical Examples**: Real-world use cases and working code
5. **Historical Awareness**: Why things are designed as they are
6. **Maintenance Focus**: How to modify and extend the codebase

### Audience Assumptions

- Proficiency in C programming
- Basic understanding of Lisp/Scheme
- Familiarity with operating system concepts
- Understanding of data structures and algorithms
- Experience with text editors (preferably Emacs)

### What This Guide Is Not

- Not a user manual (see GNU Emacs Manual)
- Not an Elisp tutorial (see Emacs Lisp Introduction)
- Not an API reference (see Elisp Reference Manual)
- Not a configuration guide (see community wikis)

---

## Literate Programming Conventions

This guide follows literate programming principles, interweaving source code with explanatory prose.

### Code Block Format

All code blocks follow this convention:

```language
// @file: path/to/source/file.c
// @lines: 123-145
// @version: Emacs 30.0.50
// @description: Brief description of what this code does

[actual source code here]
```

#### Languages Used

- `c` - C source code from `src/`
- `elisp` - Emacs Lisp code from `lisp/`
- `bash` - Build scripts and shell commands
- `makefile` - Makefile excerpts
- `assembly` - Platform-specific assembly code
- `gdb` - Debugger sessions and analysis

### Code Annotation Markers

Within code blocks, we use these annotation markers:

- `/* [1] */` - Numbered annotations explained below code block
- `/* @see: section-reference */` - Cross-reference to other sections
- `/* @perf: performance note */` - Performance-related comments
- `/* @history: historical note */` - Evolution and design decisions
- `/* @bug: known issue */` - Known bugs or limitations
- `/* @platform: specific platform */` - Platform-specific code

### Explanation Format

After each code block:

```markdown
**Annotation [1]**: Detailed explanation of marked code...

**Key Concepts**:
- Concept 1: Explanation
- Concept 2: Explanation

**Performance Considerations**: ...

**Historical Notes**: ...
```

### File Organization

Each chapter contains:

1. **README.md** - Chapter overview and learning objectives
2. **01-section-name.md** - Individual sections with literate code
3. **02-next-section.md** - Sequentially numbered sections
4. **exercises.md** - Exercises and exploration tasks (optional)
5. **references.md** - Section-specific bibliography

---

## Cross-Reference Format

### Internal Cross-References

Use the following formats for internal references:

```markdown
[@sec:buffer-management] - Reference to section
[@chap:display-engine] - Reference to chapter
[@fig:buffer-structure] - Reference to figure
[@tbl:opcode-table] - Reference to table
[@lst:eval-loop] - Reference to code listing
[@eq:gap-buffer] - Reference to equation/formula
```

### External Cross-References

```markdown
[@elisp-manual:sec:buffers] - Reference to Elisp manual
[@emacs-manual:sec:basic] - Reference to Emacs manual
[@src:buffer.c:1234] - Direct source line reference
[@src:buffer.c:make-buffer] - Reference to function in source
[@git:commit-hash] - Reference to Git commit
[@bug:12345] - Reference to bug tracker
```

### Function and Variable References

```markdown
`function-name` - Inline function reference (auto-indexed)
`variable-name` - Inline variable reference (auto-indexed)
`MACRO-NAME` - C macro reference
`struct buffer` - C structure reference
```

### Cross-Reference Index

All references are automatically collected into:
- Function index
- Variable index
- Macro index
- Type index
- Concept index

---

## Chapter Organization

### Chapter Template

Each chapter follows this structure:

```
XX-chapter-name/
├── README.md                 # Chapter overview
├── 01-introduction.md        # Chapter introduction
├── 02-core-concepts.md       # Fundamental concepts
├── 03-implementation.md      # Detailed implementation
├── 04-advanced-topics.md     # Advanced material
├── 05-case-studies.md        # Real-world examples
├── 06-exercises.md           # Practice problems (optional)
├── references.md             # Chapter bibliography
└── code-examples/            # Extracted code examples
    ├── example1.c
    ├── example2.el
    └── README.md
```

### Section Numbering

- **Chapter Level**: 00-22 (two digits)
- **Section Level**: 01-99 (two digits within chapter)
- **Subsection Level**: Used in headings (## ### ####)

Full reference example: `[@sec:03.04.2]` = Chapter 3, Section 4, Subsection 2

---

## Detailed Chapter Outlines

### Chapter 00: Introduction

**Directory**: `00-introduction/`
**Estimated Pages**: 40-60
**Prerequisites**: None
**Dependencies**: None

#### Sections

1. **What is Emacs?** (01-what-is-emacs.md)
   - Historical overview (1976-present)
   - TECO Emacs, Gosling Emacs, GNU Emacs
   - Design philosophy and goals
   - Key innovations and influence
   - Estimated: 8-10 pages

2. **Architecture Overview** (02-architecture-overview.md)
   - High-level system architecture
   - C core vs. Elisp extension layer
   - Major subsystems diagram
   - Data flow overview
   - Estimated: 10-15 pages

3. **Development Environment Setup** (03-development-setup.md)
   - Building from source
   - Development tools and workflows
   - Debugging techniques
   - Version control and patches
   - Estimated: 8-10 pages

4. **Navigating the Source Code** (04-navigating-source.md)
   - Directory structure
   - File naming conventions
   - Finding functions and variables
   - Using tags and grep
   - Estimated: 6-8 pages

5. **Reading This Guide** (05-reading-guide.md)
   - How to use this documentation
   - Reading paths for different audiences
   - Notation and conventions
   - Prerequisites and assumed knowledge
   - Estimated: 4-6 pages

6. **Contributing to Emacs** (06-contributing.md)
   - Development process
   - Coding standards
   - Submitting patches
   - Copyright assignment
   - Estimated: 6-8 pages

---

### Chapter 01: Architecture

**Directory**: `01-architecture/`
**Estimated Pages**: 80-100
**Prerequisites**: Chapter 00
**Dependencies**: None

#### Sections

1. **System Architecture** (01-system-architecture.md)
   - Overall system design
   - Two-tier architecture (C core + Elisp)
   - Component interaction
   - Initialization sequence
   - Estimated: 15-20 pages

2. **C Core Subsystems** (02-c-core-subsystems.md)
   - Memory management (alloc.c)
   - Lisp interpreter (eval.c, bytecode.c)
   - Buffer management (buffer.c)
   - Display engine (xdisp.c, dispnew.c)
   - Terminal abstraction
   - Estimated: 20-25 pages

3. **Elisp Runtime** (03-elisp-runtime.md)
   - Lisp object representation
   - Garbage collection
   - Symbol table
   - Function calling convention
   - Estimated: 15-20 pages

4. **Bootstrap Process** (04-bootstrap.md)
   - Early initialization (emacs.c:main)
   - Loading loadup.el
   - Temacs to Emacs
   - Dumping and undumping
   - Portable dumper
   - Estimated: 12-15 pages

5. **Module System** (05-module-system.md)
   - Dynamic modules
   - Module API
   - FFI and native compilation
   - Security considerations
   - Estimated: 10-12 pages

6. **Threading Model** (06-threading.md)
   - Cooperative threads
   - Thread safety
   - Asyncio and futures
   - Limitations and future work
   - Estimated: 8-10 pages

---

### Chapter 02: Core Subsystems

**Directory**: `02-core-subsystems/`
**Estimated Pages**: 120-150
**Prerequisites**: Chapter 01
**Dependencies**: Chapter 01

#### Sections

1. **Memory Management** (01-memory-management.md)
   - Object allocation strategies
   - Garbage collection algorithms
   - Mark and sweep
   - Generational GC considerations
   - Memory profiling
   - Estimated: 25-30 pages

2. **Data Structures** (02-data-structures.md)
   - Lisp objects (Lisp_Object type)
   - Cons cells and lists
   - Vectors and arrays
   - Hash tables
   - Char tables
   - Obarrays (symbol tables)
   - Estimated: 25-30 pages

3. **String Handling** (03-string-handling.md)
   - String representation
   - Multibyte vs. unibyte strings
   - String properties
   - Rope data structure considerations
   - Performance optimization
   - Estimated: 15-20 pages

4. **Regular Expressions** (04-regular-expressions.md)
   - Regex engine implementation
   - Syntax and semantics
   - Boyer-Moore optimization
   - Unicode support
   - Performance characteristics
   - Estimated: 20-25 pages

5. **Undo System** (05-undo-system.md)
   - Undo list structure
   - Redo implementation
   - Selective undo
   - Memory management
   - Estimated: 12-15 pages

6. **Syntax Tables** (06-syntax-tables.md)
   - Syntax table implementation
   - Parsing support
   - Syntax classes
   - Category tables
   - Estimated: 10-12 pages

7. **Abbrevs and Completion** (07-abbrevs-completion.md)
   - Abbreviation tables
   - Completion systems
   - Dynamic abbreviation
   - Hippie expand
   - Estimated: 12-15 pages

---

### Chapter 03: Elisp Runtime

**Directory**: `03-elisp-runtime/`
**Estimated Pages**: 150-180
**Prerequisites**: Chapters 01-02
**Dependencies**: Chapters 01-02

#### Sections

1. **Lisp Reader** (01-reader.md)
   - Read-eval-print loop
   - Lexer and parser
   - Reader macros
   - Print representation
   - Pretty printing
   - Estimated: 20-25 pages

2. **Evaluator** (02-evaluator.md)
   - eval.c implementation
   - Function application
   - Special forms
   - Lexical vs. dynamic scoping
   - Macro expansion
   - Estimated: 30-35 pages

3. **Byte Compiler** (03-byte-compiler.md)
   - Compilation process
   - Optimization passes
   - Lap (Lisp assembly program)
   - Opcode reference
   - Performance analysis
   - Estimated: 25-30 pages

4. **Native Compilation** (04-native-compilation.md)
   - libgccjit integration
   - Compilation pipeline
   - Performance characteristics
   - Trampolines and primitives
   - Async compilation
   - Estimated: 20-25 pages

5. **Function Calling** (05-function-calling.md)
   - Calling convention
   - Argument passing
   - Stack frames
   - apply and funcall
   - Tail call optimization (lack thereof)
   - Estimated: 15-18 pages

6. **Macro System** (06-macro-system.md)
   - defmacro implementation
   - Macro hygiene
   - Backquote mechanism
   - Common macro patterns
   - Debugging macros
   - Estimated: 18-22 pages

7. **Advice System** (07-advice-system.md)
   - Advice mechanism
   - nadvice.el implementation
   - Before/after/around advice
   - Use cases and patterns
   - Performance impact
   - Estimated: 12-15 pages

8. **Autoloading** (08-autoloading.md)
   - Autoload cookies
   - loaddefs.el generation
   - Lazy loading
   - Feature system
   - Estimated: 10-12 pages

---

### Chapter 04: Buffer Management

**Directory**: `04-buffer-management/`
**Estimated Pages**: 100-120
**Prerequisites**: Chapters 01-03
**Dependencies**: Chapters 01-02

#### Sections

1. **Buffer Architecture** (01-buffer-architecture.md)
   - struct buffer layout
   - Gap buffer implementation
   - Buffer-local variables
   - Buffer list management
   - Estimated: 20-25 pages

2. **Text Representation** (02-text-representation.md)
   - Character encoding
   - Multibyte vs. unibyte
   - Position vs. byte position
   - Markers and positions
   - Estimated: 18-22 pages

3. **Gap Buffer Operations** (03-gap-buffer-ops.md)
   - Insertion and deletion
   - Gap movement
   - Performance characteristics
   - Alternative data structures
   - Estimated: 15-20 pages

4. **Markers** (04-markers.md)
   - Marker implementation
   - Marker adjustment
   - Marker lists
   - Performance considerations
   - Estimated: 12-15 pages

5. **Narrowing and Restrictions** (05-narrowing.md)
   - Narrowing implementation
   - widen and narrow-to-region
   - Use cases
   - Multi-region restrictions
   - Estimated: 10-12 pages

6. **Buffer-Local Variables** (06-buffer-locals.md)
   - Implementation mechanism
   - make-local-variable
   - setq-default
   - Performance impact
   - Estimated: 12-15 pages

7. **Indirect Buffers** (07-indirect-buffers.md)
   - Implementation
   - Use cases (clones, narrowing)
   - Limitations
   - Performance
   - Estimated: 8-10 pages

8. **Saving and Reverting** (08-saving-reverting.md)
   - save-buffer implementation
   - revert-buffer
   - Auto-save
   - Backup files
   - Estimated: 12-15 pages

---

### Chapter 05: Display Engine

**Directory**: `05-display-engine/`
**Estimated Pages**: 180-220
**Prerequisites**: Chapters 01-04
**Dependencies**: Chapters 02, 04

#### Sections

1. **Display Architecture** (01-display-architecture.md)
   - xdisp.c overview (one of the largest files)
   - Display pipeline
   - Redisplay process
   - Terminal abstraction
   - Estimated: 25-30 pages

2. **Redisplay Algorithm** (02-redisplay-algorithm.md)
   - try_window variants
   - Optimistic redisplay
   - Window matrices
   - Glyph matrices
   - Performance optimization
   - Estimated: 30-35 pages

3. **Glyph Production** (03-glyph-production.md)
   - Text to glyph conversion
   - Font handling
   - Face merging
   - Bidirectional text
   - Estimated: 25-30 pages

4. **Face System** (04-face-system.md)
   - Face implementation
   - Face merging rules
   - Face caching
   - Custom faces
   - Face remapping
   - Estimated: 20-25 pages

5. **Font Rendering** (05-font-rendering.md)
   - Font backend abstraction
   - FreeType integration
   - HarfBuzz shaping
   - Font metrics
   - Font caching
   - Estimated: 25-30 pages

6. **Images** (06-images.md)
   - Image types and formats
   - Image cache
   - Image slices
   - ImageMagick/native support
   - SVG rendering
   - Estimated: 15-20 pages

7. **Display Properties** (07-display-properties.md)
   - Display specs
   - Overlay strings
   - Invisible text
   - Display margins
   - Estimated: 15-18 pages

8. **Bidirectional Text** (08-bidi.md)
   - UAX#9 implementation
   - Reordering engine
   - Performance considerations
   - RTL editing
   - Estimated: 20-25 pages

9. **Scrolling** (09-scrolling.md)
   - Scroll algorithms
   - Scroll bars
   - Pixel scrolling
   - Smooth scrolling
   - Estimated: 12-15 pages

---

### Chapter 06: Window and Frame System

**Directory**: `06-window-frame-system/`
**Estimated Pages**: 100-120
**Prerequisites**: Chapters 01, 05
**Dependencies**: Chapters 04, 05

#### Sections

1. **Window Management** (01-window-management.md)
   - struct window layout
   - Window tree structure
   - Window splitting
   - Window configuration
   - Estimated: 25-30 pages

2. **Frame Management** (02-frame-management.md)
   - struct frame layout
   - Multiple frames
   - Frame parameters
   - Frame switching
   - Estimated: 20-25 pages

3. **Window Display** (03-window-display.md)
   - Window-to-buffer mapping
   - Window point and start
   - Scroll margins
   - Window configurations
   - Estimated: 15-20 pages

4. **Window Selection** (04-window-selection.md)
   - select-window
   - with-selected-window
   - save-window-excursion
   - Window hooks
   - Estimated: 12-15 pages

5. **Display Actions** (05-display-actions.md)
   - display-buffer framework
   - Display actions and alists
   - Window rules
   - Dedicated windows
   - Estimated: 15-18 pages

6. **Tab System** (06-tabs.md)
   - Tab bar implementation
   - Tab line
   - Window configurations per tab
   - Estimated: 12-15 pages

7. **Terminal Abstraction** (07-terminal-abstraction.md)
   - Terminal types
   - Terminal methods
   - TTY vs. GUI
   - Multiple terminals
   - Estimated: 15-18 pages

---

### Chapter 07: Text Properties and Overlays

**Directory**: `07-text-properties/`
**Estimated Pages**: 80-100
**Prerequisites**: Chapters 02, 04
**Dependencies**: Chapters 02, 04

#### Sections

1. **Text Properties** (01-text-properties.md)
   - Implementation (intervals)
   - Property lists
   - add-text-properties
   - get-text-property
   - Performance characteristics
   - Estimated: 20-25 pages

2. **Overlays** (02-overlays.md)
   - Overlay implementation
   - Overlay lists
   - make-overlay
   - Overlay priorities
   - Performance issues
   - Estimated: 20-25 pages

3. **Intervals** (03-intervals.md)
   - Interval tree structure
   - Balancing and adjustment
   - Performance analysis
   - Memory overhead
   - Estimated: 15-20 pages

4. **Special Properties** (04-special-properties.md)
   - face property
   - display property
   - invisible property
   - intangible property
   - keymap property
   - Estimated: 15-20 pages

5. **Overlay vs. Text Properties** (05-overlay-vs-properties.md)
   - Performance comparison
   - Use case guidelines
   - Migration strategies
   - Future directions
   - Estimated: 10-12 pages

---

### Chapter 08: Major Modes

**Directory**: `08-major-modes/`
**Estimated Pages**: 100-120
**Prerequisites**: Chapters 03, 04
**Dependencies**: Chapters 03, 04, 07

#### Sections

1. **Major Mode System** (01-major-mode-system.md)
   - define-derived-mode
   - Mode hooks
   - Mode conventions
   - Mode inheritance
   - Estimated: 20-25 pages

2. **Text Mode** (02-text-mode.md)
   - text-mode implementation
   - Paragraph handling
   - Filling and wrapping
   - Estimated: 12-15 pages

3. **Programming Mode** (03-prog-mode.md)
   - prog-mode base class
   - Common programming features
   - Comment handling
   - Indentation framework
   - Estimated: 18-22 pages

4. **Font Lock** (04-font-lock.md)
   - Syntax highlighting engine
   - Font lock keywords
   - JIT lock
   - Performance optimization
   - Estimated: 25-30 pages

5. **Indentation** (05-indentation.md)
   - Indentation engine
   - SMIE (Simple Minded Indentation Engine)
   - Custom indentation functions
   - Estimated: 20-25 pages

6. **Tree-sitter Integration** (06-tree-sitter.md)
   - Tree-sitter API
   - Grammar usage
   - Font lock integration
   - Indentation with tree-sitter
   - Performance benefits
   - Estimated: 15-18 pages

7. **Case Study: Emacs Lisp Mode** (07-elisp-mode.md)
   - emacs-lisp-mode implementation
   - Completion
   - Documentation lookup
   - Debugging integration
   - Estimated: 12-15 pages

---

### Chapter 09: Minor Modes

**Directory**: `09-minor-modes/`
**Estimated Pages**: 60-80
**Prerequisites**: Chapter 08
**Dependencies**: Chapters 03, 08

#### Sections

1. **Minor Mode Framework** (01-minor-mode-framework.md)
   - define-minor-mode
   - Buffer-local vs. global
   - Mode variables and hooks
   - Estimated: 15-20 pages

2. **Core Minor Modes** (02-core-minor-modes.md)
   - auto-fill-mode
   - overwrite-mode
   - visual-line-mode
   - line-number-mode
   - Estimated: 15-20 pages

3. **Global Minor Modes** (03-global-minor-modes.md)
   - global-font-lock-mode
   - transient-mark-mode
   - electric-pair-mode
   - show-paren-mode
   - Estimated: 15-20 pages

4. **Mode Interaction** (04-mode-interaction.md)
   - Mode conflicts
   - Mode dependencies
   - Best practices
   - Estimated: 8-10 pages

5. **Case Study: hl-line-mode** (05-case-study-hl-line.md)
   - Implementation analysis
   - Overlay usage
   - Performance considerations
   - Estimated: 8-10 pages

---

### Chapter 10: Keybindings and Keymaps

**Directory**: `10-keybindings/`
**Estimated Pages**: 80-100
**Prerequisites**: Chapter 03
**Dependencies**: Chapters 03, 08, 09

#### Sections

1. **Keymap System** (01-keymap-system.md)
   - Keymap implementation
   - Keymap lookup
   - Sparse vs. full keymaps
   - Char tables for keymaps
   - Estimated: 20-25 pages

2. **Key Lookup** (02-key-lookup.md)
   - Key sequence parsing
   - Keymap search order
   - Prefix keys
   - Local vs. global maps
   - Estimated: 18-22 pages

3. **Keybinding Scopes** (03-keybinding-scopes.md)
   - Global keymap
   - Local keymap
   - Minor mode keymaps
   - Text property keymaps
   - Overlay keymaps
   - Estimated: 15-20 pages

4. **Special Keymaps** (04-special-keymaps.md)
   - function-key-map
   - key-translation-map
   - input-decode-map
   - Estimated: 12-15 pages

5. **Menu System** (05-menu-system.md)
   - Menu bar
   - Popup menus
   - Context menus
   - Easy menu
   - Estimated: 15-18 pages

6. **Transient** (06-transient.md)
   - Transient framework
   - Prefix commands
   - Suffix commands
   - Use cases (magit, etc.)
   - Estimated: 12-15 pages

---

### Chapter 11: Command Loop

**Directory**: `11-command-loop/`
**Estimated Pages**: 100-120
**Prerequisites**: Chapters 03, 10
**Dependencies**: Chapters 03, 04, 10

#### Sections

1. **Command Loop Architecture** (01-command-loop-arch.md)
   - Main event loop (keyboard.c)
   - Event reading
   - Command execution
   - Pre/post command hooks
   - Estimated: 25-30 pages

2. **Event Handling** (02-event-handling.md)
   - Event types
   - Event queue
   - Event processing
   - Timers and idle timers
   - Estimated: 20-25 pages

3. **Interactive Forms** (03-interactive.md)
   - interactive special form
   - Code characters
   - Interactive completion
   - call-interactively
   - Estimated: 15-20 pages

4. **Minibuffer** (04-minibuffer.md)
   - Minibuffer implementation
   - Recursive minibuffers
   - Completion system
   - History management
   - Estimated: 25-30 pages

5. **Completion** (05-completion.md)
   - Completion tables
   - Completion styles
   - Completion-at-point
   - Completion UI (icomplete, fido, etc.)
   - Estimated: 20-25 pages

6. **Keyboard Macros** (06-keyboard-macros.md)
   - Macro recording
   - Macro playback
   - Macro editing
   - Macro counters
   - Estimated: 12-15 pages

---

### Chapter 12: Process Management

**Directory**: `12-process-management/`
**Estimated Pages**: 80-100
**Prerequisites**: Chapters 03, 11
**Dependencies**: Chapters 03, 13

#### Sections

1. **Process System** (01-process-system.md)
   - struct process
   - Process creation
   - Process I/O
   - Process signals
   - Estimated: 25-30 pages

2. **Subprocess Management** (02-subprocess.md)
   - start-process
   - call-process
   - Process buffers
   - Process filters
   - Process sentinels
   - Estimated: 20-25 pages

3. **PTY and Pipe Handling** (03-pty-pipes.md)
   - Pseudo-terminals
   - Pipe communication
   - Terminal emulation
   - Estimated: 15-20 pages

4. **Asynchronous I/O** (04-async-io.md)
   - Non-blocking I/O
   - Select/poll/epoll
   - File notifications
   - Estimated: 12-15 pages

5. **Shell Integration** (05-shell.md)
   - shell-mode
   - comint-mode
   - eshell architecture
   - Estimated: 15-18 pages

---

### Chapter 13: Network and IPC

**Directory**: `13-network-io/`
**Estimated Pages**: 80-100
**Prerequisites**: Chapters 03, 12
**Dependencies**: Chapters 03, 12

#### Sections

1. **Network Processes** (01-network-processes.md)
   - Socket creation
   - TCP/UDP support
   - Network process I/O
   - SSL/TLS support
   - Estimated: 25-30 pages

2. **Server Sockets** (02-servers.md)
   - Server process creation
   - Connection handling
   - Multiple connections
   - Estimated: 15-20 pages

3. **Serial Ports** (03-serial.md)
   - Serial port access
   - Configuration
   - Use cases
   - Estimated: 10-12 pages

4. **D-Bus Integration** (04-dbus.md)
   - D-Bus API
   - Signal handling
   - Method calls
   - Desktop integration
   - Estimated: 15-18 pages

5. **IPC Mechanisms** (05-ipc.md)
   - Server sockets
   - emacsclient protocol
   - Authentication
   - Estimated: 12-15 pages

6. **JSONRPC** (06-jsonrpc.md)
   - jsonrpc.el implementation
   - LSP usage
   - Error handling
   - Estimated: 10-12 pages

---

### Chapter 14: File System

**Directory**: `14-file-system/`
**Estimated Pages**: 100-120
**Prerequisites**: Chapters 03, 04
**Dependencies**: Chapters 03, 04, 12

#### Sections

1. **File I/O** (01-file-io.md)
   - File reading and writing
   - Encoding detection
   - Line ending conversion
   - Locking mechanisms
   - Estimated: 25-30 pages

2. **File Name Handling** (02-filenames.md)
   - File name parsing
   - Directory handling
   - Relative vs. absolute paths
   - Platform differences
   - Estimated: 18-22 pages

3. **File Handlers** (03-file-handlers.md)
   - file-name-handler-alist
   - Remote files (TRAMP)
   - Archive files
   - Compressed files
   - Estimated: 20-25 pages

4. **Directory Operations** (04-directories.md)
   - Directory listing
   - Directory tracking
   - File attributes
   - Estimated: 15-18 pages

5. **Auto-save and Backup** (05-autosave-backup.md)
   - Auto-save implementation
   - Backup file creation
   - Numbered backups
   - Version control integration
   - Estimated: 15-18 pages

6. **File Notifications** (06-file-notifications.md)
   - File watch mechanisms
   - inotify/kqueue/FSEvents
   - auto-revert-mode
   - Estimated: 12-15 pages

---

### Chapter 15: Internationalization

**Directory**: `15-internationalization/`
**Estimated Pages**: 100-120
**Prerequisites**: Chapters 02, 04
**Dependencies**: Chapters 02, 04, 05

#### Sections

1. **Character Sets** (01-charsets.md)
   - Character representation
   - Unicode support
   - Character properties
   - Case tables
   - Estimated: 25-30 pages

2. **Coding Systems** (02-coding-systems.md)
   - Encoding/decoding
   - Auto-detection
   - coding-system-list
   - EOL conversion
   - Estimated: 25-30 pages

3. **Input Methods** (03-input-methods.md)
   - Input method framework
   - Quail system
   - Complex input (CJK)
   - Input method switching
   - Estimated: 20-25 pages

4. **Localization** (04-localization.md)
   - Message catalogs
   - Locale handling
   - Date/time formatting
   - Estimated: 12-15 pages

5. **Language Environments** (05-language-environments.md)
   - Language environment system
   - Per-language settings
   - Script systems
   - Estimated: 12-15 pages

6. **CCL (Code Conversion Language)** (06-ccl.md)
   - CCL implementation
   - CCL programs
   - Use cases
   - Estimated: 10-12 pages

---

### Chapter 16: Font Rendering

**Directory**: `16-font-rendering/`
**Estimated Pages**: 80-100
**Prerequisites**: Chapter 05
**Dependencies**: Chapters 05, 15

#### Sections

1. **Font Backend Architecture** (01-font-backend.md)
   - Font driver interface
   - Font objects
   - Font selection
   - Estimated: 20-25 pages

2. **Font Discovery** (02-font-discovery.md)
   - Font listing
   - Fontconfig integration
   - Font matching
   - Font caching
   - Estimated: 18-22 pages

3. **Font Rendering Backends** (03-rendering-backends.md)
   - FreeType
   - Xft
   - HarfBuzz
   - Core text (macOS)
   - DirectWrite (Windows)
   - Estimated: 25-30 pages

4. **Complex Text Layout** (04-complex-layout.md)
   - Shaping engines
   - Glyph composition
   - Ligatures
   - Emoji rendering
   - Estimated: 20-25 pages

5. **Font Performance** (05-font-performance.md)
   - Font caching strategies
   - Glyph caching
   - Performance profiling
   - Estimated: 10-12 pages

---

### Chapter 17: Package System

**Directory**: `17-package-system/`
**Estimated Pages**: 60-80
**Prerequisites**: Chapters 03, 14
**Dependencies**: Chapters 03, 13, 14

#### Sections

1. **Package Manager** (01-package-manager.md)
   - package.el implementation
   - Package archives
   - Package installation
   - Package activation
   - Estimated: 20-25 pages

2. **Package Format** (02-package-format.md)
   - Package metadata
   - Single file vs. multi-file
   - Package dependencies
   - Autoloads
   - Estimated: 15-18 pages

3. **Package Archives** (03-archives.md)
   - Archive structure
   - ELPA, MELPA
   - Archive priorities
   - Estimated: 12-15 pages

4. **use-package** (04-use-package.md)
   - use-package macro
   - Deferred loading
   - Keybinding integration
   - Best practices
   - Estimated: 15-18 pages

---

### Chapter 18: Build System

**Directory**: `18-build-system/`
**Estimated Pages**: 80-100
**Prerequisites**: Chapter 01
**Dependencies**: Chapter 01

#### Sections

1. **Autoconf Build** (01-autoconf.md)
   - configure.ac analysis
   - Feature detection
   - Platform configuration
   - Estimated: 20-25 pages

2. **Makefile System** (02-makefiles.md)
   - Top-level Makefile
   - Recursive make
   - Build targets
   - Estimated: 20-25 pages

3. **Compilation Process** (03-compilation.md)
   - C compilation
   - Elisp byte compilation
   - Native compilation
   - Optimization flags
   - Estimated: 18-22 pages

4. **Dumping** (04-dumping.md)
   - Unexec (traditional)
   - Portable dumper
   - Dump file format
   - Memory layout
   - Estimated: 20-25 pages

5. **Installation** (05-installation.md)
   - make install
   - File layout
   - Site-lisp
   - Estimated: 10-12 pages

---

### Chapter 19: Platform-Specific Code

**Directory**: `19-platform-specific/`
**Estimated Pages**: 120-150
**Prerequisites**: Chapters 01, 05, 06
**Dependencies**: Chapters 01, 05, 06

#### Sections

1. **X11 Implementation** (01-x11.md)
   - X11 terminal (xterm.c)
   - X11 resources
   - X11 events
   - X11 selections
   - Estimated: 30-35 pages

2. **GTK+ Integration** (02-gtk.md)
   - GTK+ widgets
   - GTK+ event loop
   - Native widgets
   - Estimated: 20-25 pages

3. **macOS/NextStep** (03-macos.md)
   - ns*.m implementation
   - Cocoa integration
   - macOS-specific features
   - Estimated: 25-30 pages

4. **Windows (w32)** (04-windows.md)
   - w32*.c implementation
   - Windows API usage
   - Windows-specific features
   - Estimated: 25-30 pages

5. **Android** (05-android.md)
   - Android port architecture
   - JNI integration
   - Android-specific features
   - Estimated: 20-25 pages

6. **TTY/Terminal** (06-tty.md)
   - Terminal abstraction
   - Termcap/terminfo
   - Terminal capabilities
   - Estimated: 15-18 pages

---

### Chapter 20: Testing and Debugging

**Directory**: `20-testing-debugging/`
**Estimated Pages**: 80-100
**Prerequisites**: Chapters 01-03
**Dependencies**: Chapters 01-03

#### Sections

1. **Testing Framework** (01-testing-framework.md)
   - ERT (Emacs Lisp Regression Testing)
   - Test organization
   - Test execution
   - Continuous integration
   - Estimated: 20-25 pages

2. **Debugging Tools** (02-debugging-tools.md)
   - Edebug
   - debug-on-error
   - Backtrace analysis
   - Estimated: 20-25 pages

3. **Profiling** (03-profiling.md)
   - CPU profiling
   - Memory profiling
   - Native profiler
   - Performance analysis
   - Estimated: 18-22 pages

4. **GDB Integration** (04-gdb.md)
   - Debugging C code
   - src/.gdbinit
   - Lisp object inspection
   - Backtrace reading
   - Estimated: 15-18 pages

5. **Tracing and Logging** (05-tracing.md)
   - trace.el
   - Custom tracing
   - Log analysis
   - Estimated: 10-12 pages

---

### Chapter 21: Advanced Topics

**Directory**: `21-advanced-topics/`
**Estimated Pages**: 120-150
**Prerequisites**: All previous chapters
**Dependencies**: Multiple chapters

#### Sections

1. **Extending Emacs in C** (01-c-extensions.md)
   - Adding primitives
   - DEFUN macro
   - Lisp object manipulation
   - Memory safety
   - Estimated: 25-30 pages

2. **Security Considerations** (02-security.md)
   - Sandboxing
   - Safe evaluation
   - File local variables
   - Remote code execution risks
   - Estimated: 18-22 pages

3. **Performance Optimization** (03-performance.md)
   - Common bottlenecks
   - Optimization strategies
   - Memory optimization
   - GC tuning
   - Estimated: 25-30 pages

4. **Concurrency** (04-concurrency.md)
   - Async programming patterns
   - Generators
   - Promises/futures
   - Thread usage
   - Estimated: 20-25 pages

5. **LSP Implementation** (05-lsp.md)
   - Language Server Protocol
   - eglot implementation
   - JSONRPC usage
   - Estimated: 20-25 pages

6. **Tree-sitter Deep Dive** (06-treesitter-deep.md)
   - Parser generation
   - Grammar integration
   - Query language
   - Performance characteristics
   - Estimated: 18-22 pages

---

### Chapter 22: Appendices

**Directory**: `22-appendices/`
**Estimated Pages**: 100-120
**Prerequisites**: None (reference material)
**Dependencies**: None

#### Sections

1. **Glossary** (01-glossary.md)
   - Technical terms
   - Emacs-specific terminology
   - Estimated: 15-20 pages

2. **Opcode Reference** (02-opcodes.md)
   - Complete byte-code opcode listing
   - Opcode semantics
   - Stack effects
   - Estimated: 20-25 pages

3. **C Function Index** (03-c-functions.md)
   - All C primitives (DEFUN)
   - Function signatures
   - Cross-references
   - Estimated: 25-30 pages

4. **Elisp Function Index** (04-elisp-functions.md)
   - Core Elisp functions
   - Cross-references
   - Estimated: 20-25 pages

5. **Key Structures** (05-key-structures.md)
   - struct buffer
   - struct window
   - struct frame
   - struct Lisp_Object
   - Complete field documentation
   - Estimated: 15-20 pages

6. **Build Options** (06-build-options.md)
   - Configure flags
   - Compile-time options
   - Feature flags
   - Estimated: 10-12 pages

7. **Resources** (07-resources.md)
   - Additional reading
   - External documentation
   - Community resources
   - Mailing lists
   - Estimated: 8-10 pages

---

## Dependencies and Reading Order

### Critical Path (Minimum Reading Order)

For readers who want the minimum necessary foundation:

1. **Chapter 00**: Introduction
2. **Chapter 01**: Architecture
3. **Chapter 03**: Elisp Runtime
4. **Chapter 04**: Buffer Management
5. **Chapter 11**: Command Loop

### Recommended Full Reading Order

For comprehensive understanding:

**Phase 1: Foundation** (Chapters 00-03)
- Start here, read sequentially
- Establishes core concepts
- ~370-440 pages

**Phase 2: Core Systems** (Chapters 04-07)
- Can be read in any order after Phase 1
- Focus on data management
- ~400-490 pages

**Phase 3: User Interface** (Chapters 08-11)
- Requires Phase 1 and Chapter 04
- Can interleave with Phase 2
- ~340-420 pages

**Phase 4: I/O and System** (Chapters 12-14)
- Requires Phase 1
- Can be read in any order
- ~260-320 pages

**Phase 5: Advanced Features** (Chapters 15-17)
- Requires appropriate earlier chapters
- Can be read selectively
- ~240-300 pages

**Phase 6: Implementation** (Chapters 18-20)
- Can be read early for build/debug info
- ~180-230 pages

**Phase 7: Advanced and Reference** (Chapters 21-22)
- Read after mastering earlier material
- Reference material as needed
- ~220-270 pages

### Dependency Graph

```
Chapter 00 (Introduction)
    ├─→ Chapter 01 (Architecture)
    │       ├─→ Chapter 02 (Core Subsystems)
    │       │       ├─→ Chapter 04 (Buffer Management)
    │       │       │       ├─→ Chapter 05 (Display Engine)
    │       │       │       │       ├─→ Chapter 06 (Window/Frame)
    │       │       │       │       │       └─→ Chapter 19 (Platform)
    │       │       │       │       └─→ Chapter 16 (Font Rendering)
    │       │       │       ├─→ Chapter 07 (Text Properties)
    │       │       │       │       └─→ Chapter 08 (Major Modes)
    │       │       │       │               └─→ Chapter 09 (Minor Modes)
    │       │       │       └─→ Chapter 14 (File System)
    │       │       ├─→ Chapter 15 (I18N)
    │       │       └─→ Chapter 20 (Testing/Debugging)
    │       ├─→ Chapter 03 (Elisp Runtime)
    │       │       ├─→ Chapter 10 (Keybindings)
    │       │       │       └─→ Chapter 11 (Command Loop)
    │       │       │               ├─→ Chapter 12 (Processes)
    │       │       │               │       └─→ Chapter 13 (Network)
    │       │       │               └─→ [Links to modes]
    │       │       ├─→ Chapter 17 (Package System)
    │       │       └─→ [Links to most chapters]
    │       └─→ Chapter 18 (Build System)
    └─→ Chapter 21 (Advanced Topics)
            └─→ Chapter 22 (Appendices)
```

### Topic-Based Reading Paths

**For Extension Developers:**
- Ch 00, 01, 03, 08, 09, 10, 11, 17
- Optional: 04, 07, 12, 14, 21

**For Core Developers:**
- Ch 00, 01, 02, 03, 04, 05, 18, 20
- Selective reading of specialized chapters

**For Display/UI Developers:**
- Ch 00, 01, 02, 04, 05, 06, 07, 15, 16, 19
- Focus on display pipeline

**For Mode Developers:**
- Ch 00, 01, 03, 04, 07, 08, 09, 10
- Optional: 05, 11

**For Performance Engineers:**
- Ch 01, 02, 03, 04, 05, 20, 21
- Focus on profiling sections

---

## Scope Estimates

### Total Documentation Scope

| Component | Estimated Pages |
|-----------|----------------|
| Main chapters (00-21) | 2,000-2,400 |
| Appendices (22) | 100-120 |
| Front matter | 20-30 |
| Bibliography | 30-40 |
| Indices | 100-150 |
| **Total** | **2,250-2,740** |

### Page Count by Category

| Category | Chapters | Pages |
|----------|----------|-------|
| Foundation | 00-03 | 370-440 |
| Core Systems | 04-07 | 400-490 |
| User Interface | 08-11 | 340-420 |
| I/O & System | 12-14 | 260-320 |
| Advanced Features | 15-17 | 240-300 |
| Build & Platform | 18-19 | 200-250 |
| Testing & Advanced | 20-21 | 200-250 |
| Reference | 22 | 100-120 |

### Lines of Code Coverage

Estimated source code excerpts (approximate):

- **C code**: ~15,000-20,000 lines across all chapters
- **Elisp code**: ~25,000-30,000 lines across all chapters
- **Shell/Make**: ~1,000-2,000 lines
- **Other**: ~1,000 lines

**Total code in guide**: ~42,000-53,000 lines

This represents approximately:
- 10-15% of C source (~150K lines)
- 5-10% of Elisp source (~300K lines)

Focus is on:
- Critical algorithms
- Core data structures
- Representative examples
- Educational value

---

## Index Structure

### Planned Indices

1. **Concept Index**
   - Technical concepts
   - Algorithms
   - Data structures
   - Design patterns

2. **Function Index**
   - C functions (DEFUN)
   - Elisp functions
   - Macros
   - Special forms

3. **Variable Index**
   - C variables
   - Elisp variables
   - Options and customization

4. **Type Index**
   - C structures
   - C typedefs
   - Lisp types

5. **File Index**
   - Source files referenced
   - By directory
   - By functionality

6. **Command Index**
   - Interactive commands
   - Grouped by function

### Index Entry Format

Indices use this format:

```
Entry Name, page-numbers
    Primary reference in bold: 123
    Secondary references: 45, 67, 89
    See also: Related Entry
```

### Automatic Indexing

Pandoc will automatically index:
- `function-name` - Functions (backtick formatting)
- `variable-name` - Variables (backtick formatting)
- **Key Concept** - Bold terms auto-indexed
- Terms marked with `\index{term}` LaTeX command

---

## Literate Programming Format

### Code Block Integration

Every code block follows this pattern:

````markdown
```c
// @file: src/buffer.c
// @lines: 1234-1256
// @version: Emacs 30.0.50
// @description: Gap buffer insertion implementation

/* [1] Insert LEN characters from STRING at position POS.
   If STRING is nil, then insert LEN spaces.  */

void
insert_from_string (Lisp_Object string, ptrdiff_t pos, ptrdiff_t len)
{
  ptrdiff_t string_pos = 0;

  if (NILP (string))  /* [2] */
    {
      insert_null (len);
      return;
    }

  /* [3] Ensure we have enough space */
  prepare_to_modify_buffer (pos, pos, NULL);

  /* [4] Move gap to insertion point */
  move_gap (pos);

  /* [5] Copy characters into gap */
  while (len > 0)
    {
      int c = SREF (string, string_pos);
      insert_char (c);  /* [6] */
      string_pos++;
      len--;
    }

  signal_after_change (pos, 0, len);  /* [7] */
}
```

**Annotation [1]**: The function signature declares the parameters...

**Annotation [2]**: NIL check allows inserting spaces without string...

**Annotation [3]**: `prepare_to_modify_buffer` checks read-only status,
handles undo recording, and runs before-change hooks. See
[@sec:04.05.2] for details on buffer modification preparation.

**Annotation [4]**: Gap movement is the critical operation for gap buffer
efficiency. See [@sec:04.03.1] for gap movement algorithm.

**Annotation [5]**: Character-by-character insertion is inefficient for
large strings. Modern Emacs uses bulk copy here. This is simplified
for pedagogical purposes.

**Annotation [6]**: `insert_char` handles multibyte character encoding.
See [@sec:15.01.3] for character encoding details.

**Annotation [7]**: After-change hooks notify modes and overlays of
buffer modifications. See [@sec:04.02.4] for change hooks.

**Performance Note**: For large insertions, Emacs optimizes by copying
directly into the gap without character-by-character processing. A
10,000 character insertion takes ~0.5ms vs. ~50ms with this naive
approach.

**Historical Note**: Early Emacs used a different buffer representation
(gap-less arrays). The gap buffer was introduced in Emacs 18 (1987)
and significantly improved insertion performance.

**See Also**:
- [@sec:04.03] Gap Buffer Operations
- [@sec:04.05] Buffer Modification
- [@sec:15.01] Character Encoding
````

### Sidebar Boxes

Use special boxes for supplementary information:

```markdown
┌─────────────────────────────────────────┐
│ **Performance Tip**                      │
│                                          │
│ When inserting large amounts of text,   │
│ consider using `insert-buffer-substring` │
│ instead of repeated `insert` calls.      │
│                                          │
│ Benchmark: 10K chars                     │
│ - Repeated insert: 50ms                  │
│ - Buffer substring: 0.5ms                │
│ - Speedup: 100x                          │
└─────────────────────────────────────────┘

┌─────────────────────────────────────────┐
│ **Historical Context**                   │
│                                          │
│ Gap buffers were popularized by Emacs   │
│ but invented earlier. First described   │
│ in "The Craft of Text Editing" (1991).  │
│                                          │
│ Alternative: Piece tables (used in      │
│ Microsoft Word) trade simplicity for    │
│ better undo performance.                 │
└─────────────────────────────────────────┘

┌─────────────────────────────────────────┐
│ **Common Pitfall**                       │
│                                          │
│ Don't forget to call `signal_after_      │
│ change` after modifying buffer content. │
│ This is required for proper overlay     │
│ adjustment and mode notification.        │
└─────────────────────────────────────────┘

┌─────────────────────────────────────────┐
│ **Deep Dive**                            │
│                                          │
│ The gap buffer guarantees O(1)          │
│ insertion at point but O(n) insertion   │
│ elsewhere. Emacs optimizes for the      │
│ common case: sequential editing.        │
│                                          │
│ See [@sec:21.03.2] for alternative      │
│ buffer representations and trade-offs.   │
└─────────────────────────────────────────┘
```

### Diagram Integration

Use ASCII art for simple diagrams, images for complex ones:

```
Buffer Structure (Gap Buffer Representation):

Before insertion at position 100:
┌───────────────────────────────────────────────────────┐
│ Text before │ ← GAP → │ Text after                    │
└───────────────────────────────────────────────────────┘
     ↑                                ↑
   PT=100                           Z=500
  GAP_START=100                  GAP_END=200

After insertion of 5 characters:
┌───────────────────────────────────────────────────────┐
│ Text before + New │ ← GAP → │ Text after              │
└───────────────────────────────────────────────────────┘
          ↑                            ↑
        PT=105                       Z=500
     GAP_START=105                GAP_END=200

GAP_SIZE = GAP_END - GAP_START = 95
BUFFER_SIZE = Z = 500
```

### Cross-Reference Examples

Examples of cross-references in prose:

```markdown
The display engine (see [@chap:05]) uses the redisplay algorithm
described in detail in [@sec:05.02]. This algorithm relies on
glyph matrices ([@sec:05.02.3]) and face merging ([@sec:05.04.2]).

The core function `try_window_id` ([@lst:redisplay-try-window])
implements optimistic redisplay. For comparison with the pessimistic
approach, see `try_window_reusing_current_matrix` in the same
chapter.

Font selection ([@chap:16]) interacts closely with the face system.
The function `realize_face` (@src:xfaces.c:realize_face) creates
cached face objects used during redisplay.

For historical context on this design, see Stallman's 1981 paper
[@stallman:emacs:1981] in the bibliography.
```

---

## Cross-Reference Strategy

### Reference Types and Usage

1. **Chapter References**: `[@chap:05]` → "Chapter 5"
2. **Section References**: `[@sec:05.02.3]` → "Section 5.2.3"
3. **Figure References**: `[@fig:buffer-gap]` → "Figure 4.3"
4. **Table References**: `[@tbl:opcodes]` → "Table 3.2"
5. **Code Listings**: `[@lst:eval-loop]` → "Listing 3.4"
6. **Equations**: `[@eq:gap-size]` → "Equation 4.1"
7. **Source Code**: `[@src:buffer.c:1234]` → "buffer.c line 1234"
8. **Functions**: Automatic via backticks → Function Index
9. **Variables**: Automatic via backticks → Variable Index
10. **External Docs**: `[@elisp-manual:sec:buffers]` → Elisp Manual
11. **Bibliography**: `[@knuth:literate:1984]` → Bibliography entry

### Cross-Reference Database

A structured reference database in `docs/references.yaml`:

```yaml
sections:
  - id: sec:04.03.1
    title: "Gap Movement Algorithm"
    chapter: 4
    section: 3
    subsection: 1
    file: "04-buffer-management/03-gap-buffer-ops.md"

  - id: sec:05.02.3
    title: "Glyph Matrix Structure"
    chapter: 5
    section: 2
    subsection: 3
    file: "05-display-engine/02-redisplay-algorithm.md"

functions:
  - name: "insert_from_string"
    type: "C"
    file: "src/buffer.c"
    line: 1234
    documentation: "sec:04.03.2"

  - name: "try-window-id"
    type: "Elisp"
    file: "lisp/window.el"
    line: 567
    documentation: "sec:06.03.1"

figures:
  - id: fig:buffer-gap
    title: "Gap Buffer Memory Layout"
    chapter: 4
    file: "04-buffer-management/02-text-representation.md"
    image: "images/buffer-gap.svg"
```

### Automated Link Checking

Use pandoc-crossref and custom scripts to:
- Validate all internal references
- Check broken external links
- Generate warnings for missing targets
- Build comprehensive indices

---

## Compilation and Build

### Pandoc Build Pipeline

```bash
# Full book compilation
pandoc \
  --from markdown+smart+yaml_metadata_block \
  --to latex \
  --template=templates/book-template.tex \
  --filter pandoc-crossref \
  --filter pandoc-citeproc \
  --toc \
  --toc-depth=3 \
  --number-sections \
  --top-level-division=chapter \
  --listings \
  --highlight-style=tango \
  --pdf-engine=xelatex \
  --metadata-file=docs/metadata.yaml \
  --bibliography=docs/bibliography/references.bib \
  --csl=docs/bibliography/acm.csl \
  --output=emacs-guide.pdf \
  docs/*/*.md

# HTML version
pandoc \
  --from markdown+smart \
  --to html5 \
  --standalone \
  --self-contained \
  --filter pandoc-crossref \
  --toc \
  --toc-depth=3 \
  --number-sections \
  --highlight-style=tango \
  --css=templates/book.css \
  --metadata-file=docs/metadata.yaml \
  --output=emacs-guide.html \
  docs/*/*.md

# EPUB version
pandoc \
  --from markdown+smart \
  --to epub3 \
  --filter pandoc-crossref \
  --toc \
  --toc-depth=3 \
  --number-sections \
  --metadata-file=docs/metadata.yaml \
  --epub-cover-image=images/cover.png \
  --output=emacs-guide.epub \
  docs/*/*.md
```

### Build Targets

Makefile targets:

```makefile
all: pdf html epub

pdf: emacs-guide.pdf
html: emacs-guide.html
epub: emacs-guide.epub

emacs-guide.pdf: $(SOURCES) metadata.yaml
    pandoc $(PANDOC_OPTS_PDF) -o $@ $(SOURCES)

emacs-guide.html: $(SOURCES) metadata.yaml
    pandoc $(PANDOC_OPTS_HTML) -o $@ $(SOURCES)

emacs-guide.epub: $(SOURCES) metadata.yaml
    pandoc $(PANDOC_OPTS_EPUB) -o $@ $(SOURCES)

check-links:
    @scripts/check-cross-references.sh

validate:
    @scripts/validate-code-blocks.sh

clean:
    rm -f emacs-guide.{pdf,html,epub}
```

### Quality Checks

Automated validation:

1. **Cross-reference validation**: All `[@...]` references resolve
2. **Code block validation**: All source references exist
3. **Build test**: PDF/HTML/EPUB compile without errors
4. **Link checking**: External URLs are valid
5. **Spell checking**: Technical dictionary + custom terms
6. **Style checking**: Vale or similar linter

---

## Document Metadata

See `docs/metadata.yaml` for complete book metadata including:
- Title and subtitle
- Author information
- Version and date
- Copyright and license
- Abstract
- Keywords
- Build configuration

---

## Bibliography and References

### Bibliography Structure

Located in `docs/bibliography/`:

```
bibliography/
├── references.bib          # Main BibTeX file
├── emacs-papers.bib       # Emacs-specific papers
├── comp-sci.bib           # General CS references
├── lisp-history.bib       # Lisp language history
├── text-editors.bib       # Text editor research
└── acm.csl                # Citation style (ACM)
```

### Reference Categories

1. **Emacs Documentation**
   - GNU Emacs Manual
   - Emacs Lisp Reference Manual
   - Emacs Internals Manual
   - Historical manuals

2. **Academic Papers**
   - Original Emacs papers (Stallman, et al.)
   - Lisp implementation papers
   - Text editor research
   - Redisplay algorithms
   - Garbage collection algorithms

3. **Books**
   - "The Craft of Text Editing" (Finseth)
   - "Lisp in Small Pieces" (Queinnec)
   - "Structure and Interpretation of Computer Programs" (Abelson & Sussman)
   - "Garbage Collection" (Jones & Lins)

4. **Online Resources**
   - Emacs wiki
   - Source code repositories
   - Mailing list archives
   - Bug tracker

5. **Historical Sources**
   - TECO documentation
   - ITS manuals
   - Original MIT AI Lab memos

### Citation Style

Using ACM citation style for consistency with computer science literature.

Example citations in text:
- Narrative: "Stallman [1981] describes the original Emacs design..."
- Parenthetical: "The gap buffer was introduced in Emacs 18 [Finseth 1991]."
- Multiple: "Several implementations exist [Finseth 1991; Stallman 1981]."

---

## Contributing to This Guide

### How to Add Content

1. **Identify Chapter**: Determine appropriate chapter/section
2. **Follow Template**: Use section template from chapter README
3. **Add Code**: Include literate code blocks with annotations
4. **Cross-Reference**: Link to related sections
5. **Add to Bibliography**: Include new references
6. **Build and Test**: Ensure compilation succeeds
7. **Submit Patch**: Follow Emacs contribution guidelines

### Writing Guidelines

1. **Clarity**: Explain concepts clearly and progressively
2. **Code Quality**: Use actual source code, not pseudocode
3. **Examples**: Include practical, tested examples
4. **Cross-References**: Link generously to related content
5. **Performance**: Include performance characteristics
6. **History**: Explain why things are designed this way
7. **Maintenance**: Consider future maintainability

### Review Process

All contributions should:
- Build successfully (PDF, HTML, EPUB)
- Pass cross-reference validation
- Include working code examples
- Follow style guidelines
- Be peer-reviewed

---

## Revision History

### Version 1.0.0 (2025-11-18)
- Initial master outline
- Chapter structure defined
- Scope estimates completed
- Literate programming conventions established
- Cross-reference system designed

---

## License

This documentation is licensed under the GNU Free Documentation License
(GFDL) version 1.3 or later, consistent with Emacs project licensing.

Copyright © 2025 Free Software Foundation, Inc.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License.

---

**End of Master Outline**
