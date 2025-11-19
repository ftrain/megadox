# Chapter 01: Architecture

**Status**: Planning
**Estimated Pages**: 80-100
**Prerequisites**: Chapter 00
**Dependencies**: None

## Chapter Overview

This chapter provides a comprehensive look at Emacs' system architecture, covering the C core, Elisp runtime, bootstrap process, module system, and threading model. It establishes the foundational knowledge needed to understand how all the pieces fit together.

## Learning Objectives

After reading this chapter, you should be able to:

1. Understand the overall system architecture and component relationships
2. Identify the major C subsystems and their responsibilities
3. Explain how the Elisp runtime integrates with the C core
4. Trace the bootstrap process from executable to running Emacs
5. Understand the module system and FFI
6. Grasp Emacs' threading model and limitations

## Chapter Structure

### 01-system-architecture.md (15-20 pages)

**Topics:**
- Overall system design philosophy
- Two-tier architecture (C core + Elisp)
- Component interaction patterns
- Initialization sequence overview
- System boundaries and abstractions

**Key Concepts:**
- Primitives (C functions callable from Lisp)
- Lisp objects and their C representation
- Event-driven architecture
- Separation of concerns

**Code Examples:**
```c
// DEFUN macro structure
DEFUN ("forward-char", Fforward_char, Sforward_char, 0, 2, "^p\np",
       doc: /* Move point N characters forward... */)
  (Lisp_Object n, Lisp_Object buffer)
```

**Figures:**
- System architecture diagram
- Component dependency graph
- Data flow diagram

### 02-c-core-subsystems.md (20-25 pages)

**Topics:**
- Memory management (alloc.c)
  - Object allocation
  - Garbage collection
- Lisp interpreter (eval.c, bytecode.c)
  - Evaluation engine
  - Bytecode VM
- Buffer management (buffer.c)
  - Gap buffer implementation
  - Buffer-local variables
- Display engine (xdisp.c, dispnew.c)
  - Redisplay algorithm
  - Terminal abstraction
- Terminal abstraction layer
  - X11, GTK, Windows, macOS, TTY

**Key Concepts:**
- Lisp_Object type
- Mark and sweep GC
- Bytecode interpreter
- Gap buffer data structure
- Terminal methods table

**Code Examples:**
```c
// Lisp_Object representation
typedef intptr_t Lisp_Object;

// Object allocation
Lisp_Object obj = allocate_vector(size);

// GC marking
if (VECTORP (obj))
  mark_object (obj);
```

**Critical Files:**
- src/lisp.h (core type definitions)
- src/alloc.c (memory management)
- src/eval.c (evaluator)
- src/buffer.c (buffer implementation)
- src/xdisp.c (display engine)

### 03-elisp-runtime.md (15-20 pages)

**Topics:**
- Lisp object representation
  - Tagged pointers
  - Type system
  - Immediate values
- Garbage collection details
  - Mark phase
  - Sweep phase
  - Generations (lack thereof)
- Symbol table (obarray)
  - Symbol lookup
  - Interning
- Function calling convention
  - Argument passing
  - Stack frames
  - Return values

**Key Concepts:**
- Fixnum vs. Bignum
- Cons cells
- Vectors and arrays
- String representation
- Symbol properties

**Code Examples:**
```c
// Type checking
if (!STRINGP (obj))
  wrong_type_argument (Qstringp, obj);

// Symbol lookup
Lisp_Object sym = intern ("forward-char");

// Function call
Lisp_Object result = Ffuncall (nargs, args);
```

**Data Structures:**
```
Symbol structure:
┌─────────────┐
│ name        │ → String
│ value       │ → Lisp_Object
│ function    │ → Function
│ plist       │ → Property list
│ next        │ → Next in obarray bucket
└─────────────┘
```

### 04-bootstrap.md (12-15 pages)

**Topics:**
- Early initialization (emacs.c:main)
  - Command-line parsing
  - Environment setup
  - Memory initialization
- Loading loadup.el
  - Core Lisp files
  - Loading order
  - Dependencies
- Temacs to Emacs transformation
  - Preloading
  - Function resolution
- Dumping and undumping
  - Traditional unexec
  - Portable dumper (pdumper)
- Memory layout after dump

**Key Concepts:**
- Temacs (bare Emacs)
- Preloaded Lisp
- Pure space
- Dumped vs. runtime state
- Dump file format

**Code Examples:**
```c
// Main entry point
int main (int argc, char **argv)
{
  // Early init
  init_alloc_once ();
  init_eval_once ();
  init_obarray_once ();

  // Load preloaded Lisp
  Vload_path = decode_env_path (0, normal_path, 0);
  load_file ("loadup.el");

  // Dump or run
  if (dumping)
    pdumper_dump ();
  else
    command_loop ();
}
```

**Figures:**
- Bootstrap flowchart
- Memory layout before/after dump
- Loading dependency graph

### 05-module-system.md (10-12 pages)

**Topics:**
- Dynamic modules overview
- Module API (emacs-module.h)
  - Module structure
  - Function exports
  - Type conversions
- FFI (Foreign Function Interface)
  - Calling conventions
  - Type marshalling
  - Error handling
- Native compilation (libgccjit)
  - Compilation pipeline
  - Async compilation
  - Performance
- Security considerations
  - Sandboxing
  - Trust model
  - Safe evaluation

**Key Concepts:**
- Module initialization
- Environment objects
- Value representation across boundary
- Native compiled functions
- Compilation unit cache

**Code Examples:**
```c
// Module initialization
int emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment (runtime);

  // Define function
  emacs_value fun = env->make_function (env, 1, 1, my_func,
                                        "My function", NULL);

  // Bind to symbol
  emacs_value symbol = env->intern (env, "my-func");
  env->funcall (env, env->intern (env, "defalias"), 2,
                (emacs_value[]){symbol, fun});

  return 0;
}
```

### 06-threading.md (8-10 pages)

**Topics:**
- Cooperative threads
  - Thread creation
  - Thread switching
  - Thread-local state
- Thread safety considerations
  - Global state
  - Shared buffers
  - Mutual exclusion
- Async I/O integration
  - Futures and promises
  - Async programming patterns
- Limitations and constraints
  - No true parallelism
  - GIL equivalent
  - Performance implications
- Future directions
  - Potential improvements
  - Parallel GC
  - True multi-threading

**Key Concepts:**
- Thread objects
- Thread switching points
- Thread-local bindings
- Deadlock avoidance

**Code Examples:**
```elisp
;; Create thread
(make-thread
  (lambda ()
    (message "Running in thread"))
  "my-thread")

;; Thread-local binding
(let ((lexical-binding t))
  (make-thread
    (lambda ()
      (let ((value 42))
        (message "Value: %d" value)))))
```

## Key Takeaways

1. **Two-Tier Design**: C provides performance-critical primitives; Elisp provides extensibility
2. **Unified Type System**: All Lisp objects share a common representation
3. **Garbage Collection**: Automatic memory management with mark-and-sweep GC
4. **Bootstrap Complexity**: Understanding startup is key to understanding the whole system
5. **Module System**: Modern extension mechanism for performance-critical code
6. **Threading Limitations**: Cooperative threading, not true parallelism

## Prerequisites

### Required Knowledge

- C programming (pointers, structs, memory management)
- Basic understanding of Lisp
- Familiarity with system programming concepts
- Operating system fundamentals

### Recommended Background

- Compiler design basics
- Virtual machine implementation
- Memory management techniques
- Concurrent programming concepts

## Cross-References

### This Chapter References

- [@chap:00] Introduction (for context)
- [@chap:02] Core Subsystems (detailed exploration)
- [@chap:03] Elisp Runtime (detailed implementation)
- [@chap:18] Build System (compilation and dumping)

### Referenced By

- Most subsequent chapters depend on this architectural foundation
- [@chap:05] Display Engine (terminal abstraction)
- [@chap:04] Buffer Management (gap buffer details)
- [@chap:21] Advanced Topics (extending the C core)

## Key Files Reference

### C Core Files

```
src/
├── lisp.h           # Core type definitions
├── alloc.c          # Memory management and GC
├── eval.c           # Lisp evaluator
├── bytecode.c       # Bytecode interpreter
├── buffer.c         # Buffer implementation
├── emacs.c          # Main entry point
├── pdumper.c        # Portable dumper
└── module.c         # Module system
```

### Lisp Files

```
lisp/
├── loadup.el        # Bootstrap loader
├── startup.el       # Startup sequence
└── emacs-lisp/
    ├── bytecomp.el  # Byte compiler
    └── nadvice.el   # Advice system
```

## Exercises

1. **Trace Bootstrap**: Follow execution from main() through loadup.el
2. **Find Primitive**: Locate a C primitive (DEFUN) and trace its Lisp usage
3. **Dump Analysis**: Compare temacs and dumped Emacs memory layout
4. **Module Creation**: Write a simple dynamic module
5. **Threading Experiment**: Create threads and observe switching behavior

## Further Reading

### Papers
- [@stallman:emacs:1981] Original Emacs design
- [@steele:lambda:1978] Lisp interpreter implementation
- [@jones:gc:2011] Garbage collection techniques

### Manuals
- [@elisp:manual:2024] Elisp reference
- GNU Coding Standards
- GCC libgccjit documentation

### Source Code
- src/README
- src/TUTORIAL
- Comments in src/*.c files

## Development Tips

### Debugging Architecture

```bash
# Build with debugging symbols
./configure --enable-checking='yes,glyphs' CFLAGS='-O0 -g3'
make

# Debug with GDB
gdb ./src/emacs
(gdb) source src/.gdbinit
(gdb) break main
(gdb) run
```

### Exploring Components

```elisp
;; Find C primitive source
M-x find-function RET forward-char RET

;; View primitive help
C-h f forward-char RET

;; List all primitives
M-x apropos-value RET #<subr RET
```

## Status and Todo

- [ ] Draft 01-system-architecture.md
- [ ] Draft 02-c-core-subsystems.md
- [ ] Draft 03-elisp-runtime.md
- [ ] Draft 04-bootstrap.md
- [ ] Draft 05-module-system.md
- [ ] Draft 06-threading.md
- [ ] Create architecture diagrams
- [ ] Create bootstrap flowcharts
- [ ] Create memory layout diagrams
- [ ] Test all code examples
- [ ] Write exercises with solutions
- [ ] Peer review
- [ ] Technical review by core maintainers

## Changelog

- 2025-11-18: Initial chapter structure and README created
