# Emacs Build System and Testing Infrastructure

**Comprehensive guide to building, testing, and developing GNU Emacs**

---

## Table of Contents

- [1. Build System Architecture](#1-build-system-architecture)
- [2. Testing Infrastructure](#2-testing-infrastructure)
- [3. Development Workflow](#3-development-workflow)
- [4. Quality Assurance](#4-quality-assurance)
- [5. Platform-Specific Information](#5-platform-specific-information)
- [6. Continuous Integration](#6-continuous-integration)

---

## 1. Build System Architecture

### 1.1 Overview

Emacs uses the GNU Autotools build system (Autoconf/Automake) to provide portable configuration and building across diverse platforms. The build system consists of:

- **configure.ac** (273KB): Main configuration script template
- **Makefile.in**: Top-level makefile template
- **autogen.sh**: Bootstrap script for repository builds
- **GNUmakefile**: Convenience wrapper for unconfigured builds
- **m4/**: 151 m4 macro files for feature detection
- **build-aux/**: Build helper scripts and tools

### 1.2 Autoconf/Automake Architecture

#### Configuration Process Flow

```
Repository Checkout
    ↓
autogen.sh          # Generate configure script
    ↓
configure           # Detect system features
    ↓
config.status       # Generate Makefiles and config.h
    ↓
make                # Build Emacs
```

#### Key Configuration Files

**configure.ac** - Main configuration script (2.65+ required):
```bash
# Minimum autoconf version requirement
AC_PREREQ([2.65])

# Package definition
AC_INIT([GNU Emacs], [31.0.50], [bug-gnu-emacs@gnu.org])

# Key configuration sections:
# - System type detection
# - Compiler and tool checks
# - Library dependency detection
# - Feature option processing
# - Platform-specific adaptations
```

**aclocal.m4** - Auto-generated from m4/ directory:
```bash
# Built by autogen.sh from all m4/*.m4 files
ls m4/*.m4 | LC_ALL=C sort | sed 's,.*\.m4$,m4_include([&]),' > aclocal.m4
```

### 1.3 Building from Source

#### Quick Start (Release Tarball)

```bash
# 1. Download and extract
wget https://ftp.gnu.org/gnu/emacs/emacs-VERSION.tar.xz
tar -xf emacs-VERSION.tar.xz
cd emacs-VERSION

# 2. Configure
./configure

# 3. Build
make

# 4. Test (optional)
src/emacs -Q

# 5. Install
sudo make install
```

#### Building from Repository

```bash
# 1. Clone repository
git clone https://git.savannah.gnu.org/git/emacs.git
cd emacs

# 2. Generate build system
./autogen.sh

# 3. Configure with debug options
./configure CFLAGS='-O0 -g3' --enable-checking=all

# 4. Build
make

# 5. Run tests
make check
```

#### Out-of-Tree Builds

```bash
# Create separate build directory
mkdir build
cd build

# Configure from source directory
../emacs/configure

# Build (source remains clean)
make
```

### 1.4 Configure Options

#### Essential Build Options

```bash
# Installation prefix
./configure --prefix=/opt/emacs

# Debugging build (recommended for development)
./configure \
    --enable-checking='yes,glyphs' \
    --enable-check-lisp-object-type \
    CFLAGS='-O0 -g3'

# Native compilation support
./configure --with-native-compilation

# Portable dumper (default since Emacs 27)
./configure --with-dumping=pdumper

# Disable graphical features
./configure --without-x --without-ns

# Minimal build
./configure --without-all --with-x-toolkit=no

# View all options
./configure --help
```

#### Feature Detection

The configure script automatically detects:
- Compiler capabilities (GCC, Clang, etc.)
- System libraries (X11, GTK, Cairo, etc.)
- Optional features (GnuTLS, ImageMagick, etc.)
- Platform-specific requirements

```bash
# Check detection results
./configure
# Review output for "checking for..." lines

# Force library paths if needed
./configure \
    CPPFLAGS='-I/usr/local/include' \
    LDFLAGS='-L/usr/local/lib'
```

### 1.5 Makefile.in Structure

The top-level Makefile coordinates recursive builds across subdirectories:

```makefile
# Subdirectories built in order
SUBDIR = lib lib-src src lisp

# Key variables
version=31.0.50
configuration=x86_64-unknown-linux-gnu
prefix=/usr/local
```

#### Important Make Targets

```bash
# Build targets
make all              # Standard build
make bootstrap        # Clean rebuild from scratch
make bootstrap-clean  # Prepare for bootstrap
make actual-all       # Internal target (invoked by all)

# Installation
make install          # Install everything
make install-strip    # Install with stripped binaries
make uninstall        # Remove installation

# Cleaning
make clean            # Remove build artifacts
make mostlyclean      # Remove most build artifacts
make distclean        # Remove all generated files
make maintainer-clean # Remove everything regeneratable
make extraclean       # Remove backups and autosave files

# Documentation
make docs             # Build all documentation
make info             # Build Info manuals
make html             # Build HTML documentation
make pdf              # Build PDF documentation
make ps               # Build PostScript documentation

# Testing
make check            # Run standard test suite
make check-expensive  # Include expensive tests
make check-all        # Run all tests
make check-maybe      # Run outdated tests only

# Development
make TAGS             # Update tags tables
make check-declare    # Verify function declarations
```

### 1.6 Bootstrap Process

The bootstrap process rebuilds Emacs from a clean slate when build dependencies have changed significantly.

#### When to Bootstrap

- First build from repository
- After updating loaddefs.el or autoloads
- After changes to fundamental Lisp files
- When encountering mysterious build failures
- After Git merge conflicts in generated files

#### Bootstrap Procedure

```bash
# Standard bootstrap
make bootstrap

# Bootstrap with custom configure options
make bootstrap configure="CFLAGS='-O0 -g3'"

# Bootstrap with default configuration
make bootstrap configure=default

# Fast bootstrap (keeps cache)
./configure -C
make FAST=true bootstrap

# Nuclear option: complete clean
git clean -fdx  # WARNING: Deletes all untracked files!
./autogen.sh
./configure
make
```

#### What Bootstrap Does

1. Runs `bootstrap-clean` to remove:
   - All .elc (byte-compiled) files
   - Generated loaddefs files
   - Native-compiled .eln files
   - Info documentation

2. Regenerates configuration if needed:
   - Runs autogen.sh if no configure exists
   - Rebuilds Makefiles

3. Performs a complete build:
   - Builds C code (lib, src)
   - Byte-compiles all Lisp files
   - Generates autoloads (loaddefs.el)
   - Native-compiles if enabled
   - Builds documentation

### 1.7 Portable Dumper (pdumper)

The portable dumper creates a snapshot of Emacs state for fast startup.

#### Overview

```bash
# Default dumping method since Emacs 27
./configure --with-dumping=pdumper

# Creates dump file
src/emacs.pdmp  # Loaded at startup
```

#### How It Works

1. **Dump Creation** (during build):
   ```bash
   # In src/Makefile, after building temacs:
   ./temacs --batch --load loadup.el dump
   # Creates emacs.pdmp
   ```

2. **Dump Loading** (at startup):
   - Emacs locates .pdmp file (same dir as binary)
   - Memory-maps dump contents
   - Restores Lisp objects, buffers, keymaps
   - Much faster than loading Lisp files

#### Dump File Management

```bash
# Location (installed)
/usr/local/libexec/emacs/31.0.50/x86_64-unknown-linux-gnu/emacs-*.pdmp

# Location (build directory)
src/emacs.pdmp

# Fingerprint-based naming
./src/emacs --fingerprint
# e.g., emacs-31.0.50-abc123def456.pdmp

# Rebuild dump only
cd src && make emacs.pdmp
```

### 1.8 Cross-Compilation Support

Emacs supports cross-compilation for various platforms, notably Android.

#### Android Build

```bash
# See java/INSTALL for detailed instructions

# Configure for Android
export ANDROID_CC=<ndk-toolchain-prefix>-gcc
export ANDROID_CFLAGS="-I<ndk-sysroot>/include"

./configure \
    --host=arm-linux-androideabi \
    --with-ndk-path=/path/to/ndk \
    --with-ndk-build=/path/to/ndk-build

# Build
make

# Android-specific features
cross/Makefile.in           # Cross-compilation support
cross/ndk-build/Makefile.in # NDK build system integration
```

#### Cross-Compilation Directory Structure

```
cross/
├── Makefile.in           # Cross-compilation rules
├── ndk-build/            # Android NDK build support
│   └── Makefile.in
├── README                # Cross-compilation notes
└── langinfo.h            # Platform headers

java/                     # Android-specific code
├── INSTALL               # Android build guide
├── Makefile.in
└── org/gnu/emacs/        # Java wrapper code
```

### 1.9 Native Compilation

Emacs can compile Lisp code to native machine code using libgccjit.

#### Configuration

```bash
# Enable native compilation
./configure --with-native-compilation

# Requires libgccjit
# On Debian/Ubuntu:
sudo apt install libgccjit-12-dev

# On Fedora:
sudo dnf install libgccjit-devel
```

#### How It Works

```bash
# During build, creates:
native-lisp/              # Native-compiled .eln files
└── 31.0.50-<hash>/
    └── preloaded/
        ├── emacs-lisp/
        │   └── byte-opt-<hash>.eln
        └── ...

# At runtime, compiles Lisp files to:
~/.emacs.d/eln-cache/31.0.50-<hash>/
```

#### Build Targets

```bash
# Build trampolines (native compilation support)
make trampolines

# Install native-compiled files
make install-eln
```

#### Configuration Variables

```makefile
# In Makefile.in
HAVE_NATIVE_COMP = yes

# ELN installation directory
ELN_DESTDIR = /usr/local/lib/emacs/31.0.50/
```

---

## 2. Testing Infrastructure

### 2.1 Test Directory Structure

```
test/
├── README                  # Testing overview
├── Makefile.in            # Test execution framework
├── file-organization.org  # File naming conventions
├── data/                  # Shared test data
├── infra/                 # CI infrastructure
│   ├── gitlab-ci.yml      # GitLab CI configuration
│   ├── Dockerfile.emba    # CI container definition
│   └── test-jobs.yml      # Generated test job definitions
├── lisp/                  # Lisp feature tests (42 subdirs)
│   ├── abbrev-tests.el
│   ├── files-tests.el     # 105KB - comprehensive file tests
│   ├── emacs-lisp/        # Emacs Lisp feature tests
│   │   ├── ert-tests.el   # ERT self-tests
│   │   └── ...
│   ├── net/               # Network feature tests
│   │   └── tramp-tests.el # TRAMP remote access tests
│   └── ...
├── src/                   # C implementation tests
│   ├── emacs-tests.el
│   ├── fileio-tests.el
│   └── ...
├── lib-src/               # Utility program tests
├── manual/                # Manual testing procedures
│   ├── etags/             # etags test suite
│   ├── indent/            # Indentation test files
│   └── ...
└── misc/                  # Miscellaneous tests

Total: 677 test files (.el)
```

### 2.2 ERT (Emacs Lisp Regression Testing)

ERT is Emacs's built-in testing framework, inspired by unit testing frameworks.

#### Basic Test Structure

```elisp
;;; my-feature-tests.el --- Tests for my-feature

(require 'ert)
(require 'my-feature)

;; Simple test
(ert-deftest my-feature-test-basic ()
  "Test basic functionality of my-feature."
  (should (equal (my-function 1 2) 3)))

;; Test with setup/teardown
(ert-deftest my-feature-test-with-temp-buffer ()
  "Test my-feature with a temporary buffer."
  (with-temp-buffer
    (insert "test content")
    (should (= (buffer-size) 12))))

;; Test expecting error
(ert-deftest my-feature-test-error ()
  "Test that invalid input signals an error."
  (should-error (my-function nil nil)
                :type 'wrong-type-argument))

;; Test with tag
(ert-deftest my-feature-expensive-test ()
  :tags '(:expensive-test)
  "Expensive test that runs only when requested."
  (dotimes (i 1000000)
    (my-function i (1+ i))))

(provide 'my-feature-tests)
```

#### ERT Assertions

```elisp
;; Basic assertions
(should FORM)              ; Assert FORM is non-nil
(should-not FORM)          ; Assert FORM is nil
(should-error FORM)        ; Assert FORM signals error
(should-error FORM :type 'ERROR-TYPE)

;; Examples
(should (= (+ 1 2) 3))
(should (string= "foo" (upcase "FOO")))
(should-not (zerop 5))
(should-error (/ 1 0) :type 'arith-error)

;; Custom failure messages
(ert-fail "Explicit failure message")
(ert-skip "Test not applicable in this environment")
```

#### Test Tags

```elisp
;; Recognized tags in Emacs test suite:

:expensive-test  ; Takes significant time to run
:unstable        ; Under development, may fail
:nativecomp      ; Requires native compilation
```

### 2.3 Running Tests

#### Command-Line Test Execution

```bash
# Run all standard tests
make check

# Run expensive tests too
make check-expensive

# Run absolutely all tests
make check-all

# Run only outdated tests
make check-maybe

# Byte-compile all test files
make check-byte-compile

# Run specific test file
make test/lisp/files-tests.log

# Run test file without logging
make test/lisp/files-tests

# Run tests in subdirectory
make lisp                  # All tests in test/lisp/
make check-src             # All tests in test/src/
make check-lisp-net        # All tests in test/lisp/net/
```

#### Test Selectors

```bash
# Run specific tests by selector
make test/lisp/files-tests SELECTOR='test-file-exists'

# Use regex selector (note double $$)
make test/lisp/files-tests SELECTOR='"file$$"'

# Predefined selectors
SELECTOR='$(SELECTOR_DEFAULT)'   # Exclude :expensive-test, :unstable
SELECTOR='$(SELECTOR_EXPENSIVE)' # Exclude :unstable only
SELECTOR='$(SELECTOR_ALL)'       # Run all tests
```

#### Test Execution Options

```bash
# Use source .el files instead of .elc (better backtraces)
make check TEST_LOAD_EL=yes

# Run in interactive mode (for debugging)
make test/lisp/files-tests TEST_INTERACTIVE=yes

# Increase backtrace line length
make check TEST_BACKTRACE_LINE_LENGTH=500

# Show test timing summary (top N slowest tests)
make check SUMMARIZE_TESTS=10

# Set test timeout (in seconds)
EMACS_TEST_TIMEOUT=600 make check

# Verbose test output
EMACS_TEST_VERBOSE=1 make check

# Generate JUnit report
EMACS_TEST_JUNIT_REPORT=junit-report.xml make check

# Pass extra options to Emacs
make check EMACS_EXTRAOPT="--eval '(setopt ert-batch-print-length nil)'"
```

#### Interactive Test Execution

```elisp
;; Load test file in Emacs
(load-file "test/lisp/files-tests.el")

;; Run all tests in current file
M-x ert RET t RET

;; Run specific test
M-x ert RET test-name RET

;; Run tests matching pattern
M-x ert RET "^test-file" RET

;; Run tests with selector
M-x ert RET (not (tag :expensive-test)) RET

;; Re-run failed tests
M-x ert-results-rerun-all-tests

;; In *ert* buffer:
;; r - re-run test
;; d - re-run with debugger
;; . - jump to test definition
;; b - show backtrace
;; m - show messages
```

### 2.4 Writing New Tests

#### File Organization Guidelines

From `test/file-organization.org`:

```
1. Test file naming:
   source: lisp/emacs-lisp/pcase.el
   tests:  test/lisp/emacs-lisp/pcase-tests.el

2. Mirror source directory structure:
   source: lisp/progmodes/python.el
   tests:  test/lisp/progmodes/python-tests.el

3. Resource files:
   tests:     test/lisp/progmodes/flymake-tests.el
   resources: test/lisp/progmodes/flymake-resources/

4. Multiple test files for single feature:
   test/lisp/emacs-lisp/eieio-tests/
   ├── eieio-test-persist.el
   ├── eieio-test-methodinvoke.el
   └── ...

5. Tests not tied to specific file:
   test/misc/
   └── some-descriptive-name.el  # NOT *-tests.el
```

#### Test Template

```elisp
;;; package-tests.el --- Tests for package.el  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name <you@example.com>
;; Keywords: tests

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for package.el functionality.

;;; Code:

(require 'ert)
(require 'package)

(ert-deftest package-test-feature-works ()
  "Test that package feature works correctly."
  (should (functionp 'package-initialize)))

(ert-deftest package-test-with-resources ()
  "Test using resource files."
  (let ((resource-file
         (expand-file-name "test-data.txt"
                           (expand-file-name
                            "package-resources"
                            (file-name-directory
                             (or load-file-name buffer-file-name))))))
    (should (file-exists-p resource-file))
    (with-temp-buffer
      (insert-file-contents resource-file)
      (should (> (buffer-size) 0)))))

(provide 'package-tests)
;;; package-tests.el ends here
```

#### Best Practices

```elisp
;; 1. Test one thing per test function
(ert-deftest package-parse-good ()
  "Test parsing of valid package descriptor."
  (should (package-desc-p (package--read-pkg-desc "good"))))

(ert-deftest package-parse-bad ()
  "Test parsing of invalid package descriptor."
  (should-error (package--read-pkg-desc "bad")))

;; 2. Use descriptive test names
;; Good:  package-install-activates-dependencies
;; Bad:   test-1

;; 3. Use temporary buffers for I/O tests
(ert-deftest package-test-buffer-ops ()
  (with-temp-buffer
    (insert "test")
    (should (= (point) 5))))

;; 4. Use temporary files for file tests
(ert-deftest package-test-file-ops ()
  (let ((temp-file (make-temp-file "package-test")))
    (unwind-protect
        (progn
          (write-region "content" nil temp-file)
          (should (file-exists-p temp-file)))
      (delete-file temp-file))))

;; 5. Tag expensive or unstable tests
(ert-deftest package-network-test ()
  :tags '(:expensive-test)
  "Test package download from network."
  (package-refresh-contents))

;; 6. Use fixtures for complex setup
(defvar package-test-data-dir
  (expand-file-name "package-resources"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

;; 7. Document what you're testing
(ert-deftest package-version-compare ()
  "Test that package-version-join produces correct version strings.
See bug#12345 for background on this issue."
  (should (equal (package-version-join '(1 2 3)) "1.2.3")))
```

### 2.5 Test Makefile Details

The test Makefile (`test/Makefile.in`) provides sophisticated test execution:

```makefile
# Test execution environment
EMACS = ../src/emacs
EMACSOPT = --no-init-file --no-site-file --no-site-lisp
TEST_HOME = /nonexistent  # Isolate from user config

# Selectors based on native compilation support
ifeq ($(TEST_NATIVE_COMP),yes)
  SELECTOR_DEFAULT = (not (or (tag :expensive-test) (tag :unstable)))
else
  SELECTOR_DEFAULT = (not (or (tag :expensive-test) (tag :unstable) (tag :nativecomp)))
endif

# Test execution
%.log: %.elc
	HOME=$(TEST_HOME) $(emacs) \
	  -l ert -l $(testloadfile) \
	  --batch --eval '(ert-run-tests-batch-and-exit (quote ${SELECTOR_ACTUAL}))'
```

#### Environment Variables

```bash
# Set by Makefile
EMACS_TEST_DIRECTORY=/path/to/test  # Test root directory
EMACS_EMBA_CI=1                     # Set on emba.gnu.org
EMACS_HYDRA_CI=1                    # Set on hydra.nixos.org

# User-configurable
EMACS_TEST_JUNIT_REPORT=report.xml  # JUnit report output
EMACS_TEST_TIMEOUT=3600             # Test timeout in seconds
EMACS_TEST_VERBOSE=1                # Verbose test output
REMOTE_TEMPORARY_FILE_DIRECTORY=/ssh:host:/tmp  # For remote tests
```

---

## 3. Development Workflow

### 3.1 Initial Setup

```bash
# Clone repository
git clone https://git.savannah.gnu.org/git/emacs.git
cd emacs

# Configure for development (recommended settings)
./autogen.sh
./configure \
    --enable-checking='yes,glyphs' \
    --enable-check-lisp-object-type \
    --with-native-compilation \
    CFLAGS='-O0 -g3'

# Build
make -j$(nproc)

# Verify build
src/emacs -Q --eval '(message "Emacs %s" emacs-version)'
```

### 3.2 Debugging with GDB/LLDB

#### GDB Setup

```bash
# From etc/DEBUG:

# Configure for debugging
./configure \
    --enable-checking='yes,glyphs' \
    --enable-check-lisp-object-type \
    CFLAGS='-O0 -g3 -gdwarf-4'

# Additional flags for optimized builds
CFLAGS='-O2 -g3 -fno-omit-frame-pointer -fno-crossjumping'
```

#### Starting GDB

```bash
# From command line
cd src
gdb ./emacs

# From within Emacs
M-x gdb RET
gdb -i=mi ./emacs

# Enable GUI mode
M-x gdb-many-windows

# Attach to running Emacs
gdb -i=mi -p PID
```

#### GDB Configuration

The `src/.gdbinit` file defines custom commands:

```gdb
# Lisp object inspection
pp expression        # Pretty-print Lisp object
pr                   # Print Lisp object
xpr                  # Examine Lisp object
xbacktrace           # Show Lisp backtrace

# Specialized printing
xtype                # Print type of Lisp object
xint                 # Print Lisp integer
xsymbol              # Print symbol
xstring              # Print string
xvector              # Print vector
xbuffer              # Print buffer

# Display debugging
xwindow              # Examine window
xframe               # Examine frame

# GDB safety
~/.gdbinit:
add-auto-load-safe-path /path/to/emacs/src/.gdbinit
```

#### Debugging Techniques

```gdb
# Set breakpoint in C function
(gdb) break xdisp.c:1234
(gdb) break Fsignal

# Conditional breakpoint
(gdb) break foo.c:100 if PT >= 500

# Inspect Lisp backtrace
(gdb) xbacktrace

# Print Lisp variable
(gdb) pp Vload_path

# Continue execution
(gdb) continue
(gdb) step
(gdb) next
(gdb) finish
```

#### LLDB (macOS)

```bash
# Start lldb
lldb ./emacs

# Run with arguments
(lldb) run -Q

# Set breakpoint
(lldb) breakpoint set --file xdisp.c --line 1234
(lldb) br s -n Fsignal

# Note: LLDB doesn't load .gdbinit
# Custom commands need separate configuration
```

### 3.3 Byte Compilation

#### What is Byte Compilation?

Byte compilation converts Emacs Lisp to a compact bytecode format for faster execution.

```elisp
;; Source: hello.el
(defun hello-world ()
  "Print hello world."
  (message "Hello, world!"))

;; After byte compilation: hello.elc
;; Contains bytecode representation
```

#### Byte Compiling Files

```bash
# During build
make                    # Compiles all Lisp files

# Rebuild all .elc files
cd lisp
make compile-always

# Compile single file
emacs --batch -f batch-byte-compile file.el

# From within Emacs
M-x byte-compile-file RET file.el RET
M-x byte-recompile-directory RET dir RET
```

#### Byte Compilation Targets

```bash
# In lisp/Makefile.in
compile-targets:  # Compile all Lisp files
compile-always:   # Force recompile
autoloads:        # Regenerate loaddefs.el
```

#### Byte Compilation Warnings

```elisp
;; Common warnings:

;; Warning: function 'foo' not known to be defined
;; Fix: Add (declare-function foo "file")

;; Warning: assignment to free variable 'bar'
;; Fix: Add (defvar bar) or use let-binding

;; Warning: reference to free variable 'baz'
;; Fix: Declare or pass as parameter

;; Suppress specific warning
(with-suppressed-warnings ((free-vars bar))
  (setq bar 123))
```

### 3.4 Native Compilation

Native compilation compiles Elisp to native machine code using libgccjit.

#### Setup

```bash
# Enable during configure
./configure --with-native-compilation

# Requires libgccjit
# Check if available
pkg-config --modversion libgccjit
```

#### How It Works

```elisp
;; Automatic compilation at load time
(require 'some-package)  ; Triggers native compilation if needed

;; Compiled files stored in:
~/.emacs.d/eln-cache/31.0.50-<hash>/
  └── some-package-<hash>.eln

;; System files:
/usr/local/lib/emacs/31.0.50/native-lisp/31.0.50-<hash>/
```

#### Manual Native Compilation

```elisp
;; Compile single file
(native-compile "file.el")

;; Compile asynchronously
(native-compile-async "file.el")

;; Compile directory
(native-compile-async "/path/to/dir" 'recursively)

;; Check native compilation status
(native-comp-available-p)  ; => t if available
```

#### Configuration Variables

```elisp
;; Where to store native-compiled files
comp-eln-load-path
;; => ("~/.emacs.d/eln-cache/"
;;     "/usr/local/lib/emacs/31.0.50/native-lisp/")

;; Compilation verbosity
native-comp-verbose  ; 0-3, higher = more verbose

;; Compilation optimization
native-comp-speed     ; 0-3 (optimization)
native-comp-debug     ; 0-3 (debug info)

;; Async compilation control
native-comp-async-jobs-number    ; Parallel jobs
native-comp-deferred-compilation ; Auto-compile on demand
```

#### Build System Integration

```makefile
# In Makefile.in
trampolines: src lisp
	$(MAKE) -C lisp trampolines

install-eln: lisp
	# Install native-compiled files
	find native-lisp -exec install ...
```

### 3.5 Documentation Generation

#### Info Manuals

```bash
# Build all documentation
make docs

# Build specific formats
make info              # Info files
make html              # HTML documentation
make pdf               # PDF documentation
make ps                # PostScript documentation
make dvi               # DVI files

# Individual manuals
make emacs-info        # Emacs manual
make elisp-info        # Elisp reference
make lispref-pdf       # Elisp PDF
make misc-html         # Misc manuals (org, gnus, etc.)

# Install documentation
make install-info
make install-pdf
make install-html
```

#### Manual Sources

```
doc/
├── emacs/             # Emacs user manual
│   ├── emacs.texi
│   └── *.texi
├── lispref/           # Elisp reference manual
│   ├── elisp.texi
│   └── *.texi
├── lispintro/         # Elisp introduction
│   └── emacs-lisp-intro.texi
└── misc/              # Miscellaneous manuals
    ├── org.org        # Org mode (Org format)
    ├── gnus.texi      # Gnus
    ├── tramp.texi     # TRAMP
    └── ...
```

#### Texinfo Processing

```bash
# Build Info from Texinfo
makeinfo emacs.texi -o emacs.info

# Build HTML
makeinfo --html emacs.texi

# Build PDF (requires TeX)
texi2pdf emacs.texi
```

#### Documentation Validation

```bash
# Check documentation
make check-info

# Expected output categories:
# - Texinfo documentation system
# - Emacs
# - Emacs lisp
# - Emacs editing modes
# - Emacs network features
# - Emacs misc features
# - Emacs lisp libraries
```

### 3.6 Release Process

From `admin/release-process`:

#### Release Cycle

**Phase 1: Development** (on master)
- New features
- Feature branches
- Major changes

**Phase 2: Stabilization** (on emacs-NN branch)
- Bug fixes
- Documentation
- Testing

#### Release Branch Creation

```bash
# Create release branch
git checkout -b emacs-31 master

# Update version on master
# In admin/admin.el:
(set-version "32.0.50")

# Update version on branch
(set-version "31.1")

# Update customize-changed-options-previous-release
# (for major releases only)
```

#### Pre-Release Checklist

```bash
# 1. Update copyright years
M-x set-copyright RET

# 2. Check release-blocking bugs
# See https://debbugs.gnu.org/

# 3. Proofread manuals
# Each chapter reviewed by 2+ people

# 4. Run test suite
make check-expensive

# 5. Build on multiple platforms

# 6. Create release tarball
# See admin/make-tarball.txt
```

#### Making a Release

Detailed instructions in `admin/make-tarball.txt`:

```bash
# 1. Update version numbers
admin/admin.el: (set-version "31.1")

# 2. Update NEWS
# Review all changes since last release

# 3. Tag release
git tag -a emacs-31.1 -m "Emacs 31.1 release"

# 4. Create tarball
cd admin
./make-tarball emacs-31.1

# 5. Sign and upload
gpg --detach-sign emacs-31.1.tar.xz
# Upload to ftp.gnu.org
```

---

## 4. Quality Assurance

### 4.1 Static Analysis Tools

#### Check Declare

Verify function declarations match definitions:

```bash
# Check entire codebase
make check-declare

# From Emacs
M-x check-declare-directory RET lisp/ RET

# In source file
;;;###autoload
(declare-function external-func "ext-file" (arg1 arg2))
```

#### Checkdoc

Validate documentation strings:

```elisp
;; Run on current buffer
M-x checkdoc

;; Run on file
M-x checkdoc-file

;; Run on directory
M-x checkdoc-directory

;; Common checkdoc requirements:
;; - First line is complete sentence
;; - Function args in UPPERCASE
;; - End with period
;; - Describe return value

;; Good docstring:
(defun my-function (ARG1 ARG2)
  "Do something with ARG1 and ARG2.
ARG1 should be a string.
ARG2 should be a number.
Return the result as a list."
  ...)

;; checkdoc configuration
(setq checkdoc-spellcheck-documentation-flag t)
(setq checkdoc-arguments-in-order-flag t)
```

#### Package Lint

Check package metadata and structure:

```elisp
;; From package-lint.el (ELPA)
(require 'package-lint)

;; Check current buffer
M-x package-lint-current-buffer

;; Required package headers:
;; Author: Name <email>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; URL: https://example.com
```

#### Byte Compiler Warnings

```bash
# Compile with warnings
emacs --batch -f batch-byte-compile file.el 2>&1 | grep -i warning

# Configure warning level
(setq byte-compile-warnings t)     ; All warnings
(setq byte-compile-warnings nil)   ; No warnings
(setq byte-compile-warnings '(free-vars unresolved))  ; Specific
```

### 4.2 Compiler Warnings

#### Configuration Warning Flags

```bash
# In configure.ac
WARN_CFLAGS = -Wall -Wextra -Wno-unused-parameter ...
WERROR_CFLAGS = -Werror  # Treat warnings as errors

# Build with maximum warnings
./configure CFLAGS='-Wall -Wextra -Werror'

# Disable specific warnings
./configure CFLAGS='-Wall -Wno-unused-variable'
```

#### GCC Warning Options

```bash
# From m4/manywarnings.m4
# Emacs enables numerous warnings:
-Wall                    # Standard warnings
-Wextra                  # Extra warnings
-Wcast-align             # Alignment casts
-Wdouble-promotion       # Float to double
-Wformat-security        # Printf format security
-Wimplicit-fallthrough   # Switch fallthrough
-Wmissing-prototypes     # Missing prototypes
-Wshadow                 # Variable shadowing
-Wunused                 # Unused code
# And many more...
```

### 4.3 AddressSanitizer (ASan)

AddressSanitizer detects memory errors at runtime.

#### Building with ASan

```bash
# Configure with ASan
./configure \
    CFLAGS='-fsanitize=address -fsanitize-address-use-after-scope -O1 -g3' \
    LDFLAGS='-fsanitize=address'

# Build
make

# Run (ASan enabled automatically)
src/emacs

# ASan will report errors like:
# - Heap buffer overflow
# - Stack buffer overflow
# - Use after free
# - Use after return
# - Double free
# - Memory leaks
```

#### ASan Configuration

```bash
# ASan runtime options
export ASAN_OPTIONS='detect_leaks=1:symbolize=1:abort_on_error=1'

# Symbolize backtraces
export ASAN_SYMBOLIZER_PATH=/usr/bin/llvm-symbolizer
```

#### ASan in configure.ac

```c
// Automatic detection
#if defined __SANITIZE_ADDRESS__ || __has_feature (address_sanitizer)
  // ASan is enabled
#endif

// Headers checked
sanitizer/asan_interface.h
sanitizer/lsan_interface.h
sanitizer/common_interface_defs.h
```

### 4.4 Valgrind Support

Valgrind provides memory debugging and profiling.

#### Valgrind Headers

```bash
# configure.ac checks for
AC_CHECK_HEADERS([valgrind/valgrind.h])

# When available, Emacs uses Valgrind client requests
#include <valgrind/valgrind.h>
```

#### Running Under Valgrind

```bash
# Memory error detection
valgrind --leak-check=full --track-origins=yes src/emacs -Q

# Cachegrind (cache profiling)
valgrind --tool=cachegrind src/emacs -Q

# Callgrind (call profiling)
valgrind --tool=callgrind src/emacs -Q

# Massif (heap profiling)
valgrind --tool=massif src/emacs -Q
```

#### Suppression Files

```bash
# Create suppression file for known issues
valgrind --gen-suppressions=all src/emacs -Q 2>&1 | \
  grep -A 50 "^{" > emacs.supp

# Use suppression file
valgrind --suppressions=emacs.supp src/emacs -Q
```

### 4.5 Code Coverage

#### Coverage Build

```bash
# Configure with coverage
./configure CFLAGS='--coverage -O0 -g3'

# Build
make

# Run tests
make check

# Generate coverage report
lcov --capture --directory src --output-file coverage.info
genhtml coverage.info --output-directory coverage-html
```

#### Hydra Coverage Job

From `admin/notes/hydra`:

```
The 'coverage' job does a gcov build and then runs
'make check-expensive'. Fails if any test fails.
```

---

## 5. Platform-Specific Information

### 5.1 Unix/Linux

#### Standard Build

```bash
./configure
make
sudo make install
```

#### Common Issues

```bash
# Missing dependencies
# Debian/Ubuntu:
sudo apt-get install build-essential libgtk-3-dev libgnutls28-dev \
  libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev \
  libncurses-dev texinfo

# Fedora:
sudo dnf install gcc make ncurses-devel gnutls-devel gtk3-devel

# Arch:
sudo pacman -S base-devel libx11 libxpm libjpeg-turbo libtiff giflib \
  libpng gnutls ncurses
```

### 5.2 macOS

#### Building on macOS

```bash
# Install dependencies with Homebrew
brew install autoconf automake texinfo gnutls librsvg

# Configure for macOS
./configure --with-ns --with-modules

# Build Emacs.app
make
make install

# Result: nextstep/Emacs.app
# Copy to /Applications if desired
```

#### macOS-Specific Options

```bash
# For X11 build instead
./configure --with-x-toolkit=lucid

# For Terminal-only build
./configure --without-ns --without-x
```

See `nextstep/INSTALL` for details.

### 5.3 Windows (MinGW/MSYS2)

```bash
# In MSYS2 shell
./configure
make

# Result: src/emacs.exe
```

See `nt/INSTALL` and `nt/INSTALL.W64` for details.

### 5.4 Android

```bash
# Configure for Android
export ANDROID_CC=<ndk-toolchain>-gcc
./configure --host=arm-linux-androideabi

# Build
make
```

See `java/INSTALL` for complete Android build instructions.

### 5.5 MS-DOS

See `msdos/INSTALL` for MS-DOS build instructions (historical platform).

---

## 6. Continuous Integration

### 6.1 CI Platforms

Emacs uses multiple CI platforms:

1. **Emba** (https://emba.gnu.org/emacs/emacs) - Primary GitLab CI
2. **Hydra** (https://hydra.nixos.org/jobset/gnu/emacs-trunk) - Nix-based builds
3. **GitLab CI** - Configured via `.gitlab-ci.yml`

### 6.2 Emba (GitLab CI)

#### Configuration Files

```
.gitlab-ci.yml              # Main CI entry point
test/infra/
├── gitlab-ci.yml           # Actual CI configuration
├── test-jobs.yml           # Generated test job definitions
├── Dockerfile.emba         # CI container definition
└── Makefile                # CI infrastructure tools
```

#### Workflow

From `admin/notes/emba`:

```yaml
# Pipeline stages
stages:
  - build-images          # Create Docker images
  - platform-images       # Platform-specific images
  - native-comp-images    # Native compilation images
  - normal                # Standard tests
  - platforms             # Platform-specific tests
  - native-comp           # Native compilation tests
```

#### Branch Rules

```yaml
# From test/infra/gitlab-ci.yml
workflow:
  rules:
    - if: '$CI_PIPELINE_SOURCE == "merge_request_event"'
      when: never
    - if: '$CI_COMMIT_BRANCH !~ /^(master|emacs|feature|fix)/'
      when: never
    - when: always

# Only these branches trigger CI:
# - master*
# - emacs-*
# - feature/*
# - fix/*
```

#### Environment Variables

```yaml
variables:
  EMACS_EMBA_CI: 1
  EMACS_TEST_JUNIT_REPORT: junit-test-report.xml
  EMACS_TEST_TIMEOUT: 3600
  EMACS_TEST_VERBOSE: 1
```

#### Test Execution

```bash
# Generate test jobs
make -C test generate-test-jobs

# Creates test/infra/test-jobs.yml with:
# - Job for each test subdirectory
# - Proper dependencies
# - Artifact collection
```

#### Job Types

```yaml
# Build jobs
build-image-*:
  - Create Docker image with Emacs build
  - Run only if Makefiles changed

# Test jobs
test-*:
  - Run tests in pre-built image
  - Collect JUnit reports
  - Archive logs

# Special jobs
test-tree-sitter:
  - Test tree-sitter grammar compatibility
  - Generate compatibility-report.html
```

### 6.3 Hydra (Nix-based)

From `admin/notes/hydra`:

#### Hydra Jobs

```
1. 'tarball' job:
   - Checkout from repository
   - Bootstrap
   - Run make-dist
   - Create release tarball

2. 'build' job:
   - Use tarball from (1)
   - Normal build
   - Multiple platforms

3. 'coverage' job:
   - GCov build
   - Run make check-expensive
   - Fails if tests fail
   - Generate coverage report
```

#### Notifications

```
Build status notifications sent to:
  emacs-buildstatus@gnu.org

Subscribe at:
  https://lists.gnu.org/mailman/listinfo/emacs-buildstatus
```

#### Identifying CI Environment

```bash
# Check if running on CI
if [ -n "$EMACS_EMBA_CI" ]; then
    echo "Running on Emba"
fi

if [ -n "$EMACS_HYDRA_CI" ]; then
    echo "Running on Hydra"
fi
```

### 6.4 Local CI Testing

```bash
# Build CI Docker image
cd test/infra
docker build -f Dockerfile.emba -t emacs-ci .

# Run tests in container
docker run -it emacs-ci /bin/bash
cd /checkout
make check
```

### 6.5 CI Best Practices

```elisp
;; Detect CI environment in tests
(when (getenv "EMACS_EMBA_CI")
  ;; Adjust for CI environment
  (setq some-timeout (* 2 some-timeout)))

;; Skip tests not suitable for CI
(ert-deftest my-test ()
  :tags '(:unstable)
  ...)

;; Use junit reporting
;; CI automatically sets EMACS_TEST_JUNIT_REPORT
```

---

## 7. Quick Reference

### 7.1 Common Build Commands

```bash
# First time setup
./autogen.sh
./configure
make

# Development build
./configure CFLAGS='-O0 -g3' --enable-checking=all
make -j$(nproc)

# Clean rebuild
make bootstrap

# Run Emacs
src/emacs -Q

# Install
sudo make install
```

### 7.2 Common Test Commands

```bash
# All standard tests
make check

# Specific test file
make test/lisp/files-tests

# With verbose output
EMACS_TEST_VERBOSE=1 make check

# Expensive tests
make check-expensive

# Interactive debugging
make test/lisp/files-tests TEST_INTERACTIVE=yes
```

### 7.3 Common Development Tasks

```bash
# Update after Git pull
make

# If build fails mysteriously
make bootstrap

# Check for errors before commit
make check-declare
make check

# Update TAGS
make TAGS

# Rebuild documentation
make docs
```

### 7.4 Common Configuration Options

```bash
# Minimal build
./configure --without-all

# Debug build
./configure CFLAGS='-O0 -g3' --enable-checking=all

# Native compilation
./configure --with-native-compilation

# Without X11
./configure --without-x

# With specific toolkit
./configure --with-x-toolkit=gtk3
```

---

## 8. Additional Resources

### 8.1 Documentation Files

- **INSTALL** - Building from release tarball
- **INSTALL.REPO** - Building from Git repository
- **etc/DEBUG** - Comprehensive debugging guide
- **admin/notes/** - Developer notes and procedures
  - **admin/notes/hydra** - Hydra CI information
  - **admin/notes/emba** - Emba CI information
  - **admin/release-process** - Release procedures
- **test/README** - Testing overview
- **test/file-organization.org** - Test file conventions

### 8.2 Online Resources

- **Emacs Manual**: https://www.gnu.org/software/emacs/manual/
- **Elisp Reference**: https://www.gnu.org/software/emacs/manual/elisp.html
- **ERT Manual**: https://www.gnu.org/software/emacs/manual/html_node/ert/
- **Emba CI**: https://emba.gnu.org/emacs/emacs
- **Hydra CI**: https://hydra.nixos.org/jobset/gnu/emacs-trunk
- **Bug Tracker**: https://debbugs.gnu.org/
- **Mailing Lists**: https://savannah.gnu.org/mail/?group=emacs

### 8.3 Directories to Know

```
emacs/
├── src/                # C source code
├── lisp/               # Emacs Lisp code
├── lib/                # Gnulib portability library
├── lib-src/            # Utility programs
├── etc/                # Support files
├── doc/                # Documentation sources
├── test/               # Test suite
├── admin/              # Development tools
├── build-aux/          # Build helper scripts
└── m4/                 # Autoconf macros
```

### 8.4 Key Make Variables

```makefile
# Common variables
CFLAGS          # C compiler flags
LDFLAGS         # Linker flags
prefix          # Installation prefix
DESTDIR         # Installation staging directory

# Test variables
SELECTOR        # Test selector expression
TEST_LOAD_EL    # Use .el files instead of .elc
TEST_INTERACTIVE # Run tests interactively
EMACS_TEST_VERBOSE # Verbose test output
```

---

## 9. Troubleshooting

### 9.1 Build Issues

**Problem**: `configure: error: C compiler cannot create executables`
```bash
# Solution: Install compiler
sudo apt-get install build-essential  # Debian/Ubuntu
sudo dnf install gcc make             # Fedora
```

**Problem**: `configure: error: The following required libraries were not found: gnutls`
```bash
# Solution: Install missing library
sudo apt-get install libgnutls28-dev  # Debian/Ubuntu
sudo dnf install gnutls-devel         # Fedora
```

**Problem**: `make` fails with mysterious errors
```bash
# Solution: Try bootstrap
make bootstrap

# If that fails, nuclear option:
git clean -fdx  # WARNING: Deletes all untracked files
./autogen.sh
./configure
make
```

**Problem**: `.gdbinit` not loaded
```bash
# Solution: Add to ~/.gdbinit:
add-auto-load-safe-path /path/to/emacs/src/.gdbinit
```

### 9.2 Test Issues

**Problem**: Tests fail with `(file-missing "Cannot open load file" ...)`
```bash
# Solution: Rebuild autoloads
cd lisp
make autoloads
```

**Problem**: Tests timeout
```bash
# Solution: Increase timeout
EMACS_TEST_TIMEOUT=7200 make check
```

**Problem**: Remote tests fail
```bash
# Solution: Set remote directory
REMOTE_TEMPORARY_FILE_DIRECTORY=/ssh:host:/tmp make check
```

### 9.3 Platform-Specific Issues

See platform-specific INSTALL files for detailed troubleshooting:
- `nt/INSTALL` - Windows
- `nextstep/INSTALL` - macOS
- `java/INSTALL` - Android
- `etc/PROBLEMS` - Common problems across platforms

---

**Document Version**: 1.0
**Last Updated**: 2025
**Emacs Version**: 31.0.50 (development)

For the most current information, always refer to the documentation files in the Emacs source tree and the online resources listed above.
