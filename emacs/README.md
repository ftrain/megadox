# GNU Emacs Internals: Encyclopedic Guide

**Version**: 1.0.0 (Draft)
**Date**: 2025-11-18
**Status**: Architecture Complete, Content in Development

---

## Overview

This is a comprehensive, encyclopedic guide to the GNU Emacs source code, architecture, and implementation. Using literate programming techniques, it provides in-depth documentation of all major subsystems, from the C core to the Elisp runtime, from the display engine to the package system.

### Goals

- **Comprehensive Coverage**: Document all major subsystems in detail
- **Literate Programming**: Interweave source code with explanatory text
- **Practical Focus**: Include working examples and real-world use cases
- **Educational Value**: Serve students, researchers, and developers
- **Maintenance Aid**: Help current and future maintainers understand the codebase

### Scope

- **C Source**: ~152 files, representing the core implementation
- **Elisp Source**: ~1,576 files, representing the extension layer
- **Documentation**: ~2,500 pages across 23 chapters
- **Code Examples**: ~45,000 lines of annotated source code
- **Figures**: ~100-150 diagrams and illustrations

### Target Audience

1. **Emacs Core Developers**: Understanding the full architecture
2. **Extension Developers**: Learning to write sophisticated packages
3. **Computer Science Students**: Studying a mature software system
4. **Programming Language Researchers**: Examining Lisp implementation
5. **System Architects**: Learning from large-scale system design

---

## Quick Start

### Reading the Guide

1. **Start Here**: Read the [Master Outline](00-MASTER-OUTLINE.md) for a complete overview
2. **Follow a Path**: Choose a reading path based on your goals:
   - **Extension Developers**: Ch 0, 1, 3, 8-11, 17
   - **Core Developers**: Ch 0-5, 18, 20
   - **Students**: Linear reading (Ch 0-22)
3. **Build the Guide**: See [BUILD.md](BUILD.md) for compilation instructions

### Building the Documentation

```bash
cd docs/

# Build all formats
make all

# Build individual formats
make pdf       # PDF version
make html      # HTML version
make epub      # EPUB version
```

See [BUILD.md](BUILD.md) for detailed build instructions.

### Prerequisites

To build the documentation, you need:
- Pandoc >= 3.0
- XeLaTeX (for PDF)
- pandoc-crossref filter
- Bibliography tools (biber)

See [BUILD.md](BUILD.md) for installation instructions.

---

## Documentation Structure

### Chapter Organization

The guide is organized into 23 chapters, numbered 00-22:

```
00-introduction/           # Getting started, overview, development setup
01-architecture/           # Overall system architecture
02-core-subsystems/        # Memory management, data structures
03-elisp-runtime/          # Lisp reader, evaluator, compiler
04-buffer-management/      # Buffer architecture and gap buffers
05-display-engine/         # Redisplay algorithm, rendering
06-window-frame-system/    # Window and frame management
07-text-properties/        # Text properties and overlays
08-major-modes/            # Major mode system
09-minor-modes/            # Minor mode framework
10-keybindings/            # Keymap system
11-command-loop/           # Event loop and command execution
12-process-management/     # Subprocess management
13-network-io/             # Network and IPC
14-file-system/            # File I/O and handlers
15-internationalization/   # Character sets, coding systems
16-font-rendering/         # Font selection and rendering
17-package-system/         # Package management
18-build-system/           # Build process and dumping
19-platform-specific/      # Platform-specific implementations
20-testing-debugging/      # Testing and debugging tools
21-advanced-topics/        # Advanced topics and optimization
22-appendices/             # Reference material and indices
```

### Chapter Structure

Each chapter follows a consistent structure:

```
XX-chapter-name/
├── README.md              # Chapter overview and learning objectives
├── 01-first-section.md    # Content sections (numbered)
├── 02-second-section.md
├── ...
├── references.md          # Chapter-specific bibliography
└── code-examples/         # Extracted code examples
    └── ...
```

### Supporting Directories

```
docs/
├── 00-MASTER-OUTLINE.md   # Master outline and planning document
├── BUILD.md               # Build instructions
├── metadata.yaml          # Pandoc metadata for compilation
├── crossref.yaml          # Cross-reference configuration
├── Makefile               # Build automation
│
├── bibliography/          # Bibliography files
│   ├── references.bib     # Main BibTeX file
│   ├── emacs-papers.bib   # Emacs-specific papers
│   ├── comp-sci.bib       # General CS references
│   └── acm.csl            # Citation style
│
├── images/                # Diagrams and illustrations
│   └── ...
│
├── code-examples/         # Cross-chapter code examples
│   └── ...
│
├── literate-programs/     # Literate programming extracts
│   └── ...
│
└── templates/             # Pandoc templates for building
    ├── book-template.tex  # LaTeX template
    ├── book.css           # HTML styling
    └── epub.css           # EPUB styling
```

---

## Key Documents

### Essential Reading

1. **[Master Outline](00-MASTER-OUTLINE.md)**: Complete guide structure and conventions
2. **[Build Instructions](BUILD.md)**: How to compile the documentation
3. **[Metadata](metadata.yaml)**: Book metadata and configuration
4. **Chapter READMEs**: Overview of each chapter's content

### Quick References

- **Cross-References**: See [00-MASTER-OUTLINE.md](00-MASTER-OUTLINE.md#cross-reference-format)
- **Literate Programming**: See [00-MASTER-OUTLINE.md](00-MASTER-OUTLINE.md#literate-programming-conventions)
- **Bibliography**: See [bibliography/references.bib](bibliography/references.bib)

---

## Literate Programming Format

This guide uses literate programming, interweaving source code with explanations.

### Code Block Example

````markdown
```c
// @file: src/buffer.c
// @lines: 1234-1256
// @version: Emacs 30.0.50
// @description: Gap buffer insertion

void insert_char (int c)  /* [1] */
{
  prepare_to_modify_buffer (PT, PT, NULL);  /* [2] */
  move_gap (PT);                            /* [3] */
  insert_raw_char (c);                      /* [4] */
  signal_after_change (PT, 0, 1);           /* [5] */
}
```

**Annotation [1]**: Function inserts a single character at point...
**Annotation [2]**: Check buffer modification permissions...
[etc.]
````

See the [Master Outline](00-MASTER-OUTLINE.md#literate-programming-conventions) for complete conventions.

---

## Cross-Reference System

The documentation uses a comprehensive cross-reference system:

### Reference Types

- `[@chap:05]` - Chapter reference → "Chapter 5"
- `[@sec:05.02.3]` - Section reference → "Section 5.2.3"
- `[@fig:buffer-gap]` - Figure reference → "Figure 4.3"
- `[@tbl:opcodes]` - Table reference → "Table 3.2"
- `[@lst:eval-loop]` - Code listing → "Listing 3.4"
- `[@src:buffer.c:1234]` - Source code → "buffer.c line 1234"
- `` `function-name` `` - Function (auto-indexed)

### External References

- `[@elisp-manual:sec:buffers]` - Elisp Reference Manual
- `[@emacs-manual:sec:basic]` - Emacs User Manual
- `[@git:commit-hash]` - Git commit
- `[@bug:12345]` - Bug tracker

See [00-MASTER-OUTLINE.md](00-MASTER-OUTLINE.md#cross-reference-format) for complete documentation.

---

## Reading Paths

Different readers should follow different paths through the material:

### For Extension Developers

Learn to write sophisticated Emacs packages:

**Recommended Path**: Ch 0, 1, 3, 8, 9, 10, 11, 17
**Focus**: Elisp programming, modes, keybindings, packages
**Time**: 2-4 weeks of study

### For Core Developers

Understand the complete system for core development:

**Recommended Path**: Ch 0, 1, 2, 3, 4, 5, 18, 20
**Focus**: C core, architecture, critical subsystems
**Time**: 4-8 weeks of study

### For Display/UI Developers

Work on display engine and user interface:

**Recommended Path**: Ch 0, 1, 2, 4, 5, 6, 7, 15, 16, 19
**Focus**: Display pipeline, rendering, fonts
**Time**: 3-6 weeks of study

### For Students

Comprehensive understanding of a large system:

**Recommended Path**: Linear (Ch 0-22)
**Focus**: Complete architectural understanding
**Time**: Full semester course (12-16 weeks)

### For Performance Engineers

Optimize Emacs performance:

**Recommended Path**: Ch 1, 2, 3, 4, 5, 20, 21
**Focus**: Performance-critical code, profiling, optimization
**Time**: 3-5 weeks of study

See [00-MASTER-OUTLINE.md](00-MASTER-OUTLINE.md#dependencies-and-reading-order) for dependency graphs and detailed paths.

---

## Contributing

### How to Contribute

We welcome contributions! Here's how to help:

1. **Choose a Section**: Check the TODO lists in chapter READMEs
2. **Follow Conventions**: See [00-MASTER-OUTLINE.md](00-MASTER-OUTLINE.md#literate-programming-conventions)
3. **Write Content**: Follow the chapter's structure and style
4. **Test Builds**: Ensure `make all` succeeds
5. **Submit Patch**: Follow Emacs contribution guidelines

### Content Guidelines

1. **Clarity**: Explain clearly and progressively
2. **Code Quality**: Use actual source code, not pseudocode
3. **Examples**: Include tested, working examples
4. **Cross-References**: Link to related content generously
5. **Performance**: Include performance characteristics
6. **History**: Explain design decisions and evolution

### Review Process

All contributions should:
- Build successfully (PDF, HTML, EPUB)
- Pass validation (cross-references, code blocks)
- Include working code examples
- Follow style guidelines
- Be peer-reviewed

### Getting Help

- **Emacs Development List**: emacs-devel@gnu.org
- **Documentation Issues**: File on debbugs.gnu.org
- **General Questions**: help-gnu-emacs@gnu.org

---

## Development Status

### Overall Progress

| Component | Status |
|-----------|--------|
| Architecture | ✅ Complete |
| Master Outline | ✅ Complete |
| Build System | ✅ Complete |
| Chapter READMEs | 🚧 In Progress (3/23) |
| Content | 📝 Not Started |
| Figures | 📝 Not Started |
| Code Examples | 📝 Not Started |

### Chapter Status

| Chapter | Status | Priority |
|---------|--------|----------|
| 00: Introduction | 📝 Planned | High |
| 01: Architecture | 📝 Planned | High |
| 02: Core Subsystems | 📝 Planned | High |
| 03: Elisp Runtime | 📝 Planned | High |
| 04: Buffer Management | 📝 Planned | High |
| 05: Display Engine | 📝 Planned | Medium |
| ... | ... | ... |

### Immediate Priorities

1. ✅ Complete documentation architecture
2. ✅ Create master outline
3. ✅ Set up build system
4. 🚧 Write chapter READMEs (3/23 complete)
5. 📝 Write Chapter 0: Introduction
6. 📝 Write Chapter 1: Architecture
7. 📝 Create key diagrams

---

## Build Targets

### Standard Builds

```bash
make all          # Build PDF, HTML, and EPUB
make pdf          # Build PDF only
make html         # Build HTML only
make epub         # Build EPUB only
```

### Validation

```bash
make check        # Run all validation
make check-links  # Validate cross-references
make validate     # Validate YAML metadata
make spell-check  # Run spell checker
make lint         # Run markdown linter
```

### Development

```bash
make watch-pdf    # Auto-rebuild PDF on changes
make watch-html   # Auto-rebuild HTML on changes
make serve        # Serve HTML locally
make stats        # Show documentation statistics
```

### Utilities

```bash
make clean        # Remove build artifacts
make distclean    # Deep clean
make help         # Show all targets
make version      # Show tool versions
```

See [BUILD.md](BUILD.md) for complete documentation.

---

## Documentation Standards

### File Naming

- **Chapters**: `XX-chapter-name/` (two-digit prefix)
- **Sections**: `XX-section-name.md` (two-digit prefix within chapter)
- **Images**: Descriptive names in `images/`
- **Code**: Related to section in `code-examples/`

### Markdown Style

- **Headers**: Use ATX-style headers (`#`, `##`, etc.)
- **Code Blocks**: Always specify language and include source metadata
- **Lists**: Use `-` for unordered, `1.` for ordered
- **Emphasis**: `*italic*` for emphasis, `**bold**` for strong
- **Links**: Use reference-style links for cross-references

### Code Style

- **C Code**: Follow GNU Coding Standards
- **Elisp Code**: Follow Emacs Lisp conventions
- **Comments**: Explain *why*, not *what*
- **Annotations**: Use numbered annotations `/* [1] */`

### Cross-References

- Always use the `[@type:id]` format
- Ensure all references have targets
- Use descriptive IDs
- Link generously but not excessively

---

## License

This documentation is licensed under the **GNU Free Documentation License (GFDL) version 1.3 or later**, consistent with Emacs licensing.

**Copyright © 2025 Free Software Foundation, Inc.**

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.3 or any later version published by the Free Software Foundation; with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.

See: https://www.gnu.org/licenses/fdl-1.3.html

---

## Acknowledgments

This documentation project builds on decades of work by the Emacs development community. Special thanks to:

- **Richard Stallman**: For creating GNU Emacs and the free software movement
- **All Emacs Maintainers**: For stewarding this incredible project
- **All Contributors**: For thousands of contributions over 40+ years
- **The Free Software Foundation**: For supporting GNU Emacs

---

## Resources

### Official Documentation

- [GNU Emacs Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/)
- [Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
- [GNU Emacs Website](https://www.gnu.org/software/emacs/)

### Source Code

- [Git Repository](https://git.savannah.gnu.org/cgit/emacs.git)
- [GitHub Mirror](https://github.com/emacs-mirror/emacs)
- [Source Browser](https://git.savannah.gnu.org/cgit/emacs.git/tree/)

### Community

- [EmacsWiki](https://www.emacswiki.org/)
- [/r/emacs](https://www.reddit.com/r/emacs/)
- [#emacs on Libera.Chat](irc://irc.libera.chat/#emacs)
- [Emacs Stack Exchange](https://emacs.stackexchange.com/)

### Development

- [Emacs Development List](https://lists.gnu.org/mailman/listinfo/emacs-devel)
- [Bug Tracker](https://debbugs.gnu.org/)
- [Development News](https://lists.gnu.org/mailman/listinfo/emacs-devel)

---

## Contact

### Questions and Feedback

- **Documentation Questions**: emacs-devel@gnu.org
- **Bug Reports**: M-x report-emacs-bug (or debbugs.gnu.org)
- **General Help**: help-gnu-emacs@gnu.org

### Maintainers

This documentation is maintained by the Emacs development community.

---

## Version History

### 1.0.0 (Draft) - 2025-11-18

- Initial architecture design
- Master outline complete
- Build system implemented
- Sample chapter READMEs created
- Documentation standards established

---

**Happy Reading and Contributing!**

*"Emacs is the extensible, customizable, self-documenting text editor."*
*Let's make it self-explaining too.*
