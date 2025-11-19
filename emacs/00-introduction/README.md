# Chapter 00: Introduction

**Status**: Planning
**Estimated Pages**: 40-60
**Prerequisites**: None
**Dependencies**: None

## Chapter Overview

This chapter provides an introduction to the Emacs Encyclopedic Guide and to GNU Emacs itself. It covers the historical context, architectural overview, and practical information for working with the Emacs source code.

## Learning Objectives

After reading this chapter, you should be able to:

1. Understand the historical evolution of Emacs from TECO to GNU Emacs
2. Grasp the high-level architecture of Emacs (C core + Elisp layer)
3. Set up a development environment for Emacs
4. Navigate the Emacs source code effectively
5. Understand how to use this documentation guide
6. Know how to contribute to Emacs development

## Chapter Structure

### 01-what-is-emacs.md (8-10 pages)

**Topics:**
- Historical overview (1976-present)
- TECO Emacs, Gosling Emacs, GNU Emacs
- Design philosophy and goals
- Key innovations and influence
- Emacs in the modern development landscape

**Key Concepts:**
- Self-documenting editor
- Extensibility through Lisp
- "Living in Emacs" philosophy
- Free software principles

**Code Examples:**
- Simple Emacs Lisp customization
- Basic interactive command

### 02-architecture-overview.md (10-15 pages)

**Topics:**
- High-level system architecture diagram
- C core responsibilities
- Elisp extension layer
- Major subsystems overview
- Data flow through the system

**Key Concepts:**
- Two-tier architecture
- Primitives (C functions exposed to Lisp)
- Event loop and command dispatch
- Buffer, window, and frame hierarchy

**Figures:**
- System architecture diagram
- Component interaction diagram
- Startup sequence flowchart

### 03-development-setup.md (8-10 pages)

**Topics:**
- Building Emacs from source
- Development tools and workflows
- Debugging techniques (GDB, Edebug)
- Version control and patches
- Testing framework

**Key Concepts:**
- Configure options
- Development vs. production builds
- Debugging symbols
- Patch submission process

**Code Examples:**
- Configure command
- GDB session
- ERT test

### 04-navigating-source.md (6-8 pages)

**Topics:**
- Directory structure (src/, lisp/, etc.)
- File naming conventions
- Finding functions and variables
- Using tags, grep, and specialized tools
- Documentation strings and comments

**Key Concepts:**
- DEFUN macro for C primitives
- defun for Elisp functions
- Autoload cookies
- Commentary sections

**Code Examples:**
- Using M-x find-function
- Tags table setup
- Grep patterns for code search

### 05-reading-guide.md (4-6 pages)

**Topics:**
- How to use this documentation
- Reading paths for different audiences
- Notation and conventions
- Prerequisites and assumed knowledge
- Literate programming format

**Key Concepts:**
- Progressive disclosure
- Cross-references
- Code annotations
- Supplementary boxes

**Examples:**
- Reading path for extension developers
- Reading path for core developers
- Reading path for students

### 06-contributing.md (6-8 pages)

**Topics:**
- Development process
- Emacs coding standards
- Submitting patches
- Copyright assignment
- Mailing list etiquette

**Key Concepts:**
- GNU Coding Standards
- ChangeLog format
- Bug reporting
- Feature requests

**Code Examples:**
- Properly formatted patch
- ChangeLog entry
- Copyright assignment form

## Key Takeaways

1. **Emacs is Lisp**: Understanding that Emacs is fundamentally a Lisp environment is crucial
2. **Two-Tier Design**: The C core provides primitives; Elisp provides extensibility
3. **Community Project**: Emacs is developed by a large community with established processes
4. **Source Code is Documentation**: Reading the source is essential to understanding Emacs

## Prerequisites

### Required Knowledge

- Basic familiarity with text editors
- Understanding of programming concepts
- Some C programming experience
- Basic command-line skills

### Recommended Background

- Unix/Linux system usage
- Version control (Git)
- Lisp or functional programming exposure
- Compiler and build system concepts

## Cross-References

This chapter references:
- [@chap:01] Architecture (overview preview)
- [@chap:03] Elisp Runtime (conceptual introduction)
- [@chap:20] Testing and Debugging (development tools)

Later chapters reference this chapter for:
- Architectural context
- Historical background
- Development environment setup

## Exercises (Optional)

1. **Build Emacs**: Clone the repository and build Emacs from source
2. **Explore Source**: Find the definition of `insert` in C and Elisp
3. **Write Simple Command**: Create a simple interactive command
4. **Read Code**: Read the implementation of `forward-char`
5. **Submit Patch**: Fix a typo in documentation and submit a patch

## Further Reading

### Primary Sources
- GNU Emacs Manual
- Emacs Lisp Reference Manual
- "EMACS: The Extensible, Customizable Display Editor" (Stallman 1981)

### Historical Context
- "Hackers: Heroes of the Computer Revolution" (Levy 1984)
- GNU Project history

### Community Resources
- EmacsWiki
- Planet Emacsen
- /r/emacs subreddit
- #emacs IRC channel

## Author Notes

This chapter should be welcoming to newcomers while providing value to experienced developers. Balance historical context with practical information. Keep code examples simple but illustrative.

### Style Guidelines

- Use accessible language
- Explain jargon on first use
- Include concrete examples
- Maintain enthusiasm without hyperbole
- Acknowledge Emacs' limitations honestly

### Common Pitfalls to Address

- Confusion between Emacs and Elisp
- Overwhelming newcomers with complexity
- Assuming too much prior knowledge
- Insufficient guidance on next steps

## Status and Todo

- [ ] Draft 01-what-is-emacs.md
- [ ] Draft 02-architecture-overview.md
- [ ] Draft 03-development-setup.md
- [ ] Draft 04-navigating-source.md
- [ ] Draft 05-reading-guide.md
- [ ] Draft 06-contributing.md
- [ ] Create architecture diagrams
- [ ] Test all code examples
- [ ] Peer review
- [ ] Technical review by maintainers

## Changelog

- 2025-11-18: Initial chapter structure and README created
