# Megadox

> A comprehensive collection of encyclopedic technical documentation

[![GitHub Pages](https://img.shields.io/badge/docs-live-blue)](https://ftrain.github.io/megadox/)

## Overview

Megadox is a curated collection of encyclopedic documentation covering some of the most influential software projects in computing history. Each volume has been meticulously researched, analyzed, and compiled into elegant EPUB and PDF formats.

## 📚 Documentation Collection

### Available Encyclopedias

1. **[GNU Emacs Internals](https://ftrain.github.io/megadox/)** (740KB EPUB)
   - Comprehensive guide to Emacs architecture and implementation
   - Covers C core, Elisp runtime, display engine, and major subsystems
   - 31 chapters spanning the entire codebase

2. **[libsignal Encyclopedia](https://ftrain.github.io/megadox/)** (416KB EPUB)
   - Complete guide to Signal's cryptographic protocol library
   - Cryptographic primitives, protocol specifications, and implementation details
   - Architectural evolution from 2013-2025

3. **[PDP-7 Unix: A Complete Reference](https://ftrain.github.io/megadox/)** (350KB EPUB)
   - Definitive guide to the original Unix system
   - Assembly code, filesystem, process management, and development tools
   - Historical context and the birth of the Unix philosophy

4. **[PostgreSQL Internals](https://ftrain.github.io/megadox/)** (489KB EPUB)
   - In-depth exploration of PostgreSQL's architecture
   - Storage layer, query processing, transaction management, and replication
   - Extension system and community culture

5. **[NetHack Encyclopedia](https://ftrain.github.io/megadox/)** (153KB EPUB)
   - Complete guide to the legendary roguelike game
   - Bestiary, item compendium, game mechanics, and dungeon generation
   - Codebase architecture and cultural history

## 🌐 Live Website

Visit the **[Megadox GitHub Pages site](https://ftrain.github.io/megadox/)** to browse and download all documentation.

## 🛠️ Build System

This repository includes a unified build system for generating elegant EPUB and PDF documents from Markdown sources.

### Prerequisites

```bash
# macOS
brew install pandoc
brew install --cask mactex-no-gui  # For PDF generation

# Ubuntu/Debian
sudo apt-get install pandoc texlive-xetex texlive-fonts-recommended
```

### Building Documentation

```bash
# Build all documentation projects
make all

# Build specific project
make emacs
make libsignal/encyclopedia
make postgresql
make nethack
make unix-history-repo/docs

# Build specific format
cd <project-directory>
make epub    # EPUB only
make pdf     # PDF only
make html    # Standalone HTML

# Check dependencies
make check

# View statistics
make stats

# Clean build artifacts
make clean
```

### Project Structure

```
megadox/
├── build.mk              # Common build infrastructure
├── Makefile              # Root makefile for building all projects
├── docs/                 # GitHub Pages website
│   ├── index.html
│   └── books/           # Compiled EPUB/PDF files
├── emacs/               # GNU Emacs documentation
├── libsignal/           # libsignal documentation
├── postgresql/          # PostgreSQL documentation
├── nethack/             # NetHack documentation
└── unix-history-repo/   # Unix history documentation
```

## 🎨 Typography & Design

All documents are compiled with:
- **Elegant typography** using Palatino and Menlo fonts
- **Syntax highlighting** for code blocks (Tango style)
- **Professional formatting** with XeLaTeX
- **Comprehensive TOC** with 3-level depth
- **Cross-references** throughout

## 📖 Features

- **Literate Programming**: Code and explanations interwoven
- **Historical Context**: Evolution and development patterns
- **Comprehensive Coverage**: Complete system documentation
- **Cross-Referenced**: Extensive internal linking
- **Portable Formats**: EPUB for e-readers, PDF for printing

## 🤝 Contributing

This is a documentation compilation project. To contribute:

1. Fork the repository
2. Add or improve documentation
3. Ensure builds succeed: `make check && make all`
4. Submit a pull request

## 📄 License

Documentation in this repository follows the licenses of their respective projects:

- **GNU Emacs**: GNU Free Documentation License v1.3+
- **libsignal**: AGPLv3 documentation
- **PostgreSQL**: PostgreSQL License
- **NetHack**: NetHack General Public License
- **Unix**: Historical documentation

See individual project directories for specific license information.

## 🙏 Acknowledgments

This collection was created using:
- [Pandoc](https://pandoc.org/) for document conversion
- [XeLaTeX](https://www.tug.org/xelatex/) for PDF generation
- [GitHub Pages](https://pages.github.com/) for hosting
- Research and documentation from the respective project communities

## 🔗 Links

- **Website**: [https://ftrain.github.io/megadox/](https://ftrain.github.io/megadox/)
- **Repository**: [https://github.com/ftrain/megadox](https://github.com/ftrain/megadox)

---

*Built with Pandoc and meticulous care*
