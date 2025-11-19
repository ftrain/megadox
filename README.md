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

**First time?** See [INSTALL-PANDOC.md](INSTALL-PANDOC.md) for setting up pandoc and LaTeX.

```bash
# Check if you have the required tools
make check

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
make pdf     # PDF only (requires XeLaTeX)
make html    # Standalone HTML

# View statistics
make stats

# Clean build artifacts
make clean
```

**Note**: PDF generation requires XeLaTeX. If you only want EPUB/HTML, you can skip the LaTeX installation.

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

We welcome contributions! You can help by:

- **Reporting errors or inaccuracies** in the documentation
- **Improving explanations** of complex topics
- **Adding missing content** or examples
- **Enhancing the build system** or website
- **Improving accessibility** and readability

**See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.**

Quick start for contributors:

1. Fork the repository
2. Install dependencies: see [INSTALL-PANDOC.md](INSTALL-PANDOC.md)
3. Make your changes to Markdown files
4. Test: `make check && make html`
5. Submit a pull request

All technical claims should be verifiable against the actual source code.

## 📄 License

Documentation in this repository follows the licenses of their respective projects:

- **GNU Emacs**: GNU Free Documentation License v1.3+
- **libsignal**: AGPLv3 documentation
- **PostgreSQL**: PostgreSQL License
- **NetHack**: NetHack General Public License
- **Unix**: Historical documentation

See individual project directories for specific license information.

## 🤖 AI-Generated Documentation

This entire collection—from source code analysis to documentation writing, build system creation, and website design—was generated using **Claude** (Anthropic's AI).

### How It Was Created

Starting from actual codebases, Claude:
- Examined thousands of source files across multiple programming languages
- Traced git history to understand architectural evolution
- Synthesized documentation explaining not just *what* the code does, but *why* it exists and *how* it evolved
- Designed and implemented the complete build and publishing system
- Created an elegant web interface with search and navigation features

The result is a deep, systematic exploration of each project compiled with attention to technical accuracy, historical context, and educational value.

**For full details on the AI generation methodology, see [AI-GENERATION.md](AI-GENERATION.md)**

## 🙏 Acknowledgments

This collection was created using:
- [Claude](https://www.anthropic.com/claude) (Anthropic's AI) for analysis and writing
- [Pandoc](https://pandoc.org/) for document conversion
- [XeLaTeX](https://www.tug.org/xelatex/) for PDF generation
- [GitHub Pages](https://pages.github.com/) for hosting
- Research and documentation from the respective project communities

## 🔗 Links

- **Website**: [https://ftrain.github.io/megadox/](https://ftrain.github.io/megadox/)
- **Repository**: [https://github.com/ftrain/megadox](https://github.com/ftrain/megadox)

---

*Built with Pandoc and meticulous care*
