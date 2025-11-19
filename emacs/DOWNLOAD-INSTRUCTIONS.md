# Download Instructions - Emacs Documentation Only

This branch contains **ONLY** the encyclopedic documentation for GNU Emacs, without the full source code.

---

## Quick Download (Documentation Only)

### Option 1: Sparse Checkout (Recommended)

```bash
# Clone with sparse checkout to get only the docs directory
mkdir emacs-docs && cd emacs-docs
git init
git remote add origin https://github.com/ftrain/emacs.git
git config core.sparseCheckout true
echo "docs/" >> .git/info/sparse-checkout
git pull origin claude/codebase-documentation-guide-01NwVMvgFSxW27bz7aFpiyfk --depth 1

# Navigate to the documentation
cd docs
```

**Size:** ~1.5 MB (just documentation)
**Time:** < 10 seconds

### Option 2: Download as ZIP

```bash
# Download the branch as a ZIP file
wget https://github.com/ftrain/emacs/archive/refs/heads/claude/codebase-documentation-guide-01NwVMvgFSxW27bz7aFpiyfk.zip

# Extract just the docs directory
unzip "claude/codebase-documentation-guide-01NwVMvgFSxW27bz7aFpiyfk.zip" "*/docs/*"

# Navigate to documentation
cd emacs-claude-codebase-documentation-guide-01NwVMvgFSxW27bz7aFpiyfk/docs
```

### Option 3: Using GitHub Web Interface

1. Go to: https://github.com/ftrain/emacs
2. Click the branch dropdown (currently shows "main" or similar)
3. Select **`claude/codebase-documentation-guide-01NwVMvgFSxW27bz7aFpiyfk`**
4. Click the green **"Code"** button
5. Select **"Download ZIP"**
6. Extract and navigate to the `docs/` folder

---

## What You Get

```
emacs-docs/
└── docs/
    ├── 00-introduction/
    │   ├── 01-welcome.md (1,063 lines)
    │   └── README.md
    ├── 01-architecture/
    │   ├── 02-design-philosophy.md (1,620 lines)
    │   └── README.md
    ├── 02-core-subsystems/ (5 chapters, 9,247 lines)
    ├── 03-elisp-runtime/ (2 chapters, 2,865 lines)
    ├── 04-major-subsystems/ (5 chapters, 5,537 lines)
    ├── 06-platform-support/
    ├── 07-window-systems/
    ├── 08-elisp-library/
    ├── 09-text-processing/
    ├── 17-development/
    ├── 18-development-practices/
    ├── 19-industry-context/
    ├── 20-comparative-analysis/
    ├── GLOSSARY.md (313 entries)
    ├── INDEX.md (300+ entries)
    ├── FINAL-REPORT.md
    ├── README.md
    ├── Makefile
    ├── metadata.yaml
    └── bibliography/
```

**Total:** 40 files, 46,908 lines, ~1.5 MB

---

## Reading the Documentation

### Quick Start

1. **Start here:** `docs/README.md` - Project overview
2. **Then read:** `docs/FINAL-REPORT.md` - Complete summary
3. **For topics:** `docs/INDEX.md` - Quick lookup
4. **For terms:** `docs/GLOSSARY.md` - Definitions

### Reading Paths by Audience

**Extension Developers (2-4 weeks):**
- 00-introduction/01-welcome.md
- 01-architecture/02-design-philosophy.md
- 03-elisp-runtime/
- 08-elisp-library/
- 04-major-subsystems/ (Org, VC, etc.)

**Core Developers (4-8 weeks):**
- 00-introduction/
- 01-architecture/
- 02-core-subsystems/ (all 5 chapters)
- 03-elisp-runtime/
- 06-platform-support/

**Students (12-16 weeks):**
- Read linearly from 00 through 20
- Follow cross-references
- Study code examples

---

## Compiling to PDF/EPUB

### Requirements

```bash
# Ubuntu/Debian
sudo apt-get install pandoc texlive-xetex texlive-fonts-recommended

# macOS
brew install pandoc basictex
```

### Build Commands

```bash
cd docs/

# Generate PDF (recommended)
make pdf

# Generate EPUB for e-readers
make epub

# Generate HTML for web
make html

# All formats
make all
```

### Output Files

- `emacs-guide.pdf` - Professional book-quality PDF
- `emacs-guide.epub` - E-reader compatible EPUB3
- `emacs-guide.html` - Single-page HTML5

---

## Documentation Contents

### Technical Documentation (30,000+ lines)

**Core Subsystems:**
- Buffer management (gap buffer, markers, intervals)
- Display engine (redisplay, glyphs, bidirectional text)
- Keyboard events (event loop, keymaps, command dispatch)
- Process I/O (async processes, filters, network)
- File I/O & encoding (20+ coding systems)

**Elisp Runtime:**
- Interpreter core (eval, bytecode, native compilation)
- Memory management (GC, allocation, profiling)

**Major Applications:**
- Org mode (127 files, literate programming)
- Gnus (email/news architecture)
- Version Control (Git, Mercurial, SVN)
- CEDET (parser framework, IDE tools)
- Calc (symbolic math, units)

**Platform Support:**
- Abstraction layer (8 platforms)
- X11 integration (52,000+ LOC analyzed)

### Context & Analysis (13,000+ lines)

**Historical Evolution:**
- 40-year development history
- Coding practices evolution
- Community governance

**Industry Context:**
- Lisp Machines era (1970s-80s)
- Unix wars and portability
- Rise of IDEs (1990s-2000s)
- LSP revolution (2016-present)
- Performance wars and native compilation

**Comparative Analysis:**
- Vi/Vim comparison
- Modern editors (VSCode, Sublime)
- IDEs (Visual Studio, IntelliJ)
- Cloud editors (GitHub Codespaces)

---

## File Formats

All documentation is in **Markdown** format:
- GitHub-flavored Markdown
- CommonMark compatible
- Readable in any text editor
- Renders on GitHub
- Compiles to PDF/EPUB/HTML with Pandoc

---

## Quick Reference

### Key Files

| File | Description | Lines |
|------|-------------|-------|
| `FINAL-REPORT.md` | Complete project summary | 600+ |
| `README.md` | Quick start guide | 500+ |
| `GLOSSARY.md` | 313 term definitions | 3,471 |
| `INDEX.md` | 300+ topic index | 300+ |
| `00-MASTER-OUTLINE.md` | Complete blueprint | 2,139 |

### Statistics

- **Documentation:** 46,908 lines
- **Files:** 40 markdown + infrastructure
- **Size:** 1.5 MB
- **Codebase analyzed:** 2.6M lines
- **C files:** 152 (562K lines)
- **Elisp files:** 1,576 (1.66M lines)

---

## Differences Between Download Methods

### Sparse Checkout (Option 1 - RECOMMENDED)
- ✅ **1.5 MB** - Documentation only
- ✅ Fast download (< 10 seconds)
- ✅ Easy to browse
- ✅ No source code clutter
- ✅ Uses standard git tools

### Full Branch Clone
- ❌ **600+ MB** - Full Emacs source + docs
- ❌ Slow download (minutes)
- ❌ Mixed with source code
- ✅ Can see source files referenced in docs

**Recommendation:** Use sparse checkout (Option 1) unless you need the full Emacs source code.

---

## Support

### Build Issues

If `make pdf` fails:
```bash
# Check pandoc installation
pandoc --version

# Check LaTeX installation
xelatex --version

# Try HTML instead (no LaTeX required)
make html
```

### Questions

- Check `FINAL-REPORT.md` for comprehensive overview
- See `BUILD.md` for detailed build instructions
- Use `INDEX.md` for topic lookup

---

## License

This documentation is provided as a guide to understanding GNU Emacs.
GNU Emacs itself is licensed under GPL v3+.

Documentation created: November 18, 2025

---

## Quick Commands Reference

```bash
# Download docs only (sparse checkout)
mkdir emacs-docs && cd emacs-docs
git init
git remote add origin https://github.com/ftrain/emacs.git
git config core.sparseCheckout true
echo "docs/" >> .git/info/sparse-checkout
git pull origin claude/codebase-documentation-guide-01NwVMvgFSxW27bz7aFpiyfk --depth 1

# Navigate and read
cd docs
cat README.md

# Build PDF
make pdf

# Build all formats
make all

# Validate
make check-links
```

---

**Branch:** `claude/codebase-documentation-guide-01NwVMvgFSxW27bz7aFpiyfk`
**Recommended Download:** Sparse checkout (see Option 1 above)
**Size:** ~1.5 MB (documentation only, no source code when using sparse checkout)
**Files:** 40 documentation files
**Total Lines:** 46,908 lines of comprehensive documentation
