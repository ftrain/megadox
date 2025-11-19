# Building the Emacs Encyclopedic Guide

This document explains how to build the Emacs Encyclopedic Guide from source into various formats (PDF, HTML, EPUB).

## Prerequisites

### Required Software

1. **Pandoc** (>= 3.0)
   ```bash
   # Ubuntu/Debian
   sudo apt-get install pandoc

   # macOS (via Homebrew)
   brew install pandoc

   # Or download from: https://pandoc.org/installing.html
   ```

2. **XeLaTeX** (for PDF generation)
   ```bash
   # Ubuntu/Debian
   sudo apt-get install texlive-xetex texlive-fonts-extra

   # macOS (via MacTeX)
   # Download from: https://www.tug.org/mactex/
   ```

3. **Pandoc Filters**
   ```bash
   # Install pandoc-crossref
   # Download from: https://github.com/lierdakil/pandoc-crossref/releases

   # Or install via cabal:
   cabal update
   cabal install pandoc-crossref
   ```

4. **Bibliography Tools**
   ```bash
   # Ubuntu/Debian
   sudo apt-get install texlive-bibtex-extra biber

   # macOS (included in MacTeX)
   ```

### Optional Software

- **make**: For using the Makefile
- **Git**: For version control
- **Vale** or **aspell**: For spell checking
- **yamllint**: For YAML validation

## Quick Start

### Build All Formats

```bash
cd /home/user/emacs/docs
make all
```

This will generate:
- `emacs-guide.pdf` - PDF version
- `emacs-guide.html` - HTML version
- `emacs-guide.epub` - EPUB version

### Build Individual Formats

```bash
# PDF only
make pdf

# HTML only
make html

# EPUB only
make epub
```

## Manual Build Commands

If you prefer not to use the Makefile, you can build manually:

### PDF Build

```bash
pandoc \
  --from markdown+smart+yaml_metadata_block \
  --to latex \
  --filter pandoc-crossref \
  --filter pandoc-citeproc \
  --toc \
  --toc-depth=3 \
  --number-sections \
  --top-level-division=chapter \
  --listings \
  --highlight-style=tango \
  --pdf-engine=xelatex \
  --metadata-file=metadata.yaml \
  --bibliography=bibliography/references.bib \
  --csl=bibliography/acm.csl \
  --output=emacs-guide.pdf \
  00-introduction/*.md \
  01-architecture/*.md \
  02-core-subsystems/*.md \
  03-elisp-runtime/*.md \
  04-buffer-management/*.md \
  05-display-engine/*.md \
  06-window-frame-system/*.md \
  07-text-properties/*.md \
  08-major-modes/*.md \
  09-minor-modes/*.md \
  10-keybindings/*.md \
  11-command-loop/*.md \
  12-process-management/*.md \
  13-network-io/*.md \
  14-file-system/*.md \
  15-internationalization/*.md \
  16-font-rendering/*.md \
  17-package-system/*.md \
  18-build-system/*.md \
  19-platform-specific/*.md \
  20-testing-debugging/*.md \
  21-advanced-topics/*.md \
  22-appendices/*.md
```

### HTML Build

```bash
pandoc \
  --from markdown+smart \
  --to html5 \
  --standalone \
  --filter pandoc-crossref \
  --filter pandoc-citeproc \
  --toc \
  --toc-depth=3 \
  --number-sections \
  --highlight-style=tango \
  --css=templates/book.css \
  --metadata-file=metadata.yaml \
  --bibliography=bibliography/references.bib \
  --csl=bibliography/acm.csl \
  --output=emacs-guide.html \
  00-introduction/*.md \
  01-architecture/*.md \
  [... all chapter directories ...]
  22-appendices/*.md
```

### EPUB Build

```bash
pandoc \
  --from markdown+smart \
  --to epub3 \
  --filter pandoc-crossref \
  --filter pandoc-citeproc \
  --toc \
  --toc-depth=3 \
  --number-sections \
  --metadata-file=metadata.yaml \
  --bibliography=bibliography/references.bib \
  --csl=bibliography/acm.csl \
  --epub-cover-image=images/cover.png \
  --output=emacs-guide.epub \
  00-introduction/*.md \
  01-architecture/*.md \
  [... all chapter directories ...]
  22-appendices/*.md
```

## Build System Details

### Directory Structure

```
docs/
├── BUILD.md                      # This file
├── metadata.yaml                 # Book metadata
├── crossref.yaml                 # Cross-reference config
├── 00-MASTER-OUTLINE.md          # Master documentation outline
├── Makefile                      # Build automation
│
├── 00-introduction/              # Chapter 0
│   ├── README.md
│   ├── 01-what-is-emacs.md
│   ├── 02-architecture-overview.md
│   └── ...
│
├── 01-architecture/              # Chapter 1
│   ├── README.md
│   └── ...
│
├── [... other chapters ...]
│
├── bibliography/                 # Bibliography files
│   ├── references.bib
│   ├── emacs-papers.bib
│   ├── comp-sci.bib
│   └── acm.csl
│
├── images/                       # Images and diagrams
│   ├── cover.png
│   └── ...
│
├── code-examples/                # Extracted code examples
│   └── ...
│
├── literate-programs/            # Literate programming files
│   └── ...
│
└── templates/                    # Pandoc templates
    ├── book-template.tex
    ├── book.css
    └── epub.css
```

### File Processing Order

1. **Front Matter**: Auto-generated from metadata.yaml
2. **Chapters**: Processed in numerical order (00-22)
3. **Bibliography**: Appended automatically
4. **Indices**: Generated automatically

### Metadata Configuration

The `metadata.yaml` file controls:
- Book title, author, version
- PDF layout (geometry, fonts, etc.)
- HTML styling
- EPUB metadata
- Cross-reference formats
- Bibliography style
- And much more

Edit `metadata.yaml` to customize the build.

### Cross-Reference System

Cross-references are defined in `crossref.yaml` and use this syntax:

- `[@chap:05]` - Chapter reference
- `[@sec:05.02.3]` - Section reference
- `[@fig:buffer-gap]` - Figure reference
- `[@tbl:opcodes]` - Table reference
- `[@lst:eval-loop]` - Code listing reference

The `pandoc-crossref` filter automatically resolves these and generates proper links.

### Bibliography Management

Bibliography entries are in BibTeX format in `bibliography/*.bib` files.

Cite references using:
- `[@knuth:literate:1992]` - Single reference
- `[@knuth:literate:1992; @finseth:text:1991]` - Multiple references

The `pandoc-citeproc` filter automatically formats citations and generates the bibliography.

## Build Targets

### Standard Targets

```bash
make all          # Build PDF, HTML, and EPUB
make pdf          # Build PDF only
make html         # Build HTML only
make epub         # Build EPUB only
make clean        # Remove build artifacts
```

### Validation Targets

```bash
make check-links     # Validate all cross-references
make validate        # Validate code block references
make spell-check     # Run spell checker
make lint            # Run markdown linter
```

### Development Targets

```bash
make watch-pdf       # Rebuild PDF on file changes
make watch-html      # Rebuild HTML on file changes
make serve-html      # Serve HTML with live reload
```

## Troubleshooting

### Common Build Errors

#### 1. "pandoc: command not found"

**Solution**: Install pandoc (see Prerequisites)

#### 2. "pandoc-crossref: command not found"

**Solution**: Install pandoc-crossref filter
```bash
# Download from: https://github.com/lierdakil/pandoc-crossref/releases
# Extract and place in PATH
```

#### 3. "xelatex: command not found"

**Solution**: Install TeX Live or MacTeX (see Prerequisites)

#### 4. Cross-reference errors

**Error**: `undefined reference [@sec:XX.YY]`

**Solution**: Ensure the referenced section exists and has proper label:
```markdown
## Section Title {#sec:XX.YY}
```

#### 5. Bibliography errors

**Error**: `citation not found: @some-reference`

**Solution**: Ensure reference exists in `bibliography/*.bib` files

#### 6. LaTeX font errors

**Error**: `Font not found: Liberation Serif`

**Solution**: Install required fonts or change fonts in metadata.yaml:
```yaml
# Use standard fonts
mainfont: "Times New Roman"
sansfont: "Arial"
monofont: "Courier New"
```

### Build Performance

Large documents can take time to build:

- **PDF**: 5-15 minutes (first build), 2-5 minutes (incremental)
- **HTML**: 2-5 minutes
- **EPUB**: 2-5 minutes

**Optimization tips**:
- Build individual chapters during development
- Use `make watch-*` for auto-rebuild
- Disable some features for faster builds (remove --toc, etc.)

### Partial Builds

To build a single chapter or section:

```bash
pandoc \
  --from markdown+smart \
  --to html5 \
  --standalone \
  --metadata-file=metadata.yaml \
  --output=chapter-01.html \
  01-architecture/*.md
```

## Quality Assurance

### Before Committing

Run these checks:

```bash
# Validate all cross-references
make check-links

# Validate code blocks
make validate

# Spell check
make spell-check

# Ensure all builds succeed
make all
```

### Continuous Integration

The documentation should build cleanly in CI:

```yaml
# Example .gitlab-ci.yml or .github/workflows/build.yml
build-docs:
  script:
    - cd docs
    - make all
  artifacts:
    paths:
      - docs/emacs-guide.pdf
      - docs/emacs-guide.html
      - docs/emacs-guide.epub
```

## Advanced Usage

### Custom Templates

Create custom Pandoc templates in `templates/`:

```bash
pandoc \
  --template=templates/custom-template.tex \
  --output=emacs-guide-custom.pdf \
  [... other options ...]
```

### Custom Filters

Write custom Pandoc filters for special processing:

```bash
pandoc \
  --filter=scripts/custom-filter.py \
  [... other options ...]
```

### Custom CSS

For HTML builds, customize styling:

```bash
pandoc \
  --css=templates/custom.css \
  --output=emacs-guide-custom.html \
  [... other options ...]
```

## Version Control

### Git Workflow

```bash
# Create feature branch
git checkout -b docs/chapter-05-display

# Make changes
vim 05-display-engine/02-redisplay.md

# Build and test
make pdf

# Commit
git add 05-display-engine/02-redisplay.md
git commit -m "docs: Add redisplay algorithm section"

# Push
git push origin docs/chapter-05-display
```

### Large Files

PDF builds can be large. Consider using Git LFS:

```bash
git lfs track "*.pdf"
git lfs track "*.epub"
```

## Distribution

### Publishing the PDF

```bash
# Build release version
make pdf

# Verify build
ls -lh emacs-guide.pdf

# Upload to distribution site
scp emacs-guide.pdf user@gnu.org:/var/www/emacs/docs/
```

### Publishing HTML

```bash
# Build HTML
make html

# Deploy to web server
rsync -av emacs-guide.html images/ user@gnu.org:/var/www/emacs/docs/
```

## Getting Help

### Documentation

- Pandoc documentation: https://pandoc.org/MANUAL.html
- pandoc-crossref: https://lierdakil.github.io/pandoc-crossref/
- XeLaTeX: https://www.overleaf.com/learn/latex/XeLaTeX

### Support

- Emacs development list: emacs-devel@gnu.org
- Documentation issues: File bug reports on debbugs.gnu.org
- General help: help-gnu-emacs@gnu.org

## License

This build system and documentation are part of GNU Emacs and licensed
under the GNU Free Documentation License v1.3 or later.

---

**Last updated**: 2025-11-18
**Maintained by**: Emacs Development Team
