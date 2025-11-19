# NetHack Encyclopedia Build System

Complete pandoc-based documentation build system for generating professional EPUB and PDF versions of the NetHack Encyclopedia.

## Overview

This build system compiles the NetHack documentation markdown files into:

- **EPUB** - Professional e-book format for e-readers, tablets, and mobile devices
- **PDF** - High-quality print-ready document with proper typography and layout

## Quick Start

### Build Everything

```bash
cd /home/user/NetHack/docs-build
make all
```

Output files will be created in `/home/user/NetHack/docs-output/`:
- `NetHack-Encyclopedia.epub`
- `NetHack-Encyclopedia.pdf`

### Build Individual Formats

```bash
# Build only EPUB
make epub

# Build only PDF
make pdf
```

## Prerequisites

### Required Software

1. **Pandoc** (version 2.0 or later)
   - Ubuntu/Debian: `sudo apt-get install pandoc`
   - macOS: `brew install pandoc`
   - Windows: Download from https://pandoc.org/installing.html

2. **LaTeX Distribution** (for PDF generation only)
   - Ubuntu/Debian:
     ```bash
     sudo apt-get install texlive-xetex texlive-fonts-recommended \
                          texlive-latex-recommended texlive-latex-extra
     ```
   - macOS: `brew install --cask mactex`
   - Windows: Install [MiKTeX](https://miktex.org/) or [TeX Live](https://www.tug.org/texlive/)

3. **Optional**: `pdfinfo` for PDF metadata (usually included with LaTeX)

### Check Dependencies

```bash
make check
```

This command will verify that all required tools are installed and all source files are present.

## Build System Structure

```
NetHack/
├── docs-build/              # Build system directory
│   ├── Makefile            # Main build automation
│   ├── metadata.yaml       # Document metadata and configuration
│   ├── epub.css           # EPUB styling
│   ├── build-epub.sh      # EPUB build script
│   ├── build-pdf.sh       # PDF build script
│   └── README_BUILD.md    # This file
│
├── docs-output/            # Generated output files (created automatically)
│   ├── NetHack-Encyclopedia.epub
│   └── NetHack-Encyclopedia.pdf
│
└── [Source markdown files]
    ├── README_BESTIARY.md
    ├── MONSTER_CATALOG_COMPLETE.md
    └── NETHACK_ITEM_COMPENDIUM.md
```

## Build Methods

### Method 1: Using Makefile (Recommended)

The Makefile provides the easiest and most reliable build process:

```bash
cd /home/user/NetHack/docs-build

# See all available commands
make help

# Check dependencies
make check

# Build everything
make all

# Build specific format
make epub
make pdf

# Clean output files
make clean
```

### Method 2: Using Build Scripts

Direct shell script execution:

```bash
cd /home/user/NetHack/docs-build

# Build EPUB
./build-epub.sh

# Build PDF
./build-pdf.sh
```

Make sure scripts are executable:
```bash
chmod +x build-epub.sh build-pdf.sh
```

### Method 3: Direct Pandoc Commands

For advanced users who want full control:

#### EPUB Generation

```bash
pandoc \
    --from=markdown \
    --to=epub3 \
    --standalone \
    --metadata-file=metadata.yaml \
    --css=epub.css \
    --toc \
    --toc-depth=3 \
    --number-sections \
    --output=NetHack-Encyclopedia.epub \
    ../README_BESTIARY.md \
    ../MONSTER_CATALOG_COMPLETE.md \
    ../NETHACK_ITEM_COMPENDIUM.md
```

#### PDF Generation

```bash
pandoc \
    --from=markdown \
    --to=pdf \
    --pdf-engine=xelatex \
    --standalone \
    --metadata-file=metadata.yaml \
    --toc \
    --toc-depth=3 \
    --number-sections \
    --variable=geometry:top=1in \
    --variable=geometry:bottom=1in \
    --variable=geometry:left=1.25in \
    --variable=geometry:right=1in \
    --output=NetHack-Encyclopedia.pdf \
    ../README_BESTIARY.md \
    ../MONSTER_CATALOG_COMPLETE.md \
    ../NETHACK_ITEM_COMPENDIUM.md
```

## Customization

### Metadata (metadata.yaml)

Edit `/home/user/NetHack/docs-build/metadata.yaml` to customize:

- Title, subtitle, author information
- Publication date and version
- Copyright and license information
- Abstract/description
- PDF layout options (margins, fonts, spacing)
- Typography settings

### EPUB Styling (epub.css)

Edit `/home/user/NetHack/docs-build/epub.css` to customize:

- Fonts and typography
- Colors and themes
- Spacing and layout
- Code block formatting
- Table styling

### Chapter Ordering

To change the order of chapters, edit the `SOURCES` array in:
- `Makefile` (line ~15)
- `build-epub.sh` (line ~48)
- `build-pdf.sh` (line ~60)

Current order:
1. README_BESTIARY.md (Overview and summary)
2. MONSTER_CATALOG_COMPLETE.md (Complete monster catalog)
3. NETHACK_ITEM_COMPENDIUM.md (Item compendium)

### PDF Engine

The build system uses XeLaTeX by default for better Unicode and font support. To use pdfLaTeX instead:

```bash
# In build-pdf.sh or Makefile, change:
--pdf-engine=xelatex
# to:
--pdf-engine=pdflatex
```

## Output Features

### EPUB Features

- ✅ EPUB 3 format (widely compatible)
- ✅ Embedded fonts (DejaVu font family)
- ✅ Custom CSS styling
- ✅ Table of contents with 3-level depth
- ✅ Numbered sections
- ✅ Optimized for e-readers and mobile devices
- ✅ Proper semantic HTML structure
- ✅ Cross-references and internal links

### PDF Features

- ✅ Professional book-style layout
- ✅ Two-sided printing layout (twoside)
- ✅ Proper page margins and binding offset
- ✅ Table of contents with page numbers
- ✅ Numbered sections and subsections
- ✅ Syntax highlighting for code blocks
- ✅ Headers and footers with page numbers
- ✅ Hyperlinked cross-references
- ✅ Print-ready quality

## Troubleshooting

### Common Issues

**Problem**: `pandoc: command not found`
**Solution**: Install pandoc as described in Prerequisites

**Problem**: `xelatex: command not found` (PDF build fails)
**Solution**: Install a LaTeX distribution (texlive-xetex recommended)

**Problem**: PDF build very slow or hangs
**Solution**: This is normal for first run. LaTeX may need to download fonts. Subsequent builds are faster.

**Problem**: Missing fonts in PDF
**Solution**: Install additional fonts:
```bash
sudo apt-get install fonts-dejavu fonts-dejavu-extra
```

**Problem**: EPUB doesn't display properly
**Solution**:
- Ensure your e-reader supports EPUB 3
- Try a different reader (Calibre, Adobe Digital Editions)
- Check that source markdown files have proper formatting

**Problem**: Build scripts not executable
**Solution**:
```bash
chmod +x docs-build/*.sh
```

### LaTeX Package Issues

If PDF build fails due to missing LaTeX packages:

```bash
# Ubuntu/Debian - install complete LaTeX
sudo apt-get install texlive-full

# Or install specific missing packages
sudo apt-get install texlive-latex-extra texlive-fonts-extra
```

### Debugging

Enable verbose output:

```bash
# For Makefile
make pdf VERBOSE=1

# For direct pandoc, add --verbose
pandoc --verbose [other options]
```

## Advanced Usage

### Adding New Source Files

1. Create or add markdown file to NetHack root directory
2. Edit `docs-build/Makefile`, `build-epub.sh`, and `build-pdf.sh`
3. Add file path to `SOURCES` array in correct order
4. Rebuild: `make all`

### Custom Templates

Create custom LaTeX template:

```bash
# Get default template
pandoc -D latex > custom-template.tex

# Edit custom-template.tex as needed

# Use in build
pandoc --template=custom-template.tex [other options]
```

### Filters and Preprocessing

Add pandoc filters for custom processing:

```bash
pandoc --filter=my-filter.py [other options]
```

### Split PDFs

Generate separate PDFs for each source:

```bash
for file in ../MONSTER*.md ../README*.md; do
    pandoc "$file" -o "$(basename "$file" .md).pdf" --metadata-file=metadata.yaml
done
```

## Performance Tips

1. **Use make for incremental builds** - Makefile tracks dependencies
2. **EPUB builds are fast** (~5-10 seconds)
3. **PDF builds take longer** (~30-60 seconds first time, faster after)
4. **Use `make epub` during development** for quick previews
5. **Generate PDF only for final version**

## Quality Assurance

### Validation

Validate EPUB:
```bash
# Install epubcheck
sudo apt-get install epubcheck

# Validate
epubcheck docs-output/NetHack-Encyclopedia.epub
```

Validate PDF:
```bash
pdfinfo docs-output/NetHack-Encyclopedia.pdf
```

### Testing Readers

**EPUB**: Test on multiple readers
- Calibre (desktop)
- Adobe Digital Editions
- Apple Books (macOS/iOS)
- Google Play Books (Android)
- Kindle (convert with Calibre)

**PDF**: Test printing
- Print a few pages two-sided to verify layout
- Check margins and binding offset
- Verify page numbers and headers

## Continuous Integration

### Automated Builds

Add to CI/CD pipeline:

```yaml
# Example GitHub Actions workflow
name: Build Documentation
on: [push]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y pandoc texlive-xetex
      - name: Build documentation
        run: |
          cd docs-build
          make all
      - name: Upload artifacts
        uses: actions/upload-artifact@v2
        with:
          name: documentation
          path: docs-output/
```

## License

This build system is provided as part of the NetHack project documentation.
See the main NetHack LICENSE file for details.

## Support

For issues with:
- **Pandoc**: https://pandoc.org/help.html
- **LaTeX**: https://tex.stackexchange.com/
- **NetHack**: https://www.nethack.org/

## Version History

- **2025-11-19**: Initial build system creation
  - EPUB and PDF support
  - Professional styling and typography
  - Makefile automation
  - Complete documentation

---

**Happy Building! May your documentation be as epic as your NetHack adventures!** 🎮📚
