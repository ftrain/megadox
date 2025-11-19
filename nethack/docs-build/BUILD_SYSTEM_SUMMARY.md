# NetHack Encyclopedia Build System - Complete Summary

## What Was Created

A complete, professional pandoc-based documentation build system for generating EPUB and PDF versions of the NetHack Encyclopedia.

## Directory Structure

```
/home/user/NetHack/
├── docs-build/                          # Build system directory
│   ├── Makefile                        # Main build automation (157 lines)
│   ├── metadata.yaml                   # Book metadata and configuration
│   ├── epub.css                        # EPUB styling (4.4KB)
│   ├── build-epub.sh                   # EPUB build script (executable)
│   ├── build-pdf.sh                    # PDF build script (executable)
│   ├── README_BUILD.md                 # Complete documentation
│   ├── QUICKSTART.md                   # Quick start guide
│   └── BUILD_SYSTEM_SUMMARY.md        # This file
│
├── docs-output/                        # Output directory (auto-created)
│   ├── NetHack-Encyclopedia.epub      # Generated e-book
│   └── NetHack-Encyclopedia.pdf       # Generated PDF
│
└── [Source Documentation Files]
    ├── README_BESTIARY.md              # Overview (6.5KB, 215 lines)
    ├── MONSTER_CATALOG_COMPLETE.md     # Monster catalog (20KB, 641 lines)
    └── NETHACK_ITEM_COMPENDIUM.md     # Item compendium (50KB, 1204 lines)
```

## Build System Files

### 1. Makefile (Primary Interface)

**Location:** `/home/user/NetHack/docs-build/Makefile`

**Features:**
- Automated dependency checking
- Color-coded output
- Parallel build support
- Incremental builds
- Clean target for removing outputs

**Targets:**
- `make all` - Build both EPUB and PDF (default)
- `make epub` - Build EPUB only
- `make pdf` - Build PDF only
- `make check` - Verify dependencies and files
- `make clean` - Remove generated files
- `make help` - Show usage information

### 2. metadata.yaml (Book Configuration)

**Location:** `/home/user/NetHack/docs-build/metadata.yaml`

**Contains:**
- **Title:** "The NetHack Encyclopedia: A Comprehensive Guide"
- **Subtitle:** "Complete Monster Catalog, Item Compendium, and Game Mechanics"
- **Authors:** NetHack DevTeam, Compiled by Claude AI
- **Date:** 2025-11-19
- **Version:** NetHack 3.7
- **Publisher:** NetHack Community Documentation Project
- **License:** NetHack General Public License

**PDF Settings:**
- Document class: book
- Font size: 11pt
- Page layout: Two-sided with binding offset
- Margins: 1in top/bottom, 1.25in left, 1in right
- Line spacing: 1.2
- Fonts: DejaVu font family

**Features:**
- Table of contents (3-level depth)
- Numbered sections
- Professional typography
- Hyperlinked cross-references
- Print-ready formatting

### 3. epub.css (E-book Styling)

**Location:** `/home/user/NetHack/docs-build/epub.css`

**Features:**
- Clean, readable typography
- Syntax-highlighted code blocks
- Professional table formatting
- Responsive design for various screen sizes
- E-reader optimizations
- Custom styling for NetHack-specific content
- Proper heading hierarchy
- Print-friendly styles

### 4. build-epub.sh (EPUB Builder)

**Location:** `/home/user/NetHack/docs-build/build-epub.sh`

**Features:**
- Color-coded progress output
- Automatic dependency checking
- File verification
- Embedded fonts (DejaVu family)
- Size reporting
- Error handling
- EPUB 3 standard compliance

**Usage:**
```bash
cd /home/user/NetHack/docs-build
./build-epub.sh
```

### 5. build-pdf.sh (PDF Builder)

**Location:** `/home/user/NetHack/docs-build/build-pdf.sh`

**Features:**
- XeLaTeX/pdfLaTeX auto-detection
- Professional book layout
- Page count reporting
- Progress indicators
- Error handling
- Print-optimized output

**Usage:**
```bash
cd /home/user/NetHack/docs-build
./build-pdf.sh
```

### 6. README_BUILD.md (Complete Documentation)

**Location:** `/home/user/NetHack/docs-build/README_BUILD.md`

**Sections:**
- Overview and quick start
- Prerequisites and installation
- Build system structure
- Three build methods (Makefile, scripts, direct pandoc)
- Customization guide
- Output features
- Troubleshooting
- Advanced usage
- Performance tips
- Quality assurance
- CI/CD integration

### 7. QUICKSTART.md (Quick Reference)

**Location:** `/home/user/NetHack/docs-build/QUICKSTART.md`

**Contains:**
- Fastest build commands
- First-time setup instructions
- Common commands reference
- Alternative build methods
- Quick troubleshooting

## Source Documentation

### README_BESTIARY.md
- **Size:** 6.5KB (215 lines)
- **Purpose:** Overview and summary of monster catalog
- **Content:** Quick stats, top monsters, file locations, usage examples

### MONSTER_CATALOG_COMPLETE.md
- **Size:** 20KB (641 lines)
- **Purpose:** Complete monster catalog and bestiary
- **Content:** All 394 monsters, structure definitions, attack types, AI systems

### NETHACK_ITEM_COMPENDIUM.md
- **Size:** 50KB (1,204 lines)
- **Purpose:** Complete item documentation
- **Content:** All object classes, weapons, armor, magical items, artifacts

**Total Documentation:** ~76.5KB across 2,060 lines

## Output Files

### NetHack-Encyclopedia.epub

**Format:** EPUB 3
**Target Devices:** E-readers, tablets, smartphones

**Features:**
- Embedded fonts for consistent display
- Custom CSS styling
- Responsive layout
- Table of contents with links
- Cross-references
- Optimized for reading on any device

**Compatible With:**
- Calibre
- Apple Books
- Google Play Books
- Amazon Kindle (via conversion)
- Adobe Digital Editions
- Most e-reader apps

### NetHack-Encyclopedia.pdf

**Format:** PDF (via XeLaTeX)
**Target Use:** Desktop reading, printing, archival

**Features:**
- Professional book layout
- Two-sided printing support
- Binding offset for perfect binding
- Headers and footers with page numbers
- Hyperlinked table of contents
- Syntax-highlighted code
- Print-ready quality (300+ DPI equivalent)

**Specifications:**
- Page size: Letter (8.5" × 11")
- Margins: Optimized for binding
- Font: DejaVu Serif, 11pt
- Line spacing: 1.2
- Layout: Two-sided, chapters start on right pages

## Build Process

### Quick Build (Recommended)

```bash
cd /home/user/NetHack/docs-build
make all
```

### Step-by-Step Process

1. **Setup**
   ```bash
   # Install dependencies
   sudo apt-get install pandoc texlive-xetex texlive-fonts-recommended
   ```

2. **Verify**
   ```bash
   cd /home/user/NetHack/docs-build
   make check
   ```

3. **Build**
   ```bash
   make all        # Both formats
   # OR
   make epub      # EPUB only (faster)
   make pdf       # PDF only
   ```

4. **Output**
   - Files created in: `/home/user/NetHack/docs-output/`
   - EPUB: ~100-200KB (estimated)
   - PDF: ~500KB-1MB (estimated, depends on LaTeX)

## Build Time Estimates

- **EPUB:** ~5-10 seconds
- **PDF:** ~30-60 seconds (first build), ~15-30 seconds (subsequent)
- **Both:** ~40-70 seconds total

## Dependencies

### Required
- **pandoc** (version 2.0+)
  - Ubuntu/Debian: `sudo apt-get install pandoc`
  - macOS: `brew install pandoc`
  - Windows: Download from pandoc.org

### Required for PDF
- **XeLaTeX** (recommended) or **pdfLaTeX**
  - Ubuntu/Debian: `sudo apt-get install texlive-xetex`
  - macOS: `brew install --cask mactex`
  - Windows: Install MiKTeX or TeX Live

### Optional
- **pdfinfo** - PDF metadata display
- **epubcheck** - EPUB validation
- **Calibre** - E-book management and viewing

## Customization Options

### Change Chapter Order
Edit `SOURCES` array in:
- `Makefile` (line ~13)
- `build-epub.sh` (line ~48)
- `build-pdf.sh` (line ~60)

### Modify Metadata
Edit: `/home/user/NetHack/docs-build/metadata.yaml`

### Change EPUB Styling
Edit: `/home/user/NetHack/docs-build/epub.css`

### Add New Documentation Files
1. Place markdown file in `/home/user/NetHack/`
2. Add to `SOURCES` array
3. Rebuild

### Custom PDF Layout
Modify variables in `metadata.yaml`:
- `geometry` - Page margins
- `fontsize` - Base font size
- `linestretch` - Line spacing
- `documentclass` - LaTeX document class

## Quality Features

### EPUB Quality
✅ EPUB 3 standard compliance
✅ Semantic HTML structure
✅ Accessible navigation
✅ Embedded fonts
✅ Responsive design
✅ Cross-platform compatibility

### PDF Quality
✅ Professional typography
✅ Proper hyphenation and justification
✅ Page breaks at chapter boundaries
✅ Two-sided printing optimization
✅ Hyperlinked cross-references
✅ Syntax-highlighted code
✅ Print-ready output

## Testing Recommendations

### EPUB Testing
1. Validate with epubcheck
2. Test on multiple readers (Calibre, Apple Books, etc.)
3. Check on different screen sizes
4. Verify all links work

### PDF Testing
1. View on different PDF readers
2. Print test pages (two-sided)
3. Check margins and binding offset
4. Verify all hyperlinks work
5. Review on different zoom levels

## Troubleshooting Quick Reference

| Problem | Solution |
|---------|----------|
| `pandoc: command not found` | Install pandoc |
| `xelatex: command not found` | Install texlive-xetex |
| PDF build slow/hangs | Normal on first run; be patient |
| Scripts not executable | `chmod +x *.sh` |
| Missing fonts | `sudo apt-get install fonts-dejavu` |
| EPUB validation fails | Check source markdown syntax |

## Version Control

Files safe to commit:
- ✅ All files in `docs-build/`
- ✅ Source `.md` files
- ❌ `docs-output/` directory (add to `.gitignore`)

Recommended `.gitignore` entry:
```
docs-output/
*.epub
*.pdf
```

## Maintenance

### Regular Updates
- Update `date` in `metadata.yaml` when rebuilding
- Increment `version` field for major changes
- Review and update `abstract` as content changes

### Quality Checks
- Rebuild periodically to catch issues
- Validate EPUB with epubcheck
- Review PDF output for formatting issues
- Test on target devices/readers

## Performance Tips

1. Use `make epub` for quick previews during editing
2. Build PDF only for final output
3. Makefile uses dependency tracking for efficiency
4. First PDF build is slow; subsequent builds are faster
5. Close other applications during PDF build

## Integration Options

### Git Hooks
Add to `.git/hooks/pre-commit`:
```bash
#!/bin/bash
cd docs-build && make epub
```

### GitHub Actions
Automatically build on push (see README_BUILD.md for example workflow)

### Documentation Website
Convert to HTML:
```bash
pandoc -t html5 --standalone -o index.html [source files]
```

## License and Attribution

This build system is provided as part of the NetHack Documentation Project.

- **NetHack:** NetHack General Public License
- **Build System:** Same as NetHack (GPL-compatible)
- **Generated Documentation:** Derived from NetHack source code

## Support and Resources

- **Build System Issues:** See README_BUILD.md troubleshooting section
- **Pandoc Documentation:** https://pandoc.org/MANUAL.html
- **LaTeX Help:** https://tex.stackexchange.com/
- **NetHack:** https://www.nethack.org/

## Summary Statistics

**Build System:**
- 8 files created
- 2 executable scripts
- 1 Makefile with 6 targets
- 1 metadata configuration
- 1 CSS stylesheet
- 3 documentation files

**Source Documentation:**
- 3 markdown files
- 2,060 total lines
- 76.5KB total size
- Covers 394 monsters and hundreds of items

**Output Capabilities:**
- 2 output formats (EPUB, PDF)
- Professional quality
- Print-ready
- E-reader compatible

---

## Getting Started Right Now

```bash
# Navigate to build directory
cd /home/user/NetHack/docs-build

# Check what's needed
make check

# Install dependencies (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install pandoc texlive-xetex texlive-fonts-recommended

# Build everything
make all

# Find your files
ls -lh /home/user/NetHack/docs-output/
```

**That's it! You now have a professional NetHack Encyclopedia in EPUB and PDF formats!**

---

*Build system created: 2025-11-19*
*Documentation: NetHack 3.7*
*Generated with pandoc and LaTeX*
