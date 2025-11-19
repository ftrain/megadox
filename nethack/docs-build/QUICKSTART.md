# NetHack Encyclopedia - Quick Start Guide

## Fastest Way to Build

```bash
cd /home/user/NetHack/docs-build
make all
```

That's it! Your files will be in `/home/user/NetHack/docs-output/`

## What Gets Built

- **NetHack-Encyclopedia.epub** - E-book for readers and tablets
- **NetHack-Encyclopedia.pdf** - Professional print-quality PDF

## First Time Setup

### Install Dependencies

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install pandoc texlive-xetex texlive-fonts-recommended texlive-latex-recommended
```

**macOS:**
```bash
brew install pandoc
brew install --cask mactex
```

**Check Installation:**
```bash
cd /home/user/NetHack/docs-build
make check
```

## Common Commands

```bash
# Build everything
make all

# Build only EPUB (faster)
make epub

# Build only PDF
make pdf

# Check dependencies and files
make check

# Remove generated files
make clean

# Show help
make help
```

## Alternative Methods

### Using Shell Scripts

```bash
cd /home/user/NetHack/docs-build
./build-epub.sh    # Build EPUB
./build-pdf.sh     # Build PDF
```

### Using Pandoc Directly

```bash
cd /home/user/NetHack/docs-build

# EPUB
pandoc --from=markdown --to=epub3 --standalone \
       --metadata-file=metadata.yaml --css=epub.css \
       --toc --number-sections \
       -o ../docs-output/NetHack-Encyclopedia.epub \
       ../README_BESTIARY.md \
       ../MONSTER_CATALOG_COMPLETE.md \
       ../NETHACK_ITEM_COMPENDIUM.md

# PDF
pandoc --from=markdown --to=pdf --pdf-engine=xelatex \
       --standalone --metadata-file=metadata.yaml \
       --toc --number-sections \
       -o ../docs-output/NetHack-Encyclopedia.pdf \
       ../README_BESTIARY.md \
       ../MONSTER_CATALOG_COMPLETE.md \
       ../NETHACK_ITEM_COMPENDIUM.md
```

## Output Location

All generated files go to:
```
/home/user/NetHack/docs-output/
```

## Troubleshooting

**Problem:** `pandoc: command not found`
**Fix:** Install pandoc (see First Time Setup)

**Problem:** PDF build fails with LaTeX errors
**Fix:** Install texlive-xetex package

**Problem:** Scripts not executable
**Fix:** `chmod +x /home/user/NetHack/docs-build/*.sh`

## Next Steps

- Read **README_BUILD.md** for complete documentation
- Edit **metadata.yaml** to customize book info
- Edit **epub.css** to customize e-book styling
- Modify source markdown files and rebuild

## File Overview

```
docs-build/
├── Makefile           # Build automation (USE THIS!)
├── metadata.yaml      # Book metadata and settings
├── epub.css          # E-book styling
├── build-epub.sh     # EPUB builder script
├── build-pdf.sh      # PDF builder script
├── README_BUILD.md   # Complete documentation
└── QUICKSTART.md     # This file
```

---

**Need help?** See README_BUILD.md for detailed instructions.
