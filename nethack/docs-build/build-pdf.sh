#!/bin/bash
# NetHack Encyclopedia - PDF Build Script
# This script compiles all NetHack documentation markdown files into a professional PDF

set -e  # Exit on error

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="$SCRIPT_DIR"
OUTPUT_DIR="$PROJECT_ROOT/docs-output"
METADATA="$BUILD_DIR/metadata.yaml"
OUTPUT_FILE="$OUTPUT_DIR/NetHack-Encyclopedia.pdf"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}NetHack Encyclopedia - PDF Builder${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Check for pandoc
if ! command -v pandoc &> /dev/null; then
    echo -e "${RED}Error: pandoc is not installed${NC}"
    echo "Please install pandoc: https://pandoc.org/installing.html"
    exit 1
fi

echo -e "${GREEN}✓${NC} Found pandoc: $(pandoc --version | head -n 1)"

# Check for LaTeX (required for PDF generation)
if ! command -v pdflatex &> /dev/null && ! command -v xelatex &> /dev/null; then
    echo -e "${RED}Error: LaTeX is not installed${NC}"
    echo "Please install a LaTeX distribution:"
    echo "  - Ubuntu/Debian: sudo apt-get install texlive-xetex texlive-fonts-recommended texlive-latex-recommended"
    echo "  - macOS: brew install --cask mactex"
    echo "  - Windows: Install MiKTeX or TeX Live"
    exit 1
fi

if command -v xelatex &> /dev/null; then
    echo -e "${GREEN}✓${NC} Found XeLaTeX: $(xelatex --version | head -n 1)"
    PDF_ENGINE="xelatex"
elif command -v pdflatex &> /dev/null; then
    echo -e "${GREEN}✓${NC} Found pdfLaTeX: $(pdflatex --version | head -n 1)"
    PDF_ENGINE="pdflatex"
fi
echo ""

# Define source files in logical order
SOURCES=(
    "$PROJECT_ROOT/README_BESTIARY.md"
    "$PROJECT_ROOT/MONSTER_CATALOG_COMPLETE.md"
    "$PROJECT_ROOT/NETHACK_ITEM_COMPENDIUM.md"
)

# Verify all source files exist
echo -e "${YELLOW}Checking source files...${NC}"
ALL_EXIST=true
for file in "${SOURCES[@]}"; do
    if [ -f "$file" ]; then
        SIZE=$(du -h "$file" | cut -f1)
        echo -e "  ${GREEN}✓${NC} $(basename "$file") ($SIZE)"
    else
        echo -e "  ${RED}✗${NC} Missing: $file"
        ALL_EXIST=false
    fi
done
echo ""

if [ "$ALL_EXIST" = false ]; then
    echo -e "${RED}Error: Some source files are missing${NC}"
    exit 1
fi

# Check metadata file
if [ ! -f "$METADATA" ]; then
    echo -e "${RED}Error: Metadata file not found: $METADATA${NC}"
    exit 1
fi

# Build PDF
echo -e "${YELLOW}Building PDF with $PDF_ENGINE...${NC}"
echo -e "${BLUE}Output:${NC} $OUTPUT_FILE"
echo ""
echo -e "${YELLOW}Note: This may take a few minutes...${NC}"
echo ""

pandoc \
    --from=markdown \
    --to=pdf \
    --pdf-engine="$PDF_ENGINE" \
    --standalone \
    --metadata-file="$METADATA" \
    --toc \
    --toc-depth=3 \
    --number-sections \
    --highlight-style=tango \
    --variable=linkcolor:blue \
    --variable=urlcolor:blue \
    --variable=toccolor:black \
    --variable=geometry:top=1in \
    --variable=geometry:bottom=1in \
    --variable=geometry:left=1.25in \
    --variable=geometry:right=1in \
    --variable=fontsize:11pt \
    --variable=documentclass:book \
    --variable=classoption:twoside \
    --variable=classoption:openright \
    --variable=linestretch:1.2 \
    --output="$OUTPUT_FILE" \
    "${SOURCES[@]}"

if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}✓ PDF build successful!${NC}"
    echo -e "${GREEN}========================================${NC}"
    echo ""
    OUTPUT_SIZE=$(du -h "$OUTPUT_FILE" | cut -f1)
    PAGES=$(pdfinfo "$OUTPUT_FILE" 2>/dev/null | grep "Pages:" | awk '{print $2}' || echo "unknown")
    echo -e "${BLUE}Output file:${NC} $OUTPUT_FILE"
    echo -e "${BLUE}File size:${NC} $OUTPUT_SIZE"
    if [ "$PAGES" != "unknown" ]; then
        echo -e "${BLUE}Pages:${NC} $PAGES"
    fi
    echo ""
    echo -e "You can now view this PDF with any PDF reader."
    echo -e "Recommended for printing: Use duplex/two-sided printing for book-style layout"
else
    echo -e "${RED}Error: PDF build failed${NC}"
    exit 1
fi
