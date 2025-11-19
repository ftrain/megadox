#!/bin/bash
# NetHack Encyclopedia - EPUB Build Script
# This script compiles all NetHack documentation markdown files into a single EPUB

set -e  # Exit on error

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="$SCRIPT_DIR"
OUTPUT_DIR="$PROJECT_ROOT/docs-output"
METADATA="$BUILD_DIR/metadata.yaml"
CSS="$BUILD_DIR/epub.css"
OUTPUT_FILE="$OUTPUT_DIR/NetHack-Encyclopedia.epub"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}NetHack Encyclopedia - EPUB Builder${NC}"
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

# Build EPUB
echo -e "${YELLOW}Building EPUB...${NC}"
echo -e "${BLUE}Output:${NC} $OUTPUT_FILE"
echo ""

pandoc \
    --from=markdown \
    --to=epub3 \
    --standalone \
    --metadata-file="$METADATA" \
    --css="$CSS" \
    --toc \
    --toc-depth=3 \
    --epub-embed-font=/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf \
    --epub-embed-font=/usr/share/fonts/truetype/dejavu/DejaVuSerif-Bold.ttf \
    --epub-embed-font=/usr/share/fonts/truetype/dejavu/DejaVuSerif-Italic.ttf \
    --epub-embed-font=/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf \
    --number-sections \
    --output="$OUTPUT_FILE" \
    "${SOURCES[@]}"

if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}✓ EPUB build successful!${NC}"
    echo -e "${GREEN}========================================${NC}"
    echo ""
    OUTPUT_SIZE=$(du -h "$OUTPUT_FILE" | cut -f1)
    echo -e "${BLUE}Output file:${NC} $OUTPUT_FILE"
    echo -e "${BLUE}File size:${NC} $OUTPUT_SIZE"
    echo ""
    echo -e "You can now read this EPUB on any e-reader or EPUB reader app."
    echo -e "Recommended readers: Calibre, Apple Books, Google Play Books, Adobe Digital Editions"
else
    echo -e "${RED}Error: EPUB build failed${NC}"
    exit 1
fi
