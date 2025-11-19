# NetHack Encyclopedia Build System
# Makefile for building EPUB and PDF documentation

# Configuration
PROJECT_ROOT := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
PARENT_DIR := $(shell dirname $(PROJECT_ROOT))
BUILD_DIR := $(PROJECT_ROOT)
OUTPUT_DIR := $(PARENT_DIR)/docs-output
METADATA := $(BUILD_DIR)/metadata.yaml
CSS := $(BUILD_DIR)/epub.css

# Source files (in reading order)
SOURCES := $(PARENT_DIR)/README_BESTIARY.md \
           $(PARENT_DIR)/MONSTER_CATALOG_COMPLETE.md \
           $(PARENT_DIR)/NETHACK_ITEM_COMPENDIUM.md

# Output files
EPUB_OUTPUT := $(OUTPUT_DIR)/NetHack-Encyclopedia.epub
PDF_OUTPUT := $(OUTPUT_DIR)/NetHack-Encyclopedia.pdf

# Pandoc options
PANDOC := pandoc
PANDOC_COMMON_OPTS := --from=markdown \
                      --standalone \
                      --metadata-file=$(METADATA) \
                      --toc \
                      --toc-depth=3 \
                      --number-sections

PANDOC_EPUB_OPTS := --to=epub3 \
                    --css=$(CSS) \
                    --epub-embed-font=/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf \
                    --epub-embed-font=/usr/share/fonts/truetype/dejavu/DejaVuSerif-Bold.ttf \
                    --epub-embed-font=/usr/share/fonts/truetype/dejavu/DejaVuSerif-Italic.ttf \
                    --epub-embed-font=/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf

PANDOC_PDF_OPTS := --to=pdf \
                   --pdf-engine=xelatex \
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
                   --variable=linestretch:1.2

# Colors for output
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[1;33m
BLUE := \033[0;34m
NC := \033[0m # No Color

# Default target
.PHONY: all
all: epub pdf
	@echo ""
	@echo -e "$(GREEN)========================================$(NC)"
	@echo -e "$(GREEN)✓ All documentation built successfully!$(NC)"
	@echo -e "$(GREEN)========================================$(NC)"
	@echo ""
	@echo -e "$(BLUE)Output files:$(NC)"
	@ls -lh $(OUTPUT_DIR)/NetHack-Encyclopedia.* | awk '{print "  - " $$9 " (" $$5 ")"}'
	@echo ""

# Build EPUB
.PHONY: epub
epub: $(EPUB_OUTPUT)

$(EPUB_OUTPUT): $(SOURCES) $(METADATA) $(CSS)
	@echo -e "$(BLUE)========================================$(NC)"
	@echo -e "$(BLUE)Building EPUB...$(NC)"
	@echo -e "$(BLUE)========================================$(NC)"
	@mkdir -p $(OUTPUT_DIR)
	@$(PANDOC) $(PANDOC_COMMON_OPTS) $(PANDOC_EPUB_OPTS) \
		--output=$@ $(SOURCES)
	@echo -e "$(GREEN)✓ EPUB created: $@$(NC)"
	@echo -e "$(BLUE)  Size: $$(du -h $@ | cut -f1)$(NC)"
	@echo ""

# Build PDF
.PHONY: pdf
pdf: $(PDF_OUTPUT)

$(PDF_OUTPUT): $(SOURCES) $(METADATA)
	@echo -e "$(BLUE)========================================$(NC)"
	@echo -e "$(BLUE)Building PDF (this may take a while...)$(NC)"
	@echo -e "$(BLUE)========================================$(NC)"
	@mkdir -p $(OUTPUT_DIR)
	@$(PANDOC) $(PANDOC_COMMON_OPTS) $(PANDOC_PDF_OPTS) \
		--output=$@ $(SOURCES)
	@echo -e "$(GREEN)✓ PDF created: $@$(NC)"
	@echo -e "$(BLUE)  Size: $$(du -h $@ | cut -f1)$(NC)"
	@if command -v pdfinfo > /dev/null 2>&1; then \
		echo -e "$(BLUE)  Pages: $$(pdfinfo $@ 2>/dev/null | grep 'Pages:' | awk '{print $$2}')$(NC)"; \
	fi
	@echo ""

# Clean build artifacts
.PHONY: clean
clean:
	@echo -e "$(YELLOW)Cleaning output files...$(NC)"
	@rm -rf $(OUTPUT_DIR)
	@echo -e "$(GREEN)✓ Clean complete$(NC)"

# Check dependencies
.PHONY: check
check:
	@echo -e "$(BLUE)========================================$(NC)"
	@echo -e "$(BLUE)Checking build dependencies...$(NC)"
	@echo -e "$(BLUE)========================================$(NC)"
	@echo ""
	@echo -e "$(YELLOW)Required tools:$(NC)"
	@if command -v pandoc > /dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) pandoc: $$(pandoc --version | head -n 1)"; \
	else \
		echo -e "  $(RED)✗$(NC) pandoc: NOT FOUND"; \
		echo -e "    Install: https://pandoc.org/installing.html"; \
	fi
	@if command -v xelatex > /dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) xelatex: $$(xelatex --version | head -n 1 | cut -d' ' -f1-2)"; \
	elif command -v pdflatex > /dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) pdflatex: $$(pdflatex --version | head -n 1 | cut -d' ' -f1-2)"; \
	else \
		echo -e "  $(RED)✗$(NC) LaTeX: NOT FOUND"; \
		echo -e "    Install: sudo apt-get install texlive-xetex texlive-fonts-recommended"; \
	fi
	@echo ""
	@echo -e "$(YELLOW)Source files:$(NC)"
	@for file in $(SOURCES); do \
		if [ -f $$file ]; then \
			echo -e "  $(GREEN)✓$(NC) $$(basename $$file) ($$(du -h $$file | cut -f1))"; \
		else \
			echo -e "  $(RED)✗$(NC) Missing: $$file"; \
		fi \
	done
	@echo ""
	@echo -e "$(YELLOW)Build files:$(NC)"
	@if [ -f $(METADATA) ]; then \
		echo -e "  $(GREEN)✓$(NC) metadata.yaml"; \
	else \
		echo -e "  $(RED)✗$(NC) metadata.yaml NOT FOUND"; \
	fi
	@if [ -f $(CSS) ]; then \
		echo -e "  $(GREEN)✓$(NC) epub.css"; \
	else \
		echo -e "  $(RED)✗$(NC) epub.css NOT FOUND"; \
	fi
	@echo ""

# Display help information
.PHONY: help
help:
	@echo -e "$(BLUE)========================================$(NC)"
	@echo -e "$(BLUE)NetHack Encyclopedia Build System$(NC)"
	@echo -e "$(BLUE)========================================$(NC)"
	@echo ""
	@echo -e "$(YELLOW)Available targets:$(NC)"
	@echo ""
	@echo -e "  $(GREEN)make all$(NC)     - Build both EPUB and PDF (default)"
	@echo -e "  $(GREEN)make epub$(NC)    - Build EPUB e-book only"
	@echo -e "  $(GREEN)make pdf$(NC)     - Build PDF document only"
	@echo -e "  $(GREEN)make check$(NC)   - Check build dependencies and source files"
	@echo -e "  $(GREEN)make clean$(NC)   - Remove all generated output files"
	@echo -e "  $(GREEN)make help$(NC)    - Display this help message"
	@echo ""
	@echo -e "$(YELLOW)Output directory:$(NC)"
	@echo -e "  $(OUTPUT_DIR)/"
	@echo ""
	@echo -e "$(YELLOW)Requirements:$(NC)"
	@echo -e "  - pandoc (https://pandoc.org/)"
	@echo -e "  - XeLaTeX or pdfLaTeX (for PDF generation)"
	@echo ""
	@echo -e "For detailed build instructions, see: README_BUILD.md"
	@echo ""

# Declare phony targets that don't represent files
.PHONY: all epub pdf clean check help
