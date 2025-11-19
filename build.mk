# Common Build Infrastructure for Megadox Documentation
# This file provides shared build rules for generating elegant EPUB and PDF documents
# Include this file in project-specific Makefiles

# Terminal colors
RED     := \033[0;31m
GREEN   := \033[0;32m
YELLOW  := \033[1;33m
BLUE    := \033[0;34m
MAGENTA := \033[0;35m
CYAN    := \033[0;36m
BOLD    := \033[1m
NC      := \033[0m

# Directory setup (must be defined first)
COMMON_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
MEGADOX_ROOT := $(abspath $(COMMON_DIR))
OUTPUT_DIR ?= output
BUILD_DIR ?= .build

# Build tools
PANDOC := pandoc
PANDOC_VERSION := $(shell $(PANDOC) --version 2>/dev/null | head -n1)

# Common pandoc options for all formats
PANDOC_COMMON := \
	--from markdown+smart \
	--standalone \
	--toc \
	--toc-depth=3 \
	--number-sections

# EPUB-specific options
PANDOC_EPUB := \
	--to epub3

# PDF-specific options
PANDOC_PDF := \
	--to pdf \
	--pdf-engine=xelatex \
	--variable=geometry:top=1in \
	--variable=geometry:bottom=1in \
	--variable=geometry:left=1.25in \
	--variable=geometry:right=1in \
	--variable=fontsize:11pt \
	--variable=documentclass:book \
	--variable=classoption:openany \
	--variable=linkcolor:Blue \
	--variable=urlcolor:Blue \
	--variable=toccolor:Black \
	--variable=linestretch:1.15 \
	--variable=mainfont:"Palatino" \
	--variable=monofont:"Menlo" \
	--variable=fontfamily:palatino

# HTML-specific options
PANDOC_HTML := \
	--to html5 \
	--standalone \
	--mathjax \
	--css=../book-style.css \
	--template=$(MEGADOX_ROOT)/book-template.html

# Utility functions
define print_header
	echo ""
	echo "╔════════════════════════════════════════════════════════════════╗"
	echo "║ $(1)"
	echo "╚════════════════════════════════════════════════════════════════╝"
	echo ""
endef

define print_success
	echo "✓ $(1)"
endef

define print_info
	echo "→ $(1)"
endef

define print_warning
	echo "⚠ $(1)"
endef

define print_error
	echo "✗ $(1)"
endef

# File size formatting
define show_size
	@du -h "$(1)" | cut -f1
endef

# Check dependencies
.PHONY: check-deps
check-deps:
	@$(call print_header,Checking Build Dependencies)
	@if command -v $(PANDOC) > /dev/null 2>&1; then \
		$(call print_success,pandoc: $(PANDOC_VERSION)); \
	else \
		$(call print_error,pandoc: NOT FOUND); \
		echo ""; \
		echo "Install pandoc:"; \
		echo "  macOS:   brew install pandoc"; \
		echo "  Ubuntu:  sudo apt-get install pandoc"; \
		echo "  Website: https://pandoc.org/installing.html"; \
		echo ""; \
		exit 1; \
	fi
	@if command -v xelatex > /dev/null 2>&1; then \
		$(call print_success,xelatex: $$(xelatex --version | head -n1 | cut -d' ' -f1-2)); \
	else \
		$(call print_warning,xelatex: NOT FOUND (PDF generation will fail)); \
		echo "  macOS:   brew install --cask mactex-no-gui"; \
		echo "  Ubuntu:  sudo apt-get install texlive-xetex texlive-fonts-recommended"; \
	fi
	@echo ""

# Create output directories
$(OUTPUT_DIR):
	@mkdir -p $(OUTPUT_DIR)

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

# Default clean target
.PHONY: clean-common
clean-common:
	@$(call print_info,Cleaning build artifacts...)
	@rm -rf $(OUTPUT_DIR) $(BUILD_DIR)
	@$(call print_success,Clean complete)

# Statistics target
.PHONY: stats-common
stats-common:
	@echo ""
	@echo -e "$(MAGENTA)═══════════════════════════════════════════════════════════════$(NC)"
	@echo -e "$(MAGENTA)  Documentation Statistics$(NC)"
	@echo -e "$(MAGENTA)═══════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@if [ -n "$(SOURCES)" ]; then \
		echo -e "$(CYAN)Files:$(NC)        $$(echo $(SOURCES) | wc -w | tr -d ' ')"; \
		echo -e "$(CYAN)Lines:$(NC)        $$(cat $(SOURCES) 2>/dev/null | wc -l | tr -d ' ')"; \
		echo -e "$(CYAN)Words:$(NC)        $$(cat $(SOURCES) 2>/dev/null | wc -w | tr -d ' ')"; \
		echo -e "$(CYAN)Characters:$(NC)   $$(cat $(SOURCES) 2>/dev/null | wc -c | tr -d ' ')"; \
		echo -e "$(CYAN)Total size:$(NC)   $$(du -ch $(SOURCES) 2>/dev/null | tail -1 | cut -f1)"; \
	fi
	@echo ""

# Help target
.PHONY: help-common
help-common:
	@echo ""
	@echo -e "$(BOLD)Megadox Documentation Build System$(NC)"
	@echo ""
	@echo -e "$(CYAN)Available targets:$(NC)"
	@echo "  make all         - Build all formats (EPUB and PDF)"
	@echo "  make epub        - Build EPUB only"
	@echo "  make pdf         - Build PDF only"
	@echo "  make html        - Build standalone HTML"
	@echo "  make check-deps  - Check build dependencies"
	@echo "  make stats       - Show documentation statistics"
	@echo "  make clean       - Remove generated files"
	@echo "  make help        - Show this help message"
	@echo ""
	@echo -e "$(CYAN)Requirements:$(NC)"
	@echo "  - pandoc (3.0+)"
	@echo "  - XeLaTeX (for PDF generation)"
	@echo ""

.PHONY: all epub pdf html check stats clean help
