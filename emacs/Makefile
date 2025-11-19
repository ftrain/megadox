# Makefile for GNU Emacs Internals Documentation
# Generates elegant EPUB and PDF from markdown sources

# Project configuration
PROJECT_NAME := gnu-emacs-internals
TITLE := GNU Emacs Internals: An Encyclopedic Guide

# Output configuration
OUTPUT_DIR := output
METADATA := metadata.yaml

# Source files in reading order
SOURCES := \
	README.md \
	00-introduction/01-welcome.md \
	00-introduction/README.md \
	01-architecture/README.md \
	01-architecture/02-design-philosophy.md \
	02-core-subsystems/01-buffer-management.md \
	02-core-subsystems/02-display-engine.md \
	02-core-subsystems/03-keyboard-events.md \
	02-core-subsystems/04-process-io.md \
	02-core-subsystems/05-file-io-encoding.md \
	03-elisp-runtime/01-interpreter-core.md \
	03-elisp-runtime/02-memory-management.md \
	04-major-subsystems/01-org-mode.md \
	04-major-subsystems/02-gnus.md \
	04-major-subsystems/03-version-control.md \
	04-major-subsystems/04-cedet.md \
	04-major-subsystems/05-calc.md \
	05-display-engine/README.md \
	06-platform-support/01-abstraction-layer.md \
	07-window-systems/01-x11-integration.md \
	08-elisp-library/01-standard-library.md \
	09-text-processing/01-search-and-regex.md \
	17-development/01-build-and-testing.md \
	18-development-practices/01-coding-evolution.md \
	19-industry-context/01-technology-trends.md \
	20-comparative-analysis/README.md \
	20-comparative-analysis/01-editor-comparison.md \
	ARCHITECTURE.md \
	BUILD.md \
	GLOSSARY.md \
	INDEX.md

# Output files
EPUB_FILE := $(OUTPUT_DIR)/$(PROJECT_NAME).epub
PDF_FILE := $(OUTPUT_DIR)/$(PROJECT_NAME).pdf
HTML_FILE := $(OUTPUT_DIR)/$(PROJECT_NAME).html

# Include common build infrastructure
include ../build.mk

# Default target
all: check-deps $(EPUB_FILE) $(PDF_FILE)
	$(call print_header,Build Complete: $(TITLE))
	@echo -e "$(BOLD)Output files:$(NC)"
	@echo -e "  $(GREEN)EPUB:$(NC) $(EPUB_FILE) ($$($(call show_size,$(EPUB_FILE))))"
	@echo -e "  $(GREEN)PDF:$(NC)  $(PDF_FILE) ($$($(call show_size,$(PDF_FILE))))"
	@echo ""

# Build EPUB
$(EPUB_FILE): $(SOURCES) $(METADATA) | $(OUTPUT_DIR)
	$(call print_header,Building EPUB: $(TITLE))
	$(call print_info,Processing $(words $(SOURCES)) source files...)
	@$(PANDOC) $(PANDOC_COMMON) $(PANDOC_EPUB) \
		--metadata-file=$(METADATA) \
		--output=$@ \
		$(SOURCES)
	$(call print_success,EPUB created: $@ ($$($(call show_size,$@))))
	@echo ""

# Build PDF
$(PDF_FILE): $(SOURCES) $(METADATA) | $(OUTPUT_DIR)
	$(call print_header,Building PDF: $(TITLE))
	$(call print_info,Processing $(words $(SOURCES)) source files...)
	$(call print_info,This may take several minutes...)
	@$(PANDOC) $(PANDOC_COMMON) $(PANDOC_PDF) \
		--metadata-file=$(METADATA) \
		--output=$@ \
		$(SOURCES)
	$(call print_success,PDF created: $@ ($$($(call show_size,$@))))
	@if command -v pdfinfo > /dev/null 2>&1; then \
		PAGES=$$(pdfinfo $@ 2>/dev/null | grep 'Pages:' | awk '{print $$2}'); \
		if [ -n "$$PAGES" ]; then \
			echo -e "$(BLUE)→$(NC) Pages: $$PAGES"; \
		fi; \
	fi
	@echo ""

# Build HTML
$(HTML_FILE): $(SOURCES) | $(OUTPUT_DIR)
	$(call print_header,Building HTML: $(TITLE))
	$(call print_info,Processing $(words $(SOURCES)) source files...)
	@$(PANDOC) $(PANDOC_COMMON) $(PANDOC_HTML) \
		--metadata title:"$(TITLE)" \
		--metadata subtitle:"An Encyclopedic Guide to the Architecture and Implementation" \
		--metadata pdf-url:"$(PROJECT_NAME).pdf" \
		--output=$@ \
		$(SOURCES)
	$(call print_success,HTML created: $@ ($$($(call show_size,$@))))
	@echo ""

# Individual format targets
epub: $(EPUB_FILE)
pdf: $(PDF_FILE)
html: $(HTML_FILE)

# Statistics
stats: stats-common

# Clean
clean: clean-common

# Help
help: help-common

.PHONY: all epub pdf html stats clean help
