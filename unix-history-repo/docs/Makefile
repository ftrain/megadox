# Makefile for PDP-7 Unix Documentation
# Generates elegant EPUB and PDF from markdown sources

# Project configuration
PROJECT_NAME := pdp7-unix-complete-reference
TITLE := PDP-7 Unix: A Complete Reference

# Output configuration
OUTPUT_DIR := output
METADATA := metadata.yaml

# Source files in reading order
FRONTMATTER := 00-frontmatter.md
README := README.md

CHAPTERS := \
	chapters/01-introduction.md \
	chapters/02-hardware.md \
	chapters/03-assembly.md \
	chapters/04-architecture.md \
	chapters/05-boot-initialization.md \
	chapters/06-filesystem.md \
	chapters/07-process-management.md \
	chapters/08-development-tools.md \
	chapters/09-user-utilities.md \
	chapters/10-b-language.md \
	chapters/11-legacy.md

APPENDICES := \
	appendices/glossary.md

# All markdown sources
SOURCES := $(FRONTMATTER) $(CHAPTERS) $(APPENDICES)

# Output files
EPUB_FILE := $(OUTPUT_DIR)/$(PROJECT_NAME).epub
PDF_FILE := $(OUTPUT_DIR)/$(PROJECT_NAME).pdf
HTML_FILE := $(OUTPUT_DIR)/$(PROJECT_NAME).html

# Include common build infrastructure
include ../../build.mk

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
		--metadata subtitle:"The Original Unix System on the PDP-7" \
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
