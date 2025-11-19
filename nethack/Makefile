# Makefile for NetHack Encyclopedia
# Generates elegant EPUB and PDF from markdown sources

# Project configuration
PROJECT_NAME := nethack-encyclopedia
TITLE := NetHack Encyclopedia: A Complete Guide

# Output configuration
OUTPUT_DIR := output
METADATA := metadata.yaml

# Source files in reading order
SOURCES := \
	../AI-GENERATED-DISCLAIMER.md \
	README.md \
	INTRODUCTION.md \
	README_BESTIARY.md \
	MONSTER_CATALOG_COMPLETE.md \
	NETHACK_ITEM_COMPENDIUM.md \
	CHARACTER_GUIDE.md \
	STRATEGY_GUIDE.md \
	GAME_MECHANICS.md \
	DUNGEON_GENERATION.md \
	CODEBASE_ARCHITECTURE.md \
	GIT_HISTORY.md \
	CULTURAL_HISTORY.md \
	GLOSSARY.md \
	INDEX.md \
	BIBLIOGRAPHY.md

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
$(EPUB_FILE): $(SOURCES) | $(OUTPUT_DIR)
	$(call print_header,Building EPUB: $(TITLE))
	$(call print_info,Processing $(words $(SOURCES)) source files...)
	@$(PANDOC) $(PANDOC_COMMON) $(PANDOC_EPUB) \
		--metadata=title:"$(TITLE)" \
		--output=$@ $(SOURCES)
	$(call print_success,EPUB created: $@ ($$($(call show_size,$@))))
	@echo ""

# Build PDF
$(PDF_FILE): $(SOURCES) | $(OUTPUT_DIR)
	$(call print_header,Building PDF: $(TITLE))
	$(call print_info,Processing $(words $(SOURCES)) source files...)
	$(call print_info,This may take several minutes...)
	@$(PANDOC) $(PANDOC_COMMON) $(PANDOC_PDF) \
		--metadata=title:"$(TITLE)" \
		--output=$@ $(SOURCES)
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
		--metadata subtitle:"A Complete Guide to the Legendary Roguelike" \
		--metadata pdf-url:"$(PROJECT_NAME).pdf" \
		--output=$@ \
		$(SOURCES)
	$(call print_info,Transforming TOC to use native <details> elements...)
	@$(TRANSFORM_TOC) $@
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
