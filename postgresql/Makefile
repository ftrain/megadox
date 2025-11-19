# Makefile for PostgreSQL Encyclopedia
# Generates elegant EPUB and PDF from markdown sources

# Project configuration
PROJECT_NAME := postgresql-encyclopedia
TITLE := PostgreSQL Internals: An Encyclopedia

# Output configuration
OUTPUT_DIR := output
METADATA := metadata.yaml

# Source files in reading order
SOURCES := \
	README.md \
	00-introduction.md \
	01-storage/storage-layer.md \
	02-query-processing/query-pipeline.md \
	03-transactions/transaction-management.md \
	04-replication/replication-recovery.md \
	05-processes/process-architecture.md \
	06-extensions/extension-system.md \
	07-utilities/utility-programs.md \
	08-evolution/historical-evolution.md \
	09-build-system-and-portability.md \
	09-culture/community-culture.md \
	10-sql-reference/sql-features.md \
	appendices/glossary.md \
	appendices/index.md \
	appendices/bibliography.md

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
		--output=$@ \
		$(SOURCES)
	$(call print_success,EPUB created: $@ ($$($(call show_size,$@))))
	@echo ""

# Build PDF
$(PDF_FILE): $(SOURCES) | $(OUTPUT_DIR)
	$(call print_header,Building PDF: $(TITLE))
	$(call print_info,Processing $(words $(SOURCES)) source files...)
	$(call print_info,This may take several minutes...)
	@$(PANDOC) $(PANDOC_COMMON) $(PANDOC_PDF) \
		--metadata=title:"$(TITLE)" \
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
		--metadata=title:"$(TITLE)" \
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
