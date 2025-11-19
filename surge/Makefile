# Makefile for Surge XT Encyclopedia
# Generates elegant EPUB, PDF, and HTML from markdown sources

# Project configuration
PROJECT_NAME := surge-xt-encyclopedia
TITLE := The Surge XT Synthesizer: An Encyclopedic Guide

# Output configuration
OUTPUT_DIR := output
METADATA := metadata.yaml

# Source files in reading order
SOURCES := \
	../AI-GENERATED-DISCLAIMER.md \
	00-INDEX.md \
	01-architecture-overview.md \
	02-core-data-structures.md \
	03-synthesis-pipeline.md \
	04-voice-architecture.md \
	05-oscillators-overview.md \
	06-oscillators-classic.md \
	07-oscillators-wavetable.md \
	08-oscillators-fm.md \
	09-oscillators-advanced.md \
	10-filter-theory.md \
	11-filter-implementation.md \
	12-effects-architecture.md \
	13-effects-time-based.md \
	14-effects-reverb.md \
	15-effects-distortion.md \
	16-effects-frequency.md \
	17-effects-integration.md \
	18-modulation-architecture.md \
	19-envelopes.md \
	20-lfos.md \
	21-mseg.md \
	22-formula-modulation.md \
	23-gui-architecture.md \
	24-widgets.md \
	25-overlay-editors.md \
	26-skinning.md \
	27-patch-system.md \
	28-preset-management.md \
	29-resource-management.md \
	30-microtuning.md \
	31-midi-mpe.md \
	32-simd-optimization.md \
	33-plugin-architecture.md \
	34-testing.md \
	35-osc.md \
	36-python-bindings.md \
	37-build-system.md \
	38-adding-features.md \
	39-performance.md \
	appendix-a-dsp-math.md \
	appendix-b-glossary.md \
	appendix-c-code-reference.md \
	appendix-d-bibliography.md \
	appendix-e-pandoc.md \
	appendix-f-evolution.md

# Output files
EPUB_FILE := $(OUTPUT_DIR)/$(PROJECT_NAME).epub
PDF_FILE := $(OUTPUT_DIR)/$(PROJECT_NAME).pdf
HTML_FILE := $(OUTPUT_DIR)/$(PROJECT_NAME).html

# Include common build infrastructure
include ../build.mk

# Default target
all: check-deps $(EPUB_FILE) $(PDF_FILE) $(HTML_FILE)
	$(call print_header,Build Complete: $(TITLE))
	@echo -e "$(BOLD)Output files:$(NC)"
	@echo -e "  $(GREEN)EPUB:$(NC) $(EPUB_FILE) ($$($(call show_size,$(EPUB_FILE))))"
	@echo -e "  $(GREEN)PDF:$(NC)  $(PDF_FILE) ($$($(call show_size,$(PDF_FILE))))"
	@echo -e "  $(GREEN)HTML:$(NC) $(HTML_FILE) ($$($(call show_size,$(HTML_FILE))))"
	@echo ""

# Build EPUB
$(EPUB_FILE): $(SOURCES) | $(OUTPUT_DIR)
	$(call print_header,Building EPUB: $(TITLE))
	$(call print_info,Processing $(words $(SOURCES)) source files...)
	@$(PANDOC) $(PANDOC_COMMON) $(PANDOC_EPUB) \
		--metadata=title:"$(TITLE)" \
		--metadata=author:"Surge Synth Team" \
		--metadata=rights:"GPL-3.0" \
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
		--metadata=author:"Surge Synth Team" \
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
		--metadata subtitle:"A Deep Dive into Software Synthesis" \
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
