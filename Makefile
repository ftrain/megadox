# Megadox - Root Makefile
# Build all documentation projects

# Terminal colors
RED     := \033[0;31m
GREEN   := \033[0;32m
YELLOW  := \033[1;33m
BLUE    := \033[0;34m
MAGENTA := \033[0;35m
CYAN    := \033[0;36m
BOLD    := \033[1m
NC      := \033[0m

# Projects
PROJECTS := \
	emacs \
	libsignal/encyclopedia \
	unix-history-repo/docs \
	postgresql \
	nethack \
	surge

# Output directory for collected artifacts
DIST_DIR := dist

.PHONY: all clean check help stats $(PROJECTS)

# Default target - build all projects
all:
	@echo ""
	@echo -e "$(CYAN)╔═══════════════════════════════════════════════════════════════════╗$(NC)"
	@echo -e "$(CYAN)║$(NC)  $(BOLD)Megadox Documentation Build System$(NC)                             $(CYAN)║$(NC)"
	@echo -e "$(CYAN)║$(NC)  Building $(BOLD)$(words $(PROJECTS)) documentation projects$(NC)                          $(CYAN)║$(NC)"
	@echo -e "$(CYAN)╚═══════════════════════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@for project in $(PROJECTS); do \
		echo -e "$(MAGENTA)━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━$(NC)"; \
		echo -e "$(BOLD)Building: $$project$(NC)"; \
		echo -e "$(MAGENTA)━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━$(NC)"; \
		$(MAKE) -C $$project all || exit 1; \
	done
	@$(MAKE) collect
	@echo ""
	@echo -e "$(GREEN)╔═══════════════════════════════════════════════════════════════════╗$(NC)"
	@echo -e "$(GREEN)║$(NC)  $(BOLD)✓ All documentation projects built successfully!$(NC)               $(GREEN)║$(NC)"
	@echo -e "$(GREEN)╚═══════════════════════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@echo -e "$(CYAN)Output files collected in:$(NC) $(DIST_DIR)/"
	@echo ""

# Build individual projects
$(PROJECTS):
	@echo -e "$(BOLD)Building $@...$(NC)"
	@$(MAKE) -C $@ all

# Collect all generated files into dist directory
collect:
	@echo ""
	@echo -e "$(CYAN)Collecting generated files...$(NC)"
	@mkdir -p $(DIST_DIR)
	@for project in $(PROJECTS); do \
		if [ -d $$project/output ]; then \
			cp -v $$project/output/*.epub $(DIST_DIR)/ 2>/dev/null || true; \
			cp -v $$project/output/*.pdf $(DIST_DIR)/ 2>/dev/null || true; \
		fi \
	done
	@echo -e "$(GREEN)✓$(NC) Files collected in $(DIST_DIR)/"
	@echo ""
	@echo -e "$(BOLD)Generated files:$(NC)"
	@ls -lh $(DIST_DIR)/*.{epub,pdf} 2>/dev/null | awk '{printf "  %s  %s\n", $$5, $$9}' || echo "  No files found"

# Clean all projects
clean:
	@echo -e "$(YELLOW)Cleaning all projects...$(NC)"
	@for project in $(PROJECTS); do \
		echo -e "$(BLUE)→$(NC) Cleaning $$project"; \
		$(MAKE) -C $$project clean 2>/dev/null || true; \
	done
	@rm -rf $(DIST_DIR)
	@echo -e "$(GREEN)✓$(NC) All projects cleaned"

# Check dependencies for all projects
check:
	@echo ""
	@echo -e "$(CYAN)╔═══════════════════════════════════════════════════════════════════╗$(NC)"
	@echo -e "$(CYAN)║$(NC)  $(BOLD)Checking Build Dependencies$(NC)                                    $(CYAN)║$(NC)"
	@echo -e "$(CYAN)╚═══════════════════════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@if command -v pandoc > /dev/null 2>&1; then \
		echo -e "$(GREEN)✓$(NC) pandoc: $$(pandoc --version | head -n1)"; \
	else \
		echo -e "$(RED)✗$(NC) pandoc: NOT FOUND"; \
		echo ""; \
		echo "Install pandoc:"; \
		echo "  macOS:   brew install pandoc"; \
		echo "  Ubuntu:  sudo apt-get install pandoc"; \
		echo "  Website: https://pandoc.org/installing.html"; \
		echo ""; \
		exit 1; \
	fi
	@if command -v xelatex > /dev/null 2>&1; then \
		echo -e "$(GREEN)✓$(NC) xelatex: $$(xelatex --version | head -n1 | cut -d' ' -f1-2)"; \
	else \
		echo -e "$(YELLOW)⚠$(NC) xelatex: NOT FOUND (PDF generation will fail)"; \
		echo "  macOS:   brew install --cask mactex-no-gui"; \
		echo "  Ubuntu:  sudo apt-get install texlive-xetex texlive-fonts-recommended"; \
	fi
	@echo ""
	@echo -e "$(CYAN)Projects:$(NC)"
	@for project in $(PROJECTS); do \
		echo -e "  $(GREEN)✓$(NC) $$project"; \
	done
	@echo ""

# Show statistics for all projects
stats:
	@echo ""
	@echo -e "$(MAGENTA)╔═══════════════════════════════════════════════════════════════════╗$(NC)"
	@echo -e "$(MAGENTA)║$(NC)  $(BOLD)Documentation Statistics$(NC)                                       $(MAGENTA)║$(NC)"
	@echo -e "$(MAGENTA)╚═══════════════════════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@for project in $(PROJECTS); do \
		echo -e "$(BOLD)$$project:$(NC)"; \
		$(MAKE) -C $$project stats 2>/dev/null || true; \
		echo ""; \
	done

# Help message
help:
	@echo ""
	@echo -e "$(BOLD)Megadox Documentation Build System$(NC)"
	@echo ""
	@echo -e "$(CYAN)Available targets:$(NC)"
	@echo "  make all          - Build all documentation projects"
	@echo "  make check        - Check build dependencies"
	@echo "  make clean        - Clean all generated files"
	@echo "  make stats        - Show statistics for all projects"
	@echo "  make collect      - Collect all generated files to dist/"
	@echo "  make <project>    - Build specific project"
	@echo "  make help         - Show this help message"
	@echo ""
	@echo -e "$(CYAN)Available projects:$(NC)"
	@for project in $(PROJECTS); do \
		echo "  - $$project"; \
	done
	@echo ""
	@echo -e "$(CYAN)Requirements:$(NC)"
	@echo "  - pandoc (3.0+)"
	@echo "  - XeLaTeX (for PDF generation)"
	@echo ""
	@echo -e "$(CYAN)Examples:$(NC)"
	@echo "  make all              # Build everything"
	@echo "  make emacs            # Build only emacs documentation"
	@echo "  make check            # Check if dependencies are installed"
	@echo "  make clean            # Clean all build artifacts"
	@echo ""
