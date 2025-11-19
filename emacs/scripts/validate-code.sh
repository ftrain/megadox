#!/bin/bash

################################################################################
# validate-code.sh - Validate code file references in documentation
#
# This script finds code file references (e.g., src/buffer.c, lisp/core.el)
# in documentation and verifies that the referenced files exist.
#
# Usage: ./validate-code.sh
#
# Exit codes:
#   0 - All referenced files exist
#   1 - One or more referenced files are missing
################################################################################

set -o pipefail

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DOCS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PARENT_DIR="$(cd "$DOCS_DIR/.." && pwd)"
TEMP_FILE=$(mktemp)
TEMP_FOUND=$(mktemp)
MISSING_FILES=0
FOUND_FILES=0
CHECKED_FILES=0

# Cleanup on exit
cleanup() {
    rm -f "$TEMP_FILE" "$TEMP_FOUND"
}
trap cleanup EXIT

# Print error message
error() {
    echo -e "${RED}ERROR:${NC} $1" >&2
}

# Print warning message
warning() {
    echo -e "${YELLOW}WARNING:${NC} $1" >&2
}

# Print success message
success() {
    echo -e "${GREEN}✓${NC} $1"
}

# Print info message
info() {
    echo -e "ℹ $1"
}

# Validate code file reference
validate_file() {
    local file_path="$1"
    local source_doc="$2"
    local line_num="$3"

    CHECKED_FILES=$((CHECKED_FILES + 1))

    # Check in PARENT_DIR (emacs root)
    local full_path="$PARENT_DIR/$file_path"

    if [[ -f "$full_path" ]]; then
        FOUND_FILES=$((FOUND_FILES + 1))
        echo "$file_path" >> "$TEMP_FOUND"
        return 0
    fi

    # Also check in docs directory in case it's relative to docs
    local docs_relative="$DOCS_DIR/$file_path"
    if [[ -f "$docs_relative" ]]; then
        FOUND_FILES=$((FOUND_FILES + 1))
        echo "$file_path" >> "$TEMP_FOUND"
        return 0
    fi

    error "Missing file reference in $source_doc:$line_num: $file_path"
    MISSING_FILES=$((MISSING_FILES + 1))
    echo "$source_doc:$line_num:$file_path" >> "$TEMP_FILE"
    return 1
}

# Extract and validate code file references
main() {
    echo "Validating code file references in documentation..."
    echo "Documentation directory: $DOCS_DIR"
    echo "Emacs root directory: $PARENT_DIR"
    echo ""

    # Check if docs directory exists
    if [[ ! -d "$DOCS_DIR" ]]; then
        error "Documentation directory not found: $DOCS_DIR"
        return 1
    fi

    # Patterns to match code file references
    # Matches: src/file.c, lisp/file.el, etc.
    local patterns=(
        'src/[a-zA-Z0-9._-]+\.[ch]'
        'lisp/[a-zA-Z0-9._/-]+\.el'
        'doc/man/[a-zA-Z0-9._/-]+\.(texi|txt)'
    )

    local file_count=0

    # Find all potential code file references
    for pattern in "${patterns[@]}"; do
        while IFS=: read -r file line_num content; do
            # Skip empty lines
            [[ -z "$file" ]] && continue

            # Extract file paths matching common code directory patterns
            # This regex looks for paths like: src/foo.c, lisp/foo.el, etc.
            while [[ $content =~ ([a-zA-Z0-9_-]+/[a-zA-Z0-9._/-]+\.[a-z]+) ]]; do
                local match="${BASH_REMATCH[1]}"
                content="${content#*"$match"}"

                # Only validate known code directories
                if [[ "$match" =~ ^(src|lisp|doc/man)/ ]]; then
                    validate_file "$match" "$file" "$line_num"
                    file_count=$((file_count + 1))
                fi
            done
        done < <(grep -rn "src/\|lisp/\|doc/man/" "$DOCS_DIR" --include="*.md" 2>/dev/null)
    done

    # Additional pattern: Inline code references like @file: path
    while IFS=: read -r file line_num content; do
        [[ -z "$file" ]] && continue

        if [[ $content =~ @file:[[:space:]]*([a-zA-Z0-9._/-]+\.[a-z]+) ]]; then
            local match="${BASH_REMATCH[1]}"
            validate_file "$match" "$file" "$line_num"
            file_count=$((file_count + 1))
        fi
    done < <(grep -rn "@file:" "$DOCS_DIR" --include="*.md" 2>/dev/null)

    echo ""
    echo "Code File Reference Validation Results:"
    echo "========================================"
    echo "Total file references checked: $CHECKED_FILES"
    echo -e "Files found: ${GREEN}$FOUND_FILES${NC}"
    echo -e "Missing files: ${RED}$MISSING_FILES${NC}"
    echo ""

    if [[ $MISSING_FILES -gt 0 ]]; then
        echo "Missing file references:"
        echo "========================"
        cat "$TEMP_FILE" | sort | uniq | while IFS=: read -r doc line path; do
            echo -e "  ${YELLOW}$path${NC}"
            echo "    Referenced in: $doc:$line"
        done
        return 1
    else
        if [[ $FOUND_FILES -gt 0 ]]; then
            success "All $FOUND_FILES referenced files exist!"
        else
            info "No code file references found in documentation"
        fi
        return 0
    fi
}

# Run main function
main
exit $?
