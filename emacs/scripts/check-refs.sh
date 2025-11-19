#!/bin/bash

################################################################################
# check-refs.sh - Validate cross-references in documentation
#
# This script validates @chap: and @sec: references in markdown files.
# It checks that referenced chapters and sections exist in the documentation.
#
# Usage: ./check-refs.sh
#
# Exit codes:
#   0 - All references are valid
#   1 - Broken or invalid references found
################################################################################

set -o pipefail

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
DOCS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEMP_FILE=$(mktemp)
BROKEN_REFS=0
VALID_REFS=0
CHECKED_REFS=0

# Cleanup on exit
cleanup() {
    rm -f "$TEMP_FILE"
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

# Validate chapter reference
validate_chapter() {
    local chap_num="$1"
    local source_file="$2"
    local line_num="$3"

    CHECKED_REFS=$((CHECKED_REFS + 1))

    # Pad chapter number to 2 digits
    local padded_chap=$(printf "%02d" "$chap_num" 2>/dev/null)

    if [[ -z "$padded_chap" ]]; then
        error "Invalid chapter number format in $source_file:$line_num: @chap:$chap_num"
        BROKEN_REFS=$((BROKEN_REFS + 1))
        return 1
    fi

    # Check if chapter directory exists
    local chapter_dir=$(find "$DOCS_DIR" -maxdepth 1 -type d -name "${padded_chap}-*" 2>/dev/null | head -1)

    if [[ -z "$chapter_dir" ]]; then
        error "Broken chapter reference in $source_file:$line_num: @chap:$chap_num (no matching chapter directory)"
        BROKEN_REFS=$((BROKEN_REFS + 1))
        return 1
    fi

    VALID_REFS=$((VALID_REFS + 1))
    return 0
}

# Validate section reference
validate_section() {
    local sec_ref="$1"
    local source_file="$2"
    local line_num="$3"

    CHECKED_REFS=$((CHECKED_REFS + 1))

    # Extract chapter number from section reference (format: XX.YY.ZZ)
    local chap_num=$(echo "$sec_ref" | cut -d. -f1)

    # Validate chapter part exists
    if ! validate_chapter "$chap_num" "$source_file" "$line_num"; then
        error "Invalid section reference in $source_file:$line_num: @sec:$sec_ref"
        return 1
    fi

    # Note: Full section validation would require parsing each file's headers
    # For now, we validate the chapter exists
    VALID_REFS=$((VALID_REFS + 1))
    return 0
}

# Main validation function
main() {
    echo "Validating documentation cross-references..."
    echo "Docs directory: $DOCS_DIR"
    echo ""

    # Check if docs directory exists
    if [[ ! -d "$DOCS_DIR" ]]; then
        error "Documentation directory not found: $DOCS_DIR"
        return 1
    fi

    # Find all @chap: and @sec: references
    while IFS=: read -r file line_num content; do
        # Skip empty lines
        [[ -z "$file" ]] && continue

        # Extract reference type and number
        if [[ $content =~ @chap:([0-9]+) ]]; then
            chap_num="${BASH_REMATCH[1]}"
            if ! validate_chapter "$chap_num" "$file" "$line_num"; then
                echo "$file:$line_num" >> "$TEMP_FILE"
            fi
        elif [[ $content =~ @sec:([0-9.]+) ]]; then
            sec_ref="${BASH_REMATCH[1]}"
            if ! validate_section "$sec_ref" "$file" "$line_num"; then
                echo "$file:$line_num" >> "$TEMP_FILE"
            fi
        fi
    done < <(grep -rn "@chap:\|@sec:" "$DOCS_DIR" --include="*.md" 2>/dev/null)

    # Check if there were any broken references
    if [[ -f "$TEMP_FILE" ]] && [[ -s "$TEMP_FILE" ]]; then
        BROKEN_REFS=$(wc -l < "$TEMP_FILE")
    fi

    echo ""
    echo "Cross-Reference Validation Results:"
    echo "===================================="
    echo "Total references checked: $CHECKED_REFS"
    echo -e "Valid references: ${GREEN}$VALID_REFS${NC}"
    echo -e "Broken references: ${RED}$BROKEN_REFS${NC}"
    echo ""

    if [[ $BROKEN_REFS -gt 0 ]]; then
        echo "Broken references found at:"
        cat "$TEMP_FILE" | sort | uniq
        return 1
    else
        success "All cross-references are valid!"
        return 0
    fi
}

# Run main function
main
exit $?
