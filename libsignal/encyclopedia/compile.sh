#!/bin/bash
# Compilation script for libsignal encyclopedia

echo "=== Compiling libsignal Encyclopedia ==="
echo

# Check if pandoc is available
if ! command -v pandoc &> /dev/null; then
    echo "ERROR: pandoc is not installed"
    echo "Install with: sudo apt-get install pandoc texlive-xetex"
    exit 1
fi

echo "📚 Compiling EPUB..."
pandoc metadata.yaml \
    00-INTRODUCTION.md \
    01-TABLE-OF-CONTENTS.md \
    GLOSSARY.md \
    02-CHAPTER-01-HISTORICAL-TIMELINE.md \
    03-CHAPTER-02-CRYPTOGRAPHIC-PRIMITIVES.md \
    04-CHAPTER-03-SIGNAL-PROTOCOL.md \
    05-CHAPTER-04-LANGUAGE-BINDINGS.md \
    06-CHAPTER-05-ZERO-KNOWLEDGE.md \
    07-CHAPTER-06-NETWORK-SERVICES.md \
    08-CHAPTER-07-SESSION-ESTABLISHMENT.md \
    09-CHAPTER-08-MESSAGE-ENCRYPTION.md \
    10-CHAPTER-09-GROUP-MESSAGING.md \
    11-CHAPTER-10-TESTING-ARCHITECTURE.md \
    12-CHAPTER-11-BUILD-SYSTEM.md \
    13-CHAPTER-12-ARCHITECTURAL-EVOLUTION.md \
    14-CHAPTER-13-SEALED-SENDER.md \
    15-CHAPTER-14-MESSAGE-BACKUP.md \
    RESEARCH-DATA-SUMMARY.md \
    -o libsignal-encyclopedia.epub \
    --toc --toc-depth=3 \
    && echo "✅ EPUB created: libsignal-encyclopedia.epub"

echo
echo "📄 Compiling PDF..."
pandoc metadata.yaml \
    00-INTRODUCTION.md \
    01-TABLE-OF-CONTENTS.md \
    GLOSSARY.md \
    02-CHAPTER-01-HISTORICAL-TIMELINE.md \
    03-CHAPTER-02-CRYPTOGRAPHIC-PRIMITIVES.md \
    04-CHAPTER-03-SIGNAL-PROTOCOL.md \
    05-CHAPTER-04-LANGUAGE-BINDINGS.md \
    06-CHAPTER-05-ZERO-KNOWLEDGE.md \
    07-CHAPTER-06-NETWORK-SERVICES.md \
    08-CHAPTER-07-SESSION-ESTABLISHMENT.md \
    09-CHAPTER-08-MESSAGE-ENCRYPTION.md \
    10-CHAPTER-09-GROUP-MESSAGING.md \
    11-CHAPTER-10-TESTING-ARCHITECTURE.md \
    12-CHAPTER-11-BUILD-SYSTEM.md \
    13-CHAPTER-12-ARCHITECTURAL-EVOLUTION.md \
    14-CHAPTER-13-SEALED-SENDER.md \
    15-CHAPTER-14-MESSAGE-BACKUP.md \
    RESEARCH-DATA-SUMMARY.md \
    -o libsignal-encyclopedia.pdf \
    --toc --toc-depth=3 \
    --pdf-engine=xelatex \
    -V geometry:margin=1in \
    -V fontsize=11pt \
    -V documentclass=book \
    && echo "✅ PDF created: libsignal-encyclopedia.pdf"

echo
echo "=== Compilation Complete ==="
ls -lh libsignal-encyclopedia.*
