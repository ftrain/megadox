# Rebuilding the Documentation

This guide explains how to rebuild the HTML documentation after making changes to the book template or styles.

## The Issue We Fixed

The original HTML books had a collapsible TOC feature, but it wasn't working because:
1. The JavaScript expected an element with `id="TOC"`
2. Pandoc's template didn't wrap the TOC in an element with that ID
3. Therefore, the collapsible functionality didn't initialize

## What Was Changed

### 1. Fixed the TOC Template

**File**: `book-template.html`

**Change**: Wrapped the Pandoc TOC output in a `<nav>` element with `id="TOC"`:

```html
<nav id="TOC" role="doc-toc">
$toc$
</nav>
```

This allows the JavaScript to find and initialize the collapsible TOC.

### 2. Enhanced Accessibility

**File**: `docs/book-style.css`

**Changes**:
- Improved line height (1.8) for better readability
- Added font smoothing for cleaner text rendering
- Set optimal line length (75ch) for comfortable reading
- Enhanced focus states for keyboard navigation
- Improved color contrast in the AI notice
- Added skip-to-content link support

### 3. Added Documentation

Created comprehensive guides:
- **AI-GENERATION.md** - Explains how the encyclopedias were created
- **INSTALL-PANDOC.md** - Step-by-step pandoc installation
- **CONTRIBUTING.md** - How to contribute improvements
- **This file** - How to rebuild documentation

### 4. Updated README and Homepage

- Added prominent AI methodology section
- Included links to detailed documentation
- Improved contributor guidelines
- Enhanced homepage with expandable details

## How to Rebuild

### Prerequisites

You need pandoc installed. If you don't have it:

```bash
# Check if you have pandoc
pandoc --version

# If not, see INSTALL-PANDOC.md for installation instructions
```

### Rebuild All HTML Books

Since the template changed, you need to rebuild all HTML files:

```bash
# From the repository root
cd /path/to/megadox

# Rebuild each encyclopedia
cd emacs && make html && cd ..
cd libsignal/encyclopedia && make html && cd ../..
cd postgresql && make html && cd ..
cd nethack && make html && cd ..
cd unix-history-repo/docs && make html && cd ../..
cd surge && make html && cd ..

# Or use a one-liner:
for dir in emacs libsignal/encyclopedia postgresql nethack unix-history-repo/docs surge; do
    echo "Building $dir..."
    (cd "$dir" && make html)
done
```

### Copy to GitHub Pages

The HTML files need to be copied to the `docs/` directory for GitHub Pages:

```bash
# From repository root
cp emacs/output/gnu-emacs-internals.html docs/
cp libsignal/encyclopedia/output/libsignal-encyclopedia.html docs/
cp postgresql/output/postgresql-encyclopedia.html docs/
cp nethack/output/nethack-encyclopedia.html docs/
cp unix-history-repo/docs/output/pdp7-unix-complete-reference.html docs/
cp surge/output/surge-xt-encyclopedia.html docs/

# Also copy the CSS
cp book-style.css docs/
```

## Testing Locally

After rebuilding, test the collapsible TOC:

```bash
# Option 1: Python's built-in server
cd docs
python3 -m http.server 8000

# Option 2: PHP's built-in server
php -S localhost:8000

# Then open http://localhost:8000/gnu-emacs-internals.html
```

### What to Test

1. **TOC Toggle**: Click the "Expand All" / "Collapse All" buttons
2. **Nested Sections**: Click the arrow icons (▶) to expand/collapse sections
3. **Search**: Try searching for keywords
4. **Mobile**: Resize browser to test mobile view
5. **Accessibility**: Try navigating with keyboard (Tab, Enter)

## Building PDFs and EPUBs

If you only changed the HTML template/CSS, you don't need to rebuild PDFs or EPUBs. But if you want to:

```bash
cd emacs
make all  # Builds EPUB, PDF, and HTML
```

**Note**: PDF generation requires XeLaTeX (see INSTALL-PANDOC.md)

## Common Issues

### "pandoc: command not found"

See [INSTALL-PANDOC.md](INSTALL-PANDOC.md) for installation.

### TOC Still Not Working

1. Check that `book-template.html` has `<nav id="TOC">` wrapper
2. Verify the HTML file has `<nav id="TOC">` in the source
3. Check browser console for JavaScript errors
4. Clear browser cache and reload

### CSS Not Applied

1. Make sure `book-style.css` is in the `docs/` directory
2. Check that HTML files reference the correct CSS path
3. Hard refresh browser (Ctrl+F5 or Cmd+Shift+R)

### Build Errors

```bash
# Check dependencies
make check

# Clean and rebuild
make clean
make html
```

## Deployment

After rebuilding and testing:

1. **Commit changes**:
   ```bash
   git add .
   git commit -m "Rebuild HTML with fixed collapsible TOC"
   ```

2. **Push to GitHub**:
   ```bash
   git push origin main
   ```

3. **Verify on GitHub Pages**:
   - Wait a few minutes for GitHub to rebuild
   - Visit https://ftrain.github.io/megadox/
   - Test the TOC functionality

## Automation

You could automate this with a script:

```bash
#!/bin/bash
# rebuild-all.sh

set -e  # Exit on error

echo "Rebuilding all HTML documentation..."

projects=(
    "emacs"
    "libsignal/encyclopedia"
    "postgresql"
    "nethack"
    "unix-history-repo/docs"
    "surge"
)

for project in "${projects[@]}"; do
    echo ""
    echo "Building $project..."
    (cd "$project" && make clean && make html)
done

echo ""
echo "Copying to docs/..."
cp emacs/output/gnu-emacs-internals.html docs/
cp libsignal/encyclopedia/output/libsignal-encyclopedia.html docs/
cp postgresql/output/postgresql-encyclopedia.html docs/
cp nethack/output/nethack-encyclopedia.html docs/
cp unix-history-repo/docs/output/pdp7-unix-complete-reference.html docs/
cp surge/output/surge-xt-encyclopedia.html docs/
cp book-style.css docs/

echo ""
echo "✓ Rebuild complete!"
echo "Test locally with: cd docs && python3 -m http.server"
```

Save as `rebuild-all.sh`, make executable (`chmod +x rebuild-all.sh`), and run (`./rebuild-all.sh`).

## Next Steps

After rebuilding:

1. Test thoroughly locally
2. Commit and push changes
3. Verify on GitHub Pages
4. Consider updating the version number or adding a changelog
5. Announce improvements to users

## Questions?

- Check existing documentation: README.md, CONTRIBUTING.md, INSTALL-PANDOC.md
- Open an issue: https://github.com/ftrain/megadox/issues
- Read Pandoc docs: https://pandoc.org/

---

**Last Updated**: November 2025
**Template Version**: 2.0 (with collapsible TOC fix)
