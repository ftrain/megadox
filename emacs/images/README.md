# EPUB Cover Image

## Cover File: cover.png (or cover.svg)

### Purpose
This is the front cover image for the EPUB version of "GNU Emacs: An Encyclopedic Guide to the Codebase"

### Specifications
- **Dimensions**: 1600 x 2400 pixels
- **Format**: PNG (primary) or SVG (source)
- **Aspect Ratio**: 2:3 (standard for EPUB book covers)
- **Color Space**: RGB
- **Resolution**: 96 DPI minimum (for digital)

### Design Requirements
- **Title**: "GNU Emacs"
- **Subtitle**: "An Encyclopedic Guide to the Codebase"
- **Style**: Professional technical book design
- **Color Scheme**:
  - Primary: Dark background (deep blue/navy) for technical feel
  - Accent: Cyan/bright blue (#00d4ff) for modern tech aesthetic
  - Secondary: Light colors for contrast and readability
- **Typography**:
  - Large, bold serif font for "GNU Emacs" main title
  - Italic serif for subtitle
  - Monospace font for code elements and technical tagline
- **Elements**:
  - Subtle code pattern background (Lisp/elisp snippets)
  - Decorative geometric shapes (circles, accent bars)
  - Code snippets (e.g., "(defun", "M-x customize", "C-h f describe")
  - Professional footer text

### Current Files

#### cover.svg
- Vector format source file created with full specifications
- Can be converted to PNG using tools like:
  - **ImageMagick**: `convert cover.svg cover.png`
  - **Inkscape**: `inkscape cover.svg --export-png=cover.png`
  - **rsvg-convert**: `rsvg-convert cover.svg -o cover.png`
  - **Online converters**: CloudConvert, Online-Convert, etc.

### Conversion Instructions

To convert the SVG to PNG, use one of the following methods:

#### Using ImageMagick (if installed):
```bash
convert -density 96 cover.svg -resize 1600x2400 cover.png
```

#### Using Inkscape (if installed):
```bash
inkscape cover.svg --export-type=png --export-dpi=96 --export-filename=cover.png
```

#### Using librsvg (rsvg-convert):
```bash
rsvg-convert -w 1600 -h 2400 cover.svg -o cover.png
```

### Alternative PNG Creation
If you need a PNG file now and conversion tools aren't available:
1. Use an online SVG to PNG converter (search "SVG to PNG converter")
2. Upload `cover.svg`
3. Download the resulting PNG
4. Save as `cover.png` in this directory

### EPUB Integration
- Place `cover.png` in the EPUB package's `images/` directory
- Reference in the OPF manifest: `<item id="cover-image" href="images/cover.png" media-type="image/png"/>`
- Include in spine/package as cover
- Add to HTML cover page if needed

### Notes
- The SVG is scalable and can be rendered at any resolution
- The design uses professional gradients, opacity effects, and code-themed elements
- Colors are optimized for both print and digital viewing
- The layout respects the 2:3 aspect ratio standard for ebook covers
