# Installing Pandoc

Pandoc is required to build the documentation in this repository. This guide covers installation on various platforms.

## Quick Install

### macOS

```bash
brew install pandoc

# For PDF generation, also install LaTeX:
brew install --cask mactex-no-gui
```

### Ubuntu/Debian

```bash
sudo apt-get update
sudo apt-get install pandoc texlive-xetex texlive-fonts-recommended texlive-fonts-extra
```

### Fedora/RHEL

```bash
sudo dnf install pandoc texlive-xetex texlive-collection-fontsrecommended
```

### Windows

Download the installer from: https://pandoc.org/installing.html

Or use Chocolatey:
```powershell
choco install pandoc
```

## Manual Installation (Latest Version)

If you want the latest version of Pandoc:

### Linux

```bash
# Download the latest release
cd /tmp
wget https://github.com/jgm/pandoc/releases/download/3.1.11/pandoc-3.1.11-1-amd64.deb

# Install
sudo dpkg -i pandoc-3.1.11-1-amd64.deb

# Verify
pandoc --version
```

### Alternative: Universal Binary

```bash
# Download and extract
cd /tmp
wget https://github.com/jgm/pandoc/releases/download/3.1.11/pandoc-3.1.11-linux-amd64.tar.gz
tar xzf pandoc-3.1.11-linux-amd64.tar.gz

# Copy to system location
sudo cp pandoc-3.1.11/bin/pandoc /usr/local/bin/

# Verify
pandoc --version
```

## Verifying Installation

After installation, verify that pandoc is available:

```bash
pandoc --version
```

You should see output like:
```
pandoc 3.1.11
Features: +server +lua
```

## Installing LaTeX (for PDF Generation)

PDF generation requires XeLaTeX. Install a TeX distribution:

### macOS

```bash
brew install --cask mactex-no-gui
```

Or for the full MacTeX distribution:
```bash
brew install --cask mactex
```

### Ubuntu/Debian

```bash
sudo apt-get install texlive-xetex texlive-fonts-recommended texlive-fonts-extra
```

### Fedora

```bash
sudo dnf install texlive-xetex texlive-collection-fontsrecommended
```

### Windows

Download MiKTeX from: https://miktex.org/download

## Testing the Build System

Once pandoc and LaTeX are installed, test the build system:

```bash
# Check dependencies
make check

# Build a single project
cd emacs
make html

# Build all projects
cd ..
make all
```

## Troubleshooting

### "pandoc: command not found"

- Ensure pandoc is in your PATH
- Try: `which pandoc` or `where pandoc` (Windows)
- Reinstall using the instructions above

### "xelatex: command not found"

- You need to install a LaTeX distribution (see above)
- PDF generation will not work without XeLaTeX
- EPUB and HTML generation will still work

### Font errors during PDF generation

```bash
# Install additional fonts (Ubuntu/Debian)
sudo apt-get install fonts-liberation fonts-dejavu

# Install Palatino alternatives
sudo apt-get install texlive-fonts-extra
```

### Memory errors with large documents

Increase pandoc's memory limit:

```bash
# Add to your shell rc file:
export PANDOC_TEXLIVE_MEMORY=4000000
```

## Minimal vs Full Installation

### Minimal (EPUB and HTML only)

- Just install pandoc
- No LaTeX required
- Smaller download (~100MB)

### Full (all formats including PDF)

- Install pandoc + LaTeX distribution
- Larger download (~2-4GB for full TeXLive)
- Enables PDF generation with professional typography

## Cloud/CI Environment

For automated builds in CI/CD:

```yaml
# GitHub Actions example
- name: Install Pandoc
  run: |
    wget https://github.com/jgm/pandoc/releases/download/3.1.11/pandoc-3.1.11-1-amd64.deb
    sudo dpkg -i pandoc-3.1.11-1-amd64.deb

- name: Install LaTeX
  run: |
    sudo apt-get update
    sudo apt-get install -y texlive-xetex texlive-fonts-recommended
```

## Docker

Use the official Pandoc Docker image:

```bash
docker pull pandoc/latex:latest

# Build documentation
docker run --rm -v $(pwd):/data pandoc/latex make all
```

## Next Steps

After installation:

1. Run `make check` to verify all dependencies
2. Try `make emacs` to build a single encyclopedia
3. Run `make all` to build the entire collection
4. See [README.md](README.md) for full build instructions

## Getting Help

- Pandoc documentation: https://pandoc.org/
- Pandoc user group: https://groups.google.com/g/pandoc-discuss
- File an issue: https://github.com/ftrain/megadox/issues
