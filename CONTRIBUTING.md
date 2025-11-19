# Contributing to Megadox

Thank you for your interest in improving this documentation collection! This guide explains how you can contribute.

## Ways to Contribute

### 1. Report Errors

If you find technical inaccuracies, outdated information, or unclear explanations:

1. **Check the source code** - Verify against the actual project source
2. **Open an issue** with:
   - Which encyclopedia (Emacs, PostgreSQL, etc.)
   - Section/chapter reference
   - Description of the error
   - Correct information (with source references)

### 2. Improve Explanations

AI-generated documentation can sometimes be unclear or miss important context:

- Suggest clearer wording for complex concepts
- Add missing examples or use cases
- Improve code comments and annotations
- Enhance historical context or design rationale

### 3. Add Missing Content

If you notice important topics that aren't covered:

- Identify the gap clearly
- Provide outline of what should be added
- Optionally: Submit a pull request with the new content
- Include references to source code or documentation

### 4. Enhance the Build System

Improvements to the publishing infrastructure:

- Better typography or styling
- Additional output formats
- Build optimizations
- Cross-platform compatibility fixes

### 5. Accessibility Improvements

Help make the documentation more accessible:

- Screen reader compatibility
- Better keyboard navigation
- Improved contrast and readability
- Alternative text for diagrams

## How to Submit Changes

### Quick Fixes (Issues)

For small corrections or suggestions:

1. Go to https://github.com/ftrain/megadox/issues
2. Click "New Issue"
3. Describe the problem/suggestion clearly
4. Include references (file names, line numbers, etc.)

### Content Changes (Pull Requests)

For actual content modifications:

1. **Fork the repository**
   ```bash
   git clone https://github.com/ftrain/megadox.git
   cd megadox
   ```

2. **Create a branch**
   ```bash
   git checkout -b fix/emacs-buffer-explanation
   ```

3. **Make your changes**
   - Edit the relevant Markdown files
   - Follow the existing style and structure
   - Test your changes by rebuilding

4. **Test the build**
   ```bash
   cd emacs  # or whichever project
   make html
   # Review the output in output/
   ```

5. **Commit with clear messages**
   ```bash
   git add .
   git commit -m "Fix explanation of buffer management in Emacs chapter 2

   - Clarified the relationship between buffers and windows
   - Added example from src/buffer.c
   - Fixed typo in code comment"
   ```

6. **Push and create PR**
   ```bash
   git push origin fix/emacs-buffer-explanation
   ```
   Then open a pull request on GitHub

## Content Guidelines

### Style

- **Technical but accessible** - Assume reader has programming knowledge but may be new to the specific project
- **Code examples** - Show actual source code when explaining concepts
- **Historical context** - Explain why things are designed a certain way
- **Cross-references** - Link to related sections

### Accuracy

- **Verify with source** - All technical claims should be traceable to actual code
- **Cite versions** - Note which version of the software you're describing
- **Test examples** - Code examples should compile/run
- **Check updates** - Projects evolve; note if information might be outdated

### Formatting

Use standard Markdown:

```markdown
# Chapter Title

## Section

### Subsection

Regular paragraphs...

```c
// Code blocks with language specified
void example() {
    // ...
}
\```

- Lists for
- Multiple items

1. Numbered lists
2. For sequences

**Bold** for emphasis
*Italic* for terms

[Links](to-other-sections.md) as needed
\```

## Building Documentation

### Prerequisites

See [INSTALL-PANDOC.md](INSTALL-PANDOC.md) for installing pandoc and LaTeX.

### Build Process

```bash
# Check dependencies
make check

# Build single encyclopedia
cd emacs
make all        # EPUB, PDF, and HTML
make epub       # EPUB only
make pdf        # PDF only
make html       # HTML only

# Build everything
cd ..
make all
```

### Output

Generated files go to:
- `<project>/output/` - Individual builds
- `docs/` - Published versions for GitHub Pages

## Verification Checklist

Before submitting a pull request:

- [ ] Content is technically accurate (verified against source code)
- [ ] Markdown formatting is correct
- [ ] Build succeeds without errors (`make check && make html`)
- [ ] Links work correctly
- [ ] Code examples are properly formatted
- [ ] Spelling and grammar are correct
- [ ] Changes are focused and well-explained in commit message

## AI-Generated Content Policy

This documentation was AI-generated, but that doesn't mean it can't be improved by humans!

### When to regenerate with AI

- Major software version updates
- Complete restructuring needed
- New subsystems to document

### When to edit manually

- Small corrections
- Clarifications
- Additional examples
- Style improvements

We welcome both approaches. Manual edits are often faster for small fixes, while AI regeneration might be better for large-scale updates.

## Documentation Structure

Each encyclopedia follows this structure:

```
project-name/
├── README.md           # Project-specific info
├── Makefile           # Build configuration
├── metadata.yaml      # Document metadata
├── 00-intro/         # Introduction chapters
├── 01-arch/          # Architecture chapters
├── 02-impl/          # Implementation chapters
├── ...
├── GLOSSARY.md       # Term definitions
└── INDEX.md          # Comprehensive index
```

## Code of Conduct

Be respectful and constructive:

- **Assume good faith** - AI makes mistakes; people make mistakes; that's okay
- **Be specific** - Vague complaints aren't helpful
- **Provide evidence** - Link to source code or documentation
- **Stay focused** - Keep discussions on-topic
- **Be patient** - Maintainers are volunteers

## Questions?

- **General questions**: Open a [GitHub Discussion](https://github.com/ftrain/megadox/discussions)
- **Bugs or errors**: Open an [Issue](https://github.com/ftrain/megadox/issues)
- **Build problems**: Check [INSTALL-PANDOC.md](INSTALL-PANDOC.md) first

## License

By contributing, you agree that your contributions will be licensed under the same license as the source project being documented. See individual project directories for specific licenses.

## Recognition

Contributors will be:
- Listed in commit history
- Mentioned in release notes
- Credited in the documentation (for substantial contributions)

Thank you for helping make technical documentation better! 📚
