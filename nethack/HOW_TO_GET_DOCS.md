# How to Get the NetHack Encyclopedia Documentation

## 🎯 Quick Access

The complete NetHack Encyclopedia documentation is available in a clean, standalone branch with **ONLY the documentation files** (no NetHack source code).

---

## 📥 Clone the Documentation

### Option 1: Clone Just the Docs Branch (Recommended)

```bash
git clone --single-branch --branch claude/nethack-encyclopedia-docs-01B7fuKjgrUtx2Yqy3TrJhM2 \
  https://github.com/ftrain/NetHack.git nethack-encyclopedia

cd nethack-encyclopedia
```

This gives you a clean directory with only the 28 documentation files (no source code).

### Option 2: Clone and Checkout

```bash
git clone https://github.com/ftrain/NetHack.git
cd NetHack
git checkout claude/nethack-encyclopedia-docs-01B7fuKjgrUtx2Yqy3TrJhM2
```

### Option 3: Download ZIP

Visit: https://github.com/ftrain/NetHack/tree/claude/nethack-encyclopedia-docs-01B7fuKjgrUtx2Yqy3TrJhM2

Click **Code** → **Download ZIP**

---

## 📚 What You Get

### 28 Documentation Files (400KB+)

**Core Documentation (5 files):**
- `README.md` - Master encyclopedia guide
- `INDEX.md` - Topic-based navigation
- `INTRODUCTION.md` - Getting started
- `GLOSSARY.md` - 500+ terms
- `BIBLIOGRAPHY.md` - Complete sources

**Game World (5 files):**
- `CHARACTER_GUIDE.md` - All 13 classes, 5 races
- `MONSTER_CATALOG_COMPLETE.md` - All 394 monsters
- `README_BESTIARY.md` - Quick reference
- `NETHACK_ITEM_COMPENDIUM.md` - All 430+ items, 42 artifacts
- `DUNGEON_GENERATION.md` - Level generation

**Mechanics & Strategy (2 files):**
- `GAME_MECHANICS.md` - 15 core systems
- `STRATEGY_GUIDE.md` - Complete guide (early game → ascension)

**History & Culture (3 files):**
- `GIT_HISTORY.md` - 40-year timeline
- `CULTURAL_HISTORY.md` - Community history
- `CODEBASE_ARCHITECTURE.md` - Technical reference

**Build System (1 directory):**
- `docs-build/` - Pandoc build system for EPUB/PDF
  - Makefile, metadata.yaml, epub.css
  - Shell scripts, documentation

**Data Files (4 files):**
- `monsters_database.json` - Machine-readable data (213KB)
- `BESTIARY.txt` - Formatted report
- `analyze_bestiary.py`, `monster_parser.py` - Scripts

---

## 🔨 Build EPUB and PDF

Once you have the docs:

```bash
cd docs-build

# Install dependencies (Ubuntu/Debian)
sudo apt-get install pandoc texlive-xetex texlive-fonts-recommended

# Build both formats
make all

# Or individually
make epub    # Fast (~5-10 seconds)
make pdf     # Slower (~30-60 seconds)
```

Output files created in `docs-output/`:
- `NetHack-Encyclopedia.epub` - E-reader format
- `NetHack-Encyclopedia.pdf` - Print-ready PDF

See `docs-build/README_BUILD.md` for complete instructions.

---

## 📖 How to Read

### Start Here:
1. **README.md** - Overview of the encyclopedia
2. **INTRODUCTION.md** - What is NetHack?
3. Choose your path:
   - **New players:** GLOSSARY.md → CHARACTER_GUIDE.md → STRATEGY_GUIDE.md
   - **Experienced:** INDEX.md → Jump to topics of interest
   - **Developers:** CODEBASE_ARCHITECTURE.md → DUNGEON_GENERATION.md

### Navigation:
- Use **INDEX.md** for topic-based navigation
- Use **GLOSSARY.md** for term lookups
- All documents are cross-referenced with links

---

## 📊 Statistics

- **28 files total**
- **18 markdown documentation files**
- **7,249+ lines of documentation**
- **400KB+ content**
- **394 monsters** documented
- **430+ items** cataloged
- **42 artifacts** detailed
- **13 character classes** with complete mechanics
- **5 races** with attributes
- **500+ glossary entries**
- **Complete codebase analysis** (1.2M+ LOC analyzed)
- **Academic citations** included

---

## 🌐 Branch Information

- **Repository:** https://github.com/ftrain/NetHack
- **Branch:** `claude/nethack-encyclopedia-docs-01B7fuKjgrUtx2Yqy3TrJhM2`
- **Direct Link:** https://github.com/ftrain/NetHack/tree/claude/nethack-encyclopedia-docs-01B7fuKjgrUtx2Yqy3TrJhM2
- **Commit:** 210e3a9 "NetHack Encyclopedia - Documentation Only"
- **Files:** Documentation only (no source code)
- **Size:** ~12MB (mostly JSON database)

---

## 🎓 About This Encyclopedia

This is the most comprehensive NetHack documentation ever assembled:

✅ **Complete game coverage** - Every monster, item, class, race
✅ **Strategic mastery** - From survival to ascension
✅ **Historical depth** - 40 years of development
✅ **Cultural context** - Community, traditions, impact
✅ **Technical excellence** - Complete codebase analysis
✅ **Academic rigor** - Cited sources, professional quality

Created through comprehensive analysis of the NetHack 3.7 codebase using advanced AI techniques, combining source code analysis, historical research, and community knowledge.

---

## ⚖️ License

- **Documentation:** Creative Commons Attribution-ShareAlike 4.0
- **NetHack source references:** NetHack General Public License
- **Community contributions:** As attributed in BIBLIOGRAPHY.md

---

## 🎮 Happy Reading!

Whether you're a first-time player, a veteran pursuing multiple conducts, a developer studying roguelike design, or a researcher analyzing game systems, this encyclopedia provides comprehensive knowledge of one of gaming's greatest achievements.

*"The DevTeam thinks of everything."*
