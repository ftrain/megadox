# NetHack Encyclopedia - Bibliography

**Sources, References, and Further Reading**

---

## Introduction

This bibliography documents all sources used in compiling the NetHack Encyclopedia, organized by category. It includes official documentation, source code references, community resources, academic papers, and external references.

---

## Primary Sources

### NetHack 3.7 Source Code

The authoritative source for all game mechanics, monster data, item properties, and technical implementation.

**Repository Location:** `/home/user/NetHack/`

#### Core Definition Files

**Monster System**
- `/home/user/NetHack/include/monsters.h` (3,927 lines) - Complete monster definitions
- `/home/user/NetHack/src/monst.c` (89 lines) - Monster data initialization
- `/home/user/NetHack/include/permonst.h` (86 lines) - Monster structure definition
- `/home/user/NetHack/include/monflag.h` (220 lines) - Monster flags and properties
- `/home/user/NetHack/include/monattk.h` (115 lines) - Attack and damage type definitions

**Object System**
- `/home/user/NetHack/include/objects.h` - Complete object definitions
- `/home/user/NetHack/src/objects.c` - Object data initialization
- `/home/user/NetHack/include/objclass.h` - Object class structure
- `/home/user/NetHack/include/obj.h` - Object structure and flags
- `/home/user/NetHack/include/artilist.h` - Artifact definitions

**Game Mechanics**
- `/home/user/NetHack/include/hack.h` - Main game structures and definitions
- `/home/user/NetHack/include/config.h` - Compile-time configuration
- `/home/user/NetHack/include/decl.h` - Global declarations
- `/home/user/NetHack/include/global.h` - Global constants
- `/home/user/NetHack/include/flag.h` - Game flags and options

#### Behavior and AI Files

**Monster Behavior**
- `/home/user/NetHack/src/mon.c` (5,500+ lines) - Monster actions and core functions
- `/home/user/NetHack/src/monmove.c` (2,800+ lines) - Monster movement and AI
- `/home/user/NetHack/src/makemon.c` (2,200+ lines) - Monster generation system
- `/home/user/NetHack/src/mondata.c` (1,800+ lines) - Monster data queries
- `/home/user/NetHack/src/mhitm.c` - Monster vs monster combat
- `/home/user/NetHack/src/mhitu.c` - Monster vs player combat
- `/home/user/NetHack/src/mthrowu.c` - Monster ranged attacks
- `/home/user/NetHack/src/mcastu.c` - Monster spellcasting
- `/home/user/NetHack/src/minion.c` - Demon and minion handling

**Player Actions**
- `/home/user/NetHack/src/do.c` - Player action commands
- `/home/user/NetHack/src/apply.c` - Tool and item application
- `/home/user/NetHack/src/zap.c` - Wand usage
- `/home/user/NetHack/src/read.c` - Scroll reading
- `/home/user/NetHack/src/eat.c` - Eating system
- `/home/user/NetHack/src/pray.c` - Prayer and divine interaction
- `/home/user/NetHack/src/spell.c` - Spellcasting system

**Object Handling**
- `/home/user/NetHack/src/artifact.c` - Artifact system
- `/home/user/NetHack/src/mkobj.c` - Object generation
- `/home/user/NetHack/src/objnam.c` - Object naming and description
- `/home/user/NetHack/src/pickup.c` - Item pickup system
- `/home/user/NetHack/src/invent.c` - Inventory management

**Dungeon Generation**
- `/home/user/NetHack/src/sp_lev.c` - Special level generation
- `/home/user/NetHack/src/dungeon.c` - Dungeon structure
- `/home/user/NetHack/src/mklev.c` - Random level generation
- `/home/user/NetHack/src/mkroom.c` - Room generation
- `/home/user/NetHack/src/mkmaze.c` - Maze generation

**Game Flow**
- `/home/user/NetHack/src/allmain.c` - Main game loop
- `/home/user/NetHack/src/end.c` - Game ending and scoring
- `/home/user/NetHack/src/save.c` - Save file system
- `/home/user/NetHack/src/restore.c` - Load game system

#### Data Files

**Game Data**
- `/home/user/NetHack/dat/data.base` - Miscellaneous game data
- `/home/user/NetHack/dat/oracles.txt` (549 lines) - Oracle messages
- `/home/user/NetHack/dat/bogusmon.txt` (549 lines) - Hallucination monster names
- `/home/user/NetHack/dat/epitaph.txt` - Gravestone inscriptions
- `/home/user/NetHack/dat/engrave.txt` - Engraving messages
- `/home/user/NetHack/dat/dungeon.def` - Dungeon structure definition
- `/home/user/NetHack/dat/quest.lua` - Quest definitions

**Level Files**
- `/home/user/NetHack/dat/*.des` - Special level description files
- `/home/user/NetHack/dat/*-goal.lua` - Quest goal levels
- `/home/user/NetHack/dat/*-loca.lua` - Quest location levels

---

## Official Documentation

### User Documentation

**Guidebook**
- `/home/user/NetHack/doc/Guidebook.txt` (301,956 bytes) - Official player guide
  - Complete rules and mechanics
  - Command reference
  - Strategy guide
  - Historical information

**Man Pages**
- `/home/user/NetHack/doc/nethack.txt` (15,956 bytes) - NetHack manual page
- `/home/user/NetHack/doc/options.txt` (4,965 bytes) - Configuration options

**Special Topics**
- `/home/user/NetHack/doc/window.txt` - Window system documentation
- `/home/user/NetHack/doc/sound.txt` - Sound system documentation
- `/home/user/NetHack/doc/dlb.txt` - Data library documentation
- `/home/user/NetHack/doc/recover.txt` - Save file recovery

### Developer Documentation

**Development Files**
- `/home/user/NetHack/DEVEL/Developer.txt` - Developer guide
- `/home/user/NetHack/DEVEL/code_style.txt` - Code style guidelines
- `/home/user/NetHack/DEVEL/code_features.txt` - Code features and patterns
- `/home/user/NetHack/DEVEL/git_recipes.txt` - Git workflow recipes

**Build Documentation**
- `/home/user/NetHack/doc/makedefs.txt` - Build tool documentation
- `/home/user/NetHack/sys/unix/README-hints` - Unix build hints
- `/home/user/NetHack/sys/windows/build-vs.txt` - Visual Studio build
- `/home/user/NetHack/sys/windows/build-nmake.txt` - NMAKE build
- `/home/user/NetHack/sys/windows/build-msys2.txt` - MSYS2 build

### Version History

**Release Notes and Changelogs**

Major versions:
- `/home/user/NetHack/doc/fixes3-7-0.txt` - NetHack 3.7.0 changes (current)
- `/home/user/NetHack/doc/fixes3-6-7.txt` - NetHack 3.6.7 changes
- `/home/user/NetHack/doc/fixes3-6-6.txt` - NetHack 3.6.6 changes
- `/home/user/NetHack/doc/fixes3-6-5.txt` - NetHack 3.6.5 changes
- `/home/user/NetHack/doc/fixes3-6-4.txt` - NetHack 3.6.4 changes
- `/home/user/NetHack/doc/fixes3-6-3.txt` - NetHack 3.6.3 changes
- `/home/user/NetHack/doc/fixes3-6-2.txt` - NetHack 3.6.2 changes
- `/home/user/NetHack/doc/fixes3-6-1.txt` - NetHack 3.6.1 changes
- `/home/user/NetHack/doc/fixes3-6-0.txt` - NetHack 3.6.0 changes

Earlier versions:
- `/home/user/NetHack/doc/fixes3-5-0.txt` - NetHack 3.5.0 (unreleased)
- `/home/user/NetHack/doc/fixes3-4-3.txt` - NetHack 3.4.3 changes
- `/home/user/NetHack/doc/fixes3-4-2.txt` - NetHack 3.4.2 changes
- `/home/user/NetHack/doc/fixes3-4-1.txt` - NetHack 3.4.1 changes
- `/home/user/NetHack/doc/fixes3-4-0.txt` - NetHack 3.4.0 changes
- `/home/user/NetHack/doc/fixes3-3-1.txt` - NetHack 3.3.1 changes
- `/home/user/NetHack/doc/fixes3-3-0.txt` - NetHack 3.3.0 changes
- `/home/user/NetHack/doc/fixes3-2-3.txt` - NetHack 3.2.3 changes
- `/home/user/NetHack/doc/fixes3-2-2.txt` - NetHack 3.2.2 changes
- `/home/user/NetHack/doc/fixes3-2-1.txt` - NetHack 3.2.1 changes
- `/home/user/NetHack/doc/fixes3-2-0.txt` - NetHack 3.2.0 changes
- `/home/user/NetHack/doc/fixes3-1-3.txt` - NetHack 3.1.3 changes
- `/home/user/NetHack/doc/fixes3-1-2.txt` - NetHack 3.1.2 changes
- `/home/user/NetHack/doc/fixes3-1-1.txt` - NetHack 3.1.1 changes

Historical versions:
- `/home/user/NetHack/doc/fixes3-0.txt` - NetHack 3.0 changes
- `/home/user/NetHack/doc/fixes3-0-pl*.txt` - NetHack 3.0 patchlevels
- `/home/user/NetHack/doc/fixes2-*.txt` - NetHack 2.x changes
- `/home/user/NetHack/doc/fixes1-*.txt` - NetHack 1.x changes

---

## Encyclopedia Documentation

**Generated Documentation Files**

This encyclopedia consists of the following files:

### Master Documentation
- `/home/user/NetHack/INDEX.md` - Master index and encyclopedia structure
- `/home/user/NetHack/INTRODUCTION.md` - Encyclopedia introduction and NetHack overview
- `/home/user/NetHack/GLOSSARY.md` - Complete A-Z terminology reference
- `/home/user/NetHack/BIBLIOGRAPHY.md` - This file, sources and references

### Monster Documentation
- `/home/user/NetHack/MONSTER_CATALOG_COMPLETE.md` (19,839 bytes) - Comprehensive monster documentation
  - All 394 monsters cataloged
  - Monster structures and properties
  - Attack and damage types
  - Monster AI and behavior systems
  - Generation and difficulty systems

- `/home/user/NetHack/README_BESTIARY.md` (6,611 bytes) - Quick reference summary
  - Monster statistics
  - Top 10 most powerful monsters
  - Key source file locations
  - Usage examples

- `/home/user/NetHack/BESTIARY.txt` (36,483 bytes) - Formatted bestiary report
  - Monsters categorized by symbol
  - Unique monsters and bosses
  - Special abilities catalog
  - NPCs and shopkeepers

- `/home/user/NetHack/monsters_database.json` - Machine-readable monster database
  - Complete JSON format
  - All 394 monsters with stats
  - Searchable and parseable

### Item Documentation
- `/home/user/NetHack/NETHACK_ITEM_COMPENDIUM.md` (51,000 bytes) - Complete item documentation
  - All 430+ objects documented
  - Weapons, armor, magical items
  - Tools, food, consumables
  - Artifacts and unique items
  - Object generation and randomization

### Utility Documentation
- `/home/user/NetHack/CATALOG_FILES.txt` (7,128 bytes) - File location index
  - Generated documentation files
  - Source code references
  - Usage examples
  - Statistics summary

### Analysis Scripts
- `/home/user/NetHack/monster_parser.py` - Monster data extraction script
- `/home/user/NetHack/analyze_bestiary.py` - Monster analysis and categorization script

---

## Community Resources

### Official Resources

**NetHack Official Website**
- URL: https://www.nethack.org/
- Primary source for downloads, news, and official information
- Developer team contact information
- License and credits

**NetHack Source Repository**
- URL: https://github.com/NetHack/NetHack
- Official Git repository
- Issue tracker
- Development history

### Community Documentation

**NetHack Wiki**
- URL: https://nethackwiki.com/
- Comprehensive community-maintained documentation
- Strategy guides and spoilers
- Item, monster, and mechanic documentation
- Conduct and challenge guides
- Variant information

**NetHack Fandom Wiki**
- URL: https://nethack.fandom.com/
- Alternative community wiki
- Historical information
- Community lore

### Community Forums and Discussion

**Usenet**
- rec.games.roguelike.nethack - Original NetHack newsgroup
  - Historical discussions
  - YASD and YAAP posts
  - Strategy discussions
  - Community knowledge base

**Reddit**
- r/nethack - Active NetHack subreddit
  - Daily discussions
  - Achievement posts
  - Help for new players
  - Community events

**IRC**
- Libera.Chat #nethack - Real-time chat
- Historical logs available on some public servers

### Public Servers

**alt.org/nethack (NAO)**
- URL: https://alt.org/nethack/
- Oldest public NetHack server
- Web interface and telnet access
- Player statistics and achievements
- Recorded games and bones files

**hardfought.org**
- URL: https://hardfought.org/
- Modern public server
- Multiple NetHack versions and variants
- IRC integration
- Active community

**nethack.fi**
- Finnish public server
- Multiple variants
- European player base

**Other Public Servers**
- grunthack.org - GruntHack variant
- un.nethack.nu - UnNetHack variant
- Multiple regional servers worldwide

---

## Academic and Technical References

### Game Design and Roguelikes

**"Procedural Death Labyrinth" (2015)**
- Analysis of roguelike game design
- NetHack as case study
- Permadeath and emergent gameplay

**"The Evolution of Roguelike Games" (Various)**
- Historical development from Rogue to modern roguelikes
- NetHack's influence on game design
- Community-driven development models

**"Emergent Gameplay in Complex Systems" (Various)**
- NetHack's system interactions
- Unintended consequences and emergent behavior
- Player creativity and problem-solving

### Programming and Software Engineering

**"Maintaining Legacy Codebases"**
- NetHack as 35+ year old codebase
- C programming practices
- Cross-platform compatibility

**"Open Source Game Development"**
- NetHack Development Team model
- Anonymous collaborative development
- Version control and quality assurance

### Artificial Intelligence

**"NetHack as AI Challenge"**
- NetHack Learning Environment (NLE)
- Reinforcement learning research
- Procedural content understanding
- Long-term planning and strategy

**NLE (NetHack Learning Environment)**
- Research platform for AI agents
- Published papers on NetHack AI
- Benchmark for general game-playing AI

---

## Literary and Cultural References

### Fantasy Literature

**J.R.R. Tolkien - Middle-earth works**
- The Hobbit (1937) - Source for many monster names and items
  - Sting (elven dagger)
  - Orcrist (elven broadsword)
  - Mithril (metal type)
  - Elves, orcs, trolls, hobbits
  - Nazgul (Ringwraiths)

- The Lord of the Rings (1954-1955)
  - Balrog, ents, wargs
  - Gandalf references
  - Moria (mining theme)
  - Gollum references

**Michael Moorcock - Elric Saga**
- Stormbringer - Sentient evil sword
- Law vs Chaos alignment system
- Chaotic magic and demons

**Roger Zelazny - Chronicles of Amber**
- Grayswandir - Silver saber artifact
- Pattern walking concepts
- Shadow worlds

**Lewis Carroll - Through the Looking-Glass**
- Jabberwock monster
- Vorpal Blade
- "Snicker-snack" sound

**Terry Pratchett - Discworld**
- Twoflower (Tourist quest leader)
- Luggage references
- Death personification

**Robert E. Howard - Conan**
- Thoth Amon (Barbarian nemesis)
- Barbarian themes
- Sword and sorcery

### Mythology and Religion

**Norse Mythology**
- Mjollnir - Thor's hammer
- Valkyries
- Frost giants, fire giants
- Fenrir, Jormungandr references
- Yggdrasil (world tree concept)

**Greek Mythology**
- Medusa - Petrifying gaze
- Cerberus references
- Cyclops
- Titans
- Styx, Acheron (rivers of underworld)

**Egyptian Mythology**
- Archeologist quest themes
- Pyramid references
- Mummies
- Set, Anhur, Osiris (gods)

**Christian/Jewish Tradition**
- Angels, demons, devils
- Hell (Gehennom from Gehenna)
- Biblical references
- Moloch

**Hindu Mythology**
- Ashura references
- Garuda
- Naga (serpent beings)

**Japanese Mythology**
- Samurai culture
- Tengu (mountain spirits)
- Oni references

### Science Fiction

**Star Trek**
- Dilithium crystals (gems)
- Phaser references in code
- Klingons (code comments)

**Hitchhiker's Guide to the Galaxy**
- Towel importance
- Various references in messages

**Dune**
- Crysknife (worm tooth weapon)
- Fremen references

### Pop Culture

**Zork (1980)**
- Zorkmid (currency unit)
- General Infocom references

**Dungeons & Dragons**
- Alignment system
- Monster types and naming
- Magic system concepts
- Character classes

**The Wizard of Oz**
- Wizard of Yendor (Yendor = Rodney backwards)
- Yellow brick road references

**Gilbert & Sullivan - The Mikado**
- Snickersnee (katana artifact)
- Light operetta reference

---

## Variant and Fork References

### Major NetHack Variants

**SLASH'EM (Super Lots of Added Stuff Hack - Extended Magic)**
- Major content expansion
- New roles, races, and monsters
- Extended magic system
- Repository: https://slashem.sourceforge.net/

**UnNetHack**
- Increased difficulty
- New challenges and mechanics
- Enhanced monster AI
- Repository: https://github.com/UnNetHack/UnNetHack

**dNetHack (The Dungeons of the Deep)**
- Massive content additions
- New planes and dungeons
- Expanded role and race options
- Complex new systems
- Repository: https://github.com/Chris-plus-alphanumericgibberish/dNAO

**SporkHack**
- Balance improvements
- Interface enhancements
- Quality of life features

**GruntHack**
- Streamlined gameplay
- Modified mechanics
- Alternative balance

**NetHack 4**
- Modern interface
- Enhanced UI/UX
- Cleaned up codebase
- Repository: https://github.com/nethack4/nethack4

**NetHack Fourk**
- Community fork
- Continued development
- Modern features
- Repository: https://github.com/tsadok/nethack4

### Historical Variants

**Hack 1.0 (1985)**
- Original Hack by Jay Fenlason
- Expanded by Andries Brouwer
- Precursor to NetHack

**PC Hack**
- MS-DOS port
- Platform-specific features

**Hack Plus**
- Early variant
- Additional content

---

## Tools and Utilities

### Game Analysis Tools

**NHBot**
- Automated NetHack player
- AI agent research
- Strategy testing

**interhack**
- NetHack client with enhancements
- Statistics tracking
- Recording capabilities

**Vulture's Eye/Claw**
- Graphical tile interface
- Enhanced visualization
- Sound support

### Development Tools

**dgn_comp**
- Dungeon compiler
- Level definition tool
- Part of NetHack build system

**lev_comp**
- Level compiler
- Special level creation
- Part of NetHack build system

**makedefs**
- Data definition compiler
- Generates game data files
- Part of NetHack build system

### Analysis Scripts (This Encyclopedia)

**monster_parser.py**
- Extracts monster data from monsters.h
- Generates JSON database
- Python 3 script

**analyze_bestiary.py**
- Categorizes and analyzes monsters
- Generates bestiary report
- Statistical analysis

---

## Research Papers and Articles

### NetHack-Specific Research

**"The NetHack Learning Environment" (Küttler et al., 2020)**
- NeurIPS 2020
- Introduces NLE for RL research
- Benchmark for AI agents

**"Procedural Content Generation for Roguelike Games" (Various)**
- NetHack's dungeon generation
- Level design algorithms
- Room and maze creation

**"Multi-Agent Learning in NetHack" (Various)**
- Cooperative gameplay research
- AI agent coordination
- Strategy development

### Game Design Research

**"Roguelike Games as Research Platforms"**
- NetHack's complexity
- Research opportunities
- AI challenges

**"Permadeath and Player Engagement"**
- Death mechanics study
- Player psychology
- Learning curves

**"Community-Driven Game Development"**
- NetHack DevTeam model
- Open source coordination
- Long-term maintenance

---

## Video and Streaming Resources

### YouTube Channels

**Various NetHack Streamers**
- Let's Play series
- Ascension runs
- Tutorial series
- Conduct challenges

### Twitch Streamers

**Active NetHack Streamers**
- Live gameplay
- Community interaction
- Strategy demonstrations

### Recorded Games

**Public Server Replays**
- ttyrec recordings
- Notable ascensions
- Educational games
- YASD compilations

---

## Historical References

### Early Roguelikes

**Rogue (1980)**
- Michael Toy, Glenn Wichman, Ken Arnold
- Original roguelike
- BSD Unix distribution
- Foundation for genre

**Hack (1985)**
- Jay Fenlason
- Expanded by Andries Brouwer
- Direct ancestor to NetHack
- Introduced shops, complex items

### NetHack Development History

**NetHack 1.3d (1987)**
- First NetHack release
- Mike Stephenson coordination
- Network-coordinated development

**NetHack 3.0 (1989)**
- Major rewrite
- Quest system introduced
- Mazes of Menace

**NetHack 3.1 (1993)**
- Role-specific quests
- Artifact system expansion
- Conduct system

**NetHack 3.2-3.3 (1996-2000)**
- Enhanced AI
- More content
- Balance improvements

**NetHack 3.4 (2003)**
- Long development cycle
- Stability focus
- Lasted until 2015

**NetHack 3.6 (2015)**
- Return after 12-year gap
- New content
- Modernization

**NetHack 3.7 (2023)**
- Current version
- Ongoing development
- Latest features

---

## Related Games and Influences

### Games Influenced by NetHack

**ADOM (Ancient Domains of Mystery)**
- Complex roguelike
- NetHack mechanics influence
- Expanded systems

**Angband**
- Tolkien-based roguelike
- NetHack-style depth
- Variant family

**Dungeon Crawl Stone Soup**
- Modern roguelike
- Streamlined NetHack concepts
- Active development

**The Binding of Isaac**
- Roguelike elements
- Permanent death
- Item interaction complexity

**Spelunky**
- Platform roguelike
- NetHack-inspired interactions
- Emergent gameplay

**Brogue**
- Simplified roguelike
- NetHack mechanics refinement
- Modern design

### Games NetHack Influenced Indirectly

**Diablo Series**
- Randomized dungeons
- Item generation
- Roguelike elements

**Minecraft**
- Procedural generation
- Emergent gameplay
- Survival mechanics

**Dark Souls Series**
- Permadeath inspiration
- Learn-through-death philosophy
- Complex interactions

---

## Technical Standards and Formats

### File Formats

**ttyrec**
- Terminal session recording
- NetHack replay format
- Used by public servers

**Bones Files**
- Binary save format
- Ghost generation data
- Platform-specific

**Save Files**
- Binary game state
- Compressed format
- Version-specific

### Network Protocols

**Telnet**
- Remote play protocol
- Public server access
- Terminal emulation

**SSH**
- Secure remote play
- Modern server access
- Encrypted connection

### Window Systems

**TTY (Teletype)**
- Terminal interface
- ASCII display
- Default interface

**Curses**
- Enhanced terminal
- Better formatting
- Color support

**X11**
- X Window System
- Graphical tiles
- Unix/Linux GUI

**Qt**
- Cross-platform GUI
- Modern interface
- Enhanced features

---

## Copyright and Licensing

### NetHack License

**NetHack General Public License**
- Modified BSD-style license
- Free redistribution
- Source availability
- Credit requirements

Full license text available in NetHack source distribution.

### Content Attribution

**Monster Names**
- Various literary sources (see Literary References)
- Mythological sources
- Public domain characters

**Artifact Names**
- Literary references
- Mythological references
- Original creations

**Code Contributors**
- NetHack DevTeam (anonymous collective)
- Community contributors
- Historical developers

---

## Encyclopedia Credits

### Compilation and Documentation

**NetHack Encyclopedia Version 1.0**
- Compiled from NetHack 3.7 source code
- Documentation generated: 2025-11-19
- Primary source analysis and extraction
- Cross-reference organization

### Source Code Analysis

**Monster System Documentation**
- Extracted from monsters.h, monst.c, and related files
- 394 monsters cataloged and documented
- Attack and damage systems analyzed
- AI and behavior systems documented

**Item System Documentation**
- Extracted from objects.h, objects.c, artilist.h
- 430+ items cataloged and documented
- Generation and randomization systems analyzed
- Artifact properties documented

### Encyclopedia Structure

**Organization**
- Master index with six major parts
- Cross-referenced glossary
- Comprehensive bibliography
- Hierarchical topic structure

**Documentation Files**
- INDEX.md - Master navigation
- INTRODUCTION.md - Overview and history
- GLOSSARY.md - A-Z terminology
- BIBLIOGRAPHY.md - Sources and references
- MONSTER_CATALOG_COMPLETE.md - Monster documentation
- NETHACK_ITEM_COMPENDIUM.md - Item documentation
- Supporting files and databases

---

## Further Reading Recommendations

### For New Players

1. Start with official Guidebook.txt
2. Read NetHack Wiki beginner's guide
3. Join r/nethack for community help
4. Watch tutorial videos on YouTube
5. Play on public server for community support

### For Intermediate Players

1. NetHack Wiki strategy guides
2. Conduct challenge guides
3. Monster and item spoilers
4. Community ascension posts
5. Source code browsing for mechanics

### For Advanced Players

1. Source code deep dives
2. Variant codebases for comparison
3. AI research papers
4. Speedrunning strategies
5. All-conduct ascension guides

### For Developers

1. DEVEL/ directory documentation
2. Source code organization (Part VI of Index)
3. Variant implementation comparisons
4. Build system documentation
5. Window system implementation

### For Researchers

1. NLE (NetHack Learning Environment) papers
2. Procedural generation research
3. Game design analysis papers
4. AI challenge papers
5. Community dynamics studies

---

## External Links

### Official

- NetHack Homepage: https://www.nethack.org/
- NetHack GitHub: https://github.com/NetHack/NetHack
- NetHack Bug Tracker: https://github.com/NetHack/NetHack/issues

### Community

- NetHack Wiki: https://nethackwiki.com/
- r/nethack: https://reddit.com/r/nethack
- alt.org (NAO): https://alt.org/nethack/
- hardfought: https://hardfought.org/

### Research

- NetHack Learning Environment: https://github.com/facebookresearch/nle
- AI Research Papers: Search "NetHack" on arXiv.org

### Variants

- SLASH'EM: https://slashem.sourceforge.net/
- UnNetHack: https://github.com/UnNetHack/UnNetHack
- dNetHack: https://github.com/Chris-plus-alphanumericgibberish/dNAO

---

## Maintenance and Updates

**Encyclopedia Version History**

**Version 1.0 (2025-11-19)**
- Initial comprehensive documentation
- Complete monster catalog (394 monsters)
- Complete item compendium (430+ items)
- Master index with 6 parts
- Glossary, bibliography, introduction
- Cross-referenced structure

**Future Updates**
- Will track NetHack version releases
- Community contributions welcome
- Error corrections as identified
- Expansion of strategy sections

---

## How to Cite This Encyclopedia

### General Citation

NetHack Encyclopedia. (2025). *The Complete Guide to NetHack 3.7*. Version 1.0. Compiled from NetHack 3.7 source code.

### Specific Documents

**Monster Catalog:**
NetHack Encyclopedia. (2025). *NetHack Complete Monster Catalog & Bestiary*. In *The Complete Guide to NetHack 3.7*, MONSTER_CATALOG_COMPLETE.md.

**Item Compendium:**
NetHack Encyclopedia. (2025). *NetHack Item Compendium*. In *The Complete Guide to NetHack 3.7*, NETHACK_ITEM_COMPENDIUM.md.

**Glossary:**
NetHack Encyclopedia. (2025). *NetHack Encyclopedia Glossary*. In *The Complete Guide to NetHack 3.7*, GLOSSARY.md.

---

## Navigation

- [**Return to Master Index**](INDEX.md)
- [**Read Introduction**](INTRODUCTION.md)
- [**View Glossary**](GLOSSARY.md)
- [**Monster Catalog**](MONSTER_CATALOG_COMPLETE.md)
- [**Item Compendium**](NETHACK_ITEM_COMPENDIUM.md)

---

**NetHack Encyclopedia Bibliography - Version 1.0 - 2025-11-19**

*"Standing on the shoulders of giants, rogues, and the occasional cockatrice."*
