# NetHack Encyclopedia - Master Index

**The Complete Guide to NetHack 3.7**

---

## Welcome to the NetHack Encyclopedia

This comprehensive encyclopedia documents every aspect of NetHack 3.7, from game mechanics and monster behaviors to item properties and codebase architecture. Whether you're a new adventurer or a seasoned dungeon crawler, this encyclopedia serves as your complete reference guide.

**Quick Navigation:**
- [Introduction](INTRODUCTION.md) - Start here to learn about NetHack and this encyclopedia
- [Glossary](GLOSSARY.md) - NetHack terms, abbreviations, and jargon
- [Bibliography](BIBLIOGRAPHY.md) - Sources and references

---

## Part I: Introduction and History

### Encyclopedia Meta-Documentation
- [**Introduction**](INTRODUCTION.md) - Overview of NetHack and this encyclopedia
- [**Bibliography**](BIBLIOGRAPHY.md) - Sources, references, and further reading
- [**Glossary**](GLOSSARY.md) - Complete A-Z reference of NetHack terminology

### NetHack History and Development
- **NetHack Origins** - From Hack to NetHack 3.7 (see [Introduction](INTRODUCTION.md))
- **Version History** - Release notes and changelogs
  - [NetHack 3.7.0 Changes](doc/fixes3-7-0.txt)
  - [NetHack 3.6.x Changes](doc/fixes3-6-7.txt)
  - [Complete Version History](doc/) - All fixes*.txt files
- **Development Team** - The DevTeam and contributors
- **License and Credits** - NetHack General Public License

---

## Part II: The Game World

### Races and Classes

#### Playable Races
- **Human** - Versatile and adaptable
- **Elf** - Magical affinity, sleep resistance
- **Dwarf** - Strong and hardy
- **Gnome** - Small but clever
- **Orc** - Brutal warriors

#### Playable Roles (Classes)
- **Archeologist** - Quest: The Orb of Detection
- **Barbarian** - Quest: The Heart of Ahriman
- **Caveman/Cavewoman** - Quest: The Sceptre of Might
- **Healer** - Quest: The Staff of Aesculapius
- **Knight** - Quest: The Magic Mirror of Merlin
- **Monk** - Quest: The Eyes of the Overworld
- **Priest/Priestess** - Quest: The Mitre of Holiness
- **Ranger** - Quest: The Longbow of Diana
- **Rogue** - Quest: The Master Key of Thievery
- **Samurai** - Quest: The Tsurugi of Muramasa
- **Tourist** - Quest: The Platinum Yendorian Express Card
- **Valkyrie** - Quest: The Orb of Fate
- **Wizard** - Quest: The Eye of the Aethiopica

### Monsters and Creatures
- [**Complete Monster Catalog**](MONSTER_CATALOG_COMPLETE.md) - All 394 monsters documented
- [**Bestiary Summary**](README_BESTIARY.md) - Quick reference guide
- [**Bestiary Report**](BESTIARY.txt) - Categorized monster listings
- **Monster Database** - [monsters_database.json](monsters_database.json) - Machine-readable format

#### Monster Categories
- **Lowercase Symbols (a-z)** - Ants, blobs, dogs, felines, etc.
- **Uppercase Symbols (A-Z)** - Angels, dragons, giants, liches, etc.
- **Special Symbols** - Humans (@), demons (&), golems ('), etc.

#### Special Monster Groups
- **The Four Horsemen** - Death, Pestilence, Famine (Riders of the Apocalypse)
- **Demon Lords** - Demogorgon, Asmodeus, Orcus, Baalzebub, and others
- **Dragons** - 9 colors × 2 ages = 18 dragon types
- **Undead** - Zombies, mummies, vampires, wraiths, liches, ghosts
- **Lycanthropes** - Wererats, werejackals, werewolves
- **Golems** - 11 material types
- **Quest NPCs** - Leaders, guardians, and nemeses

#### NPCs and Services
- **Shopkeepers** - General and specialized shops
- **Priests** - Temple services and donations
- **Guards** - Castle and vault protection
- **Oracle** - Advice and divination
- **Quest Leaders** - Role-specific mentors

### Items and Objects
- [**Item Compendium**](NETHACK_ITEM_COMPENDIUM.md) - Complete documentation of all 430+ items

#### Weapons
- **Projectiles** - Arrows, bolts, darts, shurikens
- **Melee Weapons** - Swords, axes, maces, polearms
- **Launchers** - Bows, crossbows, slings
- **Special Weapons** - Artifact weapons

#### Armor
- **Body Armor** - Dragon scale mail, plate mail, leather armor
- **Helmets** - Various types and magical helms
- **Cloaks** - Protection, invisibility, magic resistance
- **Shields** - Including shield of reflection
- **Gloves** - Gauntlets of power, dexterity, fumbling
- **Boots** - Speed, levitation, water walking, jumping

#### Magical Items
- **Rings** - 28 types (adornment, protection, resistances, etc.)
- **Amulets** - 13 types including the Amulet of Yendor
- **Wands** - 27 types (directional and non-directional)
- **Potions** - 26 types with randomized appearances
- **Scrolls** - 23 types with randomized labels
- **Spellbooks** - 44 spells across 7 schools of magic

#### Tools
- **Containers** - Bags, boxes, chests, ice boxes
- **Light Sources** - Lamps, lanterns, candles
- **Musical Instruments** - Horns, harps, drums, flutes, whistles
- **Utility Tools** - Keys, lock picks, stethoscopes, tins, markers

#### Consumables
- **Food** - 40+ types (rations, fruits, meat, prepared foods)
- **Gems and Stones** - 22 precious gems, 9 worthless glass, 5 gray stones

#### Artifacts
- **General Artifacts** - Excalibur, Stormbringer, Mjollnir, etc. (42 total)
- **Quest Artifacts** - Role-specific rewards (13 unique items)
- **Invocation Items** - Bell, Book, Candelabrum

### Dungeons and Levels

#### Main Dungeon
- **Dungeons of Doom** - The main vertical dungeon (30+ levels)
- **Mines of the Gnome King** - Branch dungeon (2-4 access, 8-12 levels deep)
- **Sokoban** - Puzzle branch (4 levels)
- **Quest** - Role-specific branch (5 levels)
- **Fort Ludios** - Optional vault level (treasure)
- **Medusa's Island** - Mid-game challenge
- **Castle** - Wand of wishing location
- **Valley of the Dead** - Gateway to Gehennom
- **Gehennom** - Hell levels (multiple branches)
- **Vlad's Tower** - Vampire's domain (3 levels)
- **Wizard's Tower** - Final challenge before ascension
- **Elemental Planes** - Earth, Air, Fire, Water
- **Astral Plane** - Final level with three altars

#### Dungeon Features
- **Rooms** - Ordinary, shops, temples, throne rooms, vaults, zoos
- **Corridors** - Connecting passages
- **Stairs** - Up and down connections between levels
- **Doors** - Open, closed, locked, secret
- **Traps** - 20+ trap types
- **Fountains** - Water sources with special effects
- **Altars** - Aligned (lawful/neutral/chaotic) and unaligned
- **Sinks** - Water access points
- **Thrones** - Sitting for various effects

---

## Part III: Game Mechanics

### Core Systems

#### Character Attributes
- **Strength** - Melee damage, carrying capacity
- **Dexterity** - To-hit bonus, AC bonus
- **Constitution** - Hit points, poison resistance
- **Intelligence** - Spell success, magic energy
- **Wisdom** - Magic energy, spell success
- **Charisma** - Shop prices, pet loyalty

#### Experience and Leveling
- **Experience Points** - Gained from combat and actions
- **Character Level** - 1 to 30
- **Hit Points** - Health system
- **Magic Energy** - Spell casting resource
- **Skill System** - Weapon and spell skill advancement

#### Combat System
- **To-Hit Calculation** - Attack rolls and modifiers
- **Armor Class (AC)** - Defense rating (lower is better)
- **Damage Calculation** - Dice rolls and bonuses
- **Weapon Skills** - Restricted, unskilled, basic, skilled, expert
- **Critical Hits** - Special combat outcomes
- **Monster Attacks** - 28 attack types, 43 damage types

#### Magic System
- **Spell Schools** - Attack, healing, divination, enchantment, clerical, matter, escape
- **Spell Levels** - 1 to 7 (difficulty and power)
- **Spell Success** - Based on INT/WIS, experience, armor
- **Spell Casting** - Energy cost and failure rates
- **Spell Memory** - Learning and forgetting spells

#### Movement and Time
- **Movement Speed** - Normal (12), fast, slow, very fast
- **Time System** - Turns and action economy
- **Searching** - Finding hidden doors and traps
- **Carrying Capacity** - Weight burden levels
- **Encumbrance** - Unencumbered, burdened, strained, overtaxed, overloaded

#### Status Effects
- **Intrinsics** - Permanent/semi-permanent abilities
  - Resistances (fire, cold, shock, poison, sleep, disintegration)
  - See invisible, telepathy, teleport control
  - Speed, regeneration, reflection
- **Temporary Effects** - Blindness, confusion, stunning, hallucination
- **Permanent Conditions** - Lycanthropy, sliming
- **Hunger States** - Satiated, not hungry, hungry, weak, fainting

#### Item Mechanics
- **Beatitude (BUC)** - Blessed, uncursed, cursed status
- **Enchantment** - Enhancement levels (-7 to +7 and beyond)
- **Erosion** - Rustproof, fireproof, corroded, burnt, rotted
- **Identification** - Unknown, seen, called, fully identified
- **Stacking** - Mergeable vs. non-mergeable items

#### Object Generation
- **Randomization** - Appearance shuffling per game
- **Probability Tables** - General, container, Gehennom generation
- **Wish System** - Wand of wishing mechanics
- **Artifact Generation** - Gifting and finding

### Advanced Mechanics

#### Alchemy and Dipping
- **Potion Mixing** - Combining potions for effects
- **Item Dipping** - Potion and water interactions
- **Excalibur Creation** - Fountain dipping ritual

#### Polymorphing
- **Self-Polymorph** - Changing player form
- **Object Polymorph** - Item transformation
- **Monster Polymorph** - Enemy transformation
- **Polymorph Control** - Managing transformations

#### Genocide
- **Class Genocide** - Eliminating monster types
- **Reverse Genocide** - Creating monsters
- **Blessed vs. Cursed** - Single type vs. class-wide

#### Conducts and Challenges
- **Foodless** - Never eating
- **Vegan/Vegetarian** - Dietary restrictions
- **Atheist** - Never praying or sacrificing
- **Weaponless** - No wielded weapons
- **Pacifist** - No killing (extremely difficult)
- **Illiterate** - Never reading
- **Genocideless** - No genocide usage
- **Wishless** - No wishing

#### Luck System
- **Luck Value** - -13 to +13 range
- **Luck Effects** - To-hit, damage, item generation
- **Luck Timeout** - Gradual increase/decrease
- **Luck Sources** - Luckstone, sacrificing, prayer, breaking mirrors

#### Prayer and Altars
- **Prayer Timeout** - Safe/unsafe prayer timing
- **Altar Alignment** - Lawful, neutral, chaotic detection
- **Sacrificing** - Monster corpses for benefits
- **Altar Conversion** - Changing altar alignment

#### Shops and Trading
- **Shop Types** - General, armor, weapon, food, books, etc.
- **Pricing** - Charisma effects, identification
- **Stealing** - Consequences and shopkeeper wrath
- **Credit** - Selling items for shop credit

#### Pet System
- **Pet Types** - Cats, dogs, horses
- **Pet Commands** - Stay close, go away
- **Pet Growth** - Leveling and evolution
- **Pet Loyalty** - Charisma and feeding
- **Advanced Pets** - Polymorphed creatures

---

## Part IV: Strategies and Tactics

### Early Game Survival
- **Starting Strategy** - Role-specific approaches
- **Early Exploration** - Mines vs. main dungeon
- **Resource Management** - Food, potions, scrolls
- **Identification Strategies** - Price ID, testing
- **Pet Utilization** - Testing items, combat support

### Mid-Game Development
- **Ascension Kit** - Essential items checklist
  - Magic resistance source
  - Reflection (shield or silver dragon scale mail)
  - Speed boots or intrinsic speed
  - Free action
  - Levitation or flying
  - Telepathy or warning
  - Multiple resistances (fire, cold, shock, poison, sleep)
- **Quest Completion** - Timing and preparation
- **Sokoban Strategy** - Puzzle solutions and rewards
- **Castle Approach** - Wand acquisition
- **Protection Farming** - Enchanter strategy

### Advanced Tactics
- **Gehennom Navigation** - Demon lord encounters
- **Rider Management** - Death, Pestilence, Famine tactics
- **Wizard Harassment** - Dealing with revivals
- **Astral Plane Strategy** - Final altar rush
- **Speed Ascension** - Optimized routing

### Combat Tactics
- **Ranged Combat** - Projectile and spell usage
- **Melee Positioning** - Corridors and doorways
- **Escape Options** - Teleportation, levelport, digging
- **Boss Strategies** - Quest nemeses, demon lords
- **Crowd Control** - Sleep, confusion, conflict

### Item Management
- **Inventory Organization** - Naming and sorting
- **Container Strategy** - Bag of holding usage
- **Emergency Kits** - Escape and healing items
- **Polypile Optimization** - Item transformation
- **Wishing Strategy** - Optimal wish choices

### Character Building
- **Skill Advancement** - Which skills to train
- **Resistance Acquisition** - Corpse consumption
- **Attribute Enhancement** - Gain ability usage
- **Equipment Optimization** - Enchantment priorities
- **Artifact Collection** - Useful artifacts to seek

---

## Part V: Community and Culture

### NetHack Community
- **rec.games.roguelike.nethack** - Usenet newsgroup
- **NetHack Wiki** - Community knowledge base
- **Reddit r/nethack** - Discussion forum
- **IRC Channels** - Real-time community chat
- **Public Servers** - Online play (NAO, hardfought, etc.)

### NetHack Culture
- **YASD** - "Yet Another Stupid Death"
- **YAAP** - "Yet Another Ascension Post"
- **Pudding Farming** - Experience grinding
- **Astral Call** - Final level announcements
- **Conducts** - Self-imposed challenges
- **Dev Team Thinks of Everything** - Famous phrase
- **"The DevTeam thinks of everything"** - Common discovery exclamation

### Variants and Mods
- **UnNetHack** - Enhanced difficulty variant
- **dNetHack** - Significantly expanded content
- **SLASH'EM** - Super Lots of Added Stuff Hack Extended Magic
- **SporkHack** - Balance and interface improvements
- **GruntHack** - Streamlined variant
- **NetHack 4** - Modern interface fork
- **NetHack Fourk** - Community fork

### Speedrunning and Challenges
- **Realtime Speedruns** - Fastest real-world time
- **Turncount Speedruns** - Fewest game turns
- **Streakless** - Multiple consecutive ascensions
- **Conduct Runs** - Single or multiple conducts
- **Race to Ascension** - Community events

### Notable Players and Achievements
- **First Ascension** - Historical milestone
- **Longest Streaks** - Consecutive win records
- **Fastest Times** - Speed records
- **All-Conduct Ascensions** - Ultimate challenge
- **Bot Achievements** - AI players

---

## Part VI: Technical Reference

### Codebase Architecture

#### Source Organization
```
/home/user/NetHack/
├── include/          - Header files (.h)
├── src/              - C source files (.c)
├── dat/              - Data files (text, Lua scripts)
├── doc/              - Documentation
├── sys/              - Platform-specific code
├── win/              - Window system interfaces
├── util/             - Build utilities
└── DEVEL/            - Developer documentation
```

#### Core Header Files
- **monsters.h** (3,927 lines) - Monster definitions
- **objects.h** - Object definitions
- **permonst.h** - Monster structure
- **objclass.h** - Object class structure
- **monflag.h** - Monster flags and properties
- **monattk.h** - Attack and damage types
- **artilist.h** - Artifact definitions
- **hack.h** - Main game structures
- **config.h** - Compile-time configuration

#### Core Source Files
- **monst.c** - Monster data initialization
- **objects.c** - Object data initialization
- **artifact.c** - Artifact system
- **mon.c** (5,500+ lines) - Monster actions
- **monmove.c** (2,800+ lines) - Monster AI
- **makemon.c** (2,200+ lines) - Monster generation
- **mkobj.c** - Object generation
- **do.c** - Player actions
- **apply.c** - Tool and item usage
- **zap.c** - Wand usage
- **read.c** - Scroll reading
- **pray.c** - Prayer system
- **sp_lev.c** - Special level generation
- **dungeon.c** - Dungeon structure
- **decl.c** - Global declarations

#### Data Files
- **data.base** - Various game data
- **oracles.txt** (549 lines) - Oracle messages
- **bogusmon.txt** (549 lines) - Hallucination names
- **quest.lua** - Quest definitions
- **dungeon.def** - Dungeon structure definition

#### Build System
- **Makefile** - Build configuration
- **makedefs.c** - Data compilation utility
- **dgn_comp** - Dungeon compiler
- **lev_comp** - Level compiler

### Monster System Details
- **Monster Structure** - `struct permonst` definition
- **Attack System** - Attack types and damage calculation
- **Monster Flags** - M1_*, M2_*, M3_* flags (100+ total)
- **Monster AI** - Pathfinding, combat, item use
- **Monster Generation** - Level-appropriate spawning
- **Monster Sounds** - MS_* sound types (45 types)
- **Monster Sizes** - MZ_TINY to MZ_GIGANTIC (6 sizes)

### Object System Details
- **Object Structure** - `struct objclass` definition
- **Object Classes** - 17 major categories
- **Material Types** - 16 materials (iron, wood, glass, etc.)
- **Randomization** - Appearance shuffling
- **Beatitude** - BUC system implementation
- **Enchantment** - spe field and limits
- **Erosion** - Rust, corrosion, burn, rot

### Artifact System
- **Artifact Properties** - SPFX_* flags (20+ types)
- **Artifact Restrictions** - Alignment, role, race
- **Intelligent Artifacts** - Self-willed items
- **Artifact Powers** - Carried, wielded, invoked
- **Artifact Generation** - Gifting and wishing

### Level Generation
- **Random Levels** - Procedural generation
- **Special Levels** - Hand-designed levels
- **Mazes** - Maze generation algorithms
- **Room Types** - 10+ special room types
- **Corridor Generation** - Connection algorithms

### Save System
- **Save File Format** - Binary structure
- **Bones Files** - Player ghost data
- **Level Files** - Persistent level storage
- **Compression** - Save file compression

### Window System (Interfaces)
- **TTY** - Terminal interface (default)
- **Curses** - Enhanced terminal interface
- **X11** - X Window System
- **Qt** - Cross-platform GUI
- **GTK** - GNOME interface (outdated)

### Porting and Platform Support
- **Unix/Linux** - Primary platform
- **Windows** - Native and MinGW builds
- **macOS** - Unix-based build
- **DOS** - Legacy support (outdated)

---

## Appendices

### Appendix A: Glossary
See [**GLOSSARY.md**](GLOSSARY.md) for complete A-Z terminology reference.

### Appendix B: Quick Reference Tables

#### Damage Type Abbreviations
- **PHYS** - Physical damage
- **FIRE** - Fire damage
- **COLD** - Cold damage
- **ELEC** - Electrical damage
- **ACID** - Acid damage
- **STON** - Petrification
- **DISN** - Disintegration
- **DRLI** - Drain life levels
- **DREN** - Drain energy

#### Resistance Flags
- **MR_FIRE** - Fire resistance
- **MR_COLD** - Cold resistance
- **MR_SLEEP** - Sleep resistance
- **MR_DISINT** - Disintegration resistance
- **MR_ELEC** - Shock resistance
- **MR_POISON** - Poison resistance
- **MR_ACID** - Acid resistance
- **MR_STONE** - Petrification resistance

#### Common Abbreviations
- **AC** - Armor Class
- **MC** - Magic Cancellation (0-3)
- **HP** - Hit Points
- **Pw** - Power (magic energy)
- **XP** - Experience Points
- **XL** - Experience Level
- **DL** - Dungeon Level
- **GDSM** - Gray Dragon Scale Mail
- **SDSM** - Silver Dragon Scale Mail
- **BoH** - Bag of Holding
- **WoW** - Wand of Wishing
- **GoP** - Gauntlets of Power

### Appendix C: Source File Index
Complete source code reference organized by system:
- [Monster System Files](#monster-system-details)
- [Object System Files](#object-system-details)
- [Dungeon System Files](#level-generation)
- [Combat System Files](#combat-system)
- [Magic System Files](#magic-system)

### Appendix D: Data File Index
- [**monsters_database.json**](monsters_database.json) - 394 monsters in JSON format
- [**BESTIARY.txt**](BESTIARY.txt) - Formatted bestiary report
- [**CATALOG_FILES.txt**](CATALOG_FILES.txt) - File location index

### Appendix E: External Resources
See [**BIBLIOGRAPHY.md**](BIBLIOGRAPHY.md) for complete list.

---

## Navigation Tips

### By Topic
- **Learning NetHack?** Start with [Introduction](INTRODUCTION.md), then [Glossary](GLOSSARY.md)
- **Monster Information?** See [Monster Catalog](MONSTER_CATALOG_COMPLETE.md)
- **Item Details?** See [Item Compendium](NETHACK_ITEM_COMPENDIUM.md)
- **Game Mechanics?** See Part III of this index
- **Source Code?** See Part VI - Technical Reference
- **Strategies?** See Part IV - Strategies and Tactics

### By Experience Level
- **New Players:** Introduction → Glossary → Part II (Game World) → Part IV (Basic Strategies)
- **Intermediate Players:** Part III (Mechanics) → Part IV (Advanced Tactics) → Monster/Item references
- **Advanced Players:** Part VI (Technical) → Source code → Monster/Item databases
- **Developers:** Part VI → DEVEL/ directory → Source organization

### By Goal
- **First Ascension:** Part IV (Strategies) → Ascension Kit checklist
- **Conduct Runs:** Part III (Mechanics) → Part IV (Conduct strategies)
- **Speedrunning:** Part IV (Speed Ascension) → Advanced Tactics
- **Variant Development:** Part VI (Technical) → Source code organization
- **Academic Study:** Introduction (History) → Part VI (Architecture) → Bibliography

---

## Document Maintenance

**Last Updated:** 2025-11-19
**NetHack Version:** 3.7.0
**Encyclopedia Version:** 1.0

### Contributing
This encyclopedia is compiled from NetHack 3.7 source code and documentation. To suggest corrections or additions, please refer to the official NetHack development team.

### Version History
- **1.0** (2025-11-19) - Initial comprehensive documentation structure
  - Complete monster catalog (394 monsters)
  - Complete item compendium (430+ items)
  - Master index with 6 parts
  - Glossary, bibliography, and introduction

---

## Copyright and License

NetHack is Copyright 1985-2023 by Stichting Mathematisch Centrum and M. Stephenson.
NetHack may be freely redistributed. See license for details.

This documentation is derived from the NetHack source code and is provided under the same NetHack General Public License.

---

**The NetHack Encyclopedia - Explore. Discover. Ascend.**
