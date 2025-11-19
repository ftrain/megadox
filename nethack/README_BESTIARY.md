# NetHack Monster Catalog - Summary

## Complete Catalog Generated

I have successfully cataloged all 394 monsters and NPCs in NetHack 3.7. Here are the key files created:

### Generated Files

1. **MONSTER_CATALOG_COMPLETE.md** - Comprehensive documentation of all monsters, NPCs, and systems
2. **BESTIARY.txt** - Detailed bestiary with all monsters categorized by symbol
3. **monsters_database.json** - Machine-readable JSON database of all monsters
4. **monster_parser.py** - Python script to extract monster data from source
5. **analyze_bestiary.py** - Python script to analyze and categorize monsters

## Quick Stats

- **Total Monsters:** 394 unique monster types
- **Monster Categories:** 46 different symbol classes
- **Level Range:** 0 (newt, jackal) to 106 (Demogorgon)
- **Unique/Boss Monsters:** 36+
- **Demon Lords:** 8 (Demogorgon, Asmodeus, Orcus, etc.)
- **The Four Horsemen:** Death, Pestilence, Famine (Level 30 each)
- **Dragons:** 18 types (9 colors × 2 ages)
- **NPCs:** 76 human characters including shopkeepers, priests, guards

## Monster Categories

### Most Common Types
- **@ (Humans/NPCs):** 76 monsters
- **& (Demons):** 29 monsters  
- **D (Dragons):** 24 monsters
- **d (Dogs/Canines):** 16 monsters
- **H (Giants):** 11 monsters
- **' (Golems):** 11 monsters
- **Z (Zombies):** 10 monsters
- **o (Orcs):** 9 monsters

### Top 10 Most Powerful Monsters

1. **Demogorgon** (Lv 106) - Prince of Demons
2. **Asmodeus** (Lv 105) - Overlord of the Nine Hells
3. **Baalzebub** (Lv 89) - Lord of the Flies
4. **Dispater** (Lv 78) - Archdevil
5. **Charon** (Lv 76) - Ferryman of the dead
6. **Geryon** (Lv 72) - Archdevil
7. **Orcus** (Lv 66) - Prince of the Undead
8. **Yeenoghu** (Lv 56) - Demon Lord
9. **Juiblex** (Lv 50) - Demon Lord of Slime
10. **Death/Pestilence/Famine** (Lv 30) - The Four Horsemen

## Key Source Files

### Monster Definitions
- `/home/user/NetHack/include/monsters.h` (3,927 lines) - Complete monster definitions
- `/home/user/NetHack/src/monst.c` - Monster data initialization
- `/home/user/NetHack/include/permonst.h` - Monster structure definition
- `/home/user/NetHack/include/monflag.h` - Monster flags and properties
- `/home/user/NetHack/include/monattk.h` - Attack and damage type definitions

### Monster Behavior & AI
- `/home/user/NetHack/src/monmove.c` - Monster movement and AI
- `/home/user/NetHack/src/mon.c` - Monster actions and interactions
- `/home/user/NetHack/src/makemon.c` - Monster generation system
- `/home/user/NetHack/src/mondata.c` - Monster data utilities

## Monster Properties

### Attack Types (28 total)
Physical: claw, bite, kick, butt, touch, sting, hug, tentacle, weapon
Ranged: spit, breath, gaze, magic
Special: engulf, explode (proximity/death), passive

### Damage Types (43 total)
Elemental: fire, cold, electric, acid
Draining: life, magic, str, dex, con, int
Status: sleep, stun, slow, paralyze, blind, confuse, hallucinate
Special: petrify, disintegrate, poison, disease, lycanthropy, slime, polymorph
Stealing: gold, items, multiple items (seduction)
Other: rust, corrode, disenchant, decay, digest, heal, teleport

### Resistances (8 core types)
Fire, Cold, Sleep, Disintegration, Electricity, Poison, Acid, Petrification
Plus Magic Resistance (0-127%)

### Monster Flags (100+ behavioral flags)
- Movement: fly, swim, wallwalk, tunnel, amphibious
- Anatomy: no eyes/hands/limbs/head, mindless, humanoid
- Special: regenerate, see invisible, teleport, shapeshift
- Diet: carnivore, herbivore, omnivore, metallivore
- Race: human, elf, dwarf, gnome, orc, demon, undead, giant
- Behavior: hostile, peaceful, domestic, wander, stalk
- Collection: greedy (gold), jewels (gems), magic items

## Notable Monster Groups

### Unique Named Monsters
- Wizard of Yendor - Main antagonist
- Vlad the Impaler - Vampire lord
- Medusa - Petrifying gaze
- Croesus - Fort Ludios guardian
- Oracle - Provides advice
- Charon - Ferryman (can't be killed)

### Quest-Related NPCs
Quest Leaders (role-specific mentors):
- Lord Carnarvon (Archeologist)
- Pelias (Barbarian)  
- Shaman Karnov (Caveman)
- Hippocrates (Healer)
- King Arthur (Knight)
- Grand Master (Monk)
- Orion (Ranger)
- Master of Thieves (Rogue)
- Lord Sato (Samurai)
- Twoflower (Tourist)
- Neferet the Green (Wizard)

Quest Nemeses (role-specific bosses):
- Ixoth (chromatic dragon)
- Scorpius (giant scorpion)
- Nalzok (demon)
- Thoth Amon (dark wizard)
- Master Kaen (evil monk)
- Master Assassin
- Ashikaga Takauji (samurai)
- Lord Surtur (fire giant)

### Peaceful NPCs
- Shopkeepers - Sell items, extremely tough
- Priests - Temples, donations, healing
- Guards - Castle and vault protection
- Watch - Town guards
- Oracle - Advice for gold
- Nurse - Can heal (sometimes)

### Special Monster Families

**Dragons (9 colors, 18 total)**
- Gray (magic resistance)
- Silver (reflection)
- Red (fire)
- White (cold)
- Orange (sleep)
- Black (disintegration)
- Blue (lightning)
- Green (poison)
- Yellow (acid)

**Lycanthropes**
- Wererat, Werejackal, Werewolf
- Transmit lycanthropy
- Shapeshift between forms

**Undead**
- Zombies (13 types)
- Mummies (7 types)
- Vampires (3 types)
- Wraiths (3 types)
- Liches (4 types)
- Ghosts (2 types)

**Golems (11 materials)**
- Straw, Paper, Rope, Gold, Leather, Wood
- Flesh, Clay, Stone, Glass, Iron

**Elementals (4 planes)**
- Air, Fire, Earth, Water
- Each native to their elemental plane

## Monster AI Features

The monster AI system includes:

- **Pathfinding:** A* algorithm to chase player
- **Combat:** Attack selection, spell casting
- **Item Interaction:** Pick up, use, and equip items
- **Group Behavior:** Spawn and move in packs
- **Fleeing:** Retreat when low HP or frightened
- **Stalking:** Follow player between levels
- **Covetous Behavior:** Seek quest artifacts
- **Door/Trap Interaction:** Open, close, unlock
- **Summoning:** Some monsters summon allies
- **Shapeshifting:** Chameleons, vampires, mimics

## Monster Generation

Controlled by generation flags:
- **G_UNIQ** - Unique (generated once)
- **G_NOGEN** - Special generation only
- **G_HELL** - Gehennom only
- **G_NOHELL** - Not in Gehennom
- **G_SGROUP/G_LGROUP** - Spawn in groups
- **G_GENO** - Can be genocided
- **G_FREQ** - Frequency (0-7)

## Usage

View the complete bestiary:
```bash
cat /home/user/NetHack/BESTIARY.txt | less
```

Search the JSON database:
```bash
cat /home/user/NetHack/monsters_database.json | jq '.[] | select(.level > 50)'
```

Read comprehensive documentation:
```bash
cat /home/user/NetHack/MONSTER_CATALOG_COMPLETE.md | less
```

---

*Catalog generated from NetHack 3.7 source code - 2025-11-19*
