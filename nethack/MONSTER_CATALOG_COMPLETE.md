# NetHack Complete Monster Catalog & Bestiary

## Overview
This document provides a comprehensive catalog of all monsters, NPCs, and creature systems in NetHack 3.7.

**Total Monsters:** 394 unique monster types

---

## File Locations

### Core Monster Data Files
- **`/home/user/NetHack/include/monsters.h`** - Complete monster definitions (3,927 lines)
- **`/home/user/NetHack/src/monst.c`** - Monster initialization and data structures
- **`/home/user/NetHack/include/permonst.h`** - Monster structure definition
- **`/home/user/NetHack/include/monflag.h`** - Monster flags and properties
- **`/home/user/NetHack/include/monattk.h`** - Attack types and damage definitions

### Monster Behavior & AI Files
- **`/home/user/NetHack/src/monmove.c`** - Monster movement and AI
- **`/home/user/NetHack/src/mon.c`** - Monster actions and interactions
- **`/home/user/NetHack/src/makemon.c`** - Monster generation system
- **`/home/user/NetHack/src/mondata.c`** - Monster data queries and utilities

### Generated Data
- **`/home/user/NetHack/monsters_database.json`** - Complete monster database (JSON format)
- **`/home/user/NetHack/BESTIARY.txt`** - Formatted bestiary report

---

## Monster Structure Definition

Each monster in NetHack has the following properties:

```c
struct permonst {
    const char *pmnames[NUM_MGENDERS];  // Name (male/female/neutral variants)
    const enum monnums pmidx;            // PM_ identifier
    char mlet;                           // Display symbol
    schar mlevel;                        // Base monster level
    schar mmove;                         // Move speed (12 = normal)
    schar ac;                            // Armor class (lower is better)
    schar mr;                            // Magic resistance (0-127%)
    aligntyp maligntyp;                  // Alignment (lawful/neutral/chaotic)
    unsigned short geno;                 // Generation flags
    struct attack mattk[NATTK];          // Up to 6 attacks
    unsigned short cwt;                  // Corpse weight
    unsigned short cnutrit;              // Nutritional value
    uchar msound;                        // Sound type
    uchar msize;                         // Physical size
    uchar mresists;                      // Resistances
    uchar mconveys;                      // Properties conveyed by eating
    unsigned long mflags1, mflags2;      // Behavioral flags
    unsigned short mflags3;              // Additional flags
    uchar difficulty;                    // Calculated difficulty
    uchar mcolor;                        // Display color
};
```

---

## Monster Categories by Symbol

### Lowercase Letters (a-z)

| Symbol | Category | Count | Examples |
|--------|----------|-------|----------|
| a | Ants & Insects | 6 | giant ant, killer bee, soldier ant, fire ant |
| b | Blobs | 3 | acid blob, quivering blob, gelatinous cube |
| c | Cockatrices | 3 | chickatrice, cockatrice, pyrolisk |
| d | Dogs & Canines | 13 | jackal, dog, wolf, winter wolf, hell hound |
| e | Eyes & Orbs | 4 | floating eye, freezing sphere, flaming sphere, shocking sphere |
| f | Felines | 8 | kitten, housecat, jaguar, panther, tiger |
| g | Gremlins | 2 | gremlin, gargoyle |
| h | Humanoids | 12 | hobbit, dwarf, bugbear, hobgoblin |
| i | Imps | 5 | tengu, imp, quasit, homunculus |
| j | Jellies | 4 | blue jelly, spotted jelly, ochre jelly, green slime |
| k | Kobolds | 4 | kobold, large kobold, kobold lord, kobold shaman |
| l | Leprechauns | 1 | leprechaun |
| m | Mimics | 4 | small mimic, large mimic, giant mimic, giant mummy |
| n | Nymphs | 3 | wood nymph, water nymph, mountain nymph |
| o | Orcs | 9 | goblin, hobgoblin, orc, hill orc, Uruk-hai, orc-captain |
| p | Piercers | 2 | rock piercer, iron piercer |
| q | Quadrupeds | 8 | rothe, mumak, leocrotta, wumpus |
| r | Rodents | 6 | sewer rat, giant rat, rabid rat, wererat |
| s | Spiders | 5 | cave spider, giant spider, scorpion, phase spider |
| t | Trappers | 2 | lurker above, trapper |
| u | Unicorns | 4 | white unicorn, gray unicorn, black unicorn, pony |
| v | Vortices | 6 | fog cloud, dust vortex, ice vortex, energy vortex |
| w | Worms | 5 | baby long worm, long worm, purple worm |
| x | Xan | 1 | grid bug, xan |
| y | Lights | 2 | yellow light, black light |
| z | Zruty | 1 | zruty |

### Uppercase Letters (A-Z)

| Symbol | Category | Count | Examples |
|--------|----------|-------|----------|
| A | Angels | 6 | couatl, Aleax, Angel, ki-rin, Archon |
| B | Bats | 3 | bat, giant bat, vampire bat |
| C | Centaurs | 4 | plains centaur, forest centaur, mountain centaur |
| D | Dragons | 17 | baby dragons (9 colors), adult dragons (9 colors) |
| E | Elementals | 4 | air elemental, fire elemental, earth elemental, water elemental |
| F | Fungi | 5 | lichen, brown mold, yellow mold, green mold, shrieker |
| G | Gnomes | 4 | gnome, gnome lord, gnomish wizard, gnome king |
| H | Giants | 12 | hill giant, stone giant, fire giant, frost giant, storm giant, titan |
| J | Jabberwocks | 1 | jabberwock |
| K | Kops | 4 | Keystone Kop, Kop Sergeant, Kop Lieutenant, Kop Kaptain |
| L | Liches | 4 | lich, demilich, master lich, arch-lich |
| M | Mummies | 6 | kobold mummy, gnome mummy, orc mummy, dwarf mummy, elf mummy, human mummy |
| N | Nagas | 6 | red naga, black naga, golden naga, guardian naga |
| O | Ogres | 4 | ogre, ogre lord, ogre king |
| P | Puddings | 3 | gray ooze, brown pudding, black pudding |
| Q | Quantum Mechanics | 1 | quantum mechanic |
| R | Rust Monsters | 2 | rust monster, disenchanter |
| S | Snakes | 7 | garter snake, snake, water moccasin, pit viper, python, cobra |
| T | Trolls | 4 | troll, ice troll, rock troll, water troll |
| U | Umber Hulks | 2 | umber hulk, shambling horror |
| V | Vampires | 3 | vampire, vampire lord, Vlad the Impaler |
| W | Wraiths | 3 | barrow wight, wraith, Nazgul |
| X | Xorns | 1 | xorn |
| Y | Yetis | 2 | monkey, ape, owlbear, yeti, carnivorous ape |
| Z | Zombies | 13 | kobold zombie, gnome zombie, orc zombie, dwarf zombie, elf zombie |

### Special Symbols

| Symbol | Category | Count | Examples |
|--------|----------|-------|----------|
| @ | Humans & NPCs | 76 | human, elf, shopkeeper, priest, guard, soldier, Wizard of Yendor |
| & | Demons | 29 | imp, quasit, demon lords (Orcus, Demogorgon, Asmodeus), Riders |
| ' | Golems | 11 | straw, paper, rope, leather, wood, flesh, clay, stone, glass, iron |
| space | Ghosts | 2 | ghost, shade |
| ; | Eels | 6 | jellyfish, piranha, giant eel, shark, electric eel, kraken |
| : | Lizards | 8 | newt, gecko, iguana, lizard, chameleon, crocodile, salamander |
| ~ | Worm Tails | 1 | long worm tail |

---

## Unique Monsters & Bosses

### The Four Horsemen (Riders of the Apocalypse)
**Level 30** - Found on the Astral Plane

1. **Death** - AC: -5, MR: 100%
   - Attacks: Touch (instant death), weapon
   - Cannot be killed permanently

2. **Pestilence** - AC: -5, MR: 100%
   - Attacks: Touch (disease/sickness), weapon
   - Cannot be killed permanently

3. **Famine** - AC: -5, MR: 100%
   - Attacks: Touch (hunger), weapon
   - Cannot be killed permanently

### Demon Lords & Princes
Ordered by level (weakest to strongest):

1. **Juiblex** (Lv 50) - Demon lord of slime and ooze
2. **Yeenoghu** (Lv 56) - Demon lord of gnolls
3. **Orcus** (Lv 66) - Prince of the undead
4. **Geryon** (Lv 72) - Archdevil
5. **Dispater** (Lv 78) - Archdevil
6. **Baalzebub** (Lv 89) - Lord of the Flies
7. **Asmodeus** (Lv 105) - Overlord of the Nine Hells
8. **Demogorgon** (Lv 106) - Prince of Demons (most powerful)

### Quest Nemeses (Role-Specific)
Each player role has a unique nemesis at the end of their quest:
- Archeologist: Minion of Huhetotl
- Barbarian: Thoth Amon
- Caveman: Chromatic Dragon
- Healer: Cyclops
- Knight: Ixoth
- Monk: Master Kaen
- Priest: Nalzok
- Ranger: Scorpius
- Rogue: Master Assassin
- Samurai: Ashikaga Takauji
- Tourist: Master of Thieves
- Valkyrie: Lord Surtur
- Wizard: Dark One

### Other Unique Monsters

- **Wizard of Yendor** - Main antagonist, can revive indefinitely
- **Vlad the Impaler** - Vampire lord in Vlad's Tower
- **Medusa** - Petrifying gaze, found on her island
- **Croesus** - Guards the treasure in Fort Ludios
- **Oracle** - Peaceful NPC who provides advice
- **High Priests** - Guardians of temples on the Astral Plane

---

## Attack Types

### Physical Attacks
- **AT_CLAW** - Claw/punch/hit
- **AT_BITE** - Bite attack
- **AT_KICK** - Kick
- **AT_BUTT** - Head butt
- **AT_TUCH** - Touch
- **AT_STNG** - Sting
- **AT_HUGS** - Crushing bear hug
- **AT_TENT** - Tentacle
- **AT_WEAP** - Weapon attack

### Ranged Attacks
- **AT_SPIT** - Spit substance
- **AT_BREA** - Breath weapon
- **AT_GAZE** - Gaze attack
- **AT_MAGC** - Magic spell

### Special Attacks
- **AT_ENGL** - Engulf/swallow
- **AT_EXPL** - Explodes on proximity
- **AT_BOOM** - Explodes when killed
- **AT_NONE** - Passive (contact damage)

---

## Damage Types

### Elemental
- **AD_FIRE** - Fire damage
- **AD_COLD** - Cold damage
- **AD_ELEC** - Electric damage
- **AD_ACID** - Acid damage

### Status Effects
- **AD_SLEE** - Sleep
- **AD_STUN** - Stun
- **AD_SLOW** - Slow
- **AD_PLYS** - Paralyze
- **AD_BLND** - Blind
- **AD_CONF** - Confuse
- **AD_HALU** - Hallucinate

### Draining
- **AD_DRLI** - Drain life levels
- **AD_DREN** - Drain magic energy
- **AD_DRST** - Drain strength (poison)
- **AD_DRDX** - Drain dexterity
- **AD_DRCO** - Drain constitution
- **AD_DRIN** - Drain intelligence

### Special
- **AD_STON** - Petrify (cockatrice, medusa)
- **AD_DISN** - Disintegrate
- **AD_WERE** - Lycanthropy
- **AD_SLIM** - Turn to green slime
- **AD_DISE** - Disease
- **AD_POLY** - Polymorph
- **AD_TLPT** - Teleport

### Stealing
- **AD_SGLD** - Steal gold
- **AD_SITM** - Steal item
- **AD_SEDU** - Seduce and steal multiple items

### Other
- **AD_RUST** - Rust armor
- **AD_CORR** - Corrode armor
- **AD_ENCH** - Disenchant
- **AD_DCAY** - Decay organics
- **AD_DGST** - Digest (trappers)
- **AD_HEAL** - Heal opponent (nurse)
- **AD_STCK** - Stick to target

---

## Monster Resistances

Monsters can have the following resistances:

- **MR_FIRE** - Fire resistance
- **MR_COLD** - Cold resistance
- **MR_SLEEP** - Sleep resistance
- **MR_DISINT** - Disintegration resistance
- **MR_ELEC** - Electricity resistance
- **MR_POISON** - Poison resistance
- **MR_ACID** - Acid resistance
- **MR_STONE** - Petrification resistance

Magic resistance (MR) is a percentage chance (0-127%) to resist magical attacks.

---

## Monster Flags & Properties

### Movement & Physical (M1)
- **M1_FLY** - Can fly or float
- **M1_SWIM** - Can traverse water
- **M1_AMORPHOUS** - Can flow under doors
- **M1_WALLWALK** - Can phase through rock
- **M1_CLING** - Can cling to ceiling
- **M1_TUNNEL** - Can tunnel through rock
- **M1_AMPHIBIOUS** - Can survive underwater
- **M1_BREATHLESS** - Doesn't need to breathe

### Anatomy (M1)
- **M1_NOEYES** - No eyes (immune to blindness, gazes)
- **M1_NOHANDS** - No hands
- **M1_NOLIMBS** - No arms/legs
- **M1_NOHEAD** - No head (can't be beheaded)
- **M1_MINDLESS** - No mind (golems, zombies)
- **M1_HUMANOID** - Humanoid shape
- **M1_ANIMAL** - Animal
- **M1_SLITHY** - Serpentine body

### Special Abilities (M1)
- **M1_REGEN** - Regenerates HP
- **M1_SEE_INVIS** - Can see invisible
- **M1_TPORT** - Can teleport
- **M1_TPORT_CNTRL** - Controls teleportation
- **M1_THICK_HIDE** - Thick hide or scales
- **M1_OVIPAROUS** - Lays eggs

### Diet (M1)
- **M1_CARNIVORE** - Eats corpses
- **M1_HERBIVORE** - Eats fruits
- **M1_OMNIVORE** - Eats both
- **M1_METALLIVORE** - Eats metal

### Race & Type (M2)
- **M2_UNDEAD** - Undead creature
- **M2_WERE** - Lycanthrope
- **M2_HUMAN** - Human
- **M2_ELF** - Elf
- **M2_DWARF** - Dwarf
- **M2_GNOME** - Gnome
- **M2_ORC** - Orc
- **M2_DEMON** - Demon
- **M2_GIANT** - Giant
- **M2_SHAPESHIFTER** - Can shapeshift

### Status & Behavior (M2)
- **M2_LORD** - Lord of its kind
- **M2_PRINCE** - Prince/overlord
- **M2_MINION** - Minion of a deity
- **M2_HOSTILE** - Always starts hostile
- **M2_PEACEFUL** - Always starts peaceful
- **M2_DOMESTIC** - Can be tamed by feeding
- **M2_WANDER** - Wanders randomly
- **M2_STALK** - Follows to other levels
- **M2_NASTY** - Extra nasty (more XP)
- **M2_STRONG** - Strong/big monster

### Item Collection (M2)
- **M2_GREEDY** - Likes gold
- **M2_JEWELS** - Likes gems
- **M2_COLLECT** - Picks up weapons and food
- **M2_MAGIC** - Picks up magic items

### Quest Items (M3)
- **M3_WANTSAMUL** - Wants Amulet of Yendor
- **M3_WANTSBELL** - Wants Bell of Opening
- **M3_WANTSBOOK** - Wants Book of the Dead
- **M3_WANTSCAND** - Wants Candelabrum
- **M3_WANTSARTI** - Wants quest artifact

### Other (M3)
- **M3_WAITFORU** - Waits for player
- **M3_CLOSE** - Lets you get close
- **M3_INFRAVISION** - Has infravision
- **M3_INFRAVISIBLE** - Visible by infravision
- **M3_DISPLACES** - Displaces other monsters

---

## Monster Generation System

### Generation Flags (G_*)

- **G_UNIQ** - Generated only once (unique monsters)
- **G_NOGEN** - Generated only specially (not random)
- **G_NOHELL** - Not generated in Gehennom
- **G_HELL** - Generated only in Gehennom
- **G_SGROUP** - Appears in small groups
- **G_LGROUP** - Appears in large groups
- **G_GENO** - Can be genocided
- **G_NOCORPSE** - Leaves no corpse
- **G_FREQ** - Creation frequency (0-7, mask)

### Monster Difficulty

Monsters have a calculated difficulty rating based on:
- Level
- AC (armor class)
- Hit points
- Attack power
- Special abilities
- Resistances
- Speed

---

## Monster AI & Behavior

### Key Behavior Files

1. **monmove.c** - Monster movement AI
   - Pathfinding toward player
   - Fleeing behavior
   - Item searching
   - Door/trap interaction
   - Special monster movements (teleport, phase, etc.)

2. **mon.c** - Monster actions
   - Combat calculations
   - Monster death and revival
   - Shapeshifting
   - Status effects
   - Monster-to-monster combat

3. **makemon.c** - Monster creation
   - Level-appropriate generation
   - Group spawning
   - Inventory assignment
   - Special placements

### AI Behaviors

- **Pathfinding**: Monsters use A* pathfinding to chase the player
- **Covetous monsters**: Actively seek quest artifacts
- **Pack behavior**: Some monsters spawn and move in groups
- **Fleeing**: Monsters flee when low on HP or frightened
- **Stalking**: Some monsters follow between levels
- **Wandering**: Random movement patterns
- **Item collection**: Picking up and using items
- **Door interaction**: Opening, closing, unlocking doors
- **Spell casting**: Intelligent monsters cast appropriate spells
- **Summoning**: Some monsters can summon allies

---

## NPCs & Shopkeepers

### Shopkeepers
- **General shopkeeper** (Lv 12) - Sells various items
- **Specialized shops**: armor, weapons, food, books, potions, etc.
- Extremely tough in combat
- Will pursue and punish thieves

### Priests & Temples
- **Aligned priest/priestess** (Lv 12) - Found in temples
- **High priest/priestess** (Lv 25) - Astral plane guardians
- **Arch Priest** (Lv 25) - Special temple guardians
- Offer donations, healing, and divination

### Guards & Law Enforcement
- **Guard** (Lv 12) - Castle and vault guards
- **Watchman** (Lv 6) - Town watch
- **Watch captain** (Lv 10) - Watch leader
- **Kops** (Lv 1-4) - Keystone Kops (comedic police)
- **Soldiers** (Lv 6-10) - Military forces

### Other NPCs
- **Oracle** (Lv 12) - Provides advice for 50-100 gold
- **Nurse** (Lv 11) - Can heal you (sometimes)
- **Quest leaders** - Role-specific mentors
- **Quest guardians** - Protect quest leaders

---

## Dragon Types

NetHack features 9 dragon colors, each with unique breath weapons:

1. **Gray dragon** - Magic resistance
2. **Silver dragon** - Reflection
3. **Red dragon** - Fire breath
4. **White dragon** - Cold breath
5. **Orange dragon** - Sleep gas
6. **Black dragon** - Disintegration
7. **Blue dragon** - Lightning
8. **Green dragon** - Poison gas
9. **Yellow dragon** - Acid

Each dragon has:
- Baby form (lower level)
- Adult form (higher level)
- Corresponding dragon scale mail when killed

---

## Special Monster Groups

### Lycanthropes (Wereanimals)
- **Wererat** (Lv 2)
- **Werejackal** (Lv 2)
- **Werewolf** (Lv 5)
- Can transmit lycanthropy
- Shape-shift between human and animal forms

### Undead
- **Zombies**: 13 types (various humanoid zombies)
- **Mummies**: 7 types (various mummified humanoids)
- **Vampires**: 3 types (vampire, vampire lord, Vlad)
- **Wraiths**: 3 types (barrow wight, wraith, Nazgul)
- **Liches**: 4 types (lich, demilich, master lich, arch-lich)
- **Ghosts**: 2 types (ghost, shade)

### Golems
11 types made from different materials:
- Straw, paper, rope, gold, leather, wood
- Flesh, clay, stone, glass, iron

Each has material-specific properties and vulnerabilities.

### Elementals
- **Air elemental** - Air plane native
- **Fire elemental** - Fire plane native
- **Earth elemental** - Earth plane native
- **Water elemental** - Water plane native

---

## Monster Sounds (MS_*)

Monsters make different sounds:

- **MS_SILENT** - Makes no sound
- **MS_BARK** - Barks/howls
- **MS_MEW** - Mews/hisses
- **MS_ROAR** - Roars
- **MS_GROWL** - Growls
- **MS_SQEEK** - Squeaks
- **MS_HISS** - Hisses
- **MS_BUZZ** - Buzzes
- **MS_GRUNT** - Grunts
- **MS_NEIGH** - Neighs
- **MS_WAIL** - Wails
- **MS_SHRIEK** - Shrieks (wakes others)
- **MS_LAUGH** - Laughs
- **MS_HUMANOID** - Human speech
- **MS_GUARD** - Guard dialogue
- **MS_SEDUCE** - Seductive speech
- **MS_VAMPIRE** - Vampire speech
- **MS_PRIEST** - Priest dialogue
- **MS_ORACLE** - Oracle speech
- **MS_SPELL** - Spellcasting sounds

---

## Statistics Summary

- **Total unique monster types**: 394
- **Highest level**: 106 (Demogorgon)
- **Lowest level**: 0 (human, newt, jackal)
- **Undead monsters**: 40+
- **Demons**: 29
- **Dragons**: 18 (9 colors × 2 ages)
- **Giants**: 12
- **NPCs (@)**: 76
- **Unique/boss monsters**: 30+
- **Genocidable monsters**: 300+

---

## Monster Families

### Complete List by Symbol

See BESTIARY.txt for the complete categorized listing of all 394 monsters with their stats, attacks, and special abilities.

---

## Data Files Reference

### JSON Database Schema

The `monsters_database.json` file contains:

```json
{
  "name": "monster name",
  "id": "PM_MONSTER_ID",
  "symbol": "display character",
  "level": 0,
  "speed": 12,
  "ac": 10,
  "magic_resistance": 0,
  "alignment": 0,
  "attacks": [
    {
      "type": "attack type",
      "damage_type": "damage type",
      "dice": 0,
      "sides": 0
    }
  ],
  "weight": 0,
  "nutrition": 0,
  "size": "size category",
  "resistances": [],
  "conveys": [],
  "flags": [],
  "gender_variants": null
}
```

---

## Tools & Utilities

### Generated Files

1. **monster_parser.py** - Python script to extract monster data from monsters.h
2. **analyze_bestiary.py** - Python script to categorize and analyze monsters
3. **monsters_database.json** - Complete JSON database of all monsters
4. **BESTIARY.txt** - Formatted text report of all monsters

### Usage

```bash
# Extract monster data
python3 monster_parser.py

# Generate bestiary report
python3 analyze_bestiary.py

# View JSON data
cat monsters_database.json | jq '.[] | select(.level > 50)'
```

---

## Notes

- Monster data is defined in `/home/user/NetHack/include/monsters.h` using MON() macros
- The actual monster array is initialized in `/home/user/NetHack/src/monst.c`
- Monster AI and behavior is spread across multiple source files in `/home/user/NetHack/src/`
- Special monsters like quest nemeses and demon lords have additional unique behaviors
- Some monsters have multiple name variants based on gender (male/female/neutral)
- Monster generation is affected by dungeon level, branch, and special conditions

---

*This catalog was generated from NetHack 3.7 source code.*
*Last updated: 2025-11-19*
