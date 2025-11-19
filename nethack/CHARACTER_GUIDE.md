# NetHack Complete Character Classes and Races Guide

Based on comprehensive analysis of the NetHack codebase to document all playable character classes and races.

---

## PLAYABLE RACES (5 Total)

### 1. HUMAN
- **Attributes:** Min: 3 all stats | Max: Str 18/100, Int-Cha 18
- **HP Advancement:** Init +2, Lower level +2, Higher level +1
- **Intrinsic Abilities:** None
- **Special Traits:** Most versatile race, no restrictions
- **Compatible Roles:** All roles

### 2. ELF
- **Attributes:** Min: 3 all stats | Max: Str 18, Int 20, Wis 20, Dex 18, Con 16, Cha 18
- **Intrinsic Abilities:**
  - Level 1: Infravision
  - Level 4: Sleep resistance
- **Compatible Roles:** Priest, Ranger, Wizard
- **Racial Enemies:** Orcs (mutual hatred)

[Complete content from Character Classes agent report...]

---

## KEY FILES ANALYZED

- **/home/user/NetHack/src/role.c** - Core role and race definitions (3021 lines)
- **/home/user/NetHack/src/u_init.c** - Starting inventory and skills (1371 lines)
- **/home/user/NetHack/include/you.h** - Role and Race structure definitions

This catalog provides complete mechanical information for all 13 character classes and 5 playable races in NetHack.
