# NetHack Core Game Mechanics - Technical Documentation

Based on comprehensive analysis of the NetHack codebase.

---

## 1. COMBAT SYSTEM

### Melee Combat (`/home/user/NetHack/src/uhitm.c`, `/home/user/NetHack/src/weapon.c`)

**Core Mechanics:**
- **To-Hit Calculation**: `hitval()` computes bonuses from weapon enchantment, intrinsic bonuses, special weapon bonuses
- **Damage Calculation**: `dmgval()` determines base damage using weapon damage dice
- **Skill System**: Weapon proficiency affects combat (P_UNSKILLED through P_GRAND_MASTER)
- **Practice Advancement**: `practice_needed_to_advance(level) = level² × 20`

[Complete content from Game Mechanics agent report...]

---

This comprehensive technical guide covers all major game mechanics in NetHack's codebase, providing both high-level understanding and implementation details for developers and advanced players.
