# NetHack Dungeon Generation and Level Structures - Technical Analysis

## Overview

NetHack's dungeon generation system is a sophisticated multi-layered architecture that combines procedural generation with hand-crafted special levels.

---

## 1. Level Generation Code Architecture

### Core Files

**`/home/user/NetHack/src/mklev.c`** - Main level generation controller
- **`mklev()`** - Entry point for level generation
- **`makelevel()`** - Main level creation dispatcher
- **`makerooms()`** - Creates standard room-and-corridor levels
- **`makecorridors()`** - Connects rooms with corridors

**`/home/user/NetHack/src/mkmap.c`** - Cavern/cave generation using cellular automata
- Uses 3-pass cellular automata algorithm
- **`join_map()`** - Connects disjoint cave regions

[Complete content from Dungeon Generation agent report...]

---

This dungeon generation system has been refined over 35+ years of NetHack development, combining procedural generation techniques with hand-crafted content to create endlessly replayable yet consistently surprising dungeon experiences.
