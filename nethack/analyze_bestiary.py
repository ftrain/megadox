#!/usr/bin/env python3
"""
NetHack Bestiary Analyzer
Categorizes and documents all monsters
"""

import json
from collections import defaultdict

def load_monsters():
    """Load monster database"""
    with open('/home/user/NetHack/monsters_database.json', 'r') as f:
        return json.load(f)

def categorize_by_symbol(monsters):
    """Categorize monsters by their display symbol"""
    categories = defaultdict(list)
    for mon in monsters:
        categories[mon['symbol']].append(mon)
    return dict(categories)

def identify_uniques(monsters):
    """Identify unique monsters (bosses, named NPCs)"""
    uniques = []
    for mon in monsters:
        flags = mon.get('flags', [])
        if 'proper name' in flags or any(x in flags for x in ['lord', 'prince', 'minion']):
            uniques.append(mon)
    return uniques

def identify_npcs(monsters):
    """Identify friendly NPCs and shopkeepers"""
    npcs = []
    for mon in monsters:
        name_lower = mon['name'].lower()
        flags = mon.get('flags', [])
        if any(keyword in name_lower for keyword in
               ['shopkeeper', 'priest', 'oracle', 'guard', 'watchman', 'soldier', 'nurse']):
            npcs.append(mon)
        elif 'always peaceful' in flags:
            npcs.append(mon)
    return npcs

def get_difficulty_tiers(monsters):
    """Categorize by difficulty/level"""
    tiers = {
        'early_game': [],  # level 0-5
        'mid_game': [],    # level 6-12
        'late_game': [],   # level 13-20
        'endgame': []      # level 21+
    }

    for mon in monsters:
        level = mon['level']
        if level <= 5:
            tiers['early_game'].append(mon)
        elif level <= 12:
            tiers['mid_game'].append(mon)
        elif level <= 20:
            tiers['late_game'].append(mon)
        else:
            tiers['endgame'].append(mon)

    return tiers

def analyze_special_abilities(monsters):
    """Identify monsters with special abilities"""
    special = {
        'petrifiers': [],
        'life_drainers': [],
        'spellcasters': [],
        'summoners': [],
        'stealers': [],
        'teleporters': [],
        'shapechangers': [],
        'breath_weapons': []
    }

    for mon in monsters:
        attacks = mon.get('attacks', [])
        flags = mon.get('flags', [])

        for attack in attacks:
            dtype = attack.get('damage_type', '')
            atype = attack.get('type', '')

            if 'petrify' in dtype:
                special['petrifiers'].append(mon)
            elif 'drain life' in dtype:
                special['life_drainers'].append(mon)
            elif 'spell' in dtype or atype == 'magic':
                special['spellcasters'].append(mon)
            elif 'steal' in dtype:
                special['stealers'].append(mon)
            elif 'breath' in atype:
                special['breath_weapons'].append(mon)

        if 'can teleport' in flags:
            special['teleporters'].append(mon)
        if 'shapeshifter' in flags:
            special['shapechangers'].append(mon)

    # Remove duplicates
    for key in special:
        special[key] = list({m['name']: m for m in special[key]}.values())

    return special

def generate_report(monsters):
    """Generate comprehensive bestiary report"""
    report = []

    report.append("=" * 80)
    report.append("NETHACK COMPLETE BESTIARY")
    report.append("=" * 80)
    report.append(f"\nTotal Monsters: {len(monsters)}\n")

    # By Symbol Category
    by_symbol = categorize_by_symbol(monsters)
    report.append("\n" + "=" * 80)
    report.append("MONSTERS BY CATEGORY (Symbol)")
    report.append("=" * 80)

    symbol_names = {
        'a': 'Ants & Insects', 'b': 'Blobs', 'c': 'Cockatrices', 'd': 'Dogs & Canines',
        'e': 'Eyes & Floating Orbs', 'f': 'Felines', 'g': 'Gremlins', 'h': 'Humanoids',
        'i': 'Imps & Minor Demons', 'j': 'Jellies', 'k': 'Kobolds', 'l': 'Leprechauns',
        'm': 'Mimics', 'n': 'Nymphs', 'o': 'Orcs', 'p': 'Piercers', 'q': 'Quadrupeds',
        'r': 'Rodents', 's': 'Spiders', 't': 'Trappers', 'u': 'Unicorns', 'v': 'Vortices',
        'w': 'Worms', 'x': 'Xan', 'y': 'Lights', 'z': 'Zruty', 'A': 'Angels',
        'B': 'Bats', 'C': 'Centaurs', 'D': 'Dragons', 'E': 'Elementals', 'F': 'Fungi',
        'G': 'Gnomes', 'H': 'Giants', 'I': 'Invisible Stalkers', 'J': 'Jabberwocks',
        'K': 'Kops', 'L': 'Liches', 'M': 'Mummies', 'N': 'Nagas', 'O': 'Ogres',
        'P': 'Puddings', 'Q': 'Quantum Mechanics', 'R': 'Rust Monsters', 'S': 'Snakes',
        'T': 'Trolls', 'U': 'Umber Hulks', 'V': 'Vampires', 'W': 'Wraiths', 'X': 'Xorns',
        'Y': 'Yetis', 'Z': 'Zombies', '@': 'Humans & NPCs', ' ': 'Ghosts', "'": 'Golems',
        '&': 'Demons', ';': 'Eels', ':': 'Lizards', '~': 'Worm Tails', ']': 'Mimics (default)'
    }

    for symbol in sorted(by_symbol.keys()):
        category_name = symbol_names.get(symbol, f'Unknown ({symbol})')
        mons = by_symbol[symbol]
        report.append(f"\n{category_name} [{symbol}] - {len(mons)} monsters:")
        for mon in sorted(mons, key=lambda x: x['level']):
            attacks_str = ', '.join([f"{a['type']}({a['damage_type']} {a['dice']}d{a['sides']})"
                                    for a in mon['attacks'][:3]])
            if len(mon['attacks']) > 3:
                attacks_str += '...'
            report.append(f"  - {mon['name']:30s} Lv:{mon['level']:2d} AC:{mon['ac']:3d} "
                         f"Spd:{mon['speed']:2d} MR:{mon['magic_resistance']:2d}%")
            if attacks_str:
                report.append(f"    Attacks: {attacks_str}")

    # Unique Monsters
    uniques = identify_uniques(monsters)
    report.append("\n" + "=" * 80)
    report.append("UNIQUE MONSTERS & BOSSES")
    report.append("=" * 80)
    report.append(f"\nTotal Unique Monsters: {len(uniques)}\n")

    for mon in sorted(uniques, key=lambda x: x['level']):
        report.append(f"\n{mon['name']} [{mon['symbol']}]")
        report.append(f"  Level: {mon['level']}, AC: {mon['ac']}, Speed: {mon['speed']}, MR: {mon['magic_resistance']}%")
        if mon['attacks']:
            report.append(f"  Attacks:")
            for atk in mon['attacks']:
                report.append(f"    - {atk['type']}: {atk['damage_type']} {atk['dice']}d{atk['sides']}")
        if mon['resistances']:
            report.append(f"  Resistances: {', '.join(mon['resistances'])}")
        special_flags = [f for f in mon['flags'] if f in ['minion', 'lord', 'prince', 'demon', 'undead', 'wants amulet']]
        if special_flags:
            report.append(f"  Special: {', '.join(special_flags)}")

    # NPCs
    npcs = identify_npcs(monsters)
    report.append("\n" + "=" * 80)
    report.append("FRIENDLY NPCs & SHOPKEEPERS")
    report.append("=" * 80)
    report.append(f"\nTotal NPCs: {len(npcs)}\n")

    for mon in sorted(npcs, key=lambda x: x['name']):
        peaceful = 'always peaceful' in mon['flags']
        domestic = 'can be tamed' in mon['flags']
        status = 'Peaceful' if peaceful else ('Tameable' if domestic else 'Special')
        report.append(f"  - {mon['name']:30s} [{mon['symbol']}] Lv:{mon['level']:2d} ({status})")

    # Special Abilities
    special = analyze_special_abilities(monsters)
    report.append("\n" + "=" * 80)
    report.append("MONSTERS BY SPECIAL ABILITIES")
    report.append("=" * 80)

    for category, mons in special.items():
        if mons:
            report.append(f"\n{category.replace('_', ' ').title()} ({len(mons)}):")
            for mon in sorted(mons, key=lambda x: x['level'])[:15]:  # Top 15
                report.append(f"  - {mon['name']:30s} Lv:{mon['level']:2d}")
            if len(mons) > 15:
                report.append(f"  ... and {len(mons) - 15} more")

    # Difficulty Tiers
    tiers = get_difficulty_tiers(monsters)
    report.append("\n" + "=" * 80)
    report.append("MONSTERS BY DIFFICULTY TIER")
    report.append("=" * 80)

    for tier_name, tier_mons in tiers.items():
        report.append(f"\n{tier_name.replace('_', ' ').title()} ({len(tier_mons)} monsters)")

    # Statistics
    report.append("\n" + "=" * 80)
    report.append("BESTIARY STATISTICS")
    report.append("=" * 80)

    total_attacks = sum(len(m['attacks']) for m in monsters)
    avg_level = sum(m['level'] for m in monsters) / len(monsters)
    avg_ac = sum(m['ac'] for m in monsters) / len(monsters)

    report.append(f"\nTotal unique monster types: {len(monsters)}")
    report.append(f"Total attacks defined: {total_attacks}")
    report.append(f"Average monster level: {avg_level:.1f}")
    report.append(f"Average AC: {avg_ac:.1f}")
    report.append(f"Highest level: {max(m['level'] for m in monsters)}")
    report.append(f"Lowest level: {min(m['level'] for m in monsters)}")

    # Count by special properties
    undead = len([m for m in monsters if 'undead' in m['flags']])
    demons = len([m for m in monsters if 'demon' in m['flags']])
    dragons = len([m for m in monsters if m['symbol'] == 'D'])
    giants = len([m for m in monsters if 'giant' in m['flags']])

    report.append(f"\nUndead monsters: {undead}")
    report.append(f"Demons: {demons}")
    report.append(f"Dragons: {dragons}")
    report.append(f"Giants: {giants}")

    return '\n'.join(report)

if __name__ == '__main__':
    monsters = load_monsters()
    report = generate_report(monsters)

    # Save report
    with open('/home/user/NetHack/BESTIARY.txt', 'w') as f:
        f.write(report)

    print(report[:2000])  # Print first part
    print("\n...\n")
    print(f"Full report saved to BESTIARY.txt")
