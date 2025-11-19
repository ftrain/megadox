#!/usr/bin/env python3
"""
NetHack Monster Database Parser
Extracts comprehensive monster data from monsters.h
"""

import re
import json

# Attack type mappings
ATTACK_TYPES = {
    'AT_NONE': 'passive', 'AT_CLAW': 'claw', 'AT_BITE': 'bite', 'AT_KICK': 'kick',
    'AT_BUTT': 'butt', 'AT_TUCH': 'touch', 'AT_STNG': 'sting', 'AT_HUGS': 'hug',
    'AT_SPIT': 'spit', 'AT_ENGL': 'engulf', 'AT_BREA': 'breath', 'AT_EXPL': 'explode',
    'AT_BOOM': 'boom', 'AT_GAZE': 'gaze', 'AT_TENT': 'tentacle', 'AT_WEAP': 'weapon',
    'AT_MAGC': 'magic'
}

DAMAGE_TYPES = {
    'AD_PHYS': 'physical', 'AD_MAGM': 'magic missile', 'AD_FIRE': 'fire', 'AD_COLD': 'cold',
    'AD_SLEE': 'sleep', 'AD_DISN': 'disintegrate', 'AD_ELEC': 'electric', 'AD_DRST': 'poison',
    'AD_ACID': 'acid', 'AD_BLND': 'blind', 'AD_STUN': 'stun', 'AD_SLOW': 'slow',
    'AD_PLYS': 'paralyze', 'AD_DRLI': 'drain life', 'AD_DREN': 'drain magic', 'AD_LEGS': 'leg damage',
    'AD_STON': 'petrify', 'AD_STCK': 'stick', 'AD_SGLD': 'steal gold', 'AD_SITM': 'steal item',
    'AD_SEDU': 'seduce', 'AD_TLPT': 'teleport', 'AD_RUST': 'rust', 'AD_CONF': 'confuse',
    'AD_DGST': 'digest', 'AD_HEAL': 'heal', 'AD_WRAP': 'wrap', 'AD_WERE': 'lycanthropy',
    'AD_DRDX': 'drain dex', 'AD_DRCO': 'drain con', 'AD_DRIN': 'drain int', 'AD_DISE': 'disease',
    'AD_DCAY': 'decay', 'AD_SSEX': 'succubus', 'AD_HALU': 'hallucinate', 'AD_DETH': 'death',
    'AD_PEST': 'pestilence', 'AD_FAMN': 'famine', 'AD_SLIM': 'slime', 'AD_ENCH': 'disenchant',
    'AD_CORR': 'corrode', 'AD_CLRC': 'clerical spell', 'AD_SPEL': 'magic spell', 'AD_RBRE': 'random breath',
    'AD_SAMU': 'steal amulet', 'AD_CURS': 'curse', 'AD_POLY': 'polymorph'
}

SYMBOLS = {
    'S_ANT': 'a', 'S_BLOB': 'b', 'S_COCKATRICE': 'c', 'S_DOG': 'd', 'S_EYE': 'e',
    'S_FELINE': 'f', 'S_GREMLIN': 'g', 'S_HUMANOID': 'h', 'S_IMP': 'i', 'S_JELLY': 'j',
    'S_KOBOLD': 'k', 'S_LEPRECHAUN': 'l', 'S_MIMIC': 'm', 'S_NYMPH': 'n', 'S_ORC': 'o',
    'S_PIERCER': 'p', 'S_QUADRUPED': 'q', 'S_RODENT': 'r', 'S_SPIDER': 's', 'S_TRAPPER': 't',
    'S_UNICORN': 'u', 'S_VORTEX': 'v', 'S_WORM': 'w', 'S_XAN': 'x', 'S_LIGHT': 'y',
    'S_ZRUTY': 'z', 'S_ANGEL': 'A', 'S_BAT': 'B', 'S_CENTAUR': 'C', 'S_DRAGON': 'D',
    'S_ELEMENTAL': 'E', 'S_FUNGUS': 'F', 'S_GNOME': 'G', 'S_GIANT': 'H', 'S_JABBERWOCK': 'J',
    'S_KOP': 'K', 'S_LICH': 'L', 'S_MUMMY': 'M', 'S_NAGA': 'N', 'S_OGRE': 'O',
    'S_PUDDING': 'P', 'S_QUANTMECH': 'Q', 'S_RUSTMONST': 'R', 'S_SNAKE': 'S', 'S_TROLL': 'T',
    'S_UMBER': 'U', 'S_VAMPIRE': 'V', 'S_WRAITH': 'W', 'S_XORN': 'X', 'S_YETI': 'Y',
    'S_ZOMBIE': 'Z', 'S_HUMAN': '@', 'S_GHOST': ' ', 'S_GOLEM': "'", 'S_DEMON': '&',
    'S_EEL': ';', 'S_LIZARD': ':', 'S_WORM_TAIL': '~', 'S_MIMIC_DEF': ']'
}

RESISTANCES = {
    'MR_FIRE': 'fire', 'MR_COLD': 'cold', 'MR_SLEEP': 'sleep', 'MR_DISINT': 'disintegration',
    'MR_ELEC': 'electricity', 'MR_POISON': 'poison', 'MR_ACID': 'acid', 'MR_STONE': 'petrification'
}

SIZES = {
    'MZ_TINY': 'tiny', 'MZ_SMALL': 'small', 'MZ_MEDIUM': 'medium', 'MZ_LARGE': 'large',
    'MZ_HUGE': 'huge', 'MZ_GIGANTIC': 'gigantic'
}

def parse_attack(attack_str):
    """Parse an attack definition"""
    match = re.search(r'ATTK\((\w+),\s*(\w+),\s*(\d+),\s*(\d+)\)', attack_str)
    if match:
        at_type, ad_type, dice, sides = match.groups()
        return {
            'type': ATTACK_TYPES.get(at_type, at_type),
            'damage_type': DAMAGE_TYPES.get(ad_type, ad_type),
            'dice': int(dice),
            'sides': int(sides)
        }
    return None

def parse_resistances(res_str):
    """Parse resistance flags"""
    resistances = []
    for flag, name in RESISTANCES.items():
        if flag in res_str:
            resistances.append(name)
    return resistances

def parse_flags(flags_str):
    """Parse M1/M2/M3 flags"""
    flags = []

    # M1 flags
    m1_flags = {
        'M1_FLY': 'can fly', 'M1_SWIM': 'can swim', 'M1_AMORPHOUS': 'amorphous',
        'M1_WALLWALK': 'can phase through walls', 'M1_CLING': 'can cling to ceiling',
        'M1_TUNNEL': 'can tunnel', 'M1_NEEDPICK': 'needs pick to tunnel',
        'M1_CONCEAL': 'hides under objects', 'M1_HIDE': 'mimics/blends in',
        'M1_AMPHIBIOUS': 'amphibious', 'M1_BREATHLESS': 'breathless',
        'M1_NOTAKE': 'cannot pick up objects', 'M1_NOEYES': 'no eyes',
        'M1_NOHANDS': 'no hands', 'M1_NOLIMBS': 'no limbs', 'M1_NOHEAD': 'no head',
        'M1_MINDLESS': 'mindless', 'M1_HUMANOID': 'humanoid', 'M1_ANIMAL': 'animal',
        'M1_SLITHY': 'serpentine', 'M1_UNSOLID': 'unsolid', 'M1_THICK_HIDE': 'thick hide',
        'M1_OVIPAROUS': 'lays eggs', 'M1_REGEN': 'regenerates', 'M1_SEE_INVIS': 'see invisible',
        'M1_TPORT': 'can teleport', 'M1_TPORT_CNTRL': 'teleport control',
        'M1_ACID': 'acidic', 'M1_POIS': 'poisonous', 'M1_CARNIVORE': 'carnivore',
        'M1_HERBIVORE': 'herbivore', 'M1_OMNIVORE': 'omnivore', 'M1_METALLIVORE': 'metallivore'
    }

    # M2 flags
    m2_flags = {
        'M2_NOPOLY': 'cannot polymorph into', 'M2_UNDEAD': 'undead', 'M2_WERE': 'lycanthrope',
        'M2_HUMAN': 'human', 'M2_ELF': 'elf', 'M2_DWARF': 'dwarf', 'M2_GNOME': 'gnome',
        'M2_ORC': 'orc', 'M2_DEMON': 'demon', 'M2_MERC': 'mercenary', 'M2_LORD': 'lord',
        'M2_PRINCE': 'prince', 'M2_MINION': 'minion', 'M2_GIANT': 'giant',
        'M2_SHAPESHIFTER': 'shapeshifter', 'M2_MALE': 'always male', 'M2_FEMALE': 'always female',
        'M2_NEUTER': 'neuter', 'M2_PNAME': 'proper name', 'M2_HOSTILE': 'always hostile',
        'M2_PEACEFUL': 'always peaceful', 'M2_DOMESTIC': 'can be tamed', 'M2_WANDER': 'wanders',
        'M2_STALK': 'stalks player', 'M2_NASTY': 'extra nasty', 'M2_STRONG': 'strong',
        'M2_ROCKTHROW': 'throws rocks', 'M2_GREEDY': 'likes gold', 'M2_JEWELS': 'likes gems',
        'M2_COLLECT': 'collects items', 'M2_MAGIC': 'collects magic items'
    }

    # M3 flags
    m3_flags = {
        'M3_WANTSAMUL': 'wants amulet', 'M3_WANTSBELL': 'wants bell',
        'M3_WANTSBOOK': 'wants book', 'M3_WANTSCAND': 'wants candelabrum',
        'M3_WANTSARTI': 'wants artifact', 'M3_WAITFORU': 'waits for you',
        'M3_CLOSE': 'lets you close', 'M3_INFRAVISION': 'has infravision',
        'M3_INFRAVISIBLE': 'visible by infravision', 'M3_DISPLACES': 'displaces monsters'
    }

    for flag, desc in {**m1_flags, **m2_flags, **m3_flags}.items():
        if flag in flags_str:
            flags.append(desc)

    return flags

def extract_monsters():
    """Extract all monster data from monsters.h"""
    with open('/home/user/NetHack/include/monsters.h', 'r') as f:
        content = f.read()

    monsters = []

    # Split by MON( entries
    entries = re.split(r'\n\s*MON\(', content)

    for entry in entries[1:]:  # Skip first split (header)
        try:
            # Extract name
            name_match = re.search(r'(NAM\("([^"]+)"\)|NAMS\("([^"]+)",\s*"([^"]+)",\s*"([^"]+)"\))', entry)
            if not name_match:
                continue

            if name_match.group(2):
                name = name_match.group(2)
                gender_variants = None
            else:
                name = f"{name_match.group(3)}/{name_match.group(4)}/{name_match.group(5)}"
                gender_variants = {
                    'male': name_match.group(3),
                    'female': name_match.group(4),
                    'neutral': name_match.group(5)
                }

            # Extract symbol
            symbol_match = re.search(r'S_(\w+)', entry)
            symbol = SYMBOLS.get(f"S_{symbol_match.group(1)}", '?') if symbol_match else '?'

            # Extract level, move, AC, MR, alignment
            lvl_match = re.search(r'LVL\((\d+),\s*(\d+),\s*(-?\d+),\s*(\d+),\s*(-?\d+)\)', entry)
            if lvl_match:
                level, move, ac, mr, alignment = map(int, lvl_match.groups())
            else:
                level = move = ac = mr = alignment = 0

            # Extract attacks
            attacks = []
            attack_section = re.search(r'A\((.*?)\)', entry, re.DOTALL)
            if attack_section:
                attack_strs = re.findall(r'ATTK\([^)]+\)', attack_section.group(1))
                for attack_str in attack_strs:
                    attack = parse_attack(attack_str)
                    if attack and (attack['dice'] > 0 or attack['type'] not in ['passive', 'AT_NONE']):
                        attacks.append(attack)

            # Extract size
            size_match = re.search(r'SIZ\((\d+),\s*(\d+),\s*\w+,\s*(MZ_\w+)\)', entry)
            if size_match:
                weight, nutrition, size_flag = size_match.groups()
                size = SIZES.get(size_flag, 'medium')
            else:
                weight = nutrition = 0
                size = 'medium'

            # Extract resistances
            resist_match = re.search(r'SIZ\([^)]+\),\s*([^,]+),\s*([^,]+),', entry)
            if resist_match:
                resistances = parse_resistances(resist_match.group(1))
                conveys = parse_resistances(resist_match.group(2))
            else:
                resistances = []
                conveys = []

            # Extract flags
            flags_match = re.search(r'(M1_[^,]+|0[lL]?),\s*(M2_[^,]+|0[lL]?),\s*(M3_[^,]+|\d+)', entry)
            if flags_match:
                flags = parse_flags(' | '.join(flags_match.groups()))
            else:
                flags = []

            # Extract PM_ identifier
            pm_match = re.search(r',\s*([A-Z_]+)\)', entry)
            pm_id = pm_match.group(1) if pm_match else 'UNKNOWN'

            monster = {
                'name': name,
                'id': pm_id,
                'symbol': symbol,
                'level': level,
                'speed': move,
                'ac': ac,
                'magic_resistance': mr,
                'alignment': alignment,
                'attacks': attacks,
                'weight': int(weight) if weight else 0,
                'nutrition': int(nutrition) if nutrition else 0,
                'size': size,
                'resistances': resistances,
                'conveys': conveys,
                'flags': flags,
                'gender_variants': gender_variants
            }

            monsters.append(monster)

        except Exception as e:
            print(f"Error parsing entry: {e}")
            continue

    return monsters

if __name__ == '__main__':
    monsters = extract_monsters()

    # Save to JSON
    with open('/home/user/NetHack/monsters_database.json', 'w') as f:
        json.dump(monsters, f, indent=2)

    print(f"Extracted {len(monsters)} monsters")
    print(f"Data saved to monsters_database.json")
