# NetHack Item Compendium

**Complete documentation of all objects, items, and artifacts in NetHack 3.7**

---

## Table of Contents

1. [Object Classes and Generation](#object-classes-and-generation)
2. [Weapons](#weapons)
3. [Armor](#armor)
4. [Rings](#rings)
5. [Amulets](#amulets)
6. [Tools](#tools)
7. [Food](#food)
8. [Potions](#potions)
9. [Scrolls](#scrolls)
10. [Spellbooks](#spellbooks)
11. [Wands](#wands)
12. [Gems and Stones](#gems-and-stones)
13. [Artifacts](#artifacts)
14. [Special Objects](#special-objects)

---

## Object Classes and Generation

### Object Class Probabilities

#### General Generation (mkobjprobs)
- Weapons: 10%
- Armor: 10%
- Food: 20%
- Tools: 8%
- Gems: 8%
- Potions: 16%
- Scrolls: 16%
- Spellbooks: 4%
- Wands: 4%
- Rings: 3%
- Amulets: 1%

#### Container Contents (boxiprobs)
- Gems: 18%
- Food: 15%
- Potions: 18%
- Scrolls: 18%
- Spellbooks: 12%
- Coins: 7%
- Wands: 6%
- Rings: 5%
- Amulets: 1%

#### Gehennom (Hell) Generation (hellprobs)
- Weapons: 20%
- Armor: 20%
- Food: 16%
- Tools: 12%
- Gems: 10%
- Potions: 1%
- Scrolls: 1%
- Wands: 8%
- Rings: 8%
- Amulets: 4%

---

## Weapons

### Projectiles (Arrows and Bolts)

#### Arrows
- **Arrow** - Iron arrows, 6d6 damage, weight 1
- **Elven Arrow** (appears as "runed arrow") - Wood, 7d6 damage, weight 1
- **Orcish Arrow** (appears as "crude arrow") - Iron, 5d6 damage, weight 1, black
- **Silver Arrow** - Silver, 6d6 damage, weight 1, effective against silver-hating creatures
- **Ya** (appears as "bamboo arrow") - Metal, 7d7 damage (+1 to-hit), weight 1
- **Crossbow Bolt** - Iron, 4d6 damage, weight 1

#### Thrown Weapons (non-launcher)
- **Dart** - Iron, 3d2 damage, weight 1
- **Shuriken** (appears as "throwing star") - Iron, 8d6 damage (+2 to-hit), weight 1
- **Boomerang** - Wood, 9d9 damage, weight 5

### Spears
- **Spear** - Iron, 6d8 damage, weight 30, cost 3
- **Elven Spear** (appears as "runed spear") - Wood, 7d8 damage, weight 30
- **Orcish Spear** (appears as "crude spear") - Iron, 5d8 damage, weight 30, black
- **Dwarvish Spear** (appears as "stout spear") - Iron, 8d8 damage, weight 35
- **Silver Spear** - Silver, 6d8 damage, weight 36, cost 40
- **Javelin** (appears as "throwing spear") - Iron, 6d6 damage, weight 20
- **Trident** - Iron, 6d4 damage (+1d4 small, +2d4 large), weight 25

### Daggers and Knives
- **Dagger** - Iron, 4d3 damage (+2 to-hit), weight 10
- **Elven Dagger** (appears as "runed dagger") - Wood, 5d3 damage (+2 to-hit), weight 10
- **Orcish Dagger** (appears as "crude dagger") - Iron, 3d3 damage (+2 to-hit), weight 10, black
- **Silver Dagger** - Silver, 4d3 damage (+2 to-hit), weight 12, cost 40
- **Athame** - Iron, 4d3 damage (+2 to-hit), weight 10, slash damage
- **Scalpel** - Metal, 3d3 damage (+2 to-hit), weight 5, slash damage
- **Knife** - Iron, 3d2 damage, weight 5, pierce/slash
- **Stiletto** - Iron, 3d2 damage, weight 5, pierce/slash
- **Worm Tooth** - Bone, 2d2 damage, weight 20, cost 2
- **Crysknife** - Bone, 10d10 damage (+3 to-hit), weight 20, cost 100

### Axes
- **Axe** - Iron, 6d4 damage, weight 60, cost 8
- **Battle-Axe** (appears as "double-headed axe") - Iron, 8d6 damage, weight 120, cost 40, two-handed

### Swords

#### Short Swords
- **Short Sword** - Iron, 6d8 damage, weight 30, cost 10
- **Elven Short Sword** (appears as "runed short sword") - Wood, 8d8 damage, weight 30
- **Orcish Short Sword** (appears as "crude short sword") - Iron, 5d8 damage, weight 30, black
- **Dwarvish Short Sword** (appears as "broad short sword") - Iron, 7d8 damage, weight 30

#### Curved Swords
- **Scimitar** (appears as "curved sword") - Iron, 8d8 damage, weight 40, cost 15, saber skill
- **Silver Saber** - Silver, 8d8 damage, weight 40, cost 75

#### Broadswords
- **Broadsword** - Iron, 4d6 damage (+d4 small, +1 large), weight 70, cost 10
- **Elven Broadsword** (appears as "runed broadsword") - Wood, 6d6 damage (+d4 small, +1 large), weight 70

#### Long Swords
- **Long Sword** - Iron, 8d12 damage, weight 40, cost 15
- **Katana** (appears as "samurai sword") - Iron, 10d12 damage (+1 to-hit), weight 40, cost 80

#### Two-Handed Swords
- **Two-Handed Sword** - Iron, 12d6 damage (+2d6 large), weight 150, cost 50
- **Tsurugi** (appears as "long samurai sword") - Metal, 16d8 damage (+2d6 large, +2 to-hit), weight 60, cost 500
- **Runesword** (appears as "runed broadsword") - Iron, 4d6 damage (+d4 small, +1 large), weight 40, cost 300, black

### Polearms

#### Spear-type
- **Partisan** (appears as "vulgar polearm") - Iron, 6d6 damage (+1 large), weight 80
- **Ranseur** (appears as "hilted polearm") - Iron, 4d4 damage (+d4 both), weight 50
- **Spetum** (appears as "forked polearm") - Iron, 6d6 damage (+1 small, +d6 large), weight 50
- **Glaive** (appears as "single-edged polearm") - Iron, 6d10 damage, weight 75

#### Axe-type
- **Halberd** (appears as "angled poleaxe") - Iron, 10d6 damage (+1d6 large), weight 150, pierce/slash
- **Bardiche** (appears as "long poleaxe") - Iron, 4d4 damage (+1d4 small, +2d4 large), weight 120
- **Voulge** (appears as "pole cleaver") - Iron, 4d4 damage (+d4 both), weight 125

#### Curved/Hooked
- **Fauchard** (appears as "pole sickle") - Iron, 6d8 damage, weight 60, pierce/slash
- **Guisarme** (appears as "pruning hook") - Iron, 4d8 damage (+1d4 small), weight 80
- **Bill-Guisarme** (appears as "hooked polearm") - Iron, 4d10 damage (+1d4 small), weight 120, pierce/slash

#### Other
- **Lucern Hammer** (appears as "pronged polearm") - Iron, 4d6 damage (+1d4 small), weight 150, bludgeon/pierce
- **Bec de Corbin** (appears as "beaked polearm") - Iron, 8d6 damage, weight 100, bludgeon/pierce

### Digging Tools
- **Dwarvish Mattock** (appears as "broad pick") - Iron, 12d8 damage (-1 to-hit), weight 120, cost 50

### Lances
- **Lance** - Iron, 6d8 damage (+2d10 when jousting), weight 180, cost 10

### Bludgeons
- **Mace** - Iron, 6d6 damage (+1 small), weight 30, cost 5
- **Silver Mace** - Silver, 6d6 damage (+1 small), weight 36, cost 60
- **Morning Star** - Iron, 4d6 damage (+d4 small, +1 large), weight 120, cost 10
- **War Hammer** - Iron, 4d4 damage (+1 small), weight 50, cost 5
- **Club** - Wood, 6d3 damage, weight 30, cost 3
- **Rubber Hose** - Plastic, 4d3 damage, weight 20, cost 3
- **Quarterstaff** (appears as "staff") - Wood, 6d6 damage, weight 40, cost 5, two-handed
- **Aklys** (appears as "thonged club") - Iron, 6d3 damage, weight 15, cost 4
- **Flail** - Iron, 6d4 damage (+1 small, +1d4 large), weight 15, cost 4

### Whips
- **Bullwhip** - Leather, 2d1 damage, weight 20, cost 4, brown

### Bows and Launchers
- **Bow** - Wood, 2d2 damage, weight 30, cost 60
- **Elven Bow** (appears as "runed bow") - Wood, 2d2 damage, weight 30, cost 60
- **Orcish Bow** (appears as "crude bow") - Wood, 2d2 damage, weight 30, cost 60, black
- **Yumi** (appears as "long bow") - Wood, 2d2 damage, weight 30, cost 60
- **Sling** - Leather, 2d2 damage, weight 3, cost 20
- **Crossbow** - Wood, 2d2 damage, weight 50, cost 40

---

## Armor

### Helmets
- **Elven Leather Helm** (appears as "leather hat") - Leather, AC 9, weight 3, cost 8
- **Orcish Helm** (appears as "iron skull cap") - Iron, AC 9, weight 30, cost 10, black
- **Dwarvish Iron Helm** (appears as "hard hat") - Iron, AC 8, weight 40, cost 20
- **Fedora** - Cloth, AC 10, weight 3, cost 1, brown
- **Cornuthaum** (appears as "conical hat") - Cloth, AC 10, weight 4, cost 80, blue, grants clairvoyance to wizards
- **Dunce Cap** (appears as "conical hat") - Cloth, AC 10, weight 4, cost 1, blue, sets Int/Wis to 6
- **Dented Pot** - Iron, AC 9, weight 10, cost 8, black
- **Helm of Brilliance** (appears as "crystal helmet") - Glass, AC 9, weight 40, cost 50, white
- **Helmet** (appears as "plumed helmet") - Iron, AC 9, weight 30, cost 10
- **Helm of Caution** (appears as "etched helmet") - Iron, AC 9, weight 50, cost 50, green, grants warning
- **Helm of Opposite Alignment** (appears as "crested helmet") - Iron, AC 9, weight 50, cost 50
- **Helm of Telepathy** (appears as "visored helmet") - Iron, AC 9, weight 50, cost 50, grants telepathy

### Body Armor (Suits)

#### Dragon Scale Mail and Scales
- **Gray Dragon Scale Mail** - AC 1, weight 40, cost 1200, antimagic
- **Gold Dragon Scale Mail** - AC 1, weight 40, cost 900, light source
- **Silver Dragon Scale Mail** - AC 1, weight 40, cost 1200, reflection
- **Red Dragon Scale Mail** - AC 1, weight 40, cost 900, fire resistance
- **White Dragon Scale Mail** - AC 1, weight 40, cost 900, cold resistance
- **Orange Dragon Scale Mail** - AC 1, weight 40, cost 900, sleep resistance
- **Black Dragon Scale Mail** - AC 1, weight 40, cost 1200, disintegration resistance
- **Blue Dragon Scale Mail** - AC 1, weight 40, cost 900, shock resistance
- **Green Dragon Scale Mail** - AC 1, weight 40, cost 900, poison resistance
- **Yellow Dragon Scale Mail** - AC 1, weight 40, cost 900, acid resistance

*(Dragon Scales versions: same properties, AC 7, lower cost, not magical)*

#### Other Suits
- **Plate Mail** - AC 3, weight 450, cost 600, iron
- **Crystal Plate Mail** - AC 3, weight 415, cost 820, glass
- **Bronze Plate Mail** - AC 4, weight 450, cost 400, copper
- **Splint Mail** - AC 4, weight 400, cost 80, iron
- **Banded Mail** - AC 4, weight 350, cost 90, iron
- **Dwarvish Mithril-Coat** - AC 4, weight 150, cost 240, mithril
- **Elven Mithril-Coat** - AC 5, weight 150, cost 240, mithril
- **Chain Mail** - AC 5, weight 300, cost 75, iron
- **Orcish Chain Mail** (appears as "crude chain mail") - AC 6, weight 300, cost 75, iron, black
- **Scale Mail** - AC 6, weight 250, cost 45, iron
- **Studded Leather Armor** - AC 7, weight 200, cost 15, leather
- **Ring Mail** - AC 7, weight 250, cost 100, iron
- **Orcish Ring Mail** (appears as "crude ring mail") - AC 8, weight 250, cost 80, iron, black
- **Leather Armor** - AC 8, weight 150, cost 5, leather
- **Leather Jacket** - AC 9, weight 30, cost 10, leather, black

### Shirts
- **Hawaiian Shirt** - AC 10, weight 5, cost 3, cloth, magenta
- **T-Shirt** - AC 10, weight 5, cost 2, cloth, white

### Cloaks
- **Mummy Wrapping** - AC 10, weight 3, cost 2, cloth, gray (blocks invisibility)
- **Elven Cloak** (appears as "faded pall") - AC 9, weight 10, cost 60, cloth, black, grants stealth
- **Orcish Cloak** (appears as "coarse mantelet") - AC 10, weight 10, cost 40, cloth, black
- **Dwarvish Cloak** (appears as "hooded cloak") - AC 10, weight 10, cost 50, cloth
- **Oilskin Cloak** (appears as "slippery cloak") - AC 9, weight 10, cost 50, cloth
- **Robe** - AC 8, weight 15, cost 50, cloth, red
- **Alchemy Smock** (appears as "apron") - AC 9, weight 10, cost 50, cloth, white, poison resistance
- **Leather Cloak** - AC 9, weight 15, cost 40, leather, brown
- **Cloak of Protection** (appears as "tattered cape") - AC 7, weight 10, cost 50, cloth, grants MC 3
- **Cloak of Invisibility** (appears as "opera cloak") - AC 9, weight 10, cost 60, cloth, bright magenta, invisibility
- **Cloak of Magic Resistance** (appears as "ornamental cope") - AC 9, weight 10, cost 60, cloth, white, antimagic
- **Cloak of Displacement** (appears as "piece of cloth") - AC 9, weight 10, cost 50, cloth, displacement

### Shields
- **Small Shield** - AC 9, weight 30, cost 3, wood
- **Elven Shield** (appears as "blue and green shield") - AC 8, weight 40, cost 7, wood, green
- **Uruk-hai Shield** (appears as "white-handed shield") - AC 9, weight 50, cost 7, iron
- **Orcish Shield** (appears as "red-eyed shield") - AC 9, weight 50, cost 7, iron, red
- **Large Shield** - AC 8, weight 100, cost 10, iron, two-handed
- **Dwarvish Roundshield** (appears as "large round shield") - AC 8, weight 100, cost 10, iron
- **Shield of Reflection** (appears as "polished silver shield") - AC 8, weight 50, cost 50, silver, reflection

### Gloves
- **Leather Gloves** (appears as "old gloves") - AC 9, weight 10, cost 8, leather
- **Gauntlets of Fumbling** (appears as "padded gloves") - AC 9, weight 10, cost 50, leather, causes fumbling
- **Gauntlets of Power** (appears as "riding gloves") - AC 9, weight 30, cost 50, iron, grants strength
- **Gauntlets of Dexterity** (appears as "fencing gloves") - AC 9, weight 10, cost 50, leather, grants dexterity

### Boots
- **Low Boots** (appears as "walking shoes") - AC 9, weight 10, cost 8, leather
- **Iron Shoes** (appears as "hard shoes") - AC 8, weight 50, cost 16, iron
- **High Boots** (appears as "jackboots") - AC 8, weight 20, cost 12, leather
- **Speed Boots** (appears as "combat boots") - AC 9, weight 20, cost 50, leather, grants speed
- **Water Walking Boots** (appears as "jungle boots") - AC 9, weight 15, cost 50, leather, water walking
- **Jumping Boots** (appears as "hiking boots") - AC 9, weight 20, cost 50, leather, grants jumping
- **Elven Boots** (appears as "mud boots") - AC 9, weight 15, cost 8, leather, grants stealth
- **Kicking Boots** (appears as "buckled boots") - AC 9, weight 50, cost 8, iron, enhance kicks
- **Fumble Boots** (appears as "riding boots") - AC 9, weight 20, cost 30, leather, causes fumbling
- **Levitation Boots** (appears as "snow boots") - AC 9, weight 15, cost 30, leather, levitation

---

## Rings

All rings weigh 3 and have nutritional value of 15.

- **Ring of Adornment** (appears as "wooden") - Cost 100, wood, charisma bonus
- **Ring of Gain Strength** (appears as "granite") - Cost 150, mineral, grants strength
- **Ring of Gain Constitution** (appears as "opal") - Cost 150, mineral, grants constitution
- **Ring of Increase Accuracy** (appears as "clay") - Cost 150, mineral, red, to-hit bonus
- **Ring of Increase Damage** (appears as "coral") - Cost 150, mineral, orange, damage bonus
- **Ring of Protection** (appears as "black onyx") - Cost 100, mineral, black, grants protection
- **Ring of Regeneration** (appears as "moonstone") - Cost 200, mineral, regeneration
- **Ring of Searching** (appears as "tiger eye") - Cost 200, gemstone, brown, grants searching
- **Ring of Stealth** (appears as "jade") - Cost 100, gemstone, green, grants stealth
- **Ring of Sustain Ability** (appears as "bronze") - Cost 100, copper, fixes abilities
- **Ring of Levitation** (appears as "agate") - Cost 200, gemstone, red, levitation
- **Ring of Hunger** (appears as "topaz") - Cost 100, gemstone, cyan, causes hunger
- **Ring of Aggravate Monster** (appears as "sapphire") - Cost 150, gemstone, blue, aggravates monsters
- **Ring of Conflict** (appears as "ruby") - Cost 300, gemstone, red, causes conflict
- **Ring of Warning** (appears as "diamond") - Cost 100, gemstone, white, grants warning
- **Ring of Poison Resistance** (appears as "pearl") - Cost 150, bone, white, poison resistance
- **Ring of Fire Resistance** (appears as "iron") - Cost 200, iron, fire resistance
- **Ring of Cold Resistance** (appears as "brass") - Cost 150, copper, cold resistance
- **Ring of Shock Resistance** (appears as "copper") - Cost 150, copper, shock resistance
- **Ring of Free Action** (appears as "twisted") - Cost 200, iron, free action
- **Ring of Slow Digestion** (appears as "steel") - Cost 200, iron, slow digestion
- **Ring of Teleportation** (appears as "silver") - Cost 200, silver, teleportation
- **Ring of Teleport Control** (appears as "gold") - Cost 300, gold, teleport control
- **Ring of Polymorph** (appears as "ivory") - Cost 300, bone, white, polymorph
- **Ring of Polymorph Control** (appears as "emerald") - Cost 300, gemstone, bright green, polymorph control
- **Ring of Invisibility** (appears as "wire") - Cost 150, iron, invisibility
- **Ring of See Invisible** (appears as "engagement") - Cost 150, iron, see invisible
- **Ring of Protection from Shape Changers** (appears as "shiny") - Cost 100, iron, bright cyan

---

## Amulets

All amulets weigh 20, cost 150, nutritional value 20, made of iron.

- **Amulet of ESP** (appears as "circular") - Grants telepathy
- **Amulet of Life Saving** (appears as "spherical") - Saves from death once
- **Amulet of Strangulation** (appears as "oval") - Causes strangulation (cursed item)
- **Amulet of Restful Sleep** (appears as "triangular") - Causes sleep (cursed item)
- **Amulet versus Poison** (appears as "pyramidal") - Poison resistance
- **Amulet of Change** (appears as "square") - Causes polymorph
- **Amulet of Unchanging** (appears as "concave") - Prevents polymorph
- **Amulet of Reflection** (appears as "hexagonal") - Grants reflection
- **Amulet of Magical Breathing** (appears as "octagonal") - Magical breathing
- **Amulet of Guarding** (appears as "perforated") - +2 AC and +2 MC, grants protection
- **Amulet of Flying** (appears as "cubical") - Grants flying
- **Cheap Plastic Imitation of the Amulet of Yendor** (appears as "Amulet of Yendor") - Worthless fake, plastic, weight 20, cost 0
- **Amulet of Yendor** - The quest goal, mithril, weight 20, cost 30000, unique

---

## Tools

### Containers
- **Large Box** - Weight 350, cost 8, wood
- **Chest** - Weight 600, cost 16, wood
- **Ice Box** - Weight 900, cost 42, plastic, white (keeps food fresh)
- **Sack** (appears as "bag") - Weight 15, cost 2, cloth
- **Oilskin Sack** (appears as "bag") - Weight 15, cost 100, cloth (waterproof)
- **Bag of Holding** (appears as "bag") - Weight 15, cost 100, cloth, magical
- **Bag of Tricks** (appears as "bag") - Weight 15, cost 100, cloth, magical, charged

### Lock Opening Tools
- **Skeleton Key** (appears as "key") - Weight 3, cost 10, iron
- **Lock Pick** - Weight 4, cost 20, iron
- **Credit Card** - Weight 1, cost 10, plastic, white

### Light Sources
- **Tallow Candle** (appears as "candle") - Weight 2, cost 10, wax, white
- **Wax Candle** (appears as "candle") - Weight 2, cost 20, wax, white
- **Brass Lantern** - Weight 30, cost 12, copper, yellow
- **Oil Lamp** (appears as "lamp") - Weight 20, cost 10, copper, yellow
- **Magic Lamp** (appears as "lamp") - Weight 20, cost 50, copper, yellow, magical

### Other Tools
- **Expensive Camera** - Weight 12, cost 200, plastic, black, charged
- **Mirror** (appears as "looking glass") - Weight 13, cost 10, glass
- **Crystal Ball** (appears as "glass orb") - Weight 150, cost 60, glass, magical, charged

### Eyewear
- **Lenses** - Weight 3, cost 80, glass
- **Blindfold** - Weight 2, cost 20, cloth, black (causes blindness when worn)
- **Towel** - Weight 5, cost 50, cloth, magenta (causes blindness when worn)

### Miscellaneous
- **Saddle** - Weight 200, cost 150, leather
- **Leash** - Weight 12, cost 20, leather
- **Stethoscope** - Weight 4, cost 75, iron
- **Tinning Kit** - Weight 100, cost 30, iron, charged
- **Tin Opener** - Weight 4, cost 30, iron
- **Can of Grease** - Weight 15, cost 20, iron, charged
- **Figurine** - Weight 50, cost 80, mineral, magical (creates monster)
- **Magic Marker** - Weight 2, cost 50, plastic, red, magical, charged

### Traps
- **Land Mine** - Weight 200, cost 180, iron, red
- **Beartrap** - Weight 200, cost 60, iron

### Musical Instruments
- **Tin Whistle** (appears as "whistle") - Weight 3, cost 10, metal
- **Magic Whistle** (appears as "whistle") - Weight 3, cost 10, metal, magical
- **Wooden Flute** (appears as "flute") - Weight 5, cost 12, wood
- **Magic Flute** (appears as "flute") - Weight 5, cost 36, wood, magical, charged
- **Tooled Horn** (appears as "horn") - Weight 18, cost 15, bone, white
- **Frost Horn** (appears as "horn") - Weight 18, cost 50, bone, white, magical, charged
- **Fire Horn** (appears as "horn") - Weight 18, cost 50, bone, white, magical, charged
- **Horn of Plenty** (appears as "horn") - Weight 18, cost 50, bone, white, magical, charged
- **Wooden Harp** (appears as "harp") - Weight 30, cost 50, wood
- **Magic Harp** (appears as "harp") - Weight 30, cost 50, wood, magical, charged
- **Bell** - Weight 30, cost 50, copper
- **Bugle** - Weight 10, cost 15, copper
- **Leather Drum** (appears as "drum") - Weight 25, cost 25, leather
- **Drum of Earthquake** (appears as "drum") - Weight 25, cost 25, leather, magical, charged

### Weapon-like Tools
- **Pick-Axe** - Weight 100, cost 50, 6d3 damage, iron, whack
- **Grappling Hook** - Weight 30, cost 50, 2d6 damage, iron, whack
- **Unicorn Horn** - Weight 20, cost 100, 12d12 damage, bone, white, pierce, magical (cures ailments)

### Unique Quest Tools
- **Candelabrum of Invocation** (appears as "candelabrum") - Weight 10, cost 5000, gold, magical, unique
- **Bell of Opening** (appears as "silver bell") - Weight 10, cost 5000, silver, magical, unique, charged

---

## Food

All food items have varying nutritional values and eating delays.

### Meat
- **Tripe Ration** - Nutrition 200, delay 2, weight 10, brown
- **Corpse** - Nutrition varies, delay 1, weight varies, brown (from monsters)
- **Egg** - Nutrition 80, delay 1, weight 1, white
- **Meatball** - Nutrition 5, delay 1, weight 1, brown
- **Meat Stick** - Nutrition 5, delay 1, weight 1, brown
- **Enormous Meatball** - Nutrition 2000, delay 20, weight 400, brown
- **Meat Ring** - Nutrition 5, delay 1, weight 5, brown (non-mergeable)

### Pudding Globs
- **Glob of Gray Ooze** - Nutrition 20, delay 2, weight 20, gray
- **Glob of Brown Pudding** - Nutrition 20, delay 2, weight 20, brown
- **Glob of Green Slime** - Nutrition 20, delay 2, weight 20, green
- **Glob of Black Pudding** - Nutrition 20, delay 2, weight 20, black

### Fruits and Vegetables
- **Kelp Frond** - Nutrition 30, delay 1, weight 1, green
- **Eucalyptus Leaf** - Nutrition 1, delay 1, weight 1, green
- **Apple** - Nutrition 50, delay 1, weight 2, red
- **Orange** - Nutrition 80, delay 1, weight 2, orange
- **Pear** - Nutrition 50, delay 1, weight 2, bright green
- **Melon** - Nutrition 100, delay 1, weight 5, bright green
- **Banana** - Nutrition 80, delay 1, weight 2, yellow
- **Carrot** - Nutrition 50, delay 1, weight 2, orange (improves vision)
- **Sprig of Wolfsbane** - Nutrition 40, delay 1, weight 1, green
- **Clove of Garlic** - Nutrition 40, delay 1, weight 1, white
- **Slime Mold** - Nutrition 250, delay 1, weight 5 (customizable fruit name)

### Prepared Food
- **Lump of Royal Jelly** - Nutrition 200, delay 1, weight 2, yellow
- **Cream Pie** - Nutrition 100, delay 1, weight 10, white
- **Candy Bar** - Nutrition 100, delay 1, weight 2, bright blue
- **Fortune Cookie** - Nutrition 40, delay 1, weight 1, yellow (contains messages)
- **Pancake** - Nutrition 200, delay 2, weight 2, yellow
- **Lembas Wafer** - Nutrition 800, delay 2, weight 5, white
- **Cram Ration** - Nutrition 600, delay 3, weight 15
- **Food Ration** - Nutrition 800, delay 5, weight 20
- **K-Ration** - Nutrition 400, delay 1, weight 10
- **C-Ration** - Nutrition 300, delay 1, weight 10

### Tins
- **Tin** - Nutrition varies, delay 0, weight 10, metal (contains preserved food)

---

## Potions

All potions weigh 20, have nutritional value 10, and are made of glass.

- **Potion of Gain Ability** (appears as "ruby") - Cost 300, red
- **Potion of Restore Ability** (appears as "pink") - Cost 100, bright magenta
- **Potion of Confusion** (appears as "orange") - Cost 100, orange, causes confusion
- **Potion of Blindness** (appears as "yellow") - Cost 150, yellow, causes blindness
- **Potion of Paralysis** (appears as "emerald") - Cost 300, bright green
- **Potion of Speed** (appears as "dark green") - Cost 200, green, grants speed
- **Potion of Levitation** (appears as "cyan") - Cost 200, cyan, levitation
- **Potion of Hallucination** (appears as "sky blue") - Cost 100, cyan, hallucination
- **Potion of Invisibility** (appears as "brilliant blue") - Cost 150, bright blue, invisibility
- **Potion of See Invisible** (appears as "magenta") - Cost 50, magenta
- **Potion of Healing** (appears as "purple-red") - Cost 20, magenta
- **Potion of Extra Healing** (appears as "puce") - Cost 100, red
- **Potion of Gain Level** (appears as "milky") - Cost 300, white
- **Potion of Enlightenment** (appears as "swirly") - Cost 200, brown
- **Potion of Monster Detection** (appears as "bubbly") - Cost 150, white
- **Potion of Object Detection** (appears as "smoky") - Cost 150, gray
- **Potion of Gain Energy** (appears as "cloudy") - Cost 150, white
- **Potion of Sleeping** (appears as "effervescent") - Cost 100, gray
- **Potion of Full Healing** (appears as "black") - Cost 200, black
- **Potion of Polymorph** (appears as "golden") - Cost 200, yellow
- **Potion of Booze** (appears as "brown") - Cost 50, brown, non-magical
- **Potion of Sickness** (appears as "fizzy") - Cost 50, cyan, non-magical
- **Potion of Fruit Juice** (appears as "dark") - Cost 50, black, non-magical
- **Potion of Acid** (appears as "white") - Cost 250, white, non-magical
- **Potion of Oil** (appears as "murky") - Cost 250, brown, non-magical
- **Potion of Water** (appears as "clear") - Cost 100, cyan, non-magical, fixed description

---

## Scrolls

All scrolls weigh 5, have nutritional value 6, are made of paper, and appear in paper color.

- **Scroll of Enchant Armor** (label: "ZELGO MER") - Cost 80
- **Scroll of Destroy Armor** (label: "JUYED AWK YACC") - Cost 100
- **Scroll of Confuse Monster** (label: "NR 9") - Cost 100
- **Scroll of Scare Monster** (label: "XIXAXA XOXAXA XUXAXA") - Cost 100
- **Scroll of Remove Curse** (label: "PRATYAVAYAH") - Cost 80
- **Scroll of Enchant Weapon** (label: "DAIYEN FOOELS") - Cost 60
- **Scroll of Create Monster** (label: "LEP GEX VEN ZEA") - Cost 200
- **Scroll of Taming** (label: "PRIRUTSENIE") - Cost 200
- **Scroll of Genocide** (label: "ELBIB YLOH") - Cost 300
- **Scroll of Light** (label: "VERR YED HORRE") - Cost 50
- **Scroll of Teleportation** (label: "VENZAR BORGAVVE") - Cost 100
- **Scroll of Gold Detection** (label: "THARR") - Cost 100
- **Scroll of Food Detection** (label: "YUM YUM") - Cost 100
- **Scroll of Identify** (label: "KERNOD WEL") - Cost 20
- **Scroll of Magic Mapping** (label: "ELAM EBOW") - Cost 100
- **Scroll of Amnesia** (label: "DUAM XNAHT") - Cost 200
- **Scroll of Fire** (label: "ANDOVA BEGARIN") - Cost 100
- **Scroll of Earth** (label: "KIRJE") - Cost 200
- **Scroll of Punishment** (label: "VE FORBRYDERNE") - Cost 300
- **Scroll of Charging** (label: "HACKEM MUCHE") - Cost 300
- **Scroll of Stinking Cloud** (label: "VELOX NEB") - Cost 300

### Extra Scroll Labels (shuffled at game start)
- FOOBIE BLETCH
- TEMOV
- GARVEN DEH
- READ ME
- ETAOIN SHRDLU
- LOREM IPSUM
- FNORD
- KO BATE
- ABRA KA DABRA
- ASHPD SODALG
- ZLORFIK
- GNIK SISI VLE
- HAPAX LEGOMENON
- EIRIS SAZUN IDISI
- PHOL ENDE WODAN
- GHOTI
- MAPIRO MAHAMA DIROMAT
- VAS CORP BET MANI
- XOR OTA
- STRC PRST SKRZ KRK

### Fixed Description Scrolls
- **Scroll of Mail** (label: "stamped") - Cost 0 (only in mail mode)
- **Scroll of Blank Paper** (label: "unlabeled") - Cost 60

---

## Spellbooks

All spellbooks weigh 50, have nutritional value 20, and are made of paper (or leather for parchment/vellum).

### Attack Spells
- **Dig** (appears as "parchment") - Level 5, delay 6, cost 500, P_MATTER_SPELL, ray
- **Magic Missile** (appears as "vellum") - Level 2, delay 2, cost 200, P_ATTACK_SPELL, ray
- **Fireball** (appears as "ragged") - Level 4, delay 4, cost 400, P_ATTACK_SPELL, ray
- **Cone of Cold** (appears as "dog eared") - Level 4, delay 7, cost 400, P_ATTACK_SPELL, ray
- **Sleep** (appears as "mottled") - Level 3, delay 1, cost 300, P_ENCHANTMENT_SPELL, ray
- **Finger of Death** (appears as "stained") - Level 7, delay 10, cost 700, P_ATTACK_SPELL, ray
- **Force Bolt** (appears as "red") - Level 1, delay 2, cost 100, P_ATTACK_SPELL, red, immediate
- **Drain Life** (appears as "velvet") - Level 2, delay 2, cost 200, P_ATTACK_SPELL, magenta, immediate
- **Chain Lightning** (appears as "checkered") - Level 2, delay 4, cost 200, P_ATTACK_SPELL, gray

### Divination Spells
- **Light** (appears as "cloth") - Level 1, delay 1, cost 100, P_DIVINATION_SPELL, cloth
- **Detect Monsters** (appears as "leathery") - Level 1, delay 1, cost 100, P_DIVINATION_SPELL, leather
- **Detect Food** (appears as "cyan") - Level 2, delay 3, cost 200, P_DIVINATION_SPELL, cyan
- **Clairvoyance** (appears as "dark blue") - Level 3, delay 3, cost 300, P_DIVINATION_SPELL, blue
- **Detect Unseen** (appears as "violet") - Level 3, delay 4, cost 300, P_DIVINATION_SPELL, magenta
- **Detect Treasure** (appears as "gray") - Level 4, delay 5, cost 400, P_DIVINATION_SPELL, gray
- **Magic Mapping** (appears as "dusty") - Level 5, delay 7, cost 500, P_DIVINATION_SPELL
- **Identify** (appears as "bronze") - Level 3, delay 6, cost 300, P_DIVINATION_SPELL, copper

### Healing Spells
- **Healing** (appears as "white") - Level 1, delay 2, cost 100, P_HEALING_SPELL, white, immediate
- **Cure Blindness** (appears as "yellow") - Level 2, delay 2, cost 200, P_HEALING_SPELL, yellow, immediate
- **Cure Sickness** (appears as "indigo") - Level 3, delay 3, cost 300, P_HEALING_SPELL, blue
- **Extra Healing** (appears as "plaid") - Level 3, delay 5, cost 300, P_HEALING_SPELL, green, immediate
- **Restore Ability** (appears as "light brown") - Level 4, delay 5, cost 400, P_HEALING_SPELL, brown
- **Stone to Flesh** (appears as "thick") - Level 3, delay 1, cost 300, P_HEALING_SPELL, immediate

### Enchantment Spells
- **Confuse Monster** (appears as "orange") - Level 1, delay 2, cost 100, P_ENCHANTMENT_SPELL, orange, immediate
- **Slow Monster** (appears as "light green") - Level 2, delay 2, cost 200, P_ENCHANTMENT_SPELL, bright green, immediate
- **Cause Fear** (appears as "light blue") - Level 3, delay 3, cost 300, P_ENCHANTMENT_SPELL, bright blue
- **Charm Monster** (appears as "magenta") - Level 5, delay 3, cost 500, P_ENCHANTMENT_SPELL, magenta, immediate

### Matter Spells
- **Knock** (appears as "pink") - Level 1, delay 1, cost 100, P_MATTER_SPELL, bright magenta, immediate
- **Wizard Lock** (appears as "dark green") - Level 2, delay 3, cost 200, P_MATTER_SPELL, green, immediate
- **Polymorph** (appears as "silver") - Level 6, delay 8, cost 600, P_MATTER_SPELL, silver, immediate
- **Cancellation** (appears as "shining") - Level 7, delay 8, cost 700, P_MATTER_SPELL, white, immediate

### Escape Spells
- **Haste Self** (appears as "purple") - Level 3, delay 4, cost 300, P_ESCAPE_SPELL, magenta
- **Levitation** (appears as "tan") - Level 4, delay 4, cost 400, P_ESCAPE_SPELL, brown
- **Invisibility** (appears as "dark brown") - Level 4, delay 5, cost 400, P_ESCAPE_SPELL, brown
- **Teleport Away** (appears as "gold") - Level 6, delay 6, cost 600, P_ESCAPE_SPELL, gold, immediate
- **Jumping** (appears as "thin") - Level 1, delay 3, cost 100, P_ESCAPE_SPELL, immediate

### Cleric Spells
- **Create Monster** (appears as "turquoise") - Level 2, delay 3, cost 200, P_CLERIC_SPELL, bright cyan
- **Remove Curse** (appears as "wrinkled") - Level 3, delay 5, cost 300, P_CLERIC_SPELL
- **Turn Undead** (appears as "copper") - Level 6, delay 8, cost 600, P_CLERIC_SPELL, copper, immediate
- **Create Familiar** (appears as "glittering") - Level 6, delay 7, cost 600, P_CLERIC_SPELL, white
- **Protection** (appears as "dull") - Level 1, delay 3, cost 100, P_CLERIC_SPELL

### Special Spellbooks
- **Blank Paper** (appears as "plain") - Level 0, delay 0, cost 0, no spell
- **Novel** (appears as "paperback") - Weight 10, cost 20, bright blue (tribute book, readable)
- **Book of the Dead** (appears as "papyrus") - Weight 50, cost 10000, Level 7, unique, quest item

---

## Wands

All wands weigh 7, have nutritional value 30.

### Non-Directional Wands
- **Wand of Light** (appears as "glass") - Cost 100, glass, magical
- **Wand of Secret Door Detection** (appears as "balsa") - Cost 150, wood, magical
- **Wand of Enlightenment** (appears as "crystal") - Cost 150, glass, magical
- **Wand of Create Monster** (appears as "maple") - Cost 200, wood, magical

### Special Wands
- **Wand of Wishing** (appears as "pine") - Cost 500, wood, magical (grants wishes)
- **Wand of Nothing** (appears as "oak") - Cost 100, wood, non-magical, immediate

### Immediate Effect Wands
- **Wand of Striking** (appears as "ebony") - Cost 150, wood, magical, immediate
- **Wand of Make Invisible** (appears as "marble") - Cost 150, mineral, magical, immediate
- **Wand of Slow Monster** (appears as "tin") - Cost 150, metal, magical, immediate
- **Wand of Speed Monster** (appears as "brass") - Cost 150, copper, magical, immediate
- **Wand of Undead Turning** (appears as "copper") - Cost 150, copper, magical, immediate
- **Wand of Polymorph** (appears as "silver") - Cost 200, silver, magical, immediate
- **Wand of Cancellation** (appears as "platinum") - Cost 200, platinum, white, magical, immediate
- **Wand of Teleportation** (appears as "iridium") - Cost 200, metal, bright cyan, magical, immediate
- **Wand of Opening** (appears as "zinc") - Cost 150, metal, magical, immediate
- **Wand of Locking** (appears as "aluminum") - Cost 150, metal, magical, immediate
- **Wand of Probing** (appears as "uranium") - Cost 150, metal, magical, immediate

### Ray Wands
- **Wand of Digging** (appears as "iron") - Cost 150, iron, magical, ray
- **Wand of Magic Missile** (appears as "steel") - Cost 150, iron, magical, ray
- **Wand of Fire** (appears as "hexagonal") - Cost 175, iron, magical, ray
- **Wand of Cold** (appears as "short") - Cost 175, iron, magical, ray
- **Wand of Sleep** (appears as "runed") - Cost 175, iron, magical, ray
- **Wand of Death** (appears as "long") - Cost 500, iron, magical, ray
- **Wand of Lightning** (appears as "curved") - Cost 175, iron, magical, ray

### Extra Wand Descriptions (shuffled at game start)
- "forked" (wood)
- "spiked" (iron)
- "jeweled" (iron)

---

## Gems and Stones

### Precious Gems (Real Gemstones)
- **Dilithium Crystal** (appears as "white") - Value 4500, nutrition 15, weight 1, white
- **Diamond** (appears as "white") - Value 4000, nutrition 15, weight 1, hardness 10, white
- **Ruby** (appears as "red") - Value 3500, nutrition 15, weight 1, hardness 9, red
- **Jacinth** (appears as "orange") - Value 3250, nutrition 15, weight 1, hardness 9, orange
- **Sapphire** (appears as "blue") - Value 3000, nutrition 15, weight 1, hardness 9, blue
- **Black Opal** (appears as "black") - Value 2500, nutrition 15, weight 1, hardness 8, black
- **Emerald** (appears as "green") - Value 2500, nutrition 15, weight 1, hardness 8, green
- **Turquoise** (appears as "green") - Value 2000, nutrition 15, weight 1, hardness 6, green
- **Citrine** (appears as "yellow") - Value 1500, nutrition 15, weight 1, hardness 6, yellow
- **Aquamarine** (appears as "green") - Value 1500, nutrition 15, weight 1, hardness 8, green
- **Amber** (appears as "yellowish brown") - Value 1000, nutrition 15, weight 1, hardness 2, brown
- **Topaz** (appears as "yellowish brown") - Value 900, nutrition 15, weight 1, hardness 8, brown
- **Jet** (appears as "black") - Value 850, nutrition 15, weight 1, hardness 7, black
- **Opal** (appears as "white") - Value 800, nutrition 15, weight 1, hardness 6, white
- **Chrysoberyl** (appears as "yellow") - Value 700, nutrition 15, weight 1, hardness 5, yellow
- **Garnet** (appears as "red") - Value 700, nutrition 15, weight 1, hardness 7, red
- **Amethyst** (appears as "violet") - Value 600, nutrition 15, weight 1, hardness 7, magenta
- **Jasper** (appears as "red") - Value 500, nutrition 15, weight 1, hardness 7, red
- **Fluorite** (appears as "violet") - Value 400, nutrition 15, weight 1, hardness 4, magenta
- **Obsidian** (appears as "black") - Value 200, nutrition 15, weight 1, hardness 6, black
- **Agate** (appears as "orange") - Value 200, nutrition 15, weight 1, hardness 6, orange
- **Jade** (appears as "green") - Value 300, nutrition 15, weight 1, hardness 6, green

### Worthless Glass (appears as colored glass)
All worthless glass: Value 0, nutrition 6, weight 1, hardness 5, glass

- **Worthless Piece of White Glass** (appears as "white")
- **Worthless Piece of Blue Glass** (appears as "blue")
- **Worthless Piece of Red Glass** (appears as "red")
- **Worthless Piece of Yellowish Brown Glass** (appears as "yellowish brown")
- **Worthless Piece of Orange Glass** (appears as "orange")
- **Worthless Piece of Yellow Glass** (appears as "yellow")
- **Worthless Piece of Black Glass** (appears as "black")
- **Worthless Piece of Green Glass** (appears as "green")
- **Worthless Piece of Violet Glass** (appears as "violet")

### Gray Stones (special properties)
All gray stones: 3d3 damage as weapons, nutrition 10, mineral, gray

- **Luckstone** (appears as "gray") - Weight 10, value 60, hardness 7, magical, grants luck
- **Loadstone** (appears as "gray") - Weight 500, value 1, hardness 6, magical, cursed (very heavy)
- **Touchstone** (appears as "gray") - Weight 10, value 45, hardness 6, magical (identifies gems)
- **Flint** (appears as "gray") - Weight 10, value 1, hardness 7, non-magical
- **Rock** - Weight 10, value 0, hardness 7, non-magical

---

## Artifacts

NetHack contains 42 named artifacts with special powers. Artifacts are unique items that cannot be wished for (except in wizard mode) and often have alignment, role, or race restrictions.

### Artifact Properties Legend
- **SPFX_NOGEN**: Cannot be randomly generated
- **SPFX_RESTR**: Restricted to certain alignments/roles
- **SPFX_INTEL**: Self-willed, intelligent artifact
- **SPFX_SEEK**: Helps search for things
- **SPFX_WARN**: Warns of danger
- **SPFX_ATTK**: Special attack
- **SPFX_DEFN**: Special defense
- **SPFX_DRLI**: Drains levels from monsters
- **SPFX_SEARCH**: Helps searching
- **SPFX_BEHEAD**: Can behead monsters
- **SPFX_HALRES**: Blocks hallucinations
- **SPFX_ESP**: Grants telepathy
- **SPFX_STLTH**: Grants stealth
- **SPFX_REGEN**: Grants regeneration
- **SPFX_EREGEN**: Grants energy regeneration
- **SPFX_HSPDAM**: Half spell damage in combat
- **SPFX_HPHDAM**: Half physical damage in combat
- **SPFX_TCTRL**: Teleportation control
- **SPFX_LUCK**: Increases luck
- **SPFX_XRAY**: Grants X-ray vision
- **SPFX_REFLECT**: Grants reflection
- **SPFX_PROTECT**: Grants protection

### General Artifacts

#### **Excalibur** (Long Sword)
- **Base Type**: Long Sword
- **Alignment**: Lawful
- **Role**: Knight (but usable by any lawful character)
- **Properties**:
  - NOGEN, RESTR, SEEK, DEFN, INTEL, SEARCH
  - +5 to-hit, +10 damage (physical)
  - Defense: Drains life (0d0)
  - Cost: 4000
  - Gift value: 10
- **Special**: Can be obtained by dipping long sword in fountain
- **Description**: The legendary sword of King Arthur, glows when searching

#### **Stormbringer** (Runesword)
- **Base Type**: Runesword
- **Alignment**: Chaotic
- **Properties**:
  - RESTR, ATTK, DEFN, INTEL, DRLI
  - +5 to-hit, +2 damage (drains life)
  - Also grants 8 more damage from level drain
  - Defense: Drains life (0d0)
  - Cost: 8000
  - Gift value: 9
- **Special**: Drains a level from victim, providing bonus damage
- **Description**: The evil black blade from Michael Moorcock's stories

#### **Mjollnir** (War Hammer)
- **Base Type**: War Hammer
- **Alignment**: Neutral
- **Role**: Valkyrie
- **Properties**:
  - RESTR, ATTK
  - +5 to-hit, +24 damage (electrical)
  - Cost: 4000
  - Gift value: 8
- **Special**: Can be thrown if Strength ≥ 25; returns to Valkyries 99% of the time
- **Description**: Thor's hammer from Norse mythology

#### **Cleaver** (Battle-Axe)
- **Base Type**: Battle-Axe
- **Alignment**: Neutral
- **Role**: Barbarian
- **Properties**:
  - RESTR
  - +3 to-hit, +6 damage (physical)
  - Cost: 1500
  - Gift value: 8

#### **Grimtooth** (Orcish Dagger)
- **Base Type**: Orcish Dagger
- **Alignment**: Chaotic
- **Race**: Orc
- **Properties**:
  - RESTR, WARN, DFLAG2 (warns of M2_ELF)
  - +2 to-hit, +6 damage (physical)
  - Defense: Poison (0d0)
  - Invoke: FLING_POISON
  - Cost: 1200
  - Gift value: 5
  - Color: Red (when glowing)

#### **Orcrist** (Elven Broadsword)
- **Base Type**: Elven Broadsword
- **Alignment**: Chaotic
- **Race**: Elf
- **Properties**:
  - WARN, DFLAG2 (warns of M2_ORC)
  - +5 damage (physical)
  - Gen spe: +3
  - Cost: 2000
  - Gift value: 4
  - Color: Bright blue (when glowing)
- **Description**: "Goblin-cleaver" from Tolkien's The Hobbit

#### **Sting** (Elven Dagger)
- **Base Type**: Elven Dagger
- **Alignment**: Chaotic
- **Race**: Elf
- **Properties**:
  - WARN, DFLAG2 (warns of M2_ORC)
  - +5 damage (physical)
  - Gen spe: +3
  - Cost: 800
  - Gift value: 1
  - Color: Bright blue (when glowing)
- **Description**: Bilbo and Frodo's dagger from Tolkien

#### **Magicbane** (Athame)
- **Base Type**: Athame
- **Alignment**: Neutral
- **Role**: Wizard
- **Properties**:
  - RESTR, ATTK, DEFN
  - +3 to-hit, +4 damage (stun)
  - Defense: Magic resistance
  - Cost: 3500
  - Gift value: 7
- **Special**: Causes magical fanfare, unbalances victims

#### **Frost Brand** (Long Sword)
- **Base Type**: Long Sword
- **Alignment**: None
- **Properties**:
  - RESTR, ATTK, DEFN
  - +5 damage (cold)
  - Defense: Cold resistance
  - Invoke: SNOWSTORM
  - Cost: 3000
  - Gift value: 9

#### **Fire Brand** (Long Sword)
- **Base Type**: Long Sword
- **Alignment**: None
- **Properties**:
  - RESTR, ATTK, DEFN
  - +5 damage (fire)
  - Defense: Fire resistance
  - Invoke: FIRESTORM
  - Cost: 3000
  - Gift value: 5

#### **Dragonbane** (Broadsword)
- **Base Type**: Broadsword
- **Alignment**: None
- **Properties**:
  - RESTR, DCLAS (dragons), REFLECT
  - +5 damage vs dragons (physical)
  - Grants reflection
  - Gen spe: +2
  - Cost: 500
  - Gift value: 5

#### **Demonbane** (Silver Mace)
- **Base Type**: Silver Mace
- **Alignment**: Lawful
- **Role**: Cleric
- **Properties**:
  - RESTR, DFLAG2 (M2_DEMON)
  - +5 damage vs demons (physical)
  - Invoke: BANISH
  - Gen spe: +1
  - Cost: 2500
  - Gift value: 3

#### **Werebane** (Silver Saber)
- **Base Type**: Silver Saber
- **Alignment**: None
- **Properties**:
  - RESTR, DFLAG2 (M2_WERE)
  - +5 damage vs lycanthropes (physical)
  - Defense: Lycanthropy resistance
  - Gen spe: +1
  - Cost: 1500
  - Gift value: 4

#### **Grayswandir** (Silver Saber)
- **Base Type**: Silver Saber
- **Alignment**: Lawful
- **Properties**:
  - RESTR, HALRES
  - +5 damage (physical)
  - Blocks hallucinations
  - Cost: 8000
  - Gift value: 10

#### **Giantslayer** (Long Sword)
- **Base Type**: Long Sword
- **Alignment**: Neutral
- **Properties**:
  - RESTR, DFLAG2 (M2_GIANT)
  - +5 damage vs giants (physical)
  - Gen spe: +2
  - Cost: 200
  - Gift value: 4

#### **Ogresmasher** (War Hammer)
- **Base Type**: War Hammer
- **Alignment**: None
- **Properties**:
  - RESTR, DCLAS (ogres)
  - +5 damage vs ogres (physical)
  - Gen spe: +2
  - Cost: 200
  - Gift value: 1

#### **Trollsbane** (Morning Star)
- **Base Type**: Morning Star
- **Alignment**: None
- **Properties**:
  - RESTR, DCLAS (trolls), REGEN
  - +5 damage vs trolls (physical)
  - Grants regeneration
  - Gen spe: +2
  - Cost: 200
  - Gift value: 1

#### **Vorpal Blade** (Long Sword)
- **Base Type**: Long Sword
- **Alignment**: Neutral
- **Properties**:
  - RESTR, BEHEAD
  - +5 to-hit, +1 damage (physical)
  - Can behead monsters
  - Gen spe: +1
  - Cost: 4000
  - Gift value: 5
- **Description**: From Lewis Carroll's "Jabberwocky"

#### **Snickersnee** (Katana)
- **Base Type**: Katana
- **Alignment**: Lawful
- **Role**: Samurai
- **Properties**:
  - RESTR
  - +8 damage (physical)
  - Cost: 1200
  - Gift value: 8
- **Description**: From Gilbert & Sullivan's "The Mikado"

#### **Sunsword** (Long Sword)
- **Base Type**: Long Sword
- **Alignment**: Lawful
- **Properties**:
  - RESTR, DFLAG2 (M2_UNDEAD)
  - +5 damage vs undead (physical)
  - Defense: Blindness resistance
  - Invoke: BLINDING_RAY
  - Cost: 1500
  - Gift value: 6
- **Special**: Emits light when wielded

### Quest Artifacts

All quest artifacts have SPFX_NOGEN, SPFX_RESTR, SPFX_INTEL, gen_spe: 0, gift_value: 12

#### **The Orb of Detection** (Crystal Ball)
- **Role**: Archeologist
- **Alignment**: Lawful
- **Properties**:
  - Carry: ESP, HSPDAM (half spell damage)
  - Defense: Magic resistance
  - Invoke: INVIS (grants invisibility)
  - Cost: 2500

#### **The Heart of Ahriman** (Luckstone)
- **Role**: Barbarian
- **Alignment**: Neutral
- **Properties**:
  - STLTH (stealth)
  - +5 damage when used as projectile (physical)
  - Invoke: LEVITATION
  - Cost: 2500

#### **The Sceptre of Might** (Mace)
- **Role**: Cave Dweller (Caveman/Cavewoman)
- **Alignment**: Lawful
- **Properties**:
  - DALIGN (bonus vs non-aligned monsters)
  - +5 damage (physical)
  - Defense: Magic resistance
  - Invoke: CONFLICT
  - Cost: 2500

#### **The Staff of Aesculapius** (Quarterstaff)
- **Role**: Healer
- **Alignment**: Neutral
- **Properties**:
  - ATTK, DRLI, REGEN
  - Attack: Drains life (0d0)
  - Defense: Drains life (0d0)
  - Invoke: HEALING
  - Cost: 5000

#### **The Magic Mirror of Merlin** (Mirror)
- **Role**: Knight
- **Alignment**: Lawful
- **Properties**:
  - SPEAK, ESP
  - Carry: Magic resistance
  - Cost: 1500

#### **The Eyes of the Overworld** (Lenses)
- **Role**: Monk
- **Alignment**: Neutral
- **Properties**:
  - XRAY (X-ray vision)
  - Defense: Magic resistance
  - Invoke: ENLIGHTENING
  - Cost: 2500

#### **The Mitre of Holiness** (Helm of Brilliance)
- **Role**: Cleric (Priest/Priestess)
- **Alignment**: Lawful
- **Properties**:
  - DFLAG2 (M2_UNDEAD), PROTECT
  - Carry: Fire resistance
  - Invoke: ENERGY_BOOST
  - Cost: 2000

#### **The Longbow of Diana** (Bow)
- **Role**: Ranger
- **Alignment**: Chaotic
- **Properties**:
  - REFLECT, ESP
  - +5 damage (physical)
  - Invoke: CREATE_AMMO
  - Cost: 4000

#### **The Master Key of Thievery** (Skeleton Key)
- **Role**: Rogue
- **Alignment**: Chaotic
- **Properties**:
  - SPEAK, WARN, TCTRL, HPHDAM
  - Invoke: UNTRAP
  - Cost: 3500
- **Special**: Guarantees successful untrap on doors/chests when not cursed (rogues) or blessed (non-rogues)

#### **The Tsurugi of Muramasa** (Tsurugi)
- **Role**: Samurai
- **Alignment**: Lawful
- **Properties**:
  - BEHEAD, LUCK, PROTECT
  - +8 damage (physical)
  - Cost: 4500

#### **The Platinum Yendorian Express Card** (Credit Card)
- **Role**: Tourist
- **Alignment**: Neutral
- **Properties**:
  - DEFN, ESP, HSPDAM
  - Carry: Magic resistance
  - Invoke: CHARGE_OBJ
  - Cost: 7000
- **Description**: Parody of American Express

#### **The Orb of Fate** (Crystal Ball)
- **Role**: Valkyrie
- **Alignment**: Neutral
- **Properties**:
  - LUCK, WARN, HSPDAM, HPHDAM
  - Invoke: LEV_TELE (level teleport)
  - Cost: 3500

#### **The Eye of the Aethiopica** (Amulet of ESP)
- **Role**: Wizard
- **Alignment**: Neutral
- **Properties**:
  - EREGEN, HSPDAM
  - Defense: Magic resistance
  - Invoke: CREATE_PORTAL
  - Cost: 4000

---

## Special Objects

### Unique Quest Items
- **Candelabrum of Invocation** - Gold candelabrum, weight 10, cost 5000, holds 7 candles
- **Bell of Opening** - Silver bell, weight 10, cost 5000, opens doors/locks
- **Book of the Dead** - Papyrus spellbook, weight 50, cost 10000, used on Astral Plane
- **Amulet of Yendor** - The ultimate quest goal, mithril, weight 20, cost 30000

### Miscellaneous
- **Heavy Iron Ball** - Weight 480, cost 10, 25d25 damage (+d4 when very heavy), punishment item
- **Iron Chain** - Weight 120, 4d4 damage (+1 both), attached to ball
- **Boulder** - Weight 6000, 20d20 damage, mineral, can be pushed
- **Statue** - Weight 2500 base (varies by monster), can act as container
- **Gold Piece** - Weight 1, value 1, currency

### Venom (Transitory Missiles)
- **Splash of Blinding Venom** (appears as "splash of venom") - Weight 1, liquid, blinding effect
- **Splash of Acid Venom** (appears as "splash of venom") - Weight 1, 6d6 damage, liquid, acid

---

## Object Interactions and Combinations

### Artifact Creation
- **Excalibur**: Dip a long sword in a fountain while lawful and level ≥ 5
- **Crysknife**: Enchant a stack of worm teeth (they fuse into one crysknife)

### Magical Interactions
- **Potion Dipping**: Dipping objects in potions can have various effects
- **Scroll of Enchant Weapon/Armor**: Improves enchantment level
- **Wand of Polymorph**: Can transform objects into different types
- **Scroll of Charging**: Recharges wands, tools, and other charged items

### Material Vulnerabilities
- **Rust**: IRON items can rust (weapons and armor)
- **Corrosion**: COPPER items can corrode
- **Silver**: Effective against silver-hating creatures (demons, undead, werecreatures)
- **Dragon Hide**: Dragon scale mail/scales from dragon corpses

### Item Combinations
- **Candelabrum + Candles**: Must place 7 candles in candelabrum for invocation
- **Bell of Opening + Book of the Dead + Candelabrum of Invocation**: The invocation artifacts
- **Tin + Tin Opener**: Open tins for food
- **Lock Pick/Skeleton Key/Credit Card**: Open locked doors and containers

---

## Object Generation and Randomization

### Randomized Appearances
Several object classes have randomized appearances that are shuffled at the start of each game:

1. **Potions**: Colors shuffled (e.g., "ruby potion" might be healing in one game, confusion in another)
2. **Scrolls**: Label text shuffled
3. **Spellbooks**: Cover descriptions shuffled
4. **Wands**: Material/appearance shuffled
5. **Rings**: Stone/material type shuffled
6. **Amulets**: Shape descriptions shuffled

### Object Discovery
Objects start unidentified and must be discovered through:
- Use/testing
- Scroll of Identify
- Identify spell
- Price identification (shops)
- Special abilities (touchstone for gems, etc.)

### Enchantment Levels
Most equipment has an enchantment level (spe):
- **Range**: Typically -7 to +7 (but can go beyond with wish/enchant)
- **Default**: Usually 0 for random generation
- **Artifacts**: Have gen_spe values (bonus when generated as gift/found)
- **Erosion**: Items can be corroded, burnt, rusty, or rotted

### Beatitude (BUC Status)
All items have a beatitude status:
- **Blessed**: Enhanced positive effects
- **Uncursed**: Normal effects
- **Cursed**: Often negative effects, can weld to body

### File Locations
- **Object Definitions**: `/home/user/NetHack/include/objects.h`
- **Object Initialization**: `/home/user/NetHack/src/objects.c`
- **Artifact Definitions**: `/home/user/NetHack/include/artilist.h`
- **Artifact Code**: `/home/user/NetHack/src/artifact.c`
- **Object Generation**: `/home/user/NetHack/src/mkobj.c`
- **Object Naming**: `/home/user/NetHack/src/objnam.c`

---

## Summary Statistics

- **Total Object Classes**: 17 (including RANDOM_CLASS)
- **Weapon Types**: ~70 (including projectiles, melee weapons, and launchers)
- **Armor Pieces**: ~60 (including dragon scale mail/scales)
- **Rings**: 28
- **Amulets**: 13 (including fake Amulet of Yendor)
- **Tools**: ~50 (containers, light sources, instruments, etc.)
- **Food Items**: ~40
- **Potions**: 26
- **Scrolls**: 23 (plus 20 extra label variants)
- **Spellbooks**: 44 (including blank paper and novel)
- **Wands**: 27 (including extra appearance variants)
- **Gems**: 22 precious gems + 9 worthless glass pieces
- **Stones**: 5 special gray stones
- **Artifacts**: 42 (including 1 dummy entry)
- **Special Items**: 6 (quest items, ball & chain, boulder, statue, coin, venom)

**Grand Total**: Approximately 430+ distinct object types

---

*This compendium was compiled from NetHack 3.7 source code.*
*Last updated: 2025-11-19*
