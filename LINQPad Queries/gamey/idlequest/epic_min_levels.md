# Epic 1.0 — minimum levels

Classic / Kunark-era class epics (pre–2.0). Sheet for IdleQuest planning: when a toon can **equip**, when the **click/proc matters**, and when the **quest is realistic**.

**Sources**
- Item text / quest framing: [Project 1999 — Class Epic Quest List](https://wiki.project1999.com/Class_Epic_Quest_List) (+ class epic pages)
- Item ids: local idlequest-content `data/items` (IdleQuest shortnames unchanged)
- Hard zone floors (Fear / Hate / Sky / Growth): hunt **46+** unless MQ’d

**IdleQuest note:** content `reqlevel` on these final weapons is **0** in brynnb/idlequest-content. Treat wiki **Required level of 46** rows as classic equip text — confirm on your server if IdleQuest enforces them. Stats still work when equipped; many **effects** still list an activation level.

**Practical rule of thumb**
- **Quest / planes:** plan **46+** for most classes (Fear tear, Hate, Sky, Growth, etc.).
- **Effect power:** most clickies / combat procs come online at **50**; MAG Manifest Elements is the usual **46** exception.
- **MQ:** many steps are multi-questable; final turn-ins and equip gates vary by era/server.

---

## Summary table

| Class | Final epic | Content id | Wiki equip req | Effect activates | Quest / zone floor | Notes |
| --- | --- | ---: | ---: | ---: | ---: | --- |
| Bard | **Singing Short Sword** | 20542 | **46** | Dance of the Blade **46** | 46+ | Equip + effect both 46 |
| Cleric | **Water Sprinkler of Nem Ankh** | 5532 | — | Reviviscence **50** | ~35+ turn-in cited; planes 46+ | Stats useful early; rez click at 50 |
| Druid | **Nature Walkers Scimitar** | 20490 | **46** | Wrath of Nature **50** | 46+ | |
| Enchanter | **Staff of the Serpent** | 10650 | — | Speed of the Shissar **50** | 46+ | Haste click at 50 |
| Magician | **Orb of Mastery** | 28034 | — | Manifest Elements **46** | 46+ (Sky / elements) | Pet orb; resummon when spent |
| Monk | **Celestial Fists** | 10652 | **46** | Celestial Tranquility **50** | 46+ | Hands slot |
| Necromancer | **Scythe of the Shadowed Soul** | 20544 | — | Torment of Shadows **50** | 46+ | |
| Paladin | **Fiery Defender** | 10099 | — | Holy Shock **50** | 46+ | Pre-pieces: SoulFire (5504), Fiery Avenger (11050) |
| Ranger | **Swiftwind** + **Earthcaller** | 20487 / 20488 | — | Swift Spirit (worn); Earthcall **50** | 46+ | Dual 1HS set |
| Rogue | **Ragebringer** | 11057 | **46** | Seething Fury (worn haste) | 46+ | 40% haste worn |
| Shadow Knight | **Innoruuk's Curse** | 14383 | — | Soul Consumption **50** | 46+ | Hole / Kunark pieces — see Quest-Gear |
| Shaman | **Spear of Fate** | 10651 | **46** | Curse of the Spirits **50** | Fear 46 (or MQ tear) | P99: equip gated 46 even if MQ’d |
| Warrior | **Jagged Blade of War** (+ 1HS pair) | 10908 (2HS); 10910 Strategy; 10909 Tactics | — | Rage of Zek / Vallon **50**; Tallon worn | Sky wingblade → often 46+ unless MQ | Combine/split 2HS ↔ 1HS pair |
| Wizard | **Staff of the Four** | 14341 | — | Barrier of Force **50** | 46+ | |

*—* = no “Required level of N” on the P99 epic list item text.

---

## Level meaning (don’t conflate)

| Gate | What it means |
| --- | --- |
| **Wiki equip req** | Item text “Required level of N” — cannot equip below N (classic). BRD / DRU / MNK / ROG / SHM show **46**. |
| **Effect activates** | Click / combat / worn special comes online at this level. You can often wear the weapon earlier for raw stats. |
| **Quest / zone floor** | Recommended or forced by dungeon level (Fear/Hate/Sky/Growth **46**). Steps may start lower; finales usually assume mid-40s+. |
| **Content `reqlevel`** | IdleQuest items DB — **0** for all finals above as of this sheet. |

---

## Roster hints (account NO DROP)

Same-account sharing still helps farm pieces; the **wearer** must clear equip / effect gates.

| Class | Typical idle epic timing |
| --- | --- |
| Classes with **equip 46** (BRD, DRU, MNK, ROG, SHM) | Don’t bother final turn-in before 46 |
| Most others | Can hold the stick earlier for stats; **schedule clicks/procs for 50** (MAG pet at **46**) |
| Plane-gated pieces | Treat **46** as the account gate even when MQ’d |

Related spare parts / Hole drops: LustreDamus `Quest-Gear.md` (PAL / RNG / MAG / ENC / SK Hole lines, SK Greenmist, monk Trunt’s Head, etc.).

---

## Sources

- https://wiki.project1999.com/Class_Epic_Quest_List
- Class pages (e.g. Shaman: Fear 46 / equip 46; Magician: recommended 46+)
- `D:\projects\idlequest-content\data\items` — ids listed above
- LustreDamus `assets/samples/everquest/Quest-Gear.md` — epic components that are hunt loot
