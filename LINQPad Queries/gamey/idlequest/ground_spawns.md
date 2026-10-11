# Epic-related ground spawns (IdleQuest / brynn content)

Snapshot of **epic-relevant** rows from local `idlequest-content` ground-spawn tables, with IdleQuest enablement called out.

Parent index: [Gleam.md](Gleam.md) · Related: [epic_min_levels.md](epic_min_levels.md)

---

## Content revision (for staleness)

| | |
| --- | --- |
| **Repo** | `D:\projects\idlequest-content` (`brynnb/idlequest-content`) |
| **HEAD** | `3d2eb4977612c146303c47537f4a283fc5b820ba` (`3d2eb497`, 2026-09-15 — README only) |
| **Data commit** (last touch of `data/ground_spawns`, `data/idlequest_enabled_ground_spawns`, `data/items`, `data/zone`) | `240bbf3bbd12fb488fb8359c31efbed2ce2733ff` (`240bbf3b`, 2026-09-15 — initial snapshot) |
| **Queried** | 2026-10-11 |

If `git rev-parse HEAD` or the data-commit hash above changes, re-run the refresh steps at the bottom and update this sheet.

---

## How IdleQuest treats ground spawns

- Full EQ dump: `data/ground_spawns/*.ndjson` (~1762 rows).
- IdleQuest whitelist: `data/idlequest_enabled_ground_spawns/*.ndjson` (**9** rows as of this snapshot).
- **Presence in `ground_spawns` ≠ idle-farmable.** Only the whitelist is marked for IdleQuest. Example: **Fusible Igneous Ore** is in `ground_spawns` but **not** enabled.

---

## IdleQuest-enabled (epic / epic-adjacent)

These are the only ground spawns in the enable table. Notes are from the enable rows themselves.

| GS id | Item | Item id | Zone | Respawn | Max | Enable note |
| ---: | --- | ---: | --- | ---: | ---: | --- |
| 8 | Flower (Solusek’s Flower quest) | *(see items)* | `mistmoore` | 6m (360s) | 1 | P99 Solusek's Flower — Ssynthi / Mistmoore |
| 13 | **Large Muddy Sandals** | — | `oggok` | 5m | 1 | ENC/SHM epic |
| 14 | **The One Key** | — | `overthere` | 1h (3600s) | 1 | ENC/SHM epic |
| 15 | **Lost Scroll** | — | `dalnir` | 5m | 1 | ENC/SHM epic |
| 16 | **Charm and Sacrifice** (book) | — | `airplane` | 864s (~14m) | 1 | ENC/SHM epic |
| 24 | **Unjeweled Dragon Head Hilt** | — | `lakerathe` | 5m | 1 | WAR epic |
| 25 | **Severely Damaged Dragon Head Hilt** | — | `timorous` | 5m | 1 | WAR epic |
| **107** | **Book of Souls** | **28016** | **`hateplane`** (classic Hate) | **10h (36000s)** | 1 | **ROG epic** — classic Plane of Hate |
| 1758 | **Icon of the High Scale** | — | `citymist` | 300000s (~3.5d) | 1 | ENC/SHM epic |

### Examples you asked about

**Rogue Book of Souls (Hate)**  
- **Enabled:** GS **107**, zone **`hateplane`**, item **28016**, respawn **36000s (10h)**, max 1, point ~(324.4, −59.5, 56.2).  
- **Also in dump but not enabled:** GS **29**, zone **`hateplaneb`**, same item **28016**, respawn **300s (5m)**. Treat classic Hate (`hateplane`) as the IdleQuest path; `hateplaneb` is a separate row.

**Fusible Igneous Ore (Skyfire / wizard epic chain)**  
- **In `ground_spawns`:** GS **26**, item **2448**, zoneid **91** → **`skyfire`**, respawn **360s (6m)**, **max_allowed 5**, box x −1481..−1381, y 892..992, comment `fusible igneous ore`.  
- **Not** in `idlequest_enabled_ground_spawns` → not IdleQuest-enabled in this snapshot.  
- Sibling fusible ores (2447 Coral, 2449 Earthen, 2450 Velium) have **no** `ground_spawns` rows here — only Igneous does.

---

## In `ground_spawns` + epic-tagged, but **not** IdleQuest-enabled

Useful when checking “does the data know about X?” vs “can idle pick it up?”

| GS id | Item | Zone | Respawn | Max | Comment / why listed |
| ---: | --- | --- | ---: | ---: | --- |
| **26** | **Fusible Igneous Ore** (2448) | `skyfire` | 6m | **5** | Wizard epic ore; **not enabled** |
| 29 | Book of Souls (28016) | `hateplaneb` | 5m | 1 | Rogue epic alt zone row; **not enabled** |
| 41 | Icon of the High Scale | `citymist` | 5m | 1 | Short-timer duplicate; enabled path is **1758** (~3.5d) |
| 83 | Coin Depicting Innoruuk | `charasis` | 6m | 1 | Charasis key (not a class epic final) |
| 1709–1726 | (item ids missing from `data/items` shards) | jaggedpine / necropolis / gunthak / hole / charasis / akheva / GoD zones | varies | 1 | Comments say **ranger/bard/monk/paladin 1.5** — post–1.0; items not in this items export |
| 1775–1776 | Locked Chest | freport / freeporteast | 30m | 1 | P99 “The Rogue Take” (not Ragebringer Book of Souls) |

---

## Class → enabled ground spawns (quick)

| Class epic | Enabled ground pieces in this snapshot |
| --- | --- |
| Rogue 1.0 | Book of Souls — `hateplane` GS 107 |
| Warrior 1.0 | Unjeweled + Severely Damaged Dragon Head Hilts — GS 24, 25 |
| Enchanter / Shaman 1.0 | Muddy Sandals, One Key, Lost Scroll, Charm and Sacrifice, Icon of the High Scale — GS 13–16, 1758 |
| Wizard 1.0 | Fusible Igneous Ore **exists** as GS 26 but **not enabled** |
| Other 1.0 classes | No rows in `idlequest_enabled_ground_spawns` |

---

## Agent / human: how to requery (update this file)

Run from a machine with the local clone. Refresh when content hashes change.

```powershell
cd D:\projects\idlequest-content
git fetch
git rev-parse HEAD
git log -1 --format="%H %h %ci %s" -- data/ground_spawns data/idlequest_enabled_ground_spawns data/items data/zone
```

Rebuild the enable table + join (Python 3):

```python
import json, glob, os
ROOT = r"D:\projects\idlequest-content\data"

def load_ndjson(table):
    rows = []
    for p in glob.glob(os.path.join(ROOT, table, "*.ndjson")):
        for line in open(p, encoding="utf-8"):
            if line.strip():
                rows.append(json.loads(line))
    return rows

zones = {}
for z in load_ndjson("zone"):
    zid = z.get("zoneidnumber")
    if zid is None: continue
    if zid not in zones or z.get("version") == 0:
        zones[zid] = z

items = {i["id"]: (i.get("Name") or i.get("name")) for i in load_ndjson("items")}
gs = {g["id"]: g for g in load_ndjson("ground_spawns")}
enabled = {e["ground_spawn_id"]: e.get("note") or "" for e in load_ndjson("idlequest_enabled_ground_spawns")}

def timer(s):
    if s is None: return "-"
    if s % 86400 == 0: return f"{s//86400}d"
    if s % 3600 == 0: return f"{s//3600}h"
    if s % 60 == 0: return f"{s//60}m"
    return f"{s}s"

print("ENABLED", len(enabled))
for gid, note in sorted(enabled.items()):
    g = gs[gid]; z = zones.get(g["zoneid"], {})
    print(gid, items.get(g["item"], "?"), z.get("short_name"), timer(g.get("respawn_timer")),
          "max", g.get("max_allowed"), "|", note, "|", g.get("comment"))

# spot-checks
for label, pred in [
    ("Fusible Igneous Ore", lambda g: g.get("item") == 2448 or "fusible igneous" in (g.get("comment") or "").lower()),
    ("Book of Souls", lambda g: g.get("item") == 28016 or "book of souls" in (g.get("comment") or "").lower()),
]:
    print("==", label)
    for g in gs.values():
        if pred(g):
            z = zones.get(g["zoneid"], {})
            print(g["id"], z.get("short_name"), timer(g.get("respawn_timer")),
                  "enabled" if g["id"] in enabled else "NOT enabled", g.get("comment"))
```

After re-query:

1. Update the **Content revision** hashes and **Queried** date.
2. Replace the tables if rows/timers/enablement changed.
3. Keep the distinction **in dump** vs **IdleQuest-enabled**.

Optional LINQPad: extend `spawn-lookup.linq` or a tiny `ground-spawn-lookup.linq` the same way (table scan + `SourceFile`/`SourceLine`); not required for this sheet.

---

## Sources

- `data/ground_spawns/*.ndjson`
- `data/idlequest_enabled_ground_spawns/*.ndjson`
- `data/zone/*.ndjson` (`zoneidnumber` → short/long name)
- `data/items/*.ndjson` (item display names; some 1.5 ids absent from export)
- P99 epic context: [Class Epic Quest List](https://wiki.project1999.com/Class_Epic_Quest_List)
