<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

// purpose: pull apart info from poe.trade into a more usable form (perhaps to render items with tooltips?)
type Option<'T> with
    member x.toDump() = 
        match x with
        | None -> Unchecked.defaultof<_>
        | Some x -> x
module Helpers = 
    let getText useCache = 
        let f:unit -> string = System.Windows.Forms.Clipboard.GetText
        if useCache then
            Util.Cache(Func<_>(f))
        else
            f()
    let delimit (d:string) (x:string seq) = String.Join(d,x |> Array.ofSeq)
    let dumpt t x = x.Dump(description=t); x
    let (|RMatch|_|) (pattern:string) x =
        let r = Regex.Match(x,pattern, RegexOptions.Multiline)
        if r.Success then Some r
        else None
    let (|RMatches|_|) (pattern:string) x = 
        let r = Regex.Matches(x,pattern, RegexOptions.Multiline ||| RegexOptions.IgnoreCase)
        if r.Count > 0 then Some r else None
        
open Helpers

let text = getText false
//text.Dump()   let 
let p = @"^\s*(?:corrupted\s)?(([A-Za-z][A-Za-z']+\s?)+)(?:\d+ (?:minutes?|hours?|days?|weeks?|months?)|yesterday)"
type ItemMatchInfo = {Name:string;StartIndex:int;Length:int}
// could be 1 or more items from poe.trade
type ItemInfo = {
                    Name:string
                    Corrupted:bool
                    LevelReq:int
                    Strength:int option
                    Dexterity: int option
                    Intelligence: int option
                    ILvl:int
                    MaxSockets: int
                    MaxVSockets: int
                    Attributes: string list
                    Quality: int option
                    Remainder:string list
                    Raw:obj}
let parseItemContainer text = 
    match text with
    | RMatches p r ->
        //printfn "Found match"
        let items = 
            r
            |> Seq.cast<Match>
            |> Seq.map(fun m -> {Name=m.Groups.[1].Value; StartIndex=m.Index;Length= m.Length})
            |> List.ofSeq
        items
        |> List.rev
        |> Seq.pairwise
        |> Seq.collect(fun (item,prev) ->
            seq{
                // if tail yield the whole thing
                if item.StartIndex = items.[items.Length - 1].StartIndex then
                    yield item.Name, text.[item.StartIndex..].Trim()
                // if not the tail, don't yield anything from the starting item
                yield prev.Name, text.[prev.StartIndex..item.StartIndex - 1].Trim()
            }
        )
        |> Seq.map(fun (name,x) ->
            if x.EndsWith("Item icon") then
                name,x.Replace("Item icon",String.Empty).Trim()
            else name,x
        )
        |> List.ofSeq
        |> List.rev
        //|> Seq.truncate 2
        |> List.ofSeq
        |> List.map(fun (name,text) ->
            let lines = text.SplitLines()
            let maybeField label valuePattern= sprintf @"%s: (%s)" label valuePattern
            let parseLine2 item = 
                let maybeMatch count p = 
                    match lines.[1] with
                    |RMatch p r ->
                        [1..count]
                        |> Seq.map(fun i -> r.Groups.[i].Value)
                        |> List.ofSeq
                        |> Some
                    | _ -> None
                let maybeInt name = maybeField name "\d+" |> maybeMatch 1 |> Option.map (List.head >> int)
                let ilvl = maybeInt "ilvl"
                let lvl = maybeInt "Level"
                let jSock,vSock = 
                    maybeField "Max sockets" "(\d) \((\d)\)" 
                    |> maybeMatch 3 
                    |> Option.bind (
                        function 
                        |_::s::v::[] -> 
                            Some (s,v) 
                        | _ -> None)
                    |> function
                        | None -> 0,0
                        | Some (s,v) -> s |> int, v |> int
                {item with  
                    ILvl = defaultArg ilvl 0
                    LevelReq = defaultArg lvl 0;
                    Strength = maybeInt "Strength"
                    Dexterity = maybeInt "Dexterity"
                    Intelligence = maybeInt "Intelligence"
                    MaxSockets=jSock
                    MaxVSockets=vSock
                    }
            let endAttrib x = Regex.Match(x,"^Quality|IGN:|^Armour ").Success
            let attrib = if lines.Length > 2 then lines.[2..] |> Seq.takeWhile (endAttrib>>not) |> List.ofSeq else List.empty
            
            
            let maybeGetLine2 = 
                if lines.Length > 1 then Some parseLine2 else None
            let remainder = 
                match lines.Length,maybeGetLine2 with
                | 0,_
                | 1,_ -> List.empty
                | x, _ when attrib.Length > 0 ->
                    lines.[2..] |> Seq.skipWhile (endAttrib>>not) |> List.ofSeq 
                | x,None -> lines.[1..] |> List.ofSeq
                | x, Some _ -> lines.[2..] |> List.ofSeq
            let maybeGetLine2 = 
                match maybeGetLine2 with
                | Some f-> f
                | None -> id
            {   Name=name
                Remainder = remainder
                Corrupted = text.StartsWith("corrupted")
                LevelReq= 0
                Strength= None
                Intelligence = None
                Dexterity = None
                MaxSockets = 0
                MaxVSockets = 0
                ILvl = 0
                Attributes = attrib
                Quality = None
                Raw=Util.OnDemand("rawText", fun () -> text)
            }
            |> maybeGetLine2
            
        )
        |> dumpt "result"
        |> ignore
        //Util.OnDemand("rawText", fun () -> text).Dump()
    | _ -> ()

let tests = [
    """Tabula Rasa Simple Robe 1 week ago[wiki]
ilvl: 80
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	 
13.5× online IGN: AzureStomper  Profile Whisper
 Item icon
Tabula Rasa Simple Robe 15 hours ago[wiki]
ilvl: 81
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	 
14× online IGN: SMOKING_HASH  Profile Whisper
 Item icon
corrupted Tabula Rasa Simple Robe 17 hours ago[wiki]
ilvl: 83
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	 
14× online IGN: UberZyklone  Profile Whisper
 Item icon
corrupted Tabula Rasa Simple Robe 21 hours ago[wiki]
ilvl: 77
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	 
14× online IGN: DopeRasom  Profile Whisper
 Item icon
corrupted Tabula Rasa Simple Robe 3 days ago[wiki]
ilvl: 73
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	 
14× online IGN: XSparkyTheSparkerX  Profile Whisper
 Item icon
corrupted Tabula Rasa Simple Robe 17 hours ago[wiki]
ilvl: 79
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	 
fixed price: 14× online IGN: HWALJANGE  Profile Whisper"""
    """Prismatic Eclipse Twilight Blade 24 minutes ago[wiki]
Level: 53 Strength: 91 Dexterity: 91 ilvl: 76 Max sockets: 3 (3)
40% increased Accuracy Rating
8% additional Block Chance while Dual Wielding
Adds 25 to 39 Physical Damage
25% increased Physical Damage with Weapons per Red Socket
12% increased Global Attack Speed per Green Socket
0.4% of Physical Attack Damage Leeched as Mana per Blue Socket
+2 to Melee Weapon Range per White Socket
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	66-150		1.3	140.4	140.4	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: DawMountain  Profile Whisper
 Item icon
Mark of the Doubting Knight Platinum Kris 1 month ago[wiki]
Level: 64 Dexterity: 76 Intelligence: 149 ilvl: 69 Max sockets: 3 (3)
50% increased Global Critical Strike Chance
5% additional Block Chance while Dual Wielding
259% increased Physical Damage
10% reduced Attack Speed
+9% to all Elemental Resistances
50% chance to Cause Bleeding on Critical Strike
50% chance to Cause Poison on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	90.79-360		1.08	243.43	243.43	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: Стрекозел  Profile Whisper
 Item icon
Ungil's Gauche Boot Knife 24 minutes ago[wiki]
Level: 20 Dexterity: 31 Intelligence: 45 ilvl: 76 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
12% additional Block Chance while Dual Wielding
81% increased Physical Damage
+19 to Dexterity
Adds 3 to 30 Lightning Damage
10% increased Attack Speed
50% increased Global Critical Strike Chance
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	17.77-69.96	3-30	1.54	92.96	67.55	25.41
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.3	 
1× online IGN: DawMountain  Profile Whisper
 Item icon
Taproot Ambusher 1 week ago[wiki]
Level: 60 Dexterity: 113 Intelligence: 113 ilvl: 72 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
199% increased Physical Damage
10% increased Attack Speed
17% increased Poison Duration
0.5% of Attack Damage Leeched as Mana against Poisoned Enemies
0.5% of Attack Damage Leeched as Life against Maimed Enemies
17% chance to Maim on Hit
20% chance to Poison on Hit
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	60.81-235.78		1.65	244.69	244.69	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: Hurrikaness  Profile Whisper
 Item icon
Bitterdream Shadow Sceptre 3 days ago[wiki]
Level: 32 Strength: 52 Intelligence: 62 ilvl: 73 Max sockets: 3 (3)
22% increased Elemental Damage
Socketed Gems are Supported by Level 1 Hypothermia
Socketed Gems are Supported by Level 1 Ice Bite
Socketed Gems are Supported by Level 1 Cold Penetration
Socketed Gems are Supported by Level 1 Mana Leech
Socketed Gems are Supported by Level 10 Added Cold Damage
Socketed Gems are Supported by Level 1 Reduced Mana
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	34.8-52.8		1.25	54.75	54.75	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.2	 
1× online IGN: IceeUoo  Profile Whisper
 Item icon
Lavianga's Wisdom War Hammer 3 weeks ago[wiki]
Level: 20 Strength: 71 ilvl: 71 Max sockets: 3 (3)
10% reduced Enemy Stun Threshold
153% increased Physical Damage
+19 to maximum Life
+13 to maximum Mana
5% reduced Movement Speed
10% increased Area of Effect of Area Skills
13% increased Area Damage
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	37.77-87.4		1.4	87.62	87.62	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: LemonNicholasRF  Profile Whisper
 Item icon
Taproot Ambusher 4 days ago[wiki]
Level: 60 Dexterity: 113 Intelligence: 113 ilvl: 69 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
185% increased Physical Damage
10% increased Attack Speed
17% increased Poison Duration
0.5% of Attack Damage Leeched as Mana against Poisoned Enemies
0.5% of Attack Damage Leeched as Life against Maimed Enemies
15% chance to Maim on Hit
20% chance to Poison on Hit
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	57.79-225.81		1.65	233.97	233.97	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: MíenoDísx  Profile Whisper
 Item icon
Rebuke of the Vaal Vaal Blade 1 week ago[wiki]
Level: 64 Strength: 113 Dexterity: 113 ilvl: 71 Max sockets: 3 (3)
+460 to Accuracy Rating
Adds 23 to 32 Physical Damage
Adds 27 to 36 Fire Damage
Adds 25 to 34 Cold Damage
Adds 1 to 50 Lightning Damage
Adds 28 to 32 Chaos Damage
11% increased Attack Speed
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	82.8-141.6	27-36
25-34
1-50	1.45	331.62	162.69	125.42
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: coldGaycold  Profile Whisper
 Item icon
Gorebreaker Spiked Club 18 hours ago[wiki]
Level: 10 Strength: 41 ilvl: 64 Max sockets: 3 (3)
10% reduced Enemy Stun Threshold
350% increased Physical Damage
20% reduced Attack Speed
12% reduced Enemy Stun Threshold
49% increased Stun Duration on Enemies
Extra gore
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	61.62-80.42		1.12	79.54	79.54	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: Nemrtiva  Profile Whisper
 Item icon
Bloodplay Stiletto 18 minutes ago[wiki]
Level: 15 Dexterity: 30 Intelligence: 30 ilvl: 80 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
27% increased Physical Damage
Adds 3 to 12 Physical Damage
+20 to Dexterity
10% increased Attack Speed
30% chance to cause Bleeding on Hit
Extra gore
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	15.05-57.87		1.65	60.16	60.16	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: merrycrimbolads  Profile Whisper
 Item icon
Daresso's Passion Estoc Yesterday[wiki]
Level: 43 Dexterity: 140 ilvl: 79 Max sockets: 3 (3)
+25% to Global Critical Strike Multiplier
Adds 35 to 40 Physical Damage
Adds 35 to 44 Cold Damage
20% reduced Frenzy Charge Duration
25% chance to gain a Frenzy Charge on Kill
72% increased Damage while you have no Frenzy Charges
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	67.2-108	35-44	1.5	190.65	131.4	59.25
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5.5	 
1× online IGN: merrycrimbolads  Profile Whisper
 Item icon
Mark of the Doubting Knight Platinum Kris 1 month ago[wiki]
Level: 64 Dexterity: 76 Intelligence: 149 ilvl: 77 Max sockets: 3 (3)
50% increased Global Critical Strike Chance
5% additional Block Chance while Dual Wielding
257% increased Physical Damage
10% reduced Attack Speed
+8% to all Elemental Resistances
50% chance to Cause Bleeding on Critical Strike
50% chance to Cause Poison on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	90.82-357.99		1.08	242.36	242.36	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: TeSacoLaGillette  Profile Whisper
 Item icon
Moonsorrow Imbued Wand Yesterday[wiki]
Level: 59 Intelligence: 188 ilvl: 77 Max sockets: 3 (3)
34% increased Spell Damage
Socketed Gems are supported by level 20 Blind
33% increased Spell Damage
268% increased Physical Damage
+10 to Intelligence
26% increased Lightning Damage
10% increased Cast Speed
10% chance to Blind Enemies on hit
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	92.78-170.8		1.5	197.69	197.69	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7	 
1× online IGN: SparkleThrow  Profile Whisper
 Item icon
Widowmaker Boot Blade 3 weeks ago[wiki]
Level: 44 Dexterity: 63 Intelligence: 90 ilvl: 70 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 39 to 56 Physical Damage
29% increased Critical Strike Chance
+38% to Global Critical Strike Multiplier
100% increased Critical Strike Chance against Enemies on Full Life
1% of Attack Damage Leeched as Life on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	64.8-138		1.4	141.96	141.96	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8.12	 
1× online IGN: _Shakal_  Profile Whisper
 Item icon
Goredrill Skinning Knife 23 hours ago[wiki]
Level: 5 Dexterity: 16 ilvl: 74 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
55% increased Physical Damage
Adds 1 to 3 Physical Damage
+11 to Dexterity
30% increased Critical Strike Chance
40% increased Attack Damage against Bleeding Enemies
50% chance to cause Bleeding on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	10.16-38.39		1.3	31.56	31.56	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7.8	 
1× online IGN: Gargaritzza  Profile Whisper
 Item icon
Mark of the Doubting Knight Platinum Kris 3 days ago[wiki]
Level: 64 Dexterity: 76 Intelligence: 149 ilvl: 68 Max sockets: 3 (3)
50% increased Global Critical Strike Chance
5% additional Block Chance while Dual Wielding
253% increased Physical Damage
10% reduced Attack Speed
+9% to all Elemental Resistances
50% chance to Cause Bleeding on Critical Strike
50% chance to Cause Poison on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
19+1	89.24-353.95		1.08	239.32	239.32	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: GreenGoe  Profile Whisper
 Item icon
Vengeance Etcher Siege Axe 3 weeks ago
Level: 59 Strength: 119 Dexterity: 82 ilvl: 59 Max sockets: 3 (3)
32% increased Physical Damage??
Adds 4 to 74 Lightning Damage P?
+29% to Global Critical Strike Multiplier S3
+3 Mana gained on Kill S2
+43 to Accuracy Rating??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	57.58-105.94	4-74	1.5	181.14	122.64	58.5
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
fixed price: 1× online IGN: SparkOneUp  Profile Whisper
 Item icon
Obliteration Demon's Horn 20 hours ago[wiki]
Level: 56 Intelligence: 179 ilvl: 83 Max sockets: 3 (3)
31% increased Spell Damage
Adds 30 to 80 Physical Damage
29% increased Critical Strike Chance
Gain 15% of Physical Damage as Extra Chaos Damage
Enemies you Kill have a 20% chance to Explode, dealing a quarter of their maximum Life as Chaos Damage
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	74.4-166.8		1.2	144.72	144.72	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	9.03	 
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Last Resort Nailed Fist 27 minutes ago[wiki]
ilvl: 73 Max sockets: 3 (3)
+3 Life gained for each Enemy hit by Attacks
25% increased Attack Speed when on Low Life
98% increased Physical Damage
Adds 2 to 10 Physical Damage
100% increased Claw Physical Damage when on Low Life
100% increased Accuracy Rating when on Low Life
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	13.21-46.24		1.6	47.56	47.56	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.2	 
1× online IGN: JimZnDP  Profile Whisper
 Item icon
Bloodplay Stiletto 23 hours ago[wiki]
Level: 15 Dexterity: 30 Intelligence: 30 ilvl: 79 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
22% increased Physical Damage
Adds 5 to 11 Physical Damage
+20 to Dexterity
10% increased Attack Speed
30% chance to cause Bleeding on Hit
Extra gore
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
7+13	16.51-53.94		1.65	58.12	58.12	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: Gargaritzza  Profile Whisper
 Item icon
Rebuke of the Vaal Vaal Blade 1 week ago[wiki]
Level: 64 Strength: 113 Dexterity: 113 ilvl: 80 Max sockets: 3 (3)
+460 to Accuracy Rating
Adds 26 to 31 Physical Damage
Adds 22 to 36 Fire Damage
Adds 21 to 32 Cold Damage
Adds 1 to 59 Lightning Damage
Adds 23 to 40 Chaos Damage
14% increased Attack Speed
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
11+9	86.49-140.54	22-36
21-32
1-59	1.48	341.16	168	126.54
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Ewar's Mirage Antique Rapier 3 days ago[wiki]
Level: 26 Dexterity: 89 ilvl: 75 Max sockets: 3 (3)
+25% to Global Critical Strike Multiplier
Adds 1 to 49 Lightning Damage
21% increased Attack Speed
Attacks Chain an additional time when in Main Hand
Attacks have an additional Projectile when in Off Hand
51% increased Elemental Damage with Attack Skills
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	14.4-55.2	1-49	1.57	93.89	54.64	39.25
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: Shikoyate  Profile Whisper
 Item icon
Onslaught Crusher Ancestral Club 1 week ago
Level: 56 Strength: 179 ilvl: 57 Max sockets: 3 (3)
10% reduced Enemy Stun Threshold
Adds 12 to 22 Physical Damage P?
+19 to Strength S7
+15 to Accuracy Rating S8
15% chance to cause Bleeding on Hit??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	72-122.4		1.25	121.5	121.5	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: ThatPlayer_  Profile Whisper
 Item icon
Torment Song Dusk Blade 3 weeks ago
Level: 32 Strength: 57 Dexterity: 57 ilvl: 32 Max sockets: 3 (3)
40% increased Accuracy Rating
20% increased Physical Damage??
+14% to Cold Resistance S7
+7 Life gained on Kill S2
+11 to Accuracy Rating??
12% increased Poison Duration??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	26.83-75.83		1.3	66.73	66.73	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: DawMountain  Profile Whisper
 Item icon
War Axe of Mastery 2 weeks ago
Level: 45 Strength: 106 Dexterity: 49 ilvl: 58 Max sockets: 3 (3)
13% increased Attack Speed
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	42-78		1.47	88.2	88.2	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× Verify online IGN: FENRIR_WRATH  Profile Whisper
 Item icon
Corpse Fang Gemini Claw 1 week ago
Level: 72 Dexterity: 82 Intelligence: 82 ilvl: 83 Max sockets: 3 (3)
+38 Life gained for each Enemy hit by Attacks
+14 Mana gained for each Enemy hit by Attacks
56% increased Physical Damage P7
30% increased Critical Strike Chance S2
32% reduced Attribute Requirements S1
11% increased Elemental Damage with Attack Skills??
20% chance to Poison on Hit??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
20	40-120		1.5	120	120	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8.19	 
1× online IGN: Gladi_aattori  Profile Whisper
 Item icon
Miracle Etcher Golden Kris 28 minutes ago
Level: 60 Dexterity: 51 Intelligence: 110 ilvl: 75 Max sockets: 3 (3)
50% increased Global Critical Strike Chance
+44 to Dexterity S2
Adds 46 to 86 Cold Damage P?
24% increased Attack Speed S2
+36 to maximum Mana P8
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	22.8-90	46-86	1.49	182.38	84.04	98.34
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
fixed price: 1× online IGN: Vaghuntress  Profile Whisper
 Item icon
Cameria's Maul Gavel 3 days ago[wiki]
Level: 60 Strength: 212 ilvl: 77 Max sockets: 3 (3)
15% reduced Enemy Stun Threshold
144% increased Physical Damage
Adds 11 to 43 Cold Damage
24% increased Critical Strike Chance
40% increased Rarity of Items Dropped by Frozen Enemies
33% increased Cold Damage with Attack Skills
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	142.82-266.16	11-43	1.15	266.21	235.16	31.05
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.2	 
1× online IGN: Stankoria  Profile Whisper
 Item icon
corrupted Hypnotic Spiker Platinum Kris 1 week ago
Level: 64 Dexterity: 76 Intelligence: 149 ilvl: 78
50% increased Global Critical Strike Chance
Adds 31 to 61 Fire Damage P?
Adds 26 to 59 Cold Damage P?
Adds 4 to 60 Lightning Damage P?
26% increased Attack Speed S1
+216 to Accuracy Rating S2
12% increased Poison Duration??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0	24-95	31-61
26-59
4-60	1.51	271.8	89.84	181.96
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: Pitava  Profile Whisper
 Item icon
Taproot Ambusher 5 days ago[wiki]
Level: 60 Dexterity: 113 Intelligence: 113 ilvl: 75 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
186% increased Physical Damage
15% increased Attack Speed
16% increased Poison Duration
0.5% of Attack Damage Leeched as Mana against Poisoned Enemies
0.5% of Attack Damage Leeched as Life against Maimed Enemies
17% chance to Maim on Hit
20% chance to Poison on Hit
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	57.78-226.83		1.72	244.76	244.76	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× Verify online IGN: QimoRF  Profile Whisper
 Item icon
Ungil's Gauche Boot Knife Yesterday[wiki]
Level: 20 Dexterity: 31 Intelligence: 45 ilvl: 77 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
12% additional Block Chance while Dual Wielding
94% increased Physical Damage
+17 to Dexterity
Adds 3 to 30 Lightning Damage
10% increased Attack Speed
50% increased Global Critical Strike Chance
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	18.75-75.01	3-30	1.54	97.61	72.2	25.41
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.3	 
1× Verify online IGN: QimoRF  Profile Whisper
 Item icon
Widowmaker Boot Blade 1 month ago[wiki]
Level: 44 Dexterity: 63 Intelligence: 90 ilvl: 73 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 35 to 58 Physical Damage
22% increased Critical Strike Chance
+31% to Global Critical Strike Multiplier
100% increased Critical Strike Chance against Enemies on Full Life
1% of Attack Damage Leeched as Life on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	60-140.4		1.4	140.28	140.28	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7.68	 
fixed price: 1× online IGN: Uber_malg  Profile Whisper
 Item icon
Widowmaker Boot Blade 1 week ago[wiki]
Level: 44 Dexterity: 63 Intelligence: 90 ilvl: 75 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 36 to 60 Physical Damage
25% increased Critical Strike Chance
+35% to Global Critical Strike Multiplier
100% increased Critical Strike Chance against Enemies on Full Life
1% of Attack Damage Leeched as Life on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	61.2-142.8		1.4	142.8	142.8	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7.87	 
fixed price: 1× online IGN: ZekeCyc  Profile Whisper
 Item icon
Horror Goad Omen Wand 2 weeks ago
Level: 53 Intelligence: 200 ilvl: 55 Max sockets: 3 (3)
30% increased Spell Damage
100% increased Physical Damage P5
9% increased Lightning Damage S5
+39 to maximum Mana P8
42% increased Mana Regeneration Rate S3
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	59.4-110		1.2	101.64	101.64	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8	 
1× Verify online IGN: FENRIR_WRATH  Profile Whisper
 Item icon
Bitterdream Shadow Sceptre 3 weeks ago[wiki]
Level: 32 Strength: 52 Intelligence: 62 ilvl: 76 Max sockets: 3 (3)
22% increased Elemental Damage
Socketed Gems are Supported by Level 1 Hypothermia
Socketed Gems are Supported by Level 1 Ice Bite
Socketed Gems are Supported by Level 1 Cold Penetration
Socketed Gems are Supported by Level 1 Mana Leech
Socketed Gems are Supported by Level 10 Added Cold Damage
Socketed Gems are Supported by Level 1 Reduced Mana
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	34.8-52.8		1.25	54.75	54.75	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.2	 
b/o: 1× · c/o: 1× Verify online IGN: AntaresUltima  Profile Whisper
 Item icon
Moonsorrow Imbued Wand Yesterday[wiki]
Level: 59 Intelligence: 188 ilvl: 77 Max sockets: 3 (3)
34% increased Spell Damage
Socketed Gems are supported by level 20 Blind
33% increased Spell Damage
274% increased Physical Damage
+10 to Intelligence
30% increased Lightning Damage
10% increased Cast Speed
10% chance to Blind Enemies on hit
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	94.81-173.82		1.5	201.47	201.47	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7	 
1× online IGN: ZekeCyc  Profile Whisper
 Item icon
Goredrill Skinning Knife 29 minutes ago[wiki]
Level: 5 Dexterity: 16 ilvl: 69 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
62% increased Physical Damage
Adds 2 to 4 Physical Damage
+10 to Dexterity
30% increased Critical Strike Chance
40% increased Attack Damage against Bleeding Enemies
50% chance to cause Bleeding on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	12.36-41.57		1.3	35.05	35.05	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7.8	 
1× online IGN: ZekeCyc  Profile Whisper
 Item icon
Bloodplay Stiletto Yesterday[wiki]
Level: 15 Dexterity: 30 Intelligence: 30 ilvl: 18 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
31% increased Physical Damage
Adds 5 to 9 Physical Damage
+20 to Dexterity
10% increased Attack Speed
30% chance to cause Bleeding on Hit
Extra gore
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	18.44-54.18		1.65	59.91	59.91	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: SparkleThrow  Profile Whisper
 Item icon
Ichimonji Corsair Sword 2 days ago[wiki]
Level: 58 Strength: 81 Dexterity: 117 ilvl: 75 Max sockets: 3 (3)
40% increased Accuracy Rating
80% increased Physical Damage
Adds 7 to 15 Physical Damage
24% increased Attack Speed
10% increased Effect of Buffs on You
5% reduced Mana Reserved
Allies' Aura Buffs do not affect you
Your Aura Buffs do not affect allies
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
7+13	53.48-190.37		1.92	234.1	234.1	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: merrycrimbolads  Profile Whisper
 Item icon
Widowmaker Boot Blade 1 week ago[wiki]
Level: 44 Dexterity: 63 Intelligence: 90 ilvl: 76 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 38 to 57 Physical Damage
25% increased Critical Strike Chance
+33% to Global Critical Strike Multiplier
100% increased Critical Strike Chance against Enemies on Full Life
1% of Attack Damage Leeched as Life on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	63.6-139.2		1.4	141.96	141.96	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7.87	 
1× online IGN: Gladi_aattori  Profile Whisper
 Item icon
Cybil's Paw Thresher Claw 2 days ago[wiki]
Level: 37 Dexterity: 53 Intelligence: 77 ilvl: 70 Max sockets: 3 (3)
+25 Life gained for each Enemy hit by Attacks
9% increased Cast Speed
+34 to maximum Mana
+6 Life gained for each Enemy hit by your Spells
8% increased Spell Damage per 5% Block Chance
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	24-63.6		1.3	56.94	56.94	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
fixed price: 1× online IGN: PashEnter  Profile Whisper
 Item icon
Gavel 15 hours ago
Level: 60 Strength: 212 ilvl: 62 Max sockets: 3 (3)
15% reduced Enemy Stun Threshold
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	64.8-121.2		1.15	106.95	106.95	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× Verify online IGN: FENRIR_WRATH  Profile Whisper
 Item icon
Widowmaker Boot Blade 2 months ago[wiki]
Level: 44 Dexterity: 63 Intelligence: 90 ilvl: 68 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 38 to 55 Physical Damage
27% increased Critical Strike Chance
+31% to Global Critical Strike Multiplier
100% increased Critical Strike Chance against Enemies on Full Life
1% of Attack Damage Leeched as Life on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	63.6-136.8		1.4	140.28	140.28	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8	 
fixed price: 1× online IGN: Uber_malg  Profile Whisper
 Item icon
Serrated Fright Claw of the Penguin 15 hours ago
Level: 34 Dexterity: 61 Intelligence: 61 ilvl: 35 Max sockets: 3 (3)
+20 Life gained for each Enemy hit by Attacks
54% increased Physical Damage
+22% to Cold Resistance
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	20.34-80.22		1.5	75.42	75.42	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.3	 
1× Verify online IGN: FENRIR_WRATH  Profile Whisper
 Item icon
Kraken Bite Ambusher 2 weeks ago
Level: 60 Dexterity: 113 Intelligence: 113 ilvl: 72 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 4 to 8 Physical Damage P?
Adds 1 to 6 Lightning Damage P?
+33% to Global Critical Strike Multiplier S2
+13 to Accuracy Rating S8
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	27.6-98.4	1-6	1.5	99.75	94.5	5.25
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: DawMountain  Profile Whisper
 Item icon
Apocalypse Mangler Spectral Axe 1 week ago
Level: 33 Strength: 85 Dexterity: 37 ilvl: 36 Max sockets: 3 (3)
Adds 13 to 23 Cold Damage P?
6% reduced Enemy Stun Threshold S5
+18% to Lightning Resistance S6
18% increased Stun Duration on Enemies S4
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	34.8-57.6	13-23	1.3	83.46	60.06	23.4
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× Verify online IGN: FENRIR_WRATH  Profile Whisper
 Item icon
Cataclysm Barb Imbued Wand 24 minutes ago
Level: 59 Intelligence: 188 ilvl: 78 Max sockets: 3 (3)
36% increased Spell Damage
+11 to Intelligence S9
Adds 23 to 42 Fire Damage to Spells P?
8% increased Attack Speed S3
54% increased Critical Strike Chance for Spells S4
+47 to maximum Mana P6
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	28.8-52.8		1.62	66.1	66.1	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7	 
1× online IGN: DawMountain  Profile Whisper
 Item icon
Apocalypse Needle Twilight Blade 1 week ago
Level: 53 Strength: 91 Dexterity: 91 ilvl: 56 Max sockets: 3 (3)
40% increased Accuracy Rating
Adds 11 to 16 Physical Damage P?
+30% to Fire Resistance S4
+192 to Accuracy Rating S3
29% increased Damage with Bleeding??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	49.2-122.4		1.3	111.54	111.54	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: Katpisseverstink  Profile Whisper
 Item icon
Cataclysm Song Basket Rapier 4 hours ago
Level: 17 Dexterity: 62 ilvl: 18 Max sockets: 3 (3)
+25% to Global Critical Strike Multiplier
Adds 12 to 28 Fire Damage P?
Adds 1 to 27 Lightning Damage P?
+16% to Lightning Resistance S7
+1 Mana gained on Kill S3
+18 to Accuracy Rating S7
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	13.2-31.2	12-28
1-27	1.5	84.3	33.3	51
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5.5	 
fixed price: 1× Verify online IGN: Pseudism  Profile Whisper
 Item icon
Soul Barb Jagged Foil 4 hours ago
Level: 22 Dexterity: 77 ilvl: 24 Max sockets: 3 (3)
+25% to Global Critical Strike Multiplier
34% increased Physical Damage??
Adds 7 to 13 Cold Damage P?
Adds 2 to 27 Lightning Damage P?
+40 to Accuracy Rating??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
7+13	18.57-44.78	7-13
2-27	1.6	89.88	50.68	39.2
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5.5	 
fixed price: 1× Verify online IGN: Pseudism  Profile Whisper
 Item icon
Taproot Ambusher 1 week ago[wiki]
Level: 60 Dexterity: 113 Intelligence: 113 ilvl: 83 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
195% increased Physical Damage
13% increased Attack Speed
20% increased Poison Duration
0.5% of Attack Damage Leeched as Mana against Poisoned Enemies
0.5% of Attack Damage Leeched as Life against Maimed Enemies
18% chance to Maim on Hit
20% chance to Poison on Hit
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	59.8-232.78		1.69	247.23	247.23	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: _Shakal_  Profile Whisper
 Item icon
Mark of the Doubting Knight Platinum Kris Yesterday[wiki]
Level: 64 Dexterity: 76 Intelligence: 149 ilvl: 70 Max sockets: 3 (3)
50% increased Global Critical Strike Chance
5% additional Block Chance while Dual Wielding
268% increased Physical Damage
10% reduced Attack Speed
+10% to all Elemental Resistances
50% chance to Cause Bleeding on Critical Strike
50% chance to Cause Poison on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	92.78-369.02		1.08	249.37	249.37	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
fixed price: 1× online IGN: Laura_the_Witch_HUN  Profile Whisper
 Item icon
Eagle Song Demon Dagger 2 weeks ago
Level: 68 Dexterity: 76 Intelligence: 149 ilvl: 75 Max sockets: 3 (3)
40% increased Global Critical Strike Chance
Adds 6 to 14 Physical Damage P?
Adds 9 to 136 Lightning Damage P?
98% increased Critical Strike Chance for Spells S2
36% increased Mana Regeneration Rate S4
+39% to Lightning Resistance S3
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	36-133.2	9-136	1.2	188.52	101.52	87
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: HERbinJER  Profile Whisper
 Item icon
Armageddon Charm Omen Wand 1 week ago
Level: 53 Intelligence: 200 ilvl: 73 Max sockets: 3 (3)
27% increased Spell Damage
38% increased Cold Damage??
Adds 2 to 28 Lightning Damage P?
Adds 8 to 17 Cold Damage to Spells P?
5% increased Cast Speed S7
+9% to Fire Resistance S8
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	32.4-60	2-28	1.2	73.44	55.44	18
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8	 
1× online IGN: HERbinJER  Profile Whisper
 Item icon
Viper Needle Golden Kris 4 days ago
Level: 47 Dexterity: 51 Intelligence: 110 ilvl: 72 Max sockets: 3 (3)
50% increased Global Critical Strike Chance
18% increased Spell Damage??
Adds 1 to 28 Lightning Damage P?
Adds 1 to 21 Lightning Damage to Spells P?
+15 to maximum Mana??
32% increased Stun Duration on Enemies S1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
12+8	22.5-90	1-28	1.2	84.9	67.5	17.4
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: HERbinJER  Profile Whisper
 Item icon
Rebuke of the Vaal Vaal Blade 20 minutes ago[wiki]
Level: 64 Strength: 113 Dexterity: 113 ilvl: 73 Max sockets: 3 (3)
+460 to Accuracy Rating
Adds 24 to 36 Physical Damage
Adds 24 to 37 Fire Damage
Adds 21 to 34 Cold Damage
Adds 1 to 66 Lightning Damage
Adds 20 to 33 Chaos Damage
14% increased Attack Speed
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	84-146.4	24-37
21-34
1-66	1.48	345.14	170.5	135.42
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
fixed price: 1× online IGN: _speedy_  Profile Whisper
 Item icon
Rift Song Fiend Dagger 4 days ago
Level: 53 Dexterity: 58 Intelligence: 123 ilvl: 72 Max sockets: 3 (3)
40% increased Global Critical Strike Chance
+34 to Intelligence S4
Adds 18 to 27 Cold Damage P?
Adds 4 to 60 Lightning Damage to Spells P?
25% increased Stun Duration on Enemies S3
+169 to Accuracy Rating S3
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	26.4-104.4	18-27	1.2	105.48	78.48	27
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: HERbinJER  Profile Whisper
 Item icon
Gale Barb Ambusher 3 weeks ago
Level: 60 Dexterity: 113 Intelligence: 113 ilvl: 76 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
+1 to Level of Socketed Melee Gems P2
Adds 1 to 3 Cold Damage P?
25% increased Attack Speed S2
+6 Mana gained on Kill S1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	22.8-88.8	1-3	1.88	108.66	104.9	3.76
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: HERbinJER  Profile Whisper
 Item icon
Widowmaker Boot Blade 20 minutes ago[wiki]
Level: 44 Dexterity: 63 Intelligence: 90 ilvl: 75 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 38 to 55 Physical Damage
26% increased Critical Strike Chance
+35% to Global Critical Strike Multiplier
100% increased Critical Strike Chance against Enemies on Full Life
1% of Attack Damage Leeched as Life on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	63.6-136.8		1.4	140.28	140.28	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7.93	 
fixed price: 1× online IGN: _speedy_  Profile Whisper
 Item icon
Rapture Scratch Platinum Kris 1 week ago
Level: 64 Dexterity: 76 Intelligence: 149 ilvl: 75 Max sockets: 3 (3)
50% increased Global Critical Strike Chance
+27 to Intelligence S6
77% increased Critical Strike Chance for Spells S3
28% increased Critical Strike Chance S3
+30 to maximum Mana P9
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	28.8-114		1.2	85.68	85.68	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8.32	 
1× online IGN: HERbinJER  Profile Whisper
 Item icon
Apocalypse Spell Tornado Wand 3 weeks ago
Level: 65 Intelligence: 212 ilvl: 74 Max sockets: 3 (3)
35% increased Spell Damage
Adds 12 to 24 Physical Damage P?
Adds 2 to 35 Lightning Damage to Spells P?
+38 to maximum Mana P8
35% increased Projectile Speed S2
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	39.6-103.2		1.3	92.82	92.82	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7	 
1× online IGN: HERbinJER  Profile Whisper
 Item icon
Bloodplay Stiletto 1 month ago[wiki]
Level: 15 Dexterity: 30 Intelligence: 30 ilvl: 73 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
30% increased Physical Damage
Adds 5 to 10 Physical Damage
+20 to Dexterity
10% increased Attack Speed
30% chance to cause Bleeding on Hit
Extra gore
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	18.46-55.38		1.65	60.92	60.92	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: Byrdclawz  Profile Whisper
 Item icon
Goredrill Skinning Knife Yesterday[wiki]
Level: 5 Dexterity: 16 ilvl: 50 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
58% increased Physical Damage
Adds 2 to 3 Physical Damage
+15 to Dexterity
30% increased Critical Strike Chance
40% increased Attack Damage against Bleeding Enemies
50% chance to cause Bleeding on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	12.39-39.43		1.3	33.68	33.68	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7.8	 
fixed price: 1× online IGN: Colddud  Profile Whisper
 Item icon
Taproot Ambusher 2 days ago[wiki]
Level: 60 Dexterity: 113 Intelligence: 113 ilvl: 79 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
192% increased Physical Damage
11% increased Attack Speed
16% increased Poison Duration
0.5% of Attack Damage Leeched as Mana against Poisoned Enemies
0.5% of Attack Damage Leeched as Life against Maimed Enemies
16% chance to Maim on Hit
20% chance to Poison on Hit
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	58.77-230.79		1.67	241.78	241.78	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
fixed price: 1× online IGN: AreQeNIDE  Profile Whisper
 Item icon
Pain Impaler Gemini Claw 2 months ago
Level: 72 Dexterity: 121 Intelligence: 121 ilvl: 72 Max sockets: 3 (3)
+38 Life gained for each Enemy hit by Attacks
+14 Mana gained for each Enemy hit by Attacks
Adds 1 to 2 Physical Damage P?
12% increased Attack Speed S6
+2 Life gained for each Enemy hit by Attacks S4
25% increased Damage with Poison??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	28.8-84		1.68	94.75	94.75	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.3	 
fixed price: 1× Verify online IGN: BudLacerHar  Profile Whisper
 Item icon
Cybil's Paw Thresher Claw 24 minutes ago[wiki]
Level: 37 Dexterity: 53 Intelligence: 77 ilvl: 65 Max sockets: 3 (3)
+25 Life gained for each Enemy hit by Attacks
8% increased Cast Speed
+37 to maximum Mana
+7 Life gained for each Enemy hit by your Spells
8% increased Spell Damage per 5% Block Chance
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	24-63.6		1.3	56.94	56.94	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: AaandOne  Profile Whisper
 Item icon
Ewar's Mirage Antique Rapier 23 minutes ago[wiki]
Level: 26 Dexterity: 89 ilvl: 72 Max sockets: 3 (3)
+25% to Global Critical Strike Multiplier
Adds 1 to 46 Lightning Damage
18% increased Attack Speed
Attacks Chain an additional time when in Main Hand
Attacks have an additional Projectile when in Off Hand
52% increased Elemental Damage with Attack Skills
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	14.4-55.2	1-46	1.54	89.78	53.59	36.19
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
fixed price: 1× online IGN: Selena_el_Cid  Profile Whisper
 Item icon
Axiom Perpetuum Bronze Sceptre 28 minutes ago[wiki]
Level: 10 Strength: 22 Intelligence: 22 ilvl: 70 Max sockets: 3 (3)
12% increased Elemental Damage
Adds 2 to 6 Fire Damage to Spells
Adds 2 to 6 Cold Damage to Spells
Adds 1 to 11 Lightning Damage to Spells
5% increased Cast Speed
109% increased Critical Strike Chance for Spells
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	14.4-27.6		1.25	26.25	26.25	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	 
1× online IGN: Gosmo  Profile Whisper
 Item icon
Rebuke of the Vaal Vaal Blade 1 week ago[wiki]
Level: 64 Strength: 113 Dexterity: 113 ilvl: 71 Max sockets: 3 (3)
+460 to Accuracy Rating
Adds 23 to 34 Physical Damage
Adds 26 to 32 Fire Damage
Adds 23 to 31 Cold Damage
Adds 1 to 52 Lightning Damage
Adds 20 to 40 Chaos Damage
17% increased Attack Speed
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	82.8-144	26-32
23-31
1-52	1.52	343.37	172.37	125.4
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
fixed price: 1× online IGN: short_Shadow  Profile Whisper
 Item icon
Wildslash Awl 28 minutes ago[wiki]
Level: 12 Dexterity: 25 Intelligence: 25 ilvl: 70 Max sockets: 3 (3)
+7 Life gained for each Enemy hit by Attacks
Adds 4 to 21 Physical Damage
+12 to Strength
+10 to Dexterity
15% increased Attack Speed
20% increased Damage with Movement Skills
15% increased Attack Speed with Movement Skills
15% reduced Accuracy Rating
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	13.2-54		1.72	57.79	57.79	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.3	 
1× online IGN: Gosmo  Profile Whisper
 Item icon
Lavianga's Wisdom War Hammer Yesterday[wiki]
Level: 20 Strength: 71 ilvl: 74 Max sockets: 3 (3)
10% reduced Enemy Stun Threshold
135% increased Physical Damage
+18 to maximum Life
+16 to maximum Mana
5% reduced Movement Speed
10% increased Area of Effect of Area Skills
10% increased Area Damage
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	35.81-81.38		1.4	82.03	82.03	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: SparkleThrow  Profile Whisper
 Item icon
Cameria's Maul Gavel 3 weeks ago[wiki]
Level: 60 Strength: 212 ilvl: 79 Max sockets: 3 (3)
15% reduced Enemy Stun Threshold
140% increased Physical Damage
Adds 11 to 35 Cold Damage
17% increased Critical Strike Chance
40% increased Rarity of Items Dropped by Frozen Enemies
31% increased Cold Damage with Attack Skills
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	140.83-262.17	11-35	1.15	258.18	231.72	26.45
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5.85	 
1× online IGN: SirCharlesPopoloch  Profile Whisper
 Item icon
Gorebreaker Spiked Club Yesterday[wiki]
Level: 10 Strength: 41 ilvl: 73 Max sockets: 3 (3)
10% reduced Enemy Stun Threshold
307% increased Physical Damage
20% reduced Attack Speed
17% reduced Enemy Stun Threshold
36% increased Stun Duration on Enemies
Extra gore
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	55.6-72.39		1.12	71.67	71.67	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: Gosmo  Profile Whisper
 Item icon
Brimstone Star Tyrant's Sekhem 3 weeks ago
Level: 58 Strength: 99 Intelligence: 99 ilvl: 66 Max sockets: 3 (3)
26% increased Elemental Damage
62% increased Physical Damage P7
Adds 17 to 38 Physical Damage P?
16% increased Fire Damage S4
7% increased Lightning Damage S6
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	108.98-214.58		1.25	202.22	202.22	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	 
fixed price: 1× online IGN: SparkOneUp  Profile Whisper
 Item icon
Ashcaller Quartz Wand 11 minutes ago[wiki]
Level: 18 Intelligence: 65 ilvl: 72 Max sockets: 3 (3)
20% increased Spell Damage
10% chance to Trigger Level 8 Summon Raging Spirit on Kill
Adds 14 to 20 Fire Damage
Adds 5 to 9 Fire Damage to Spells
42% increased Burning Damage
20% chance to Ignite
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	14.4-26.4	14-20	1.3	48.62	26.52	22.1
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7	 
1× online IGN: SellingGF_Ten_EX  Profile Whisper
 Item icon
Obliteration Demon's Horn 11 minutes ago[wiki]
Level: 56 Intelligence: 179 ilvl: 70 Max sockets: 3 (3)
31% increased Spell Damage
Adds 24 to 84 Physical Damage
27% increased Critical Strike Chance
Gain 15% of Physical Damage as Extra Chaos Damage
Enemies you Kill have a 20% chance to Explode, dealing a quarter of their maximum Life as Chaos Damage
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	67.2-171.6		1.2	143.28	143.28	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8.89	 
1× online IGN: SellingGF_Ten_EX  Profile Whisper
 Item icon
Rebuke of the Vaal Vaal Blade 3 weeks ago[wiki]
Level: 64 Strength: 113 Dexterity: 113 ilvl: 76 Max sockets: 3 (3)
+460 to Accuracy Rating
Adds 22 to 36 Physical Damage
Adds 23 to 32 Fire Damage
Adds 24 to 36 Cold Damage
Adds 1 to 59 Lightning Damage
Adds 24 to 35 Chaos Damage
18% increased Attack Speed
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	81.6-146.4	23-32
24-36
1-59	1.54	355.74	175.56	134.75
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× Verify online IGN: AntaresUltima  Profile Whisper
 Item icon
Glyph Chant Tornado Wand 5 days ago
Level: 65 Intelligence: 212 ilvl: 78 Max sockets: 3 (3)
38% increased Spell Damage
18% increased Spell Damage??
Adds 18 to 36 Fire Damage to Spells P?
45% increased Critical Strike Chance for Spells S4
32% increased Critical Strike Chance S2
+14 to maximum Mana??
+104 to Accuracy Rating S6
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	25.2-74.4		1.3	64.74	64.74	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	9.24	 
1× online IGN: Pastasaws  Profile Whisper
 Item icon
Vengeance Sunder Ceremonial Axe 1 month ago
Level: 51 Strength: 134 Dexterity: 39 ilvl: 61 Max sockets: 3 (3)
Adds 21 to 43 Fire Damage P?
Adds 11 to 21 Cold Damage P?
25% increased Attack Speed S2
11% increased Stun Duration on Enemies S5
23% increased Damage with Bleeding??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	54-99.6	21-43
11-21	1.5	187.2	115.2	72
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: Fulgur_Aranearum  Profile Whisper
 Item icon
corrupted Ungil's Gauche Boot Knife 6 days ago[wiki]
Level: 40 Dexterity: 31 Intelligence: 45 ilvl: 78
0.2% of Fire Damage Leeched as Life
12% additional Block Chance while Dual Wielding
99% increased Physical Damage
+11 to Dexterity
Adds 3 to 30 Lightning Damage
10% increased Attack Speed
50% increased Global Critical Strike Chance
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
20	20-77	3-30	1.54	100.1	74.69	25.41
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.3	 
1× online IGN: kurukuru_desuyo  Profile Whisper
 Item icon
Shadow Sceptre 1 week ago
Level: 32 Strength: 52 Intelligence: 62 ilvl: 36 Max sockets: 3 (3)
22% increased Elemental Damage
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	34.8-52.8		1.25	54.75	54.75	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.2	 
1× Verify online IGN: FENRIR_WRATH  Profile Whisper
 Item icon
Daresso's Passion Estoc 3 days ago[wiki]
Level: 43 Dexterity: 140 ilvl: 81 Max sockets: 3 (3)
+25% to Global Critical Strike Multiplier
Adds 31 to 40 Physical Damage
Adds 32 to 47 Cold Damage
20% reduced Frenzy Charge Duration
25% chance to gain a Frenzy Charge on Kill
60% increased Damage while you have no Frenzy Charges
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	62.4-108	32-47	1.5	187.05	127.8	59.25
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5.5	 
fixed price: 1× online IGN: beeskneesmcfreeze  Profile Whisper
 Item icon
Taproot Ambusher 4 days ago[wiki]
Level: 60 Dexterity: 113 Intelligence: 113 ilvl: 77 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
193% increased Physical Damage
11% increased Attack Speed
19% increased Poison Duration
0.5% of Attack Damage Leeched as Mana against Poisoned Enemies
0.5% of Attack Damage Leeched as Life against Maimed Enemies
18% chance to Maim on Hit
20% chance to Poison on Hit
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	59.82-231.81		1.67	243.51	243.51	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.1	 
1× online IGN: FlickrFloppr  Profile Whisper
 Item icon
Chilled War Hammer 2 weeks ago
Level: 20 Strength: 71 ilvl: 20 Max sockets: 3 (3)
10% reduced Enemy Stun Threshold
Adds 8 to 14 Cold Damage
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	16.8-38.4	8-14	1.4	54.04	38.64	15.4
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× Verify online IGN: FENRIR_WRATH  Profile Whisper
 Item icon
Obliteration Demon's Horn Yesterday[wiki]
Level: 56 Intelligence: 179 ilvl: 66 Max sockets: 3 (3)
31% increased Spell Damage
Adds 29 to 86 Physical Damage
32% increased Critical Strike Chance
Gain 14% of Physical Damage as Extra Chaos Damage
Enemies you Kill have a 20% chance to Explode, dealing a quarter of their maximum Life as Chaos Damage
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	73.2-174		1.2	148.32	148.32	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	9.24	 
1× online IGN: SparkleThrow  Profile Whisper
 Item icon
Brood Stinger Skean 4 days ago
Level: 28 Dexterity: 42 Intelligence: 60 ilvl: 73 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
+1 to Level of Socketed Melee Gems P2
Adds 1 to 2 Physical Damage P?
Adds 3 to 43 Lightning Damage P?
51% increased Critical Strike Chance for Spells S4
24% increased Damage with Poison??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	14.4-54	3-43	1.45	82.94	49.59	33.35
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.3	 
1× online IGN: JimZnDP  Profile Whisper
 Item icon
Bitterdream Shadow Sceptre 1 week ago[wiki]
Level: 32 Strength: 52 Intelligence: 62 ilvl: 74 Max sockets: 3 (3)
22% increased Elemental Damage
Socketed Gems are Supported by Level 1 Hypothermia
Socketed Gems are Supported by Level 1 Ice Bite
Socketed Gems are Supported by Level 1 Cold Penetration
Socketed Gems are Supported by Level 1 Mana Leech
Socketed Gems are Supported by Level 10 Added Cold Damage
Socketed Gems are Supported by Level 1 Reduced Mana
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	34.8-52.8		1.25	54.75	54.75	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.2	 
1× online IGN: IceeUoo  Profile Whisper
 Item icon
Rune Scalpel Imperial Skean 2 days ago
Level: 66 Dexterity: 95 Intelligence: 131 ilvl: 75 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 7 to 15 Cold Damage P?
14% increased Critical Strike Chance S6
+30 to maximum Mana P9
10% increased Accuracy Rating??
15% increased Light Radius??
33% increased Damage with Poison??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	22.8-91.2	7-15	1.45	98.6	82.65	15.95
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7.18	 
1× online IGN: Aredhael  Profile Whisper
 Item icon
Tempest Song Copper Kris 2 days ago
Level: 28 Dexterity: 28 Intelligence: 60 ilvl: 35 Max sockets: 3 (3)
50% increased Global Critical Strike Chance
Adds 1 to 3 Cold Damage P?
Adds 18 to 31 Cold Damage to Spells P?
+13% to Global Critical Strike Multiplier S6
+40 to maximum Mana P7
12% increased Poison Duration??
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	14.4-55.2	1-3	1.2	44.16	41.76	2.4
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: Lesta  Profile Whisper
 Item icon
Cameria's Maul Gavel Yesterday[wiki]
Level: 60 Strength: 212 ilvl: 75 Max sockets: 3 (3)
15% reduced Enemy Stun Threshold
159% increased Physical Damage
Adds 15 to 34 Cold Damage
20% increased Critical Strike Chance
40% increased Rarity of Items Dropped by Frozen Enemies
33% increased Cold Damage with Attack Skills
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	150.81-282.23	15-34	1.15	277.17	249	28.18
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	 
1× online IGN: Dictarion  Profile Whisper
 Item icon
Moonsorrow Imbued Wand 4 weeks ago[wiki]
Level: 59 Intelligence: 188 ilvl: 78 Max sockets: 3 (3)
33% increased Spell Damage
Socketed Gems are supported by level 20 Blind
30% increased Spell Damage
250% increased Physical Damage
+10 to Intelligence
30% increased Lightning Damage
10% increased Cast Speed
10% chance to Blind Enemies on hit
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	88.8-162.8		1.5	188.7	188.7	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7	 
fixed price: 1× Verify online IGN: ChoboPewPew  Profile Whisper
 Item icon
Widowmaker Boot Blade 1 week ago[wiki]
Level: 44 Dexterity: 63 Intelligence: 90 ilvl: 76 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 37 to 57 Physical Damage
27% increased Critical Strike Chance
+37% to Global Critical Strike Multiplier
100% increased Critical Strike Chance against Enemies on Full Life
1% of Attack Damage Leeched as Life on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	62.4-139.2		1.4	141.12	141.12	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8	 
fixed price: 1× online IGN: xXSaboXx  Profile Whisper
 Item icon
Obliteration Demon's Horn 22 hours ago[wiki]
Level: 56 Intelligence: 179 ilvl: 79 Max sockets: 3 (3)
33% increased Spell Damage
Adds 28 to 80 Physical Damage
27% increased Critical Strike Chance
Gain 14% of Physical Damage as Extra Chaos Damage
Enemies you Kill have a 20% chance to Explode, dealing a quarter of their maximum Life as Chaos Damage
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	72-166.8		1.2	143.28	143.28	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8.89	 
1× online IGN: ImAPotatoMonster  Profile Whisper
 Item icon
Vengeance Sunder Ceremonial Axe 1 month ago
Level: 51 Strength: 134 Dexterity: 39 ilvl: 61 Max sockets: 3 (3)
Adds 21 to 43 Fire Damage P?
Adds 11 to 21 Cold Damage P?
25% increased Attack Speed S2
11% increased Stun Duration on Enemies S5
23% increased Damage with Bleeding??
Mirrored
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	54-99.6	21-43
11-21	1.5	187.2	115.2	72
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	 
1× online IGN: Fulgur_Aranearum  Profile Whisper
 Item icon
Goredrill Skinning Knife 1 week ago[wiki]
Level: 5 Dexterity: 16 ilvl: 36 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
66% increased Physical Damage
Adds 1 to 4 Physical Damage
+13 to Dexterity
30% increased Critical Strike Chance
40% increased Attack Damage against Bleeding Enemies
50% chance to cause Bleeding on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	11.2-42.58		1.3	34.96	34.96	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	7.8	 
1× Verify online IGN: KelpiX  Profile Whisper
 Item icon
Widowmaker Boot Blade 6 days ago[wiki]
Level: 44 Dexterity: 63 Intelligence: 90 ilvl: 74 Max sockets: 3 (3)
30% increased Global Critical Strike Chance
Adds 36 to 60 Physical Damage
27% increased Critical Strike Chance
+30% to Global Critical Strike Multiplier
100% increased Critical Strike Chance against Enemies on Full Life
1% of Attack Damage Leeched as Life on Critical Strike
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	61.2-142.8		1.4	142.8	142.8	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	8	 
1× online IGN: Fulgur_Aranearum  Profile Whisper
 Item icon
Al Dhih Timeworn Claw Yesterday[wiki]
Level: 26 Dexterity: 39 Intelligence: 56 ilvl: 76 Max sockets: 3 (3)
+19 Life gained for each Enemy hit by Attacks
Socketed Gems have 10% chance to cause Enemies to Flee on Hit
Trigger Level 1 Abyssal Cry on Hit
118% increased Physical Damage
3% of Physical Attack Damage Leeched as Life
10% reduced Enemy Stun Threshold with this Weapon
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	38.21-102.62		1.3	91.54	91.54	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6.5	 
1× online IGN: Nemrtiva  Profile Whisper
 Item icon
Grinning Fetish 1 week ago
Level: 35 Strength: 62 Intelligence: 62 ilvl: 36 Max sockets: 3 (3)
18% increased Elemental Damage
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	28.8-43.2		1.5	54	54	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	 
1× Verify online IGN: FENRIR_WRATH  Profile Whisper
 Item icon
Daresso's Passion Estoc Yesterday[wiki]
Level: 43 Dexterity: 140 ilvl: 78 Max sockets: 3 (3)
+25% to Global Critical Strike Multiplier
Adds 31 to 41 Physical Damage
Adds 30 to 42 Cold Damage
20% reduced Frenzy Charge Duration
25% chance to gain a Frenzy Charge on Kill
64% increased Damage while you have no Frenzy Charges
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0+20	62.4-109.2	30-42	1.5	182.7	128.7	54
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5.5	 
1× online IGN: Greate_Balls_Of_Fire  Profile Whisper"""
    """
Rain of Arrows 1 week ago
Level: 12 Dexterity: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Cold Snap 1 week ago
Level: 4 Intelligence: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Minefield Support 1 week ago
Level: 8
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Trap Cooldown Support 2 months ago
Level: 31 Dexterity: 52
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Purity of Lightning 1 week ago
Level: 24 Intelligence: 58
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Cold to Fire Support 1 week ago
Level: 18 Strength: 21 Intelligence: 14
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Ice Shot 1 week ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Smoke Mine 1 month ago
Level: 10 Dexterity: 18
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Bear Trap 1 week ago
Level: 4 Dexterity: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Trap Cooldown Support 2 months ago
Level: 50 Dexterity: 81
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	10
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Bladefall 1 week ago
Level: 28 Dexterity: 67
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Siege Ballista 3 weeks ago
Level: 4 Dexterity: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Ice Shot 1 week ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Rain of Arrows 1 week ago
Level: 12 Dexterity: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Caustic Arrow 1 week ago
Level: 4 Dexterity: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
5			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Detonate Dead 1 week ago
Level: 4
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Vigilant Strike 1 week ago
Level: 20 Strength: 31 Intelligence: 22
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	6
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Rejuvenation Totem 1 month ago
Level: 4 Strength: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Abyssal Cry 1 week ago
Level: 34 Strength: 79
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Vigilant Strike 1 week ago
Level: 4
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Glacial Hammer 1 week ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Dark Pact 3 weeks ago
Level: 28 Intelligence: 67
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Conductivity 3 weeks ago
Level: 24 Intelligence: 58
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Contagion 2 weeks ago
Level: 4
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Glacial Hammer 3 weeks ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Animate Guardian 3 weeks ago
Level: 28 Strength: 42 Intelligence: 29
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Riposte 3 weeks ago
Level: 4 Dexterity: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Cold Snap 1 week ago
Level: 6 Intelligence: 20
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	2
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Flesh Offering 1 week ago
Level: 12 Intelligence: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Deadly Ailments Support 3 weeks ago
Level: 18 Dexterity: 21 Intelligence: 14
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Frost Wall 3 weeks ago
Level: 4 Intelligence: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Contagion 3 weeks ago
Level: 4
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Sweep 1 week ago
Level: 12 Strength: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Cast on Melee Kill Support 2 weeks ago
Level: 38 Strength: 39 Intelligence: 27
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Shockwave Totem 3 weeks ago
Level: 28 Strength: 67
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Rejuvenation Totem 1 month ago
Level: 4 Strength: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Cluster Traps Support 2 weeks ago
Level: 38 Dexterity: 63
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Fireball 2 months ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	1
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Chance to Flee Support 2 months ago
Level: 8
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Conversion Trap 1 month ago
Level: 4
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Viper Strike 2 months ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Chance to Flee Support 1 month ago
Level: 8
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: Dear_Desolation  Profile Whisper
 Item icon
Puncture an hour ago
Level: 4
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Herald of Thunder 13 minutes ago
Level: 16 Intelligence: 41
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Rejuvenation Totem an hour ago
Level: 4 Strength: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Frost Blades an hour ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
12			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Earthquake an hour ago
Level: 28 Strength: 67
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Blind Support an hour ago
Level: 8 Dexterity: 18
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Increased Area of Effect Support 49 minutes ago
Level: 38 Intelligence: 63
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Scorching Ray an hour ago
Level: 12 Intelligence: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
7			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Wild Strike an hour ago
Level: 28 Strength: 29 Dexterity: 42
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Righteous Fire an hour ago
Level: 16 Strength: 18 Intelligence: 26
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Pierce Support an hour ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Flicker Strike an hour ago
Level: 10 Dexterity: 29
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Summon Raging Spirit an hour ago
Level: 4 Intelligence: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Heavy Strike 1 month ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Agroumia  Profile Whisper
 Item icon
Raise Zombie 1 month ago
Level: 60 Intelligence: 134
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	17
1× online IGN: Kuchen_Meister  Profile Whisper
 Item icon
Conductivity 1 month ago
Level: 60 Intelligence: 134
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	15
1× online IGN: _FLiKeRiNO_  Profile Whisper
 Item icon
Explosive Arrow 2 weeks ago
Level: 60 Dexterity: 134
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	15
1× online IGN: CitrusFlick  Profile Whisper
 Item icon
Rejuvenation Totem 2 weeks ago
Level: 52 Strength: 117
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	14
1× online IGN: CitrusFlick  Profile Whisper
 Item icon
Cast when Stunned Support 2 weeks ago
Level: 64 Dexterity: 44 Intelligence: 64
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	14
1× online IGN: CitrusFlick  Profile Whisper
 Item icon
Shrapnel Shot 2 weeks ago
Level: 28 Dexterity: 67
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	9
1× online IGN: CitrusFlick  Profile Whisper
 Item icon
Reckoning 1 month ago
Level: 4 Strength: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: JohnnSnow  Profile Whisper
 Item icon
Arc 1 week ago
Level: 12 Intelligence: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
10			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Sweep 1 week ago
Level: 12 Strength: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Glacial Hammer 1 week ago
Level: 7 Strength: 22
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	4
1× online IGN: HaHaHaHeHeHexd  Profile Whisper
 Item icon
Animate Weapon 1 week ago
Level: 4
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Void Manipulation Support 1 week ago
Level: 37 Dexterity: 39 Intelligence: 27
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	9
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Enduring Cry 1 week ago
Level: 28 Strength: 67
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	4
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Summon Chaos Golem 1 week ago
Level: 36 Intelligence: 83
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	2
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Elemental Hit 1 week ago
Level: 40 Dexterity: 92
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	12
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Flame Dash 1 week ago
Level: 10 Intelligence: 29
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	1
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Animate Guardian 1 week ago
Level: 28 Strength: 42 Intelligence: 29
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Magma Orb 1 week ago
Level: 11 Intelligence: 31
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	5
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Summon Raging Spirit 3 weeks ago
Level: 4 Intelligence: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Flicker Strike 1 week ago
Level: 10 Dexterity: 29
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Lightning Tendrils 1 week ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	1
1× online IGN: HoLeeSheets  Profile Whisper
 Item icon
Frost Bomb an hour ago
Level: 4 Intelligence: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
10			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Assassin's Mark an hour ago
Level: 24 Dexterity: 25 Intelligence: 37
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Sunder an hour ago
Level: 12 Strength: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Viper Strike an hour ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Iron Grip Support 40 minutes ago
Level: 18 Strength: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Lightning Tendrils an hour ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Static Strike 40 minutes ago
Level: 12 Strength: 21 Intelligence: 14
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: DingDongScallywag  Profile Whisper
 Item icon
Essence Drain 3 weeks ago
Level: 12 Dexterity: 14 Intelligence: 21
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Wild Strike 3 weeks ago
Level: 28 Strength: 29 Dexterity: 42
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Ice Trap 3 weeks ago
Level: 28 Dexterity: 42 Intelligence: 29
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Frost Blades 3 weeks ago
Level: 1
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Vulnerability 3 weeks ago
Level: 24 Intelligence: 58
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Animate Guardian 3 weeks ago
Level: 28 Strength: 42 Intelligence: 29
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Freeze Mine 3 weeks ago
Level: 10 Dexterity: 18
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Flameblast 3 weeks ago
Level: 28 Intelligence: 67
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	5	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Vengeance 3 weeks ago
Level: 24 Strength: 58
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Raise Spectre 3 weeks ago
Level: 28 Intelligence: 67
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Hypothermia Support 3 weeks ago
Level: 34 Dexterity: 57
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	2
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Additional Accuracy Support 3 weeks ago
Level: 8
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Reckoning 3 weeks ago
Level: 4 Strength: 16
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Reave 3 weeks ago
Level: 12 Dexterity: 21 Intelligence: 14
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	 	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper
 Item icon
Storm Call 3 weeks ago
Level: 12 Intelligence: 33
Quality	Phys.	Elem.	APS	DPS	pDPS	eDPS
0			 	 	 	 
Armour	Evasion	Shield	Block	Crit.	Level
 	 	 	 	6	1
fixed price: 1× online IGN: Rennisa_Reverof  Profile Whisper"""
    ]
let t1 = tests |> List.rev
text::t1
|> List.map parseItemContainer