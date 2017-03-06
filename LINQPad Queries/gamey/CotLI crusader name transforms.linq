<Query Kind="FSharpProgram">
  <NuGetReference>Inflector</NuGetReference>
  <Namespace>Inflector</Namespace>
</Query>


let text = """01	The Bush Whacker
01a	RoboRabbit
01b	Warwick The Warlock
02	Jim the Lumberjack
02a	Pilot Pam
02b	Veronica, the Android Archer
03	Emo Werewolf
03a	Sally the Succubus
03b	Karen, the Cat Teenager
04	Sasha the Fierce Warrior
04a	Groklok the Orc
04b	Mindi the Mimic
05	The Washed Up Hermit
05a	Kyle the Party-Bro
05b	Serpent King Draco
05c	Henry, the Scaredy-Ghoul
06	Detective Kaine
06a	Mister the Monkey
06b	Larry the Leprechaun
06c	Bernard the Bartender
07	The Princess
07a	RoboTurkey
07b	Ranger Rayna
08	Natalie Dragon
08a	Jack O'Lantern
08b	President Billy Smithsonian
08c	Karl the Kicker
09	Jason, Master of Shadows
09a	Pete the Carney
09b	Broot
09c	Paul the Pilgrim
10	Artaxes, the Lion
10a	Drizzle the Dark Elf
10b	Bubba, the Swimming Orc
11	Khouri, the Witch Doctor
11a	Momma Kaine
11b	Brogon, Prince of Dragons
11c	Half-Blood Elf
12	Dark Gryphon
12a	Rocky the Rockstar
12b	Montana James
12c	The Dark Helper
13	Sarah, the Collector
13a	The Metal Soldierette
14	Gold Panda
14a	RoboSanta
14b	Leerion, the Royal Dwarf
14c	Katie the Cupid
15	Prince Sal, the Merman
15a	Wendy the Witch
15b	Robbie Raccoon
15c	Princess Val the Mermaid
16	Fire Phoenix
16a	Alan the ArchAngel
16b	Fright-o-Tron 4000
17	King Reginald IV
17a	Queen Siri
17b	Mr. Boggins, the Substitute
18	Thalia, the Thunder King
18a	Frosty the Snowman
18b	Littlefoot
18c	Cindy the Cheer Orc
19	Merci, the Mad Wizard
19a	The Bat Billionaire
19b	Petra the Pilgrim
20	Nate Dragon
20a	Kizlblyp the Alien Traitor
20b	RoboRudolph
21	The Exterminator
21a	Gloria, the Good Witch
22	The Shadow Queen
22a	Ilsa, the Insane Wizard
23	Greyskull the Pirate
23a	Eiralon, the Blood Mage
24	Priestess of Time"""
let replacements =
    [   ", "
        " - "
        " "
        ","
        "-"
        "'"
        "."]
        
let crusaders =         
    text.SplitLines()
    |> Seq.map (fun x -> x.Trim()) 
    |> Seq.map (fun x -> Regex.Split(x,@"\s+") |> Seq.map (fun x -> x.Trim()) |> List.ofSeq |> fun l -> l.[0], l.[1..] |> delimit " ")
    |> Seq.map (fun (slot,x) -> x,slot, replacements |> Seq.fold (fun (x:string) r -> x.Replace(r,"_")) x |> Inflector.Pascalize) //x.Replace(", ","_").Replace(" - ", "_").Replace(" ","_").Replace(",","_").Replace("-","_")
    |> Dump

let (|IsInt|_|) (x:string) = match Int32.TryParse x with | true, x -> Some x |_ -> None
let slotMap= 
    function
    |IsInt x when x < 21 -> x
    // Karen
    |"03b" -> 53
    // Draco
    |"05b" -> 46
    // Henry
    |"05c" -> 64
    // Mister
    | "06a" -> 21
    // Rayna
    |"07b" -> 49
    // Pete
    | "09a" -> 22
    // Momma Kaine
    |"11a" -> 26
    // Brogon
    |"11b" -> 44
    |"13a" -> 71
    // Wendy
    |"15a" -> 23
    // Robbie
    |"15b" -> 42
    // Boggins
    |"17b" -> 54
    // Frosty
    |"18a" -> 28
    //Littlefoot
    |"18b" -> 50
    //Petra
    |"19b" -> 68
    //Kiz
    |"20a" -> 48
    // Exterminator
    | "21" -> 55
    // Gloria
    | "22" -> 56
    | _ -> 0
// for forums
//crusaders
//|> Seq.choose(fun (_,slot,pas) -> match slot |> slotMap with | 0 -> Some (pas,None) | x when x > 20 -> Some (pas,Some x) | _ -> None)
//|> Seq.map (fun (pas,slotOpt) -> sprintf "%s:%s" pas (slotOpt |> Option.map string |> Option.getOrDefault "?"))
//|> delimit ","
//|> Dump
//|> ignore
crusaders
|> Seq.map (fun (_,slot,pascalized) -> sprintf """    {Slot="%s";Id=%i;Name="%s"}""" slot (slot |> slotMap) pascalized)
|> delimit "\r\n"
|> (+) "let crusaders = [\r\n"
|> flip (+) "  ]"
|> Dump
|> ignore