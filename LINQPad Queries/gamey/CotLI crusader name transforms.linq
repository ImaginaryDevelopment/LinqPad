<Query Kind="FSharpProgram">
  <NuGetReference>Inflector</NuGetReference>
  <Namespace>Inflector</Namespace>
</Query>


let text = """The Bush Whacker
RoboRabbit
Warwick The Warlock
Jim the Lumberjack
Pilot Pam
Veronica, the Android Archer
Emo Werewolf
Sally the Succubus
Karen, the Cat Teenager
Sasha the Fierce Warrior
Groklok the Orc
Mindi the Mimic
The Washed Up Hermit
Kyle the Party - Bro
Serpent King Draco
Henry, the Scaredy - Ghoul
Detective Kaine
Mister the Monkey
Larry the Leprechaun
Bernard the Bartender
The Princess
RoboTurkey
Ranger Rayna
Natalie Dragon
Jack O'Lantern
President Billy Smithsonian
Karl the Kicker
Jason, Master of Shadows
Pete the Carney
Broot
Paul the Pilgrim
Artaxes, the Lion
Drizzle the Dark Elf
Bubba, the Swimming Orc
Khouri, the Witch Doctor
Momma Kaine
Brogon, Prince of Dragons
Half - Blood Elf
  Dark Gryphon
  Rocky the Rockstar
Montana James
The Dark Helper
Sarah, the Collector
The Metal Soldierette
Gold Panda
RoboSanta
Leerion, the Royal Dwarf
Katie the Cupid
Prince Sal, the Merman
Wendy the Witch
Robbie Raccoon
Princess Val the Mermaid
Fire Phoenix
Alan the ArchAngel
Fright - o - Tron 4000
King Reginald IV
Queen Siri
Mr.Boggins, the Substitute
Thalia, the Thunder King
Frosty the Snowman
Littlefoot
Cindy the Cheer Orc
Merci, the Mad Wizard
The Bat Billionaire
Petra the Pilgrim
Nate Dragon
Kizlblyp the Alien Traitor
RoboRudolph
The Exterminator
Gloria, the Good Witch
The Shadow Queen
Ilsa, the Insane Wizard
Greyskull the Pirate
Eiralon, the Blood Mage
Priestess of Time"""
let replacements =
    [   ", ";" - ";" "; ",";"-";"'"]
        
        
text.SplitLines()
|> Seq.map (fun x -> x.Trim())
|> Seq.map (fun x -> x, replacements |> Seq.fold (fun (x:string) r -> x.Replace(r,"_")) x |> Inflector.Pascalize) //x.Replace(", ","_").Replace(" - ", "_").Replace(" ","_").Replace(",","_").Replace("-","_")
|> Dump
|> Seq.map snd
|> delimit "\r\n|"
|> Dump
|> ignore