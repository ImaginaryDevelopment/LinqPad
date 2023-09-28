<Query Kind="FSharpProgram" />

type STClass =
    | Berserker
    | Dancer
    | Geomancer
    | Knight
    | Monk
    | Musketeer
    | Ninja
    | Ranger
    | Samurai
    | Spellblade
    | Thief
    | Wanderer
    with
        member x.ToDump() = sprintf "%A" x
type Track = {
    Name:string
    Type: STClass
    Level: int
    Power: int
} with
    member x.Eff = float x.Power / float x.Level
let owned = [
    "A Blinken", Ranger, 23, 6616
    "Erik the Red", Wanderer, 23, 4824
    "Sarah", Knight, 24, 6383
    "Ister Gray", Ranger, 20, 5001
    "Jill", Monk, 23, 4560
    "Michonne", Samurai, 18, 4474
    "Iris", Ninja, 17, 3510
    "Dp Roberts", Thief, 19, 3994
    "Jenna", Ninja, 15, 3396
    "Beefcake", Samurai, 15, 3302
    "Nayla", Musketeer, 24, 8243
    "Celeste", Wanderer, 24, 5619
    "Norris", Spellblade, 17, 3637
]
let slots = 15
let recommended = [
    [Thief;Musketeer;Ranger;Wanderer; Samurai; Samurai; Spellblade; Ninja]
    [Musketeer; Wanderer; Spellblade; Samurai; Samurai; Geomancer; Geomancer; Ninja;Ninja;Ninja;Ninja;Dancer;Dancer;Berserker]
]
owned
|> Seq.map(fun (n,t,l,p) -> {Name=n;Type=t;Level=l;Power=p})
|> Seq.sortBy(fun x -> x.Eff)
|> Dump
|> ignore
recommended
|> Seq.map(List.sort)
|> Dump
|> ignore