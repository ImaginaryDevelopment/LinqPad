<Query Kind="FSharpProgram" />


let inline ratio (min:int) act (max:int) = 
    if float min > float max then failwithf "min was greater than max"
    if float act > float max  then failwithf "actual was greater than max"
    if float min > float act then failwithf "actual was less than min"
    float(act) / (float(min + max) / 2.)

let oldmyGauntDmg: float = ratio 833 932 1042
let myGauntRat : float =
    let dmg = ratio 833 1030 1042
    let baseBoss = ratio 2 5 5
    Seq.average [| dmg; baseBoss|]
let myArmorRat: float = ratio 450 591 643

let doomMantle bossDmg ingeniousDef eldritch ignoreDmg : float = 
    [bossDmg;ingeniousDef;eldritch;ignoreDmg] |> Seq.average
    
let myMantleRat : float= 
    let dmg: float = ratio 225 316 418
    let dmgIgnore: float = ratio 2 3.4 5
    doomMantle dmg (ratio 5 7 7) (ratio 3 4 7) dmgIgnore
let myMantleRat2 = 
    doomMantle (ratio 225 317 418) (ratio 3 5 7) (ratio 3 7 7) (ratio 2 2.1 5)

(myMantleRat,myMantleRat2).Dump()
let myBoardDmg = ratio 450 619 643
let myPortraitDmg = ratio 431 542 616 |> Some

type GearRatio = {Slot1:float option; Slot2:float option;Slot3:float option;Slot4:float option;Slot5:float option} 
    with 
        member x.Seq = [ x.Slot1; x.Slot2; x.Slot3; x.Slot4; x.Slot5 ]
        member x.Overall() = Seq.choose id

let DoomDmg = {Slot1=Some myGauntRat; Slot2 = Some myArmorRat; Slot3 = Some myMantleRat; Slot4= Some myBoardDmg; Slot5= myPortraitDmg}

DoomDmg.Dump("doom")

let myRatio = Seq.average [| myGauntRat ;myArmorRat; myMantleRat; myBoardDmg |]
myRatio.Dump("overall")

