<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

// trim poe.trade clipboard
let sample = """
Victory Mantle Sacrificial Garb 1 week ago
Level: 72 Strength: 66 Dexterity: 66 Intelligence: 66 ilvl: 100 Max sockets: 6 (6)
+39 to Dexterity S3
+11 to Intelligence S9
97% increased Armour, Evasion and Energy Shield Tier 2 prefix: Evanescent, min=[83] max=[100]
+87 to maximum Energy Shield P3
crafted+23% to Cold Resistance"""

let trim =
    function | null | "" -> null | x -> x.Trim()
let rRemove p x =
    Regex.Replace(x,pattern=p,replacement="")
    |> trim
let getClip() = 
    let txt = System.Windows.Forms.Clipboard.GetText()
    txt.Dump(sprintf "retrieved %s" txt)
    txt
let splitLines=
    function
    | null | "" -> null
    | x -> x.Split([| "\r\n";"\n";"\r" |], StringSplitOptions.RemoveEmptyEntries)
let setClip x = System.Windows.Forms.Clipboard.SetText(x)
let clean =
    function
    | null | "" -> null
    | x -> 
        x
        |> LINQPad.Extensions.Dump
        |> fun x -> printfn ""; x
        |> splitLines
        |> Seq.map trim
        |> Seq.map (rRemove @"(P|S|\?)(\d|\?)")
        |> Seq.map (rRemove @"Tier \d .*$")
        |> Seq.map (rRemove "\d \w+ ago$")
        |> Seq.map (rRemove @"^crafted")
        |> Seq.filter(String.IsNullOrWhiteSpace>>not)
        |> delimit "\r\n"
let loop() =
    while true do
        let txt = getClip()
        ()
        
        

clean sample    