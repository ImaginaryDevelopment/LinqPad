<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

module Helpers =
    let getText() = Util.Cache(fun () -> System.Windows.Forms.Clipboard.GetText())
    let (|StartsWith|_|) d =
        function
        | null|"" -> None
        | x when x.StartsWith d -> Some x
        | _ -> None
    let (|RMatch|_|) p x =
        match p with | null |"" -> invalidOp "bad pattern" | _ -> ()
        let m = Regex.Match(x,p)
        if m.Success then Some m
        else None
    let (|RMatchGroup|_|) (i:int) p =
        function
        | null | "" -> None
        | RMatch p m ->
            Some m.Groups.[i].Value
        | _ -> None
        

open Helpers
let getpDps minP maxP speed =
    let pDps = (minP + maxP) / 2m * 1.3m
    pDps//, pDps * 0.2m
type Weapon = {Name:string; MinP:decimal;MaxP:decimal;Speed:decimal; IncChaos:decimal} with
    member this.PDps =
        if this.Speed < 0.1m || this.Speed > 4.0m then invalidOp "bad speed"
        getpDps this.MinP this.MaxP this.Speed
    member this.PoiDps =
        let basePoi = this.PDps * 0.2m
        basePoi + basePoi*this.IncChaos
    static member Zero = {Name=null;MinP=0m;MaxP=0m;Speed=0m;IncChaos=0m}
let weapon = 
    let text = getText()
    text.Dump("weapon")
    text
    |> Seq.unfold(
        function
        | null | "" -> None
        | x ->
            match x.IndexOf("\r\n") with
            | i when i < 0 -> Some(x,null)
            | i -> Some (x.Substring(0,i),x.Substring(i+2))
            
    )
    |> Seq.fold(fun w line ->
        match line with
        | RMatch @"Physical Damage: (\d+)-(\d+)" m ->
            m.Dump("phy")
            (m.Value, m.Groups.[1].Value).Dump()
            {w with MinP=decimal m.Groups.[1].Value;MaxP=decimal m.Groups.[2].Value}
        | RMatchGroup 1 @"Attacks per Second: ((\d|\.)+)" spd ->
            printfn "Found speed:%s" spd
            {w with Speed=decimal spd}
        | StartsWith "Rarity:" _ -> w
        | name when String.IsNullOrEmpty w.Name -> {w with Name=name}
        | _ -> w
            
    ) Weapon.Zero

    
       
    
weapon.Dump()
printfn "%A" weapon
    
[   {Name="Lakishu";MinP=49m;MaxP=77m;Speed=1.69m; IncChaos=0.0m}   
    {Name="Dusk Hunger";MinP=68m;MaxP=128m;Speed= 1.3m; IncChaos=0.0m}

    {Name="Plague Fang";MinP=21m;MaxP=85m;Speed= 1.55m;IncChaos=0.17m}
    {Name="Innsbury Edge";MinP=90M;MaxP=164M;Speed=1.57M;IncChaos=0M}
]

|> Dump
|> ignore