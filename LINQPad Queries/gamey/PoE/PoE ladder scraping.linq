<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Numerics.dll</Reference>
  <NuGetReference>FSharp.Control.AsyncSeq</NuGetReference>
  <Namespace>FSharp.Control</Namespace>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
  <Namespace>System.Net.Http</Namespace>
  <Namespace>System.Threading.Tasks</Namespace>
</Query>

// read the ladder, look for stuff
// unfold isn't working the way it was intended (bringing abyss jewels up as items)

let deserialize<'t> x = Newtonsoft.Json.JsonConvert.DeserializeObject<'t> x
let tryf f x =
    try
        f x
        |> Some
    with _ -> None
module Seq =
    let inline ofType<'t>(x:_ seq) : 't seq=
        x
        |> Seq.choose(box >> function | :? 't as x -> Some x | _ -> None)
module Option =
    let ofObj =
        function
        | null -> None
        | x -> Some x
    let inline defaultValue dv x =
        match x with | None -> dv | Some x -> x
        
module JObject =
    let prop (name:string) (x:JObject) = x.Property(name) |> Option.ofObj |> Option.bind(fun x -> x.Value |> Option.ofObj)
    let propbool name x: bool option=
        prop name x
        |> Option.map(fun v -> v.Value<bool>())
        

module Async =
    let map f x =
        async {
            let! x' = x
            return f x'
        }
    let bind f x =
        async{
            let! x' = x
            return! f x'
        }
        
let isValueString =
    function
    | null | "" -> false
    | x when String.IsNullOrWhiteSpace x -> false
    | _ -> true

let cl = new HttpClient()

cl.DefaultRequestHeaders.Accept.Clear()
cl.DefaultRequestHeaders.Accept.Add <| Headers.MediaTypeWithQualityHeaderValue("application/json")

module Ladders =
    type LadderArgs = {Name:string;Realm:string;Limit:int option;Offset:int option;Type:string;AccountName:string}
    let makeQuery x = // https://www.pathofexile.com/developer/docs/api-resource-leagues
        let uri =
            [
                if isValueString x.Realm then
                    yield sprintf "realm=%s" x.Realm
            ]
            |> String.concat "&"
            |> sprintf "http://api.pathofexile.com/ladders/%s?%s" x.Name
//        uri.Dump("uri")
        uri
    type LadderAccount = {Name:string}
    type LadderCharacter = {Name:string;Level:int;Class:string}
    type LadderEntry = {Account:LadderAccount;Character:LadderCharacter}
    type Ladder = {Total:int;Entries:LadderEntry list}
    let getMetamorph () =
        {   Name="metamorph"
            Realm=null
            Limit=Some 3
            Offset=None
            Type=null
            AccountName=null
        }
        |> makeQuery 
        |> cl.GetStringAsync
        |> Async.AwaitTask
    let mapMetaMorph x =
        async{
        
            let! data = x
//            data.Dump("Raw")
            return deserialize<Ladder> data
        }
        
module CharacterWindow =
    type CharacterIdent = {AccountName:string;Character:string}
    module Items =
        let makeQuery x = // https://app.swaggerhub.com/apis-docs/Chuanhsing/poe/1.0.0#/default/get_character_window_get_items
            let uri =
                [
                    sprintf "accountName=%s" x.AccountName
                    sprintf "character=%s" x.Character
                ]
                |> String.concat "&"
                |> sprintf "https://www.pathofexile.com/character-window/get-items?%s"
            uri.Dump("items uri")
            uri
        type AccountCharacter = {Name:string;League:string;ClassId:int;AscendancyClass:int;Class:string;Level:int;Experience:int64}
        type ItemResponse<'t> = {Items:'t list; Character:AccountCharacter}
        type Item (raw:Newtonsoft.Json.Linq.JObject) =
            let getProp x =
                JObject.prop x raw
            let getString x = 
                getProp x
                |> Option.map(fun v -> v.ToString())
                |> Option.defaultValue null
            let getStringArray x =
                getProp x
                |> Option.map(fun v -> v.ToArray() |> Array.map string)
            let getObjArray x =
                getProp x
                |> Option.map(fun v -> v.ToArray())
            let getBool x =
                getProp x
                |> Option.map(fun v -> v.Value<bool>())
                |> Option.defaultValue false
            let getInt x =
                getProp x
                |> Option.map(fun v -> v.Value<int>())
                |> Option.defaultValue -1
            member x.Name = getString "name"
            member x.Type = getString "typeLine"
            member x.InventoryId = getString "inventoryId"
            member x.Corrupted = getBool "corrupted"
            member x.icon = Util.Image(getString "icon")
            member x.FrameType = getInt "frameType"
            member x.w = getString "w"
            member x.h = getString "h"
            member x.Raw = Util.OnDemand("Raw",fun () -> string raw)
                
            member private x.EnchantMods = getStringArray "enchantMods" |> Option.defaultValue Array.empty
            member private x.ImplicitMods = getStringArray "implicitMods" |> Option.defaultValue Array.empty
            member private x.ExplicitMods = getStringArray "explicitMods" |> Option.defaultValue Array.empty
            member private x.Sockets = getObjArray "sockets" |> Option.defaultValue Array.empty
            member private __.RawSocketedItems = getObjArray "socketedItems" |> Option.defaultValue Array.empty |> Seq.cast<JObject> |> Seq.map (JObject.prop "typeLine") |> Seq.choose id
            member x.SocketedItems =  x.RawSocketedItems |> Seq.map string |> Array.ofSeq
            member x.Unfold() =
                try
                    x.RawSocketedItems
                    |> Seq.ofType<JObject>
                    |> Seq.filter(JObject.propbool "abyssJewel" >> Option.defaultValue false)
                    |> List.ofSeq
                    |> Some
                with ex ->
                    x.RawSocketedItems.Dump("Failure")
                    None
                
                    
            member x.Text = 
                let enc = x.EnchantMods
                let imp = x.ImplicitMods
                let exp = x.ExplicitMods
                [
                    if enc.Length > 0 then
                        yield! enc
                        yield String.replicate (enc |> Seq.map(String.length) |> Seq.max) "-"
                    if imp.Length > 0 then
                        yield! imp
                        yield String.replicate (imp |> Seq.map(String.length) |> Seq.max) "-"
                    yield! exp
                ]
                |> String.concat "\r\n"
            
        [<NoComparison>]
        type GetItemResponse = {Account:string;Entry:AccountCharacter;Items: Item list; RawItems: JObject list}    
        let unfoldItems =
            List.collect(fun (x:Item) ->
                [
                    yield x
                    yield! x.Unfold() |> Option.defaultValue List.empty |> Seq.choose (tryf Item) |> List.ofSeq
                ]
            )
            
        let getItems x : Async<Choice<GetItemResponse,_>> =
            let formatErr (x:CharacterIdent) typeTxt msg = sprintf "%s(%s): %s: %s" x.Character x.AccountName typeTxt msg
            async{
                try
                    do! Async.Sleep 100
                    let! value =
                        makeQuery x
                        |> cl.GetStringAsync
                        |> Async.AwaitTask
//                    value.Dump("got items?")
                    try
                        let ir = deserialize<ItemResponse<JObject>> value
                        if ir.Items.Length > 0 then
                            return  Choice1Of2 {Account= x.AccountName;Entry= ir.Character;Items=ir.Items |> List.map(Item) |> unfoldItems;RawItems= ir.Items}
                        else
                            let text = formatErr x "Empty" "No Items Found"
                            return Choice2Of2 text
//                            (ir.Items.[0] |> string).Dump("an item")
                            
                    with ex ->
                        let text = formatErr x "JError" ex.Message
                        return Choice2Of2 text
                with ex ->
                    let text = formatErr x "Error" ex.Message
                    return Choice2Of2 text
            }
        
Ladders.getMetamorph()
|> Ladders.mapMetaMorph
|> Async.RunSynchronously
|> fun x ->
    ([],x.Entries)
    ||> Seq.fold(fun results x ->
        let asynctask =
            CharacterWindow.Items.getItems {AccountName=x.Account.Name;Character=x.Character.Name}
//            |> Async.map(fun itemRaw -> x,itemRaw)
        asynctask :: results
    )
    |> fun x -> x
    |> AsyncSeq.ofSeqAsync
    |> AsyncSeq.choose(function | Choice2Of2 _ -> None | Choice1Of2 b -> Some b)
    |> AsyncSeq.filter(fun x -> x.Items.Length > 0)
    |> AsyncSeq.truncate 5
    |> AsyncSeq.toListAsync
    |> Async.RunSynchronously
|> fun x -> x
|> Dump
|> ignore