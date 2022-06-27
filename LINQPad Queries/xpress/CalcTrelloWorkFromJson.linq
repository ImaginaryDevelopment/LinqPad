<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>TextCopy</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// the other doesn't account for overlaps, we need to format data so the normal calc hours with overlap detection works
let key = Util.GetPassword("trelloapikey")
let hc = new System.Net.Http.HttpClient()
let cacheIt (key:string) (f:Func<_>) =
    Util.Cache(f,key)
let getTotalMinutes (startdt:DateTime) (stop:DateTime) =
    (stop - startdt).TotalMinutes
let toDt hr mn =
    DateTime.Today.AddHours(float hr).AddMinutes(float mn)
let replace (d:string) r (x:string) =
    x.Replace(d,r)
let keyBy f items =
    items
    |> Seq.map f
    |> Seq.groupBy fst
    |> Seq.map(fun (k,v) -> k, v |> Seq.map snd)
    |> Map.ofSeq
    
let storeClipboard key =
    cacheIt key (Func<_>(fun _ -> 
        let c = TextCopy.Clipboard()
        c.GetText()
    ))
let inline isNotNull x =
    not <| isNull x
let eqI (x:string) (y:string) = 
    (isNull x && isNull y) || (isNotNull x && x.Equals(y,StringComparison.InvariantCultureIgnoreCase))
let (|EqI|_|) d x = if eqI d x then Some() else None
let after (d:string) (x:string) =
    match x.IndexOf d with
    | x when x < 0 -> failwithf "Delimiter '%s' not found" d
    | i -> x.[i+d.Length..]
let before (d:string) (x:string) =
    match x.IndexOf d with
    | x when x < 0 -> failwithf "Delimiter '%s' not found" d
    | i -> x.[0..i]
    
let fetchString (url:string) = 
    hc.GetStringAsync(url)
    |> Async.AwaitTask
    |> Async.RunSynchronously

//let empty = System.String.Empty
//hc.GetStringAsync(sprintf "https://api.trello.com/1/boards/4d5ea62fd76aa1136000000c?key=%s" key)

// this doesn't seem to work via api, perhaps we have to do this step manually by just adding.json to the url
let getBoardLongId url =
    //let boardLocator = url |> after "b/"
    //let shortId = boardLocator |> before "/"
    sprintf "%s.json?key=%s" url key
    |> fetchString
       
// not authorized for our boards, so this approach failed        
//getBoard "6088cbd6ff4e922c080a02bb" Map.empty
//getBoardLongId "https://trello.com/b/GqlTEO3O/edc-phase-13"
let getBoard boardId m =
    let authority = "https://api.trello.com"
    let query = sprintf "/1/boards/%s" boardId
    let qparams = 
        m
        |> Map.add "key" key
        |> Map.toSeq
        |> Seq.map(fun (k,v) -> sprintf "%s=%s" k (System.Uri.EscapeDataString(v)))
        |> String.concat "&"
    
    hc.GetStringAsync(sprintf "%s%s?%s" authority query qparams)
    |> Async.AwaitTask
    |> Async.RunSynchronously
    
let getSample() =
    getBoard "4d5ea62fd76aa1136000000c" Map.empty
    
//getSample()


// wound up just using the raw response from adding .json to the board url into the clipboard
let raw = storeClipboard "trelloresponse"    
module Cereal = 
    open Newtonsoft.Json
    open FSharp.Reflection
    type JsonConverter = Newtonsoft.Json.JsonConverter
    type JsonConverterS = Json.Serialization.JsonConverter
    type OptionConverter() =
        inherit Newtonsoft.Json.JsonConverter()
        override x.CanConvert t =
            t.IsGenericType && typedefof<option<_>>.Equals (t.GetGenericTypeDefinition())
        override x.WriteJson(writer,value,serializer) =
            printfn "WritingJson option?"
            let value =
                if isNull value then
                    null
                else 
                    let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                    fields.[0]
            serializer.Serialize(writer, value)
        override x.ReadJson(reader,t,_existingValue,serializer) =
            //printfn "ReadingJson option?"
            let innerType = t.GetGenericArguments().[0]
            let innerType =
                if innerType.IsValueType then
                    typedefof<Nullable<_>>.MakeGenericType(Array.singleton innerType)
                else innerType
            let value = serializer.Deserialize(reader, innerType)
            let cases = FSharpType.GetUnionCases t

            if value = null then
              FSharpValue.MakeUnion(cases.[0], Array.empty)
            else
              FSharpValue.MakeUnion(cases.[1], Array.singleton value)
                
    let deserialize<'t>(raw) =
        let settings = Newtonsoft.Json.JsonSerializerSettings()
        settings.Converters.Add(OptionConverter())
        JsonConvert.DeserializeObject<'t>(raw,settings)
        
type TrelloCard = {
    id:string
    dateLastActivity:string
    name:string
    idList:string
}
type TrelloMCreator = {
    id:string
    username:string
    initials:string
}
type TrelloActionDCard = {
    idList:string
    id:string
    name:string
    idShort:int
    shortLink:string
}
type ListAction = {id:string;name:string}
type ListDetail = {id:string;name:string;pos:int}
type TrelloActionData = {
    old:obj
    card:TrelloActionDCard
    listBefore:ListAction option
    listAfter:ListAction option
    list:ListAction option
}
type TrelloAction = {
    id:string
    idMemberCreator:string
    data:TrelloActionData
    ``type``:string
    date:string
    memberCreator:TrelloMCreator
}
type TrelloJson = {
    id:string
    name:string
    cards:TrelloCard[]
    actions:TrelloAction[]
    lists: ListDetail[]
}
let viewRaw() =
    let parsed = JObject.Parse(raw)
    parsed.Dump()
    
let getParsed() =
    //Json.JsonSerializer.Deserialize<TrelloJson>(raw)
    Cereal.deserialize<TrelloJson>(raw)
let parsed = getParsed()
let listNameMap = parsed.lists |> Seq.map(fun l -> l.name,l.id) |> Map.ofSeq
let listIdMap = parsed.lists |> Seq.map(fun l -> l.id, l.name) |> Map.ofSeq
(
    parsed.cards
    |> Seq.map(fun c ->
        c.name,c.id, //if Map.containsKey c.idList listMap then listMap.[c.idList] else c.idList
            listIdMap.[c.idList]
    )
    |> List.ofSeq
    |> fun x -> x.Dump("cards")
)

type CardMove = {FromList:ListAction;ToList:ListAction}
type CardChangeAct = 
    | Created of list:string
    | Moved of CardMove
type CardDisplayDetail = {
    Dt:DateTime;Act:CardChangeAct;RawDt:string
}
type CardFocusInfo = { Name:string;Detail:CardDisplayDetail}
let (|CreationCard|_|) act =
    if act.``type`` = "createCard" then
        match act.data.list with
        | None -> None
        | Some l -> Some l
    else None
let (|MoveCard|_|) act =
    match act.data.listBefore,act.data.listAfter with
    | Some lb, Some la  -> Some {FromList=lb;ToList=la}
    | _ -> None
let hasDoing {FromList=lb;ToList=la}=
    eqI lb.name "doing" || eqI la.name "doing"
    
let filterMoves f =
    function
    |{Act=Moved m} -> f m
    | _ -> true
let inDoing list = 
    eqI "doing" list
let filterCreates f =
    function
    |{Act=Created l} -> f l
    | _ -> true
    
let unpaidId = listNameMap.["Unpaid+Done"]
//let excludingCards = parsed.cards |> Seq.filter(fun c -> c.idList <> paidId)
let includingCards = parsed.cards |> Seq.filter(fun c -> c.idList = unpaidId)

let focusedMap =
    parsed
    |> fun p -> p.actions    
    |> Seq.choose(fun act -> //Option.isSome act.data.listBefore 
        let createDetail (cca:CardChangeAct) =
            let cdd ={Dt=DateTime.Parse act.date;Act=cca;RawDt=act.date}
            Some {Name=act.data.card.name;Detail=cdd}
        match act with    
        | MoveCard m -> Moved m |> createDetail 
        | CreationCard l ->
            Created l.name
            |> createDetail
        | _ -> None
        |> function
            | None ->
                //try
                //    (act.data.card.name,act.data)
                //    |> ignore //.Dump("?");
                    None
                //with _ -> act.Dump("null ref?")None
            | Some x -> Some x
    )
    |> Seq.filter(fun x ->
        includingCards |> Seq.exists(fun ec -> eqI ec.name x.Name)
    )
    |> Seq.filter(fun x -> filterMoves hasDoing x.Detail)
    |> Seq.filter(fun x -> filterCreates inDoing x.Detail)
    |> keyBy(fun x ->
        x.Name,x.Detail
    )
    |> Map.map(fun _ details ->
        details
        |> Seq.filter(filterMoves hasDoing)
        |> Seq.filter(filterCreates inDoing)
        |> Seq.sortBy(fun a -> a.Dt)
        |> List.ofSeq
        //filtered 
    )
// track when it was last moved to doing
    
type CardTotalState = {InDoingSince:DateTime option;Work:(DateTime*DateTime) list}
type CardSummary =
    | Valid of minutes:int
    | Invalid of CardDisplayDetail
type MapSummaryState = Result<CardTotalState,string>     
let foldDetail (state:MapSummaryState) (detail:CardDisplayDetail) =
    state
    |> Result.bind(fun cts ->
        let (|FromDoing|ToDoing|Other|) =
            function
            | {FromList={name=EqI "doing"}} -> FromDoing
            | {ToList={name=EqI "doing"}} -> ToDoing
            | _ -> Other
            
        match detail.Act, cts.InDoingSince with
        // not created in doing, we don't care
        | Created _, Some _ -> Error "Created when already in doing"
        | Moved ToDoing, Some _ -> Error "Moved to doing when already there"
        | Moved FromDoing, None -> Error "Moved from doing without previous event"
        
        | Moved ToDoing, None// -> Ok {cts with InDoing = Some detail.Dt}
        | Created (EqI "doing"), None -> Ok {cts with InDoingSince = Some detail.Dt}
        | Created _, None -> Ok cts
        //| Moved FromDoing, Some dt -> Ok {InDoingSince = None;totalMinutes = cts.totalMinutes + int (detail.Dt - dt).TotalMinutes}
        | Moved FromDoing, Some dt -> Ok {InDoingSince = None;Work= (dt,detail.Dt)::cts.Work }
        | _ -> failwithf "What are we doing here? %A,%A" detail.Act cts.InDoingSince
    )
    
(Map.empty,focusedMap)
||> Map.fold(fun (state:Map<string,MapSummaryState>) name details ->
    let casOpt = state |> Map.tryFind name
    let result =
        (casOpt|> Option.defaultValue (Ok {InDoingSince=None;Work=List.empty}),details)
        ||> List.fold foldDetail
    state
    |> Map.add name result
)
|> Map.map(fun _k v ->
    v
    |> Result.bind(fun v ->
        match v.InDoingSince with
        | None -> Ok v
        | Some _ -> Error "Last state was in doing"
    )
    |> Result.map(fun x -> x.Work)
)
// manual additions
|> Map.map(fun k v ->
    match k with
    | EqI "Unbreaking the breaking change" ->
        let st = toDt 9 15
        let stp = toDt 12 15
        v
        |> Result.map(fun v -> (st,stp)::v)
    | _ -> v
)
|> Map.map(fun _k v ->
    match v with
    | Ok v -> v |> List.map(fun (dt1,dt2) -> sprintf "\"%A\",\"%A\"" dt1 dt2 ) |> Ok
    | Error e -> Error e
)
|> Map.toSeq
|> Seq.map(fun (k,v) ->
    let k = k |> replace "\"" "\\\""
    match v with
    | Ok values ->
        values
        |> Seq.map(fun v -> sprintf "%s,\"%s\"" v k)
        |> String.concat "\r\n"
    | Error e -> e
)
|> String.concat "\r\n"
|> fun r -> r.Dump("focusmap")
|> ignore

viewRaw()
|> Dump
|> ignore
