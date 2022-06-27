<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>TextCopy</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

[<Measure>] type Min // minutes
let key = Util.GetPassword("trelloapikey")
let hc = new System.Net.Http.HttpClient()

// doing it this way so the name is declared up top and is searchable in the code
let cacheIt (key:string) (f:Func<_>) =
    Util.Cache(f,key)
let inline isNotNull x =
    not <| isNull x
let eqI (x:string) (y:string) = 
    (isNull x && isNull y) || (isNotNull x && x.Equals(y,StringComparison.InvariantCultureIgnoreCase))
let (|EqI|_|) d x = if eqI d x then Some() else None
let toDt hr mn =
    DateTime.Today.AddHours(float hr).AddMinutes(float mn)
let getTotalMinutes (startdt:DateTime) (stop:DateTime) =
    (stop - startdt).TotalMinutes
type DateTimeType =
    | Overridden
    | Regular
    
type DateTimeSpan= {Started:DateTime;Finished:DateTime;DateTimeType:DateTimeType}
let manualAdditions k (v:Result<int<Min>*DateTimeSpan list,_>):Result<int<Min>*DateTimeSpan list,_> =
        match k with
        | EqI "Troubleshooting and adding Diagnostics" ->
            match v with
            | Ok v -> Ok v
            | Error e ->
                eprintfn "Card says: %s; Overriding" e
                // all work activity on this card occured on May 13th
                let dt (h:int) (m:int) = DateTime(2021,05,13,hour=h,minute=m,second=0)
                let dts =
                    [
                        dt 16 12, dt 17 34
                        dt 18 13, dt 19 22
                        dt 20 48, dt 21 19
                    ]
                ((0<Min>,List.empty),dts)
                ||> List.fold(fun (total,items) (st,stp) ->
                    let x = {Started = st; Finished=stp;DateTimeType=Regular}
                    getTotalMinutes st stp |> int |> (*) 1<Min> |> (+) total, x::items
                )
                |> Ok
            
        | EqI "Unbreaking the breaking change" ->
            let st = toDt 9 15
            let stp = toDt 12 15
            let result =
                v
                |> Result.map(fun (minutes,times) ->
                    let m = getTotalMinutes st stp |> int |> (*) 1<Min> |> (+) minutes
                    m, {Started = st; Finished = stp;DateTimeType= Overridden}::times
                    
                )
            result
            
            
        | _ -> v
    
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
                    None
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
    )
// track when it was last moved to doing
    
type CardTotalState = {InDoingSince:DateTime option;StartStop: DateTimeSpan list;totalMinutes:int<Min>}
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
        | Moved FromDoing, Some dt ->
            Ok {
                InDoingSince = None
                totalMinutes = cts.totalMinutes + int (detail.Dt - dt).TotalMinutes * 1<Min>
                StartStop = [{Started= dt;Finished=detail.Dt;DateTimeType=Regular}]
            }
        | _ -> failwithf "What are we doing here? %A,%A" detail.Act cts.InDoingSince
    )
let validateOrder start finish =
    if start > finish then
        failwithf "bad validation"
let subtotal x =
    ((0<Min>,Map.empty),x) ||> Map.fold(fun (total,m) k v ->
        let k = string k
        match v with
        | Ok (i,dts) ->  total + i, m |> Map.add k (string i, dts)
        | Error e -> total, m |> Map.add k (e, [])
    )
    |> fun r ->
        r.Dump("focusmap")
        r
    |> fst
    |> fun r -> r.Dump("total minutes");r
    |> fun minutes -> float minutes / 60.
    |> Dump
    |> ignore
    ()
let getDuplication (tolerance:float<Min>) (dts1:DateTimeSpan) (dts2:DateTimeSpan) =
    match dts1,dts2 with
    | {DateTimeType=Overridden}, _
    | _, {DateTimeType=Overridden} ->
        false,[dts1;dts2]
    | _ ->
        [dts1;dts2] |> Seq.iter (fun x -> validateOrder x.Started x.Finished)
        let tolerate (dt1:DateTime) (dt2:DateTime) = (dt2 - dt1) |> fun ts -> abs ts.TotalMinutes < (tolerance / 1.0<Min>)
        let x1,x2 = if dts1.Started < dts2.Started then dts1,dts2 elif dts2.Started < dts1.Started then dts2,dts1 elif dts1.Finished<= dts2.Finished then dts1,dts2 else dts2,dts1
        if x1.Started = x2.Started
            || tolerate x2.Started x1.Started
            || x1.Finished = x2.Finished
            || tolerate x2.Finished x1.Finished
            || tolerate x1.Finished x2.Started
            || x1.Started < x2.Started && x1.Finished > x2.Started
            then
                true,[{Started=x1.Started;Finished=max x1.Finished x2.Finished;DateTimeType=Regular}]
        else false,[x1;x2]
 
(Map.empty,focusedMap)
||> Map.fold(fun (state:Map<string,MapSummaryState>) name details ->
    let casOpt = state |> Map.tryFind name
    let result =
        (casOpt|> Option.defaultValue (Ok {InDoingSince=None;totalMinutes=0<Min>;StartStop=[]}),details)
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
    |> Result.map(fun x -> x.totalMinutes,x.StartStop)
)
// manual additions
|> Map.map(fun k v ->
    manualAdditions k v
)
|> fun x -> 
    subtotal x
    let dts =
        (List.empty,x)
        ||> Map.fold(fun items k v ->
            match v with
            | Ok (_,x) -> x@items
            | _ -> items
        )
        |> List.sortBy(fun x -> x.Started)
    dts.Dump("dts")
    
    // deduplicate time

//viewRaw()
//|> Dump
//|> ignore
//