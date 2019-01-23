<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

// example
open Newtonsoft.Json.Linq

let equalsI x =
    function
    | null |"" -> false
    | y -> y.Equals(x,StringComparison.InvariantCultureIgnoreCase)
let deserialize<'t> x = JsonConvert.DeserializeObject<'t>(x)
let target = "https://poe.ninja/api/Data/GetCurrencyOverview?league=Betrayal"
let input =
    """{
            "id": 1,
            "icon": "https://web.poecdn.com/image/Art/2DItems/Currency/CurrencyRerollRare.png?scale=1&w=1&h=1",
            "name": "Chaos Orb",
            "poeTradeId": 4
    }"""
let hasRaw(t:Type) =t.GetProperties() |> Array.exists(fun p -> p.Name.Equals("raw",StringComparison.InvariantCultureIgnoreCase))
type [<NoComparison>] ReadSequence =
    |SimpleSequence of obj list
    |ComplexSequence of Map<string,ReadValue> list
    |NestedSequence of ReadSequence list
and [<NoComparison>] ReadValue =
    |SimpleValue of obj
    |ComplexValue of Map<string,ReadValue>
    |SequenceValue of ReadSequence
let readOrFail (r:JsonReader) = if not <| r.Read() then raise (JsonSerializationException("Unexpected end when reading object"))

let (|EmptySeq|NonEmpty|) =
    function
    | SimpleSequence [] -> EmptySeq
    | ComplexSequence [] -> EmptySeq
    | NestedSequence [] -> EmptySeq
    | SimpleSequence _ | ComplexSequence _ | NestedSequence _ -> NonEmpty
let rec readValue (reader:JsonReader) =
    match reader.TokenType with
    | JsonToken.StartObject ->
        readObject reader Map.empty
        |> ComplexValue
    | JsonToken.StartArray ->
        readArray reader (SimpleSequence [])
        |> SequenceValue
    | JsonToken.Integer
    | JsonToken.Float
    | JsonToken.String
    | JsonToken.Boolean
    | JsonToken.Undefined
    | JsonToken.Null
    | JsonToken.Date
    | JsonToken.Bytes -> SimpleValue reader.Value
    | _ -> raise (JsonSerializationException(sprintf "Unexpected token when reading object: %O" reader.TokenType))

and readArray(reader:JsonReader) (rs: ReadSequence):ReadSequence =
    readOrFail reader
    reader.TokenType
    |> function
        |JsonToken.Comment -> readArray reader rs
        |JsonToken.EndArray ->
            rs
        |_ ->
            match readValue reader,rs with
            | SimpleValue sv, EmptySeq -> [sv] |> SimpleSequence
            | ComplexValue cv, EmptySeq -> [cv] |> ComplexSequence
            | SequenceValue nv, EmptySeq -> [nv] |> NestedSequence
            | SimpleValue v, NonEmpty & SimpleSequence ss -> v::ss |> SimpleSequence
            | ComplexValue v, NonEmpty & ComplexSequence cs -> v::cs |> ComplexSequence
            | SequenceValue v, NonEmpty & NestedSequence ns -> v :: ns |> NestedSequence
            | SimpleValue v, NestedSequence _ -> raise (JsonSerializationException <| sprintf "Unexpected simple value in Nested Sequence array %A" v)
            | ComplexValue cv, SimpleSequence _ -> raise (JsonSerializationException <| sprintf "Unexpected complex value in Simple Sequence array %A" cv)
            | ComplexValue cv, NestedSequence _ ->raise (JsonSerializationException <| sprintf "Unexpected complex value in Nested Sequence array %A" cv)
            | x,y -> (x,y).Dump("why are we here?");failwithf "gonna have a bad time"
                
            
    
and readObject (reader:JsonReader) (obj:Map<string,ReadValue>):Map<string,ReadValue>=
    readOrFail reader
    reader.TokenType
    |> function
        |JsonToken.Comment -> readObject reader obj
        |JsonToken.PropertyName ->
            let pn = string reader.Value
            readOrFail reader
            let value = readValue reader
            readObject reader (obj.Add(pn, value))
        |JsonToken.EndObject ->
            // let's add Raw eh?
            obj
        | _ -> raise (JsonSerializationException(sprintf "Unexpected token when reading object: %O" reader.TokenType))
            
    
let unwrapPropertyInfo =
    Option.bind(fun (t:Type) ->
        if t.IsGenericType && t.GenericTypeArguments.Length > 0 then
            Some t.GenericTypeArguments.[0]
        else
            t.Dump("no gta?")
            None
    )
let rec mapProp fRaw (pn,t:PropertyInfo option,v) =
    let rec mapSeq (t:Type option) (x:ReadSequence):JArray =
        let t = t |> unwrapPropertyInfo 
        x
        |> function
            |SimpleSequence ss -> JArray(ss |> List.map JValue|> Array.ofList)
            |ComplexSequence cs -> JArray(cs |> List.map (mapObject fRaw t) |> Array.ofList)
            |NestedSequence rs -> 
                let ja = JArray()
                rs
                |> List.map (mapSeq t)
                |> List.iter(ja.Add)
                ja
    let propType = t |> Option.map(fun x -> x.PropertyType)
    match v with
    | SimpleValue v -> JProperty(pn,JValue(v))
    | ComplexValue cv ->
        let jo:JObject = mapObject fRaw propType cv
        JProperty(pn,jo)
    | SequenceValue rs ->
        let ja:JArray = mapSeq propType rs
        JProperty(pn,ja)
        
and mapObject f (t:Type option) (obj:Map<string,ReadValue>):JObject =
    let t = t |> Option.bind Option.ofObj
    let jo = JObject()
    let typeProps = t |> Option.map(fun t -> t.GetProperties() |> List.ofSeq) |> function | Some x -> x | None -> List.empty
    let props = 
        obj
        |> Map.toSeq
        |> Seq.map(fun (k,v) -> k, typeProps |> List.tryFind(fun p -> p.Name |> equalsI k),v)
        |> List.ofSeq
    props
    |> Seq.iter(fun (k,tOpt, v) ->
        let jp = mapProp f (k,tOpt,v)
        jo.Add(jp.Name,jp.Value)
    )
    f t props jo
    
let descendRead x =
    let rec descend x =
        match x with
        | SimpleValue _ -> None
        | ComplexValue cv -> Some cv
        | SequenceValue (SimpleSequence _) -> None
        | SequenceValue (ComplexSequence(x::[])) -> Some x
        | SequenceValue (NestedSequence(x::[])) -> descend (SequenceValue x)
        | _ -> None
    descend x
let mapRaw t x =
    // scrape out mapped properties
    let getRaw (_objT:Type option) props (jo:JObject) =
        
        props
        |> List.fold(fun (jRawOpt:JObject option) (pn:string,piOpt:PropertyInfo option,v:ReadValue) ->
            match piOpt with
            // property is mapped ignore
            | Some _ -> jRawOpt
            | None ->
                match jRawOpt with
                | None -> JObject(JProperty(pn,string v)) |> Some
                | Some jRaw ->
                    let jp = JProperty(pn, JValue(string v))
                    jRaw.Add jp
                    Some jRaw
        ) None
        |> Option.iter(fun (r:JObject) ->
            jo.Add("Raw",JValue(r.ToString()))
        )
        jo
        
    let jo = mapObject getRaw (Some t) x
    jo.ToObject(t)
    
let mapRawT<'t> x =
    mapRaw typeof<'t> x :?> 't
    
type RawConverter() =
    inherit JsonConverter()

    override __.CanConvert t = hasRaw t
    override __.WriteJson (_writer:JsonWriter, _value: obj, _:JsonSerializer) = failwithf"can't do it"
    override __.ReadJson (reader:JsonReader, t: Type, existingValue:obj, _js:JsonSerializer) =
        if not <| isNull existingValue then
            (reader.TokenType,t.Name,existingValue).Dump("why is there a value?")
        let rv = readObject reader Map.empty
        let result = mapRaw t rv
        result
        
let deserializeWithRaw<'t>(x:string):'t=  JsonConvert.DeserializeObject<'t>(x,converters=[| RawConverter()|])
    
// example capture types without recursion/hierarchy to deal with
type Currency' ={Id:int;Icon:string;Name:string;PoeTradeId:int;Raw:string}  // since all properties in the source are accounted for Raw would be null or empty string

type CurrencyNeedsCapture = {Id:int;Name:string;Raw:string} // 2 properties weren't accounted for, Raw would be {"icon": "...";"poeTradeId":4}

let runSimple() =
    input
    |> deserializeWithRaw<CurrencyNeedsCapture>
    |> Dump
    |> ignore

let complex =
    JObject(
        JProperty("lines",JArray(
                    JObject(
                        JProperty("currencyTypeName","Mirror of Kalandra"),
                        JProperty("pay",JObject(
                                            JProperty("id",1),
                                            JProperty("league_id",50),
                                            JProperty("pay_currency_id",22)
                                            )
                                )
            
                    )
            )
        ),
        JProperty("currencyDetails",
            JArray(
                    JObject(
                        JProperty("id",1),
                        JProperty("icon","https://..."),
                        JProperty("name","Chaos Orb"),
                        JProperty("poeTradeId",4)
                        
                    
                    )
            )
        )
    )
type Pay={Id:int;LeagueId:int;Value:decimal;Raw:string}
type Line = {CurrencyTypeName:string;Pay:Pay;Raw:string}
type Complex = {Lines:Line list;CurrencyDetails:CurrencyNeedsCapture list}

let runComplex() =
    complex
    |> string
    |> fun x -> deserializeWithRaw<Complex> x, x
    |> Dump
    |> ignore
    
runComplex()
