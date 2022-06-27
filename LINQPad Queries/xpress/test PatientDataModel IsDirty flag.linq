<Query Kind="FSharpExpression">
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Schema.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>Pm.Schema</Namespace>
  <Namespace>Pm.Schema.DataModels</Namespace>
  <Namespace>Pm.Schema.DataModels.PatientsInfoes</Namespace>
  <Namespace>Schema.BReusable</Namespace>
  <Namespace>Schema.BReusable.Reflection</Namespace>
  <Namespace>Schema.BReusable.StringPatterns</Namespace>
</Query>

let nonDirties = ["IsDirty"]

// goal take built-in type value (possibly null) and a target type and generate something that is not equal to the existing value
// support primitives, strings, #IEnumerable<_> (Seq.empty vs. null perhaps?)
let (|NullableValueT|_|) (v,t) : obj option =
    match t with
    | NullableT t -> Some t
    | _ -> None
    |> Option.bind(fun t ->
        let inline (|NotNull|IsNull|) (x:obj) = match x with null -> IsNull |_ -> NotNull
        match v with
        |NotNull ->
            Some null
        |IsNull ->
            match t with 
            | TypeOf(isType:int) -> Nullable 2 |> box |> Some
            | TypeOf(isType:DateTime) -> DateTime.MinValue |> box |> Some
            | TypeOf(isType:bool) -> true |> box |> Some
            | _ -> None
    )
        
       
        

let props =
    typeof<PatientDataModel>.GetProperties()
    |> Array.filter(fun p -> p.CanWrite)
    |> Array.filter (fun prop -> nonDirties |> List.contains prop.Name |> not)

let testDirtable (pt:#PatientDataModel) (name,typeName) makesDirty f :string =
    // if it makes things dirty, it must be clean, if it doesn't, it must also be clean
    pt.IsDirty <- false
    f pt
    let f = sprintf "%s(%s):%s" name typeName
    match pt.IsDirty, makesDirty with
    | true, true
    | false, false -> "pass"
    | _ -> "fail"
    |> f
let getTypeString =
    function
    | NullableT t -> sprintf "Nullable<%s>" t.Name
    | t when t.IsValueType -> t.Name
    | TypeOf (isType:Guid)
    | TypeOf  (isType:String)
        as t -> t.Name
    | x -> 
        printfn "No print type for %s" x.FullName
        x.Name
        
let testOne (pt:PatientDataModel) (prop:PropertyInfo) :string option= 
    let inline fTry f =
        try
            f()
        with ex ->
            ex.Dump(sprintf "%s:%s" prop.Name <| getTypeString prop.PropertyType)
            reraise()
    pt.IsDirty <- false

    fTry (fun () ->
        let inline f (x:obj) pt = prop.SetValue(pt,x)
        match prop.GetValue(pt), prop.PropertyType with
        | null, TypeOf(isType:string) ->
            "hello" |> f |> Some
        | :? string as x, TypeOf(isType:string) ->
            if x = "hello" then "world" else "hello"
            |> f
            |> Some
        | NullableValueT x -> f x |> Some
        | Int i, TypeOf (Reflection.isType:int) ->
            let x = [1..3] |> Seq.find((<>) i)
            Some <| f x 
        | Boolean x, TypeOf(isType:Nullable<bool>)
        | Boolean x, TypeOf(isType:bool) ->
            Some <| f (not x)
            
        | :? DateTime as x, TypeOf(isType:DateTime) ->
            if x = DateTime.MinValue then
                DateTime.MaxValue
            else DateTime.MinValue
            |> f
            |> Some
        | :? Guid as x, TypeOf(isType:Guid) ->
            let y = Guid.NewGuid()
            if x = y then
                Guid.NewGuid()
            else y
            |> f
            |> Some
                
        | null, TypeOf(isType:byte[]) ->
            Some <| f Array.empty<byte>
        | null, pt ->
            printfn "No match for %s" <| getTypeString pt
            None
        | vt,pt -> 
            printfn "No match for %s,%s" (getTypeString <| vt.GetType()) (getTypeString pt)
            None
        |> Option.map (testDirtable pt (prop.Name,prop.PropertyType.Name)  true)
    )
 
let pt = PatientDataModel(PatientsInfoRecord.Zero(), {PatientCoreInfo.RcopiaID=null; ForeignEhrId=nullable; PatientGuid=nullable},DateTime.MinValue, false)
let (|Pass|_|) = 
    function
    | null | "" -> None
    | s when s.EndsWith "pass" -> Some()
    | _ -> None
props
|> Seq.map(fun p ->
    p.Name,testOne pt p
)
|> Seq.sortBy(snd >> Option.getOrDefault null >> (|Pass|_|))
