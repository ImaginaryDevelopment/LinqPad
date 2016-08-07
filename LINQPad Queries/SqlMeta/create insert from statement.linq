<Query Kind="FSharpProgram">
  <Connection>
    <ID>3da1b433-c8cb-407a-9c25-1d4f2ea04d64</ID>
    <Persist>true</Persist>
    <Server>192.168.0.187</Server>
    <SqlSecurity>true</SqlSecurity>
    <UserName>xpu10</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAAfs+fvOIuHkq5uisIQafUpAAAAAACAAAAAAAQZgAAAAEAACAAAACQAkUvjSn5aeB96QXgdsjjFqXPvptKHgnaCGMhNRDMSgAAAAAOgAAAAAIAACAAAABbiFmT0lVrhHmtBPdMe3xyU1OjyKeaaH7eR33/SwSecRAAAAABJ/MPPwEPOgIlPCyxKQuIQAAAAIT7pUPnPxbVciisX+r/MmPla5oOVfqYvr9sRtZbeRrH/OKX3Rc7PSVpRIZFGC/3DIrOyo2W19KDU8GleIkcCIM=</Password>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>ApplicationDatabase</Database>
    <ShowServer>true</ShowServer>
  </Connection>
</Query>

// map inserts of one table/type to another

#if INTERACTIVE
let dump x = printfn "%A" x
open System
open System.Linq
#r "System.Data.Linq"
open System.Data.Linq

type System.Object with
    member x.Dump() = dump x
#else
let dump x = x.Dump() |> ignore
let dc = new TypedDataContext()
#endif
type ColumnAttribute = System.Data.Linq.Mapping.ColumnAttribute 

//let (|StartsWithI|) (s:string) (d:string) = if s.StartsWith d then Some () else None
let (|StartsWithI|_|) s1 (toMatch:string) = if toMatch <> null && toMatch.StartsWith(s1, StringComparison.InvariantCultureIgnoreCase) then Some () else None

type System.String with
    static member delimit (delimiter:string) (x:string seq) = 
        String.Join(delimiter,values = x)
        
type Attr = 
    |Column of System.Data.Linq.Mapping.ColumnAttribute * Attribute list
    | Other of Attribute list

type TypeMatch =
    | NoMatch
    | SameType of Attr 
    | DifferentType of Type * Attr

type ColumnMate = { TargetName:string; TargetType: Type; TargetAttr : Attr; SourceNameOpt: string option; SourceTypeMatch: TypeMatch }

let filterColumnTypeOpt (t:Type) =
    match t.Name with
        | StartsWithI "EntitySet" -> None
        | _ -> Some ()

let filterColumnName name = 
    match name with
    |"Dia5" |"Dia6" |"Dia7" | "Dia8" -> None
    | _ -> Some ()

let mapSourceColumnName name = 
    match name with
    | "ProcedureCodeID" -> "CodeID"
    | "Amount" -> "Charge"
    | "RenderingProviderID" -> "ProviderID"
    | "Modifier1" -> "CodeUnique"
    | x -> x
//let mapTypeDisplay (t:Type) = 
//    match t.Name with
//    |StartsWith  "Entity"

let createInsertMap (t1:Type) (t2:Type) = //dc.Charges dc.Charge1 
    //t1,t2
    let getTableName (t:Type) =
        t.GetCustomAttributes(false) 
        |> Seq.head 
        |> fun a -> a :?> System.Data.Linq.Mapping.TableAttribute
        |> fun ta -> ta.Name
    
    let getColumns (t:Type) =
        let fields = t.GetFields() |> Seq.map (fun f -> f.Name,f.FieldType, f.GetCustomAttributes(false)|> Seq.cast<Attribute> |> List.ofSeq)
        let props = t.GetProperties() |> Seq.map (fun f -> f.Name, f.PropertyType, f.GetCustomAttributes(false) |> Seq.cast<Attribute> |> List.ofSeq)
        Seq.concat [fields; props]
        |> Seq.map (fun (n,t,attribs) ->
            let caOpt = attribs |> Seq.tryFind (fun a -> match a with | :? ColumnAttribute -> true | _ -> false)
            
            let attribs = 
                match caOpt with 
                | Some ca -> 
                    let attribs = attribs |> List.except [ca]
                    Column (ca :?> ColumnAttribute, attribs)
                | None -> Other attribs

            n, t, attribs
        )

    let srcTable = getTableName t1
    let trgTable = getTableName t2
    let targetColumns = getColumns t2 
    let targetColumns = 
        targetColumns
        |> Seq.choose (fun (n,t,a) -> 
            match filterColumnTypeOpt t with 
                | Some _ -> Some (n,t,a) 
                | _ -> None)
    let targetColumns =
        targetColumns
        |> Seq.choose (fun (n,t,a) ->
            match filterColumnName n with
            |Some () -> 
//                if not <| isNull a && a |> Seq.exists (fun _ -> true) then
//                    (a,t,n).Dump("attributes!")
                Some (n,t,a)
            | None -> None
            )

    let sourceColumns = 
        getColumns t1
        |> Seq.choose (fun (n,t,a) -> 
            match filterColumnTypeOpt t with 
                | Some _ -> Some (n,t,a) 
                | _ -> None)
                
    let q = 
        query {
            for (cn,t,a) in targetColumns do
            let joinName = mapSourceColumnName cn
            leftOuterJoin (n,t,a) in sourceColumns on (joinName = n) into _scLeft
            for scOpt in _scLeft.DefaultIfEmpty() do
            select (cn,t,a,(if cn <> joinName then Some joinName else None), scOpt)
        }
        |> Seq.map (fun (cn,t,a,snOpt,scOpt) -> 
                        if isNull (box scOpt) then 
                            { TargetName=cn; TargetType = t; TargetAttr = a; SourceNameOpt = snOpt; SourceTypeMatch = NoMatch}
                            //cn, t,a,snOpt, NoMatch
                        else 
                            match scOpt with 
                                | _,st,sa -> 
                                    if st <> t then 
                                        { TargetName=cn; TargetType = t; TargetAttr = a; SourceNameOpt = snOpt; SourceTypeMatch = DifferentType (st,sa)}
                                        //cn,t,a,snOpt, (DifferentType (st,sa))
                                    else
                                        { TargetName=cn; TargetType = t; TargetAttr = a; SourceNameOpt = snOpt; SourceTypeMatch = SameType sa}
                                        //cn,t,a,snOpt, SameType sa)
                                        )
        |> List.ofSeq

    let isNullableOf t v = 
        Nullable.GetUnderlyingType(t) = v

    let mapToSql () = 
        let tIndent,sIndent = "  ", "    "
        //let tColumns = targetColumns |> Seq.map (fun (c,t,_) -> sprintf "%s -- %s" c t.Name) |> String.delimit (Environment.NewLine + tIndent  + ",")
        //let sColumns = sourceColumns |> Seq.map (fun (c,t,_) -> sprintf "%s -- %s" c t.Name) |> String.delimit (Environment.NewLine + sIndent + ",")
        let columnNamePadding = q |> Seq.map(fun x -> x.TargetName.Length) |> Seq.max |> (+) 1
        let tColumns = 
            q 
            |> List.mapi (fun i x -> 
                match x.SourceTypeMatch with
                    | SameType (Column (ca,attrs)) -> String.Empty
                    | SameType x -> 
                        sprintf "(from %A)" x
                    | DifferentType (t, Column (ca,attrs)) -> 
                        sprintf "(from %A)" ca
                    | DifferentType (st, _) -> // ignoring attribute list of target
                        sprintf "(from %A)" st
                    | NoMatch ->
                         "(no match)"
                |> sprintf "%-*s -- %2i %s %s" columnNamePadding x.TargetName i x.TargetType.Name // does not account for non-fixed width fonts
            ) 
            |> String.delimit (Environment.NewLine + tIndent  + ",")
        let sColumns =
            q
            |> List.mapi (fun i x ->
                let sourceName = match x.SourceNameOpt with | Some sn -> sn | None -> x.TargetName
                // TODO: account for nullability vs non nullability

                let baseComment sn st = sprintf "-- %2i (Target = %s %s) (Source = %s %A)" i x.TargetName x.TargetType.Name  sn st
                match x.SourceTypeMatch with
                    //| SameType (Column (ca,_)) -> sprintf "%s %s" c baseComment
                    | SameType sa -> sprintf "%s %s" sourceName (baseComment sourceName x.TargetType.Name)
//                    | DifferentType (st, Column (ca, attrs)) ->
//                        // if the only difference is nullability then account for that else use ?
//                        sprintf "? %s (from %s") baseComment st.Name
                    | DifferentType (st, _) ->
                        if x.TargetType.IsAssignableFrom st then
                            sprintf "%s %s (assignable)" sourceName (baseComment sourceName st.Name)
                        elif isNullableOf x.TargetType st then
                            sprintf "%s %s (isNullableOf1)" sourceName (baseComment sourceName st)
                        elif isNullableOf st x.TargetType then
                            sprintf "coalesce(%s,?) %s (isNullableOf2)" sourceName (baseComment sourceName st)
                        else
                            sprintf "? %s" (baseComment sourceName st.Name)
                    | NoMatch ->
                        sprintf "? -- %s " (baseComment "NoMatch" None)
                )
            |> String.delimit (Environment.NewLine + sIndent + ",")
            
        sprintf "insert into %s (%s%s) %s%s    SELECT%s %s %sFROM %s" trgTable (Environment.NewLine + tIndent) tColumns Environment.NewLine Environment.NewLine (Environment.NewLine + sIndent) sColumns Environment.NewLine srcTable,q
        
    mapToSql()

#if INTERACTIVE

#else
let sql,q = createInsertMap (dc.Charges.GetType().GenericTypeArguments.[0]) (dc.Charge.GetType().GenericTypeArguments.[0])
sql |> dump
q|> dump
#endif