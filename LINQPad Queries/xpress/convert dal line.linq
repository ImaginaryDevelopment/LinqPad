<Query Kind="FSharpExpression" />

let flip f x y= f y x
let replace (t:string) (r:string) (s:string) = s.Replace(t,r)

let dumpt (t:string) x =
    x.Dump(t)
    x
    
let inline add s x = x + s
let inline prepend (s:string) (x:string) = s + x

let regMatch (p:string) (x:string) = Regex.Match(x, p)

let regReplace (p:string) (r:string) (t:string) = Regex.Replace(t, p, r)

let convertParameters (p:string): string = 
    if p.StartsWith "new Dictionary" then 
        Regex.Matches(p,@"\[(""@\w+"")\]\s*=\s* (\w+)") 
        |> dumpt "parameters matches"
        |> Seq.cast<Match> 
        |> Seq.map ( fun m -> sprintf "%s, %s :> obj" m.Groups.[1].Value m.Groups.[2].Value )
        |> Array.ofSeq 
        |> fun a -> String.Join("; ",a)
        |> prepend "dict ["
        |> add "] |> Some"
    elif p = "null" then "None"
    else p

let convertReaderMap (r:string) = 
    if r.StartsWith "r =>" then
        r 
        |> replace "r =>" "(fun r ->"
        |> replace "r[" "r.["
        |> add ")"
    else r
    
let convertExecute txt : string = 
    txt
    |> regReplace @"r.ReadValueMap\((nameof\(\w+\.\w+\)), (Convert.To\w+)" "getRecordT r $1 |> Option.map $2"
    //|> regReplace @"r.ReadValueMap\((.*)\)" "getRecordT r $1"
    |> regMatch @"\s*(?:Pm.Dal.AdoHelper.)?ExecuteReaderArray\(([^,]+), (CommandType.\w+), (\w+), (.*),\s*(.*)\)?"
    |> fun m -> m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value, convertParameters m.Groups.[4].Value, convertReaderMap m.Groups.[5].Value
    |> dumpt "parts"
    |> fun (cmdText, ct, cn, p,r) -> 
        sprintf "Pm.Dal.AdoHelper.getReaderArray %s {CommandText=%s; OptCommandType = Some %s; OptParameters = %s} %s" cn cmdText ct p r
    |> regReplace @"nameof\((\w+)Record\.(\w+)\)" @"$1Meta.$2"
    |> convertReaderMap

let toConvert = 
    [   """            Pm.Dal.AdoHelper.ExecuteReaderArray(cmdText, CommandType.StoredProcedure, cn, new Dictionary<string, object> { ["@PatientID"] = patientId }, r => Convert.ToBoolean(r[0]))"""
        """            Pm.Dal.AdoHelper.ExecuteReaderArray(cmdBuilder.ToString(), CommandType.Text, cn, null, r => r["MessageText"].ToString())"""
        """Pm.Dal.AdoHelper.ExecuteReaderArray(cmdText, CommandType.Text, cnOuter, null, r => r.ReadValueMap(nameof(ClaimRecord.Balance), Convert.ToDecimal))"""
        """Pm.Dal.AdoHelper.ExecuteReaderArray("select {nameof(ClaimRecord.Balance)} from {ClaimHelpers.Meta.tableName} where {nameof(ClaimRecord.AppointmentID)}='{apptId}' ", CommandType.Text, cn, null,
                r => r.ReadValueMap(nameof(ClaimRecord.Balance), Convert.ToDecimal))"""
        """        Pm.Dal.AdoHelper.ExecuteReaderArray(cmdText, CommandType.Text, cn, null, r => r.ReadValueMap(nameof(ClaimRecord.AppointmentID), Convert.ToInt32)"""
        """ Pm.Dal.AdoHelper.ExecuteReaderArray(cmdText, CommandType.Text, cn, null, (r => ))"""
        """         Pm.Dal.AdoHelper.ExecuteReaderArray(cmdText, CommandType.StoredProcedure, cn, @params, r =>"""
        """         return Pm.Dal.AdoHelper.ExecuteReaderArray(cmdText, CommandType.StoredProcedure, cn, @params, r =>"""
    ]
let expected = 
    [   Some """Pm.Dal.AdoHelper.getReaderArray cn {CommandText=cmdText; OptCommandType = Some CommandType.StoredProcedure; OptParameters = dict ["@PatientID", patientId :> obj] |> Some} (fun r -> Convert.ToBoolean(r.[0]))"""
        Some """Pm.Dal.AdoHelper.getReaderArray cn {CommandText=cmdBuilder.ToString(); OptCommandType = Some CommandType.Text; OptParameters = None} (fun r -> r.["MessageText"].ToString())"""
    ]
let expected' = 
    [expected.Length..toConvert.Length] 
    |> List.map (fun _ -> None)
    |> fun l -> expected@l
toConvert
|> Seq.map convertExecute
|> Seq.zip expected'
|> Seq.map (fun (expected, converted) -> 
                seq{
                    match expected with 
                    |Some e -> 
                        if e <> converted then  
                            (e,converted) |> dumpt "bad conversion" |> ignore
                            failwithf "conversion expectations not met "
                    |None -> 
                        yield converted
                    })
|> Seq.collect id

