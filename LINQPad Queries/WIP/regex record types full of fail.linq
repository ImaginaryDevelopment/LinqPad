<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// map record type to new shape
let dumpt t x = x.Dump(description=t); x
let trim1 d (x:string) = x.Trim(d)
type OldFKeyRecord = {Schema:string; Table:string; Column: string}
type OldRefKey = {FKey:OldFKeyRecord; GenerateReferenceTable:bool; ReferenceValuesWithComment:IDictionary<string,string>; Comments: string list} with
    static member Zero = 
        {FKey= {Schema=null; Table=null; Column = null}; GenerateReferenceTable=false; ReferenceValuesWithComment=null; Comments = List.empty}
type TableIdentifier = {Schema:string; Name:string;}
type FKeyIdentifier = {Table: TableIdentifier; Column:string}

type ReferenceData = {FKey:FKeyIdentifier; GenerateReferenceTable:bool; ValuesWithComment: IDictionary<string,string>} 
    with 
        static member FromLegacy (x:OldRefKey) = 
            {   ReferenceData.FKey= {FKeyIdentifier.Table={Schema=x.FKey.Schema;Name=x.FKey.Table}; Column=x.FKey.Column}
                GenerateReferenceTable= x.GenerateReferenceTable
                ValuesWithComment= x.ReferenceValuesWithComment
            }
type FKey = 
    |FKeyIdentifier of FKeyIdentifier
    |FKeyWithReference of ReferenceData




let (|RegexMatch|_|) regexp (x:string) = 
    let m = Regex.Match(x, regexp)
    if m.Success then
        Some m
    else None
let (|RegexMultiMatch|_|) regexp (x:string) = 
    let m = Regex.Match(x,regexp, RegexOptions.Multiline)
    if m.Success then
        Some m
    else None
let (|RegexMultiMatches|_|) regexp (x:string) = 
    let m = Regex.Matches(x,regexp, RegexOptions.Multiline)
    if m.Count > 0 then
        Some m
    else None
let getNamedValue (m:Match) (x:string) = 
    m.Groups.[x].Value
let makeNamedTuple (m:Match) (x:string) = 
    x, getNamedValue m x |> trim1 [| '"' |]    
module DomainRegex = 
    let tupleRegexp r1 r2 = sprintf "\s*%s\s*,\s*%s" r1 r2
    let dictSemiLiteral r1 r2 =tupleRegexp r1 r2 |> sprintf "\s*dict\s*\[(%s;)+\s*\]" 
    
    
    let stringValueRegexp = "\"[^\"]+\""
    let dictSemiLiteralStr = dictSemiLiteral stringValueRegexp stringValueRegexp
    let assignRegexp (fieldName:string) (valueRegexp:string) : string = sprintf "%s\s*=\s*(?<%s>%s)\s*;?" fieldName fieldName valueRegexp
    let stringAssignRegexp fieldName :string = assignRegexp fieldName stringValueRegexp
    let boolAssignRegexp fieldName :string = assignRegexp fieldName "true|false"
    let refValuesRegexp = 
        let cases = [
                "null"
                "dict\s*\[[^\]]+\]"
                "(\[.*\|>\s*dict)"]
            
        assignRegexp "ReferenceValuesWithComment" (cases |> delimit "|")
    let commentsRegexp = assignRegexp "Comments" (sprintf "null|\[(?:\s*%s\s*)*]" stringValueRegexp)
    let (oldFKeyRegexp:string,oldFKeyExtractor) = 
        sprintf @"{\s*%s\s*%s\s*%s\s*}" (stringAssignRegexp "Schema") (stringAssignRegexp "Table")  (stringAssignRegexp "Column"),
            fun (m:Match) -> [makeNamedTuple m "Schema"; makeNamedTuple m "Table"; makeNamedTuple m "Column"]
    let refRegex = sprintf @"%s\s*with\s*%s\s*%s\s*(?:%s)?" oldFKeyRegexp (boolAssignRegexp "GenerateReferenceTable") refValuesRegexp commentsRegexp
    // failure to be useful: let columnRegex = sprintf "([\w\s]+)|({.*}$)"
let (|OldRefKey|_|) (x:string) = 
    match x with
    |RegexMatch DomainRegex.refRegex m -> 
        //m.Groups.["ReferenceValuesWithComment"].Dump("future rvc")
        
        DomainRegex.oldFKeyExtractor m
        |> List.append [ makeNamedTuple m "GenerateReferenceTable"; makeNamedTuple m "Comments"; makeNamedTuple m "ReferenceValuesWithComment"]
        |> Some
    | _ -> None
let (|OldFKey|_|) (x:string) = 
    match x with
    | RegexMatch DomainRegex.oldFKeyRegexp m -> DomainRegex.oldFKeyExtractor m |> Some
    | _ -> None
type LegacyGenData = 
    | ComputedRefData of leadingCall:string * ((string*string) list)
    //makeUserIdColumn null AllowNull "null to allow system inserts/adjustments that aren't done by a user"
    //makePatientIdColumn null AllowNull null
    // { makeNonFKeyColumn "Created" (Other typeof<DateTime>) AllowNull with Comments = ["was timestamp"]}
    | NoTranslationNecessary of string
    | NoTranslationFound of string
let boolTupleToOption (b,v) = if b then Some v else None    
let split (d:string) (x:string) = x.Split([d] |> Array.ofList, StringSplitOptions.None)
let (|StartsWith|_|) (d:string) (x:string) = if x.StartsWith d then Some() else None
let (|DictKeyValue|_|) (d:#IDictionary<_,_>) k = if d.ContainsKey k then Some d.[k] else None
let (|MappedData|_|) (x:string) = 
    match x with
    | OldRefKey v -> 
        v 
        |> dict 
        |> (fun v ->  
            let rvc = v.TryGetValue "ReferenceValuesWithComment" |> boolTupleToOption
            
            {
                        FKey={Table={Schema=v.["Schema"]; Name=v.["Table"]};Column= v.["Column"]}
                        GenerateReferenceTable= System.Boolean.TryParse v.["GenerateReferenceTable"] |> boolTupleToOption |> Option.getOrDefault false
                        ValuesWithComment= 
                            match rvc with 
                            | Some value -> 
                                match value with
                                | "null" -> null
                                //dict[
                                //"Cash",null;"CC",null;"Check",null;"Ach",null;"Fsa",null;"Other","for when Era amount is 0 or a catch-all"
                                //]
                                | RegexMultiMatch DomainRegex.dictSemiLiteralStr m ->
                                    m.Dump("did the inner grouping capture?")
                                    null
                                 
                                | StartsWith "dict" -> 
                                    try
                                        value
                                        |> after "[" 
                                        |> before "]" 
                                        |> split ";" 
                                        |> Seq.map (split "," >> Seq.map (trim1 [| '"' |]) >> List.ofSeq >> function | [k;v] -> (k,v))
                                        |> dict
                                    with ex ->
                                        value.Dump("in startswith dict")
                                        reraise()
                                | RegexMatch "\[\s*(\"\w+\"\s*;?)+\]\s*\|>\s*Seq\.map\s*\(fun (\w)\s*->\s*\2\s*,\s*null\s*\)\s*\|>\s*dict" m -> 
                                    m.Groups.[1].Captures 
                                    |> Seq.cast<Capture> 
                                    |> Seq.map (fun c -> c.Value |> trim1 [| ';' |] , null)
                                    |> dict
//                                    (value,m).Dump("wth is the structure of this capture")
                                    
                            | _ -> null
                            
                                            
                    }, v.TryGetValue "Comments" |> boolTupleToOption, rvc
        )
        |> sprintf "%A" 
        |> Choice1Of4 
        |> Some
    | OldFKey v -> v |> dict |> (fun v ->  {Table={Schema=v.["Schema"]; Name=v.["Table"]}; Column= v.["Column"]} )|> sprintf "%A" |> Choice2Of4 |> Some
    | RegexMatch "^\s*make" _ -> Choice3Of4 x |> Some
    | _ -> Choice4Of4 x |> Some
    
let toFKeyId (x:OldFKeyRecord) : FKeyIdentifier = {Table={Schema=x.Schema; Name=x.Table}; Column=x.Column}

//let serial x = Newtonsoft.Json.JsonConvert.SerializeObject(x, Formatting.Indented)
//
//let fakeIt (fk:OldFKeyRecord) =
//    {OldRefKey.Zero with FKey = fk}

let old =[  """      {makeStrFkey50 "PaymentTypeId" {Schema="Accounts";Table="PaymentType";Column="PaymentTypeId"} with 
                        GenerateReferenceTable = true
                        ReferenceValuesWithComment = [ "Patient";"ThirdParty";"Era"] |> Seq.map (fun n -> n,null) |> dict
                        Comments = [
                                    "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                        ] }"""
            """
                    {makeStrFkey50 "PaymentTypeId" {Schema="Accounts";Table="PaymentType";Column="PaymentTypeId"} with 
                        GenerateReferenceTable = true
                        ReferenceValuesWithComment = [ "Patient";"ThirdParty";"Era"] |> Seq.map (fun n -> n,null) |> dict
                        Comments = [
                                    "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                        ] } """
            """
                    {makeStrFkey50 "PaymentMethodId" {Schema="Accounts"; Table="PaymentType"; Column="PaymentMethodId"} with
                        GenerateReferenceTable = true
                        ReferenceValuesWithComment = 
                            dict[
                                "Cash",null;"CC",null;"Check",null;"Ach",null;"Fsa",null;"Other","for when Era amount is 0 or a catch-all"
                        ] }"""
            """     {makeStrFkey50 "PaymentStatusId" {Schema="Accounts"; Table="PaymentStatus"; Column= "PaymentStatusId"} with
                        GenerateReferenceTable = true
                        ReferenceValuesWithComment = ["New";"Partial";"Complete"] |> Seq.map (fun n -> n,null) |> dict }"""
            """     {   Name="TotalAmount"; 
                        Type=Decimal (Some {Precision=12; Scale=2}); IsUnique=false; Attributes = List.empty; AllowNull = NotNull; FKey=None;
                        GenerateReferenceTable=false
                        ReferenceValuesWithComment=null
                        Comments = ["was amount (18,2)"] }"""
            """     makeUserIdColumn null AllowNull "null to allow system inserts/adjustments that aren't done by a user" """
            """     {makeIntFkey "PayerID" {Schema="dbo";Table="Payers";Column="PayerID"} with AllowNull=AllowNull}"""
            """     makePatientIdColumn null AllowNull null """
            """     { makeNonFKeyColumn "Created" (Other typeof<DateTime>) AllowNull with Comments = ["was timestamp"]}"""
            """     { makeNonFKeyColumn "TransactionNumber" (VarChar (Length 30)) AllowNull with Comments = ["was checkNumber now will store check number or ACH number (when applicable)"]}"""
            """     { makeNonFKeyColumn "Rcd" (Other typeof<DateTime>) AllowNull with Comments = ["Payment Recvd"]}"""
            """     makeNonFKeyColumn "IsElectronic" (Other typeof<bool>) NotNull"""
            """     { makeIntFkey "CCItemID" {Schema="dbo";Table="Accounts";Column="CCItem"} with AllowNull=AllowNull}"""
            """     makeNonFKeyColumn "Comments" (VarChar Max) AllowNull"""
        ]
//                        
//
//{ fakeIt {Schema="Accounts";Table="PaymentType";Column="PaymentTypeId"} with 
//                        GenerateReferenceTable = true
//                        ReferenceValuesWithComment = [ "Patient";"ThirdParty";"Era"] |> Seq.map (fun n -> n,null) |> dict
//                        Comments = [
//                                    "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
//                        ] }
//                        
//                        
//                        
//                    
//|> ReferenceData.FromLegacy
//|> sprintf "%A"
//|> replace ", ]" ",null]"
//|> Dump

let translated = 
    old
    |> Seq.map (function | MappedData x as input -> Some (x,Util.OnDemand("src", fun () -> input)) | _ -> None)
    |> List.ofSeq
    
let firstExpected =
      """{makeStrRefFkey50 "PaymentTypeId" 
                        {   ReferenceData.FKey = {Table = {Schema = "Accounts"; Name = "PaymentType"}; Column = "PaymentTypeId"} 
                            GenerateReferenceTable = true;
                            ValuesWithComment = dict ["Patient", null; "ThirdParty", null; "Era", null]}
                        with
                            Comments = [ "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                        ] }"""

translated
|> Dump
|> ignore