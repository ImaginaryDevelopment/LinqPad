<Query Kind="FSharpProgram" />

// translate sql generator to F#
                    
module Seq = 
    let isIn (x: _ seq) item = 
        x |> Seq.contains item
    let containsAnyOF (x: _ seq) (y: _ seq) =
        x |> Seq.exists (fun x -> y |> Seq.exists (fun y -> x = y))


[<AutoOpen>]
module Helpers = 
    let trimc (c:char) (s:string) = s.Trim(c)
    let regex (p:string) opts (input:string) = match opts with | None -> Regex.Match(input,p) | Some o -> Regex.Match(input,p, o)
    let regexMany (p:string) opts (input:string) = match opts with | None -> Regex.Matches(input,p) | Some o -> Regex.Matches(input,p, o)
//    let split (d:string) (text:string) = text.Split([| d |], StringSplitOptions.RemoveEmptyEntries)
//    let before (delimiter:string) (x:string) = x.Substring(0, x.IndexOf delimiter)
    let wrap (d:string) s = sprintf "\"%s\"" s  
    let wordify (s:string) = s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq
    let containsAnyOf (items:string seq) (s:string) = items |> Seq.exists (fun x -> s.IndexOf x >= 0)
    let (|Matched|_|) (m:Match) = 
        if m.Success then
            Some m
        else None
//    let after (delimiter:string) (x:string) =  
//        match x.IndexOf delimiter with
//        | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
//        | i -> x.Substring(i + delimiter.Length)
    let tryF contextMsg f = 
        try
            f()
        with ex ->
            printfn "%s" contextMsg
            reraise()

let matchSimpleStringAssignment propName text = 
    regex (@"[^\w]" + propName + @"\s*=\s*""(\w+)""") None text
let getSimpleStringAssignment propName text = 
    matchSimpleStringAssignment propName text |> fun x -> x.Groups.[1].Value
let columnStarters = ["new ColumnInfo";"CreateFKeyedColumn";"CreateUserIdColumn";"CreatePatientIdColumn";"MakeNullable50"] |> delimit "|"
let outerPairs (items: _ seq) =
    [
        yield! Seq.pairwise items |> Seq.map (fun (a,b) -> a, Some b)
        yield Seq.last items |> fun i -> i, None
    ]
let getColumnText (text:string) (i, endI:int option) = 
    
    match endI with
    | None -> tryF (sprintf "getting text after index %i in string of length %i text = '%s'" i text.Length text) (fun () -> text.Substring(i))
    | Some endI -> 
        let toTake = endI - i
        tryF (sprintf "getting text after index %i taking %i in string of length %i text = '%s'" i toTake text.Length text) (fun () -> text.Substring(i, toTake))
[<RequireQualifiedAccess>]
type ColumnCreateTypes = 
    |CreateColumn of string
//    |CreateFKeyed of string*string
    //|MakeNullable50 of string
    //|CreateUserIdColumn of string seq
    //|CreatePatientIdColumn of string seq
    |Translated of string*string
    
let columnInputPropNames = 
                """Name:string
                Type:Type
                Length: int option
                Precision:int option
                Scale: int option
                UseMax: bool
                AllowNull: Nullability
                Attributes:string list
                FKey:FKeyInfo option
                Comments: string list
                GenerateReferenceTable: bool
                ReferenceValuesWithComment: IDictionary<string,string>
                IsUnique: bool"""
                |> regexMany "(\w+)\s?:(.*)" (Some RegexOptions.IgnoreCase)
                |> Seq.cast<Match>
                |> Seq.map (fun m -> m.Groups.[1].Value)
                |> List.ofSeq
                |> dumpt "columnInputPropNames"
let getColumns (text:string) = 
    // sample desired column outputs:
    //ColumnInput.createUserIdColumn null Nullability.AllowNull ["null to allow system inserts/adjustments that aren't done by a user"]
    //ColumnInput.createFKeyedNColumn<int> "PayerID" { Schema="dbo"; Table="Payers"; Column = null } 
    //ColumnInput.createPatientIdColumn null Nullability.AllowNull List.empty
    //{ ColumnInput.create "Created" typeof<DateTime> with AllowNull = Nullability.AllowNull; Comments = ["was timestamp"]}
    let (|StartsWithAfter|_|) (d:string) (s:string) = 
        if s.StartsWith(d) then
            Some (s |> after d)
        else None
        
    let (|CreateColumn|CreateFKeyed|MakeNullable50|CreateUserIdColumn|CreatePatientIdColumn|) (columnText:string) = 
        match columnText with
        | StartsWithAfter "new ColumnInfo" s -> CreateColumn (s)
        | StartsWithAfter "CreateFKeyedColumn<" s ->
            
            let t = s |> before ">"
            CreateFKeyed (t, s |> after t)
        | StartsWithAfter "MakeNullable50" s -> MakeNullable50 (s |> after "\"" |> before "\"")
        | StartsWithAfter "CreateUserIdColumn" s -> CreateUserIdColumn ( s|> after "(" |> before ")" |> String.split [","],columnText)
        | StartsWithAfter "CreatePatientIdColumn" s -> CreatePatientIdColumn (s |> after "(" |> before ")" |> String.split [","],columnText)
        | _ -> failwithf "No match for %s" columnText
    let mapBoolToNullability (b:string) = 
        match b with
        | "true" -> "AllowNull"
        | "false" -> "NotNull"
        | _ -> failwithf "unexpected b: %s" b
    let text = text |> after "Columns" 
    text
    |> regexMany columnStarters None
    |> Seq.cast<Match> 
    |> Seq.map (fun m -> m.Index) 
    |> outerPairs 
    |> Seq.map (getColumnText text)
    |> Seq.map (function 
        |CreateColumn s -> ColumnCreateTypes.CreateColumn s 
        |CreateFKeyed (t,s) -> 
            let s = s.Trim(',')
            
            let (|SimpleFCreate|SimpleNCreate|CreateNWithComment|) (s:string) = 
                // based on all 3 overloads if there are args, the first is a bool, if there is another, it is string
                match regex "}\s*,.*(true|false).*(\".*\")?" None s with
                | Matched m ->
                    match m.Groups.[2].Success with
                    | true -> CreateNWithComment (m.Groups.[1].Value, m.Groups.[2].Value)
                    | false -> SimpleNCreate (m.Groups.[1].Value)
                | _ -> SimpleFCreate

            let simpleCreateText = 
                let columnText = 
                    match matchSimpleStringAssignment "Column" s with 
                    | Matched _ -> getSimpleStringAssignment "Column" s |> wrap "\"" 
                    | _ -> "null"
                (sprintf "ColumnInput.createFKeyedNColumn<%s> \"%s\" { Schema=\"%s\"; Table=\"%s\"; Column = %s }" t (s|> after "\"" |> before "\"") (getSimpleStringAssignment "Schema" s) (getSimpleStringAssignment "Table" s) columnText)
            
            match s with
            | SimpleFCreate -> ColumnCreateTypes.Translated (simpleCreateText,s)
            | SimpleNCreate b -> sprintf "{ %s with AllowNull = Nullability.%s}" simpleCreateText (mapBoolToNullability b) |> fun x ->  ColumnCreateTypes.Translated(x,s)
            | CreateNWithComment (b,c) -> sprintf "{ %s with AllowNull = Nullability.%s;Comment=[%s]}" simpleCreateText (mapBoolToNullability b) c |> fun x ->  ColumnCreateTypes.Translated(x,s)
//                
//            match s |> after "}" |> wordify |> Seq.containsAnyOF columnInputPropNames || s |> after "}" |> containsAnyOf ["true";"false";","] with
//            | false ->
//            
//            // <int>("ChargeID", new FKeyInfo{Schema="dbo",Table="Charge"},true),
//            // ->
//            //ColumnInput.createFKeyedNColumn<int> "PayerID" { Schema="dbo"; Table="Payers"; Column = null } 
//                
//            | true -> ColumnCreateTypes.CreateFKeyed (t,s)
            
        |MakeNullable50 s -> sprintf "ColumnInput.makeNullable50 \"%s\"" s |> (fun x -> ColumnCreateTypes.Translated(x,s))
        |CreateUserIdColumn (args,columnText) -> 
            //public static ColumnInfo CreateUserIdColumn(string prefix, bool allowNull, string comment)
            // ->
            //ColumnInput.createUserIdColumn null Nullability.AllowNull ["null to allow system inserts/adjustments that aren't done by a user"]
            sprintf "ColumnInput.createUserIdColumn %s Nullability.%s [\"%s\"]" args.[0] (args.[1] |> trim |> mapBoolToNullability) (args.[2] |> trim |> trimc '"')
            |> fun x -> ColumnCreateTypes.Translated(x,columnText)
            //ColumnCreateTypes.CreateUserIdColumn args
        |CreatePatientIdColumn (args,columnText) -> 
            //CreatePatientIdColumn(null, true,null),
            // ->
            //
            sprintf "ColumnInput.createPatientIdColumn %s Nullability.%s %s" args.[0]
            ColumnCreateTypes.CreatePatientIdColumn args
        )
    |> List.ofSeq
        
let text = """
        {
            new TableInfo
            {
                Name="Payment",
                Schema="dbo",
                Columns = new []
                {
                    new ColumnInfo{Name="PaymentID", Type = typeof(int), 
                        Attributes = new []{"identity","primary key"},
                        },
                    CreateFKeyedColumn<int>("AppointmentId", new FKeyInfo{Schema="dbo",Table="Appointments",Column="AppointmentId"}, /* allowNull= */ true),
                    new ColumnInfo
                    {
                        Name="PaymentTypeId", Type= typeof(string), Length=50,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentType",Column="PaymentTypeId" },
                        GenerateReferenceTable = true,
                        ReferenceValuesWithComment= new Dictionary<string,string>{
                            {"Patient",null},{"ThirdParty",null},
                            {"Era",null}
                            },
                        Comments= new[]{
                            "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                            }
                        },
                    new ColumnInfo{
                        Name="PaymentMethodId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentMethod"},
                        ReferenceValuesWithComment = new Dictionary<string,string>{
                            {"Cash",null},{"CC",null},{"Check",null},{"Ach",null},{"Fsa",null},{"Other","for when Era amount is 0 or a catch-all"}
                            },
                        },
                    new ColumnInfo{
                        Name="PaymentStatusId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentStatus"},
                        ReferenceValuesWithComment = new []{"New", "Partial", "Complete"}.ToDictionary(f=>f,f=> (string)null),
                        },
                    new ColumnInfo{
                        Name="TotalAmount", Type = typeof(decimal),
                        Precision=12,Scale=2, // see: http://stackoverflow.com/questions/2377174/how-do-i-interpret-precision-and-scale-of-a-number-in-a-database
                        Comments = new[]{ "was Amount (18,2)"}
                        },
                    CreateUserIdColumn(null, true, "null to allow system inserts/adjustments that aren't done by a user"),
                    CreateFKeyedColumn<int>("PayerID", new FKeyInfo{ Schema="dbo", Table="Payers" }, /* allowNull= */ true),
                    CreatePatientIdColumn(null, true,null),
                    new ColumnInfo{
                        Name="Created", Type = typeof(DateTime),
                        AllowNull=true,
                        Comments = new[]{ "was timestamp"}
                        },
                    new ColumnInfo{
                        Name="TransactionNumber", Type = typeof(string),
                        Length=30,
                        AllowNull=true,
                        Comments = new[]{ "was checkNumber now will store check number or ACH number (when applicable)"}
                        },
                    new ColumnInfo{
                        Name="Rcd", Type = typeof(DateTime),
                        Comments = new []{"Payment Recvd"},
                        AllowNull=true,
                        },
                    new ColumnInfo{
                        Name="IsElectronic", Type = typeof(bool),
                        },
                    CreateFKeyedColumn<int>("CCItemID", new FKeyInfo{ Schema="Accounts", Table="CCItem"},true),
                    new ColumnInfo{
                        Name="Comments", Type = typeof(string),
                        UseMax=true,
                        AllowNull=true,
                        },
                    }
                },
            new TableInfo{
                Schema="Accounts",
                Name="CCItem",
                Columns = new []
                {
                    new ColumnInfo{
                        Name="CCItemID", Type = typeof(int), Attributes = new []{"identity","primary key"},
                        },
                    MakeNullable50("ResponseCode"),
                    MakeNullable50("ResponseDescription"),
                    MakeNullable50("TransactionID"),
                    MakeNullable50("TransactionType"),
                    MakeNullable50("CardType"),
                    MakeNullable50("MaskedAcctNum"),
                    MakeNullable50("ExpDate"),
                    MakeNullable50("AcctNumSource"),
                    MakeNullable50("CardholderName"),
                    MakeNullable50("Alias"),
                    MakeNullable50("ProcessorResponse"),
                    MakeNullable50("BatchNum"),
                    MakeNullable50("BatchAmount"),
                    MakeNullable50("ApprovalCode"),
                    }
                },
            new TableInfo{
                Schema="Accounts",
                Name="PaymentItem",
                Columns = new []{
                    new ColumnInfo{ Name = "PaymentItemID", Type = typeof(int), Attributes = new []{"identity","primary key"}},
                    new ColumnInfo{ Name = "PaymentID", Type = typeof(int), FKey= new FKeyInfo{Schema="dbo",Table="Payment"}},
                    new ColumnInfo{ Name = "PaymentItemTypeId", Type = typeof(string), Length=50,
                        AllowNull=true,
                        GenerateReferenceTable=true, FKey= new FKeyInfo{Schema="Accounts", Table="PaymentItemType", Column="PaymentItemTypeId"},
                        ReferenceValuesWithComment = new []{"EraPayment", "EraAdjustment", "PtRespDeductible", "PtRespCoPay","PtRespCoIns","Other"}.ToDictionary(f => f, f => (string)null),
                    },
                    new ColumnInfo{
                        Name="PaymentTierId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        AllowNull=true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentTier",Column="PaymentTierId" },
                        ReferenceValuesWithComment = new []{"Primary", "Secondary", "Tertiary", "Worker'sComp"}.ToDictionary(f=>f,f=> (string)null),
                        },
                    new ColumnInfo{
                        Name="PtRespTypeId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        AllowNull=true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PtRespType",Column="PtRespTypeId" },
                        ReferenceValuesWithComment = new []{"Deductible", "CoIns", "CoPay"}.ToDictionary(f=>f,f=> (string)null),
                        },
                    new ColumnInfo{ Name = "Created", Type= typeof(DateTime)},
                    new ColumnInfo{ Name = "Amount", Type= typeof(decimal), Precision=8, Scale=2},
                    new ColumnInfo{ Name = "PatientResponsiblityAmt", Type = typeof(decimal), Precision=8, Scale=2},
                    CreateFKeyedColumn<int>("ChargeID", new FKeyInfo{Schema="dbo",Table="Charge"},true),
                    MakeNullable50("RemarkCode"),
                    MakeNullable50("AdjustmentCode"),
                    new ColumnInfo{ Name = "PaymentItemStatusId", Type = typeof(string), Length=50,
                        GenerateReferenceTable=true, FKey= new FKeyInfo{Schema="Accounts", Table="PaymentItemStatus"},
                        ReferenceValuesWithComment = new []{"Posted", "Unposted"}.ToDictionary(f=>f,f=> (string)null),
                    },
                    new ColumnInfo{
                        Name="Comments", Type = typeof(string),
                        UseMax=true,
                        AllowNull=true,
                    },
                }
            }
        }
"""

text
|> String.split ["new TableInfo"]
|> Seq.filter(matchSimpleStringAssignment "Name" >> (fun x -> x.Success))
|> Seq.map (fun text -> getSimpleStringAssignment "Schema" text, getSimpleStringAssignment "Name" text, text)
|> Seq.map (fun (name,schema,text) -> name,schema, getColumns text)
|> Dump
|> ignore
//|> Seq.map (fun (name, schema, columnMatches, text) -> name,schema, columnMatches |> Seq.cast<Match> |> List.ofSeq |> outerPairs)


