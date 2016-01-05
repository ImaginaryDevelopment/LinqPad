<Query Kind="FSharpProgram" />

// reverse engineer sql file into generator

// assume foreign keys of type VARCHAR(50) are single column reference tables
// do not attempt to account for multi-line comments or lines that are proceeded by comments

(* helpers  *)
let trim (s:string) = if isNull s then null else s.Trim()
let after (delimiter:string) (s:string) = s.Substring(s.IndexOf delimiter + delimiter.Length)
let before (delimiter:string) (s:string) = s.Substring(0,s.IndexOf delimiter)
let beforeI (delimiter:string) (s:string) = s.Substring(0,s.IndexOf(delimiter, StringComparison.InvariantCultureIgnoreCase))
let doubleQuote s = "\"" + s + "\""
let dumpCont t = 
    t.Dump()
    t
let isRegexMatchI (pattern:string) (s:string) = Regex.IsMatch(s,pattern, RegexOptions.IgnoreCase)
let captureI (pattern:string) (s:string) = Regex.Match(s, pattern,RegexOptions.IgnoreCase).Groups.[1].Value
let capture2I (pattern:string) (s:string) = 
    Regex.Match(s,pattern, RegexOptions.IgnoreCase)
    |> fun m -> m.Groups.[1].Value, m.Groups.[2].Value
//let capture4 (pattern:string) (s:string) = 
//    Regex.Match(s,pattern)
//    |> fun m -> m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value, m.Groups.[4].Value
let capture5I (pattern:string) (s:string) =
    Regex.Match(s,pattern, RegexOptions.IgnoreCase)
    |> fun m -> m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value, m.Groups.[4].Value, m.Groups.[5].Value
let stringJoin (delimiter:string) (items:#seq<string>) = String.Join(delimiter,items)
let flip f x y = f y x

let (|EqualsI|_|) matchArg input = if String.Equals(input,matchArg, StringComparison.InvariantCultureIgnoreCase) then Some () else None
let (|StartsWithI|_|) matchArg (input:string) = if input.StartsWith(matchArg, StringComparison.InvariantCultureIgnoreCase) then Some() else None

let (|Integer|Bit|VarChar|Money|Datetime|) (s:string) =
        match s with
        | EqualsI "int" -> Integer
        | EqualsI "bit" -> Bit
        | EqualsI "datetime" -> Datetime
        | StartsWithI "varchar" -> VarChar (captureI @"varchar\s*\(\s*([0-9]+|MAX)\s*\)" s)
        | StartsWithI "decimal" -> Money(capture2I @"decimal\s*\(\s*([0-9]+)\s*,\s*([0-9]+)\s*\)" s)
        | _ -> failwithf "Failed to match type for %s" s

let getTableName filename = Path.GetFileNameWithoutExtension(Path.GetFileNameWithoutExtension(filename))

let target = @"C:\TFS\PracticeManagement\devDb\Schema Objects\Schemas\dbo\Tables\Payment.table.sql" // C:\TFS\PracticeManagement\devDb\Schema Objects\Schemas\dbo\Tables\Payment.table.sql
let tableName = getTableName target

let readColumns (lines:#seq<string>) = 
    lines
    |> Seq.map trim
    |> Seq.filter (fun s-> s.StartsWith("["))    
    |> Seq.map (fun s-> s |> after "[" |> before "]", s |> after "]" |> captureI @"\s+(\w+\s*(\(([0-9, ]+|MAX)\))?)\s*", s)
    |> Seq.map (fun (columnName, columnType, line) -> columnName,columnType |> trim,line |> after columnType |> trim)
    //|> Seq.map (fun (columnName, columnType, constraints)
    |> Array.ofSeq

let columns = 
    Util.Cache( 
        fun () -> 
        File.ReadAllLines target
        )
        |> readColumns
        //|> dumpCont
    
let mapColumnType = function
        | Integer -> "int"
        | Bit -> "bool"
        | Datetime -> "DateTime"
        | VarChar _ -> "string"
        | Money _ -> "decimal"
        
let parseConstraints (s:string) = 
    let comments = if s.Contains("--") then s |> after "--" |> trim |> Some else None
    let parseConstraint constraint' = 
           constraint'
           //|> dumpCont
           |> capture5I @"\[FK_(\w+)_(\w+)_(\w+)_(\w+)\] FOREIGN KEY \(\[\w*\]\) REFERENCES \[(\w+)\]\.\[\w+\] \(\[\w+\]\)"
           |> fun (tableName, targetColumn, fKeyTable, fKeyColumn, fKeySchema) -> sprintf """FKey=new FKeyInfo{ Schema="%s", Table="%s",Column="%s" },""" fKeySchema fKeyTable fKeyColumn
    seq{
        let isIdentity = isRegexMatchI @"\bidentity\b" s
        let isPk = isRegexMatchI @"\bprimary key\b" s
        let nullable = not <| isRegexMatchI @"(\b|^)not\s+null\b" s && not isPk
        //(isIdentity,isPk,nullable,s).Dump()
        match isIdentity || isPk with
        | true -> 
            let items = []
            let items = if isIdentity then "identity"::items else items
            let items = if isPk then "primary key"::items else items
            yield sprintf """Attributes = new []{%s},""" (items |> Seq.map doubleQuote |> stringJoin ",")
        | false -> ()
        if nullable then
            yield "AllowNull=true,"
        if s.Contains("CONSTRAINT ") then
            let cleaned = 
                s
                |> after "CONSTRAINT "
            yield parseConstraint cleaned
        match comments with
        | Some comments -> yield sprintf """Comments = new[]{ "%s"}""" comments
        | None -> ()
    }
    |> Array.ofSeq
    
let generateColumn startingIndentation (columnName,columnType,constraints) =
    let indent x = sprintf "%s%s" startingIndentation ([0..x] |> Seq.map(fun _ -> "    ") |> stringJoin String.Empty)
    try
        columnType,constraints,seq {
            yield sprintf "%snew ColumnInfo{" (indent 0) 
            
            yield sprintf "%sName=\"%s\", Type = typeof(%s)," (indent 1) columnName (mapColumnType columnType)
            match columnType with
            | VarChar l -> 
                let propText = match l with |EqualsI "max" l -> "UseMax=true" | _ -> sprintf "Length=%s" l
                
                yield sprintf "%s%s," (indent 1) propText
                if l ="50" then
                    yield sprintf "%sGenerateReferenceTable = true," (indent 1)
            | Money (p,s) -> yield sprintf "%sPrecision=%s,Scale=%s," (indent 1) p s
            | _ -> ()
            yield! (parseConstraints constraints |> Seq.map (sprintf "%s%s" (indent 1)))
            yield sprintf "%s}" (indent 0)
        }

        |> Array.ofSeq
    with ex -> 
        ex.Data.Add("ColumnName",columnName)
        ex.Data.Add("ColumnType",columnType)
        ex.Data.Add("Constraints", constraints)
        reraise()
    
columns
|> dumpCont
|> Seq.map (generateColumn "                ")
|> dumpCont
|> Seq.map (fun (_, _, lines) -> lines)
|> Seq.map (stringJoin Environment.NewLine)
|> stringJoin (","+Environment.NewLine)
//|> Seq.map (stringJoin Environment.NewLine)
|> Dump
    (* 
    new ColumnInfo{Name="PaymentID", Type = typeof(int), 
                        Attributes = new []{"identity","primary key"},
                        },
                    new ColumnInfo
                        {
                        Name="PaymentTypeId", Type= typeof(string), Length=50,
                        FKey=new FKeyInfo{ Schema="Payments", Table="PaymentType",Column="PaymentTypeId" },
                        GenerateReferenceTable = true,
                        Comments= new[]{
                                "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod",
                                "Patient, ThirdParty or Era in this column"
                            }
                        },
                        *)
    
//let generateTable startingIndentation (columnName,columnType,line) =
//    printfn "%snew TableInfo" startingIndentation
//    printfn "%s    {" startingIdentation
//    printfn "%s        Name=\"%s\"," 
    
    

(* target example:

                    new TableInfo
            {
                Name="Payment",
                Schema="dbo",
                Columns = new []
                {
                    new ColumnInfo
                    {
                        Name="PaymentMethodId", Type=typeof(string), Length=50,
                        FKey=new FKeyInfo{ Schema="Payments", Table="PaymentMethod", Column="PaymentMethodId" },
                        GenerateReferenceTable = true,
                        Comments = new[]{ "(CC/Check/Ach/Other)"}
                    }
                }
            }
                        
*)