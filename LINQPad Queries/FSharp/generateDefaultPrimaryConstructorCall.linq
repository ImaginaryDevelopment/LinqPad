<Query Kind="FSharpProgram">
  <NuGetReference>FParsec</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

//take a list of arguments (typically a large class' primary constructor) and generate a call to it with all default arguments script
type GenStrategy = 
    |UseUnchecked
    |UseDefaults

type Naming = 
    |Named
    |Ordinal
    
type ParseStrategy = 
    |Split // works great if there are no 2+ term generics
    |Regex // alpha does not work very well at all
    |Parsec // not implemented
let dumpt t x = x.Dump(sprintf "%s" t)
let constructorArgs = """userId : Guid, originalUserId : Guid, userDetailId : int64, originalUserDetailId : int64, userFirstName : string, userLastName : string, userEmailAddress : string, invoiceId : Nullable<int64>, trackingNumber : string, invoiceNumber : int64, invoiceDate : DateTime, companyId : int64, companyName : string, companyAddressLine1 : string, companyAddressLine2 : string, companyCity : string, companyState : string, companyPostalCode : string, companyOfficePhone : string, companyFax : string, billToCustomerId : Nullable<int64>, billToCompanyName : string, billToContactName : string, billToEmailAddres : string, billToAddressLine1 : string, billToAddressLine2 : string, billToCity : string, billToState : string, billToZip : string, shipFromCustomerId : Nullable<int64>, shipFromCompanyName : string, shipFromContactName : string, shipFromEmailAddress : string, shipFromAddressLine1 : string, shipFromAddressLine2 : string, shipFromCity : string, shipFromState : string, shipFromZip : string, shipToCustomerId : Nullable<int64>, shipToCompanyName : string, shipToContactName : string, shipToEmailAddress : string, shipToAddressLine1 : string, shipToAddressLine2 : string, shipToCity : string, shipToState : string, shipToZip : string, invoiceMessage : string, signature : byte [], surchargePercentage : Nullable<decimal>, surchargeAmount : Nullable<decimal>, driverCommissionPercentage : Nullable<decimal>, driverCommissionAmount : Nullable<decimal>, discountPercentage : Nullable<decimal>, discountAmount : Nullable<decimal>, subTotal : Nullable<decimal>, total : Nullable<decimal>, status : string, defaultRate : Nullable<decimal>, created : DateTimeOffset, deleted : DateTimeOffset, ratesMatch:bool, submittedToReceiver:Nullable<bool>, submittedToReceiverDate:Nullable<DateTime>"""
//TODO: split parse incase of comma in type def (account for <T,T2> for example)

let parse = 
    function
        | Split -> constructorArgs.Split(',') |> Seq.map (fun p -> p.Split ':' |> fun a -> a.[0],a.[1])
        | Regex -> Regex.Matches(constructorArgs, @"(\w+)\s*:\s*(\w+\s*(\w+)?)\s*,\s*") |> Seq.cast<Match> |> Seq.map (fun m -> m.Groups.[1].Value, m.Groups.[2].Value)
    >> Seq.map (fun (name,t) -> name.Trim(), t.Trim())
    
let args = parse Split
    //constructorArgs.Split(',')  //
args.Dump()

let mapArg s n (name,t) = 
    let unchecked = "Unchecked.defaultof<_>"
    let n = match n with | Named -> sprintf "%s:" name | Ordinal -> String.Empty
    match s with
    | UseUnchecked -> unchecked
    //| UseUncheckedNamed -> sprintf "%s:%s" name unchecked
    | UseDefaults -> 
        match t with
        | "string" -> "null"
        | n when n.StartsWith("Nullable") -> "Nullable()"
        | "DateTime" -> "DateTime.MinValue"
        | "DateTimeOffset" -> "DateTimeOffset.MinValue"
        | "bool" -> "false"
        | "int64" -> "0L"
        | "Guid" -> "Guid.Empty"
        | "byte []" -> "Array.empty"
    |> sprintf "%s%s" n
args
|> Seq.map (mapArg UseDefaults Ordinal)
|> Array.ofSeq
|> fun a -> String.Join(", ",a)
|> Dump