<Query Kind="FSharpProgram">
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Dal.dll</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Schema.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

open Pm.Schema.DataModels.Payments
let cast<'T> (x:obj) = x :?> 'T
let dbNull = box System.DBNull.Value
module Option = 
    let getValueOrDefault (n: 'a option) = match n with | Some x -> x | None -> Unchecked.defaultof<_>
    let toNullable = function | Some x-> Nullable x | None -> Nullable()
let stringEqualsI x y = String.Equals(x,y, StringComparison.CurrentCultureIgnoreCase)    
let toOption (x:Nullable<_>) =  if x.HasValue then Some x.Value else None
let FillDataFromReader (r:System.Data.IDataRecord) = 
    PaymentHelpers.FromF(Func<_,_>(fun s -> Pm.Dal.AdoHelper.dbNullToOption r.[s]))
let oldMethod r = 
    let adjustmentReasonPacked = Pm.Dal.AdoHelper.getRecordOptT<string> r "adjustmentcode"
    let adjustmentReasonCodeOpt = 
        match adjustmentReasonPacked with
        | Some x -> (x.BeforeOrSelf " " |> System.Int32.Parse |> Nullable)
        | _ -> Nullable()
    let adjustmentDescription : string = 
        match adjustmentReasonPacked with
        | Some null -> null
        | Some x when x.Contains " - " -> 
            x.After(" - ") 
        | _ -> 
            Pm.Dal.AdoHelper.getRecordOptT<string> r "adjustmentReasonDescription" |> Option.getValueOrDefault
    ()
let newMethod r = 
    let adjustmentReasonPacked = Pm.Dal.AdoHelper.getRecordOptT<string> r "adjustmentcode"
    let adjustmentReasonCodeOpt = Pm.Dal.DataAccess.PaymentsModule.tryUnpackAdjustmentCode(adjustmentReasonPacked)
    ()
let toDataRecord (x:(string*obj) list) = 
    {new System.Data.IDataRecord with
        member __.FieldCount = 0
        member __.GetBoolean i = failwith "Not Implemented"
        member __.Item with get(index:int) : obj = failwith "Not Implemented"
        member __.Item 
            with get(name:string) : obj = 
                x 
                |> Seq.tryFind (fst >> stringEqualsI name) 
                |> Option.map snd |> function | Some null -> dbNull | Some x -> x | None -> dbNull
        member __.GetName i = failwith "Not Implemented"
        member __.GetDataTypeName i = failwith "Not Implemented"
        member __.GetFieldType i = failwith "Not Implemented"
        member __.GetValue i = failwith "Not Implemented"
        member __.GetValues _ = failwith "Not Implemented"
        member __.GetOrdinal _ = failwith "Not Implemented"
        member __.GetByte _ = failwith "Not Implemented"
        member __.GetBytes(_,_,_,_,_) = failwith "Not Implemented"
        member __.GetChar _ = failwith "Not Implemented"
        member __.GetChars(_,_,_,_,_) = failwith "Not Implemented"
        member __.GetGuid _ = failwith "Not Implemented"   
        member __.GetInt16 _ = failwith "Not Implemented"
        member __.GetInt32 _ = failwith "Not Implemented"
        member __.GetInt64 _ = failwith "Not Implemented"
        member __.GetFloat _ = failwith "Not Implemented"
        member __.GetDouble _ = failwith "Not Implemented"
        member __.GetString _ = failwith "Not Implemented"
        member __.GetDecimal _ = failwith "Not Implemented"
        member __.GetDateTime _ = failwith "Not Implemented"
        member __.GetData _ = failwith "Not Implemented"
        member __.IsDBNull _ = failwith "Not Implemented"
    }
let testData = [
    ["adjustmentcode", box null]
    ["adjustmentcode", box "B16"]
]
let splitTest x = 
    let data = toDataRecord x
    let old = 
        try
            oldMethod data
            |> Choice1Of2
        with ex -> Choice2Of2 ex
    let funNew = 
        newMethod data
    x,old,funNew

testData
|> Seq.map splitTest
|> Dump
|> ignore
