<Query Kind="FSharpProgram" />

// generate the equivalent Partial<'T> from a record declaration

module Helpers =     
        
    type String with
        static member trim (x:string) = 
            x.Trim()
        static member trim1 (d:string) (x:string) = 
            x.Trim(Array.ofSeq d)
        static member split (d:string) (x:string) = 
            x.Split(Array.singleton d, StringSplitOptions.None)
        static member before (d:string) (x:string) = 
            x.[0.. x.IndexOf d - d.Length ]
        static member after (d:string) (x:string) = 
            x.[x.IndexOf d + d.Length .. ]
    let replace (d:string) (r:string) (x:string) = x.Replace(d,r)
    let delimit (d:string) (x:string seq) = String.Join(d, values=x)
    module StringPatterns = 
        let (|Contains|_|) (d:string) (x:string) = 
            if x.Contains d then
                Some ()
            else None
        let (|BeforeXEquals|_|) (d:string) (equalValue:string) (x:string) = 
            if x.Contains d then
                if x |> String.before d |> (=) equalValue then
                    Some ()
                else None
            else None
        let (|EndsWith|_|) (d:string) (x:string) = if x.EndsWith d then Some() else None
        let (|StartsWith|_|) (d:string) (x:string) = if x.StartsWith d then Some() else None
        let (|Before|_|) (d:string) (x:string) = if x.Contains d then x |> String.before d |> Some else None
    open StringPatterns
    let inline getValue x = (^T: (member Value: _) x)
    // is this worth using?
    let inline getItem i x = (^T: (member get_Item: _ -> _) (x,i))
    let (|RMatch|_|) pattern x = 
        let r = Regex.Match(x,pattern)
        if r.Success then Some r else None
open Helpers
open Helpers.StringPatterns

type ConvertType<'T> =
    | Type of 'T
    | RecordText of string

//module Seq = 
//    let materialize = List.ofSeq >> Seq.ofList

module Sample =     
    [<Measure>] type PaymentId
    [<Measure>] type PayerId
    [<Measure>] type ApptId
    [<Measure>] type ChargeId
    type EraItem = {ChargeId:int<ChargeId>;AppointmentId:int<ApptId> Nullable; UInt:uint16; IsTest:bool; Count:int; Count64: System.Int64; Test:int16<PaymentId>; IsNullable: Nullable<int>; IsNullable2: int Nullable; IsOpt: string option; IsOpt2: Option<string>}    
let text = "type EraItem = {ChargeId:int<ChargeId>;AppointmentId:int<ApptId> Nullable; UInt:uint16; IsTest:bool; Count:int; Count64: System.Int64; Test:int16<PaymentId>; IsNullable: Nullable<int>; IsNullable2: int Nullable; IsOpt: string option; IsOpt2: Option<string>}" |> replace "}" ";Name : string ;}"

module StackSample1 = 
    let inline getLength s = (^a: (member Length: _) s)
    //usage:
    getLength "Hello World" // or "Hello World" |> getLength
    |> ignore<int>
    // returns 11

module TypeParsing = 
    type ValueTypeInfo = {Name:string;MeasureType:string option}

    type TypeType =
        |ValueType of ValueTypeInfo
        |RefType of string
        |AlreadyPartial of string
         with
            member x.ToDump() = sprintf "%A" x
    let (|MeasurableValueType|_|) = 
        function
        | RMatch @"(int|int32|int64|float|decimal)(<(\w+)>)?" r ->
            {Name=r.Groups.[1].Value; MeasureType = if r.Groups.[3].Success then r.Groups.[3].Value |> Some else None}
            |> Some
        | _ -> None
    
    let (|IsValueType|_|) = 
        function
        | MeasurableValueType _ -> Some ()
        // value types that can't have measures
        | "bool"
        | "uint16"
        | "uint32"
        | "uint64"
        | "System.Boolean"
        | "System.Int16"
        | "System.Int32"
        | "System.Int64"
        | "System.Decimal"
        
        | "byte" -> Some ()
        
        | _ -> None

open TypeParsing 

let convertFromText text = 
    let typeName, items = Regex.Match(text,"type\s+(\w+)\s*=\s*{\s*(\w+\s*:\s*[^;]+\s*;?\s*)+}") |> fun r -> r.Groups |> getItem 1 |> getValue , r.Groups.[2].Captures |> Seq.cast<Capture> |> Seq.map(getValue >> String.split ":" >> Seq.map (String.trim>>String.trim1 ";" )>> Seq.pairwise)
    let mapType = // we can not differentiate between nullable ref types and non-nullable ref types, so lets make them all options for safety
        function
        | StartsWith "Nullable<"
        | StartsWith "Option<"
        | EndsWith " Nullable"
        | EndsWith " option" as x -> AlreadyPartial x
        | MeasurableValueType x ->
            ValueType x
        | IsValueType as x -> ValueType {Name=x;MeasureType=None}
        | x -> 
            //printfn "Why u no match? %s" x
            RefType x
            
    let generateType = 
        function
        | ValueType {Name=x;MeasureType=None} ->
            sprintf "%s option" x
        | ValueType {Name=x;MeasureType=Some m} ->
            sprintf "%s<%s> option" x m
        | AlreadyPartial x -> x
        | RefType x ->
            sprintf "%s option" x
            
    let props = items
    let mappedItems = items |> Seq.collect id |> Seq.map (fun (x,t) -> x,mapType t |> generateType, mapType t, t) |> List.ofSeq
    mappedItems.Dump()
    sprintf "type %sPartial = {%s}" typeName (mappedItems |> Seq.map(fun (name,t,_,_) -> sprintf "%s : %s" name t) |> delimit"; ")

convertFromText text
|> Dump
|> ignore
