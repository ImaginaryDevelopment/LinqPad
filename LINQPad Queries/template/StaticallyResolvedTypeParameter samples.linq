<Query Kind="FSharpProgram" />

// generate the equivalent Partial<'T> from a record declaration
type String with
    static member trim (x:string) = 
        x.Trim()
    static member split (d:string) (x:string) = 
        x.Split(Array.singleton d, StringSplitOptions.None)
type ConvertType<'T> =
    | Type of 'T
    | RecordText of string
let inline getValue x = (^T: (member Value: _) x)
let inline getItem i x = (^T: (member get_Item: _ -> _) (x,i))


module Sample =     
    [<Measure>] type PaymentId
    [<Measure>] type PayerId
    [<Measure>] type ApptId
    [<Measure>] type ChargeId
    type EraItem = {ChargeId:int<ChargeId>;AppointmentId:int<ApptId>}    
let text = "type EraItem = {ChargeId:int<ChargeId>;AppointmentId:int<ApptId>}"
module StackOverflowDocumentation = 
    // Record
    type Ribbon = {Length:int}
    // Class
    type Line(len:int) = 
        member x.Length = len
    type IHaveALength =
        abstract Length:int
    
    let inline getLength s = (^a: (member Length: _) s)
    let ribbon = {Length=1}
    let line = Line(3)
    let someLengthImplementer =
        { new IHaveALength with
            member __.Length = 5}
    printfn "Our ribbon length is %i" (getLength ribbon)
    printfn "Our Line length is %i" (getLength line)
    printfn "Our Object expression length is %i" (getLength someLengthImplementer)
module StackSample1 = 
    let inline getLength s = (^a: (member Length: _) s)
    //usage:
    getLength "Hello World" // or "Hello World" |> getLength
    // returns 11
// doesn't compile alone, not sure what I was trying to do here:
// ((^a : (static member GetLength : int) ()))


let convert = 
    function
    | RecordText text -> // \s*[^;]+;)+}
        let typeName, items = Regex.Match(text,"type\s+(\w+)\s*=\s*{\s*(\w+\s*:\s*[^;]+;?)+}") |> fun r -> r.Groups |> getItem 1 |> getValue (* r.Groups.[1].Value *), r.Groups.[2].Captures |> Seq.cast<Capture> |> Seq.map(getValue >> String.split ":" >> Array.map String.trim)
        typeName, items

convert (RecordText text)
|> Dump
|> ignore
