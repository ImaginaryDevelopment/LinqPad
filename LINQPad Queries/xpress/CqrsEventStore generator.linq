<Query Kind="FSharpProgram" />

// generate cqrs boilerplate
module Helpers = 
    let curry f x y = f (x, y)
    let uncurry f (x,y) = f x y
    let uncurry1 f (x,y,z) = f x y z
    let uncurry2 f (a,b,c,d) = f a b c d
    let delimit (delim:string) x = String.Join(delim,value=(x |> Array.ofSeq))
    let (|TypeDefOf|_|) (_:'a) t =
        if t = typedefof<'a> then Some() else None
    let (|TypeOf|_|) (_:'a) t =
        if t = typeof<'a> then Some ()
        else
            //printfn "did not match %A to %A" typeof<'a> t ;
            None
    let isType<'a> = Unchecked.defaultof<'a>
    let mapNonWhitespaces f x = 
        if String.IsNullOrWhiteSpace x then
            x
        else f x
    let indent delim (x:string) = 
        x.Split([| "\r\n";"\n";"\r"|], StringSplitOptions.None)
        |> Seq.map(mapNonWhitespaces(sprintf "%s%s" delim)) 
        |> delimit "\r\n"
open Helpers
module Generation = 
    // use this placeholder for serializing a partial update
    type PartialUpdate = | PartialUpdate
    // identity keys to other rows of the same table
    type SelfIdReference = | SelfIdReference
    type RecordType = 
        | Regular
        | Partial

    let generateProp forceOption useMeasure typeName name (p:Type) = 
        let fMeasure = lazy(if useMeasure then sprintf "int<%sId>" typeName else "int")
        match p with
        | TypeOf(isType:SelfIdReference) ->        
            fMeasure.Value
        | TypeOf(isType:SelfIdReference option) ->
            sprintf "%s option" fMeasure.Value
        | _ -> p.Name
        |> function
            |x when forceOption -> sprintf "%s option" x
            | x -> x
        |> sprintf "%s:%s" name
    let generatePartialRecordValue propName = sprintf "%s=None" propName
    let generateRecord rType useMeasure typeName (properties:PropertyInfo seq) :string list = 
        let isPartial = match rType with | Partial -> true | _ -> false
        let trailer = 
            if isPartial then
                " with"
            else ""
        [
            yield 
                properties
                |> Seq.map (fun p -> generateProp isPartial useMeasure typeName p.Name p.PropertyType) 
                |> delimit ";"
                |> fun x -> sprintf "type %s = { %s }%s" (if isPartial then sprintf "Partial%s" typeName else typeName) x trailer
            if isPartial then
                yield
                    properties
                    |> Seq.map (fun p -> generatePartialRecordValue p.Name)
                    |> delimit ";"
                    |> sprintf "    static member Zero = {%s}" 
        ]
    let generate abbrev useMeasure (t:Type) (commandDU:obj) : string list = 
        let props = t.GetProperties ()
        [   
            if useMeasure then yield (sprintf "type [<Measure>] %sId" t.Name)
            yield! (generateRecord Regular useMeasure t.Name props)
            yield! (generateRecord Partial useMeasure t.Name props)
            yield sprintf "let es%s =" abbrev
            yield "    //TODO: let validation (v:EventsToValidate) = "
            yield sprintf "    cInMemory<%s> validation" t.Name]
    // TODO: partial update function
    let generatePartialUpdate () = ()
// end generation code section

// begin sample usage
module Patients = 
    type Patient = { LastName:string; FirstName:string; DoB:DateTime;Guarantor:Generation.SelfIdReference option} 

let types = [
    "Pt", true, typeof<Patients.Patient>, typeof<Patients.PatientCommand>
]
types.[0]
|> uncurry2 (Generation.generate)
|> delimit "\r\n"
//|> indent "    "
|> Dump
|> ignore
