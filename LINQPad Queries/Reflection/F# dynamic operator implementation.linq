<Query Kind="FSharpProgram" />

// demonstrate F#'s dynamic operator
// usage examples reimplemented from http://codebetter.com/matthewpodwysocki/2010/02/05/using-and-abusing-the-f-dynamic-lookup-operator/
type Nest(value:string) =
    member val Value = value with get,set
type Test(code:string) = 
    let mutable code = code
    member val Code = code with get,set
    member val private Code2 = code with get,set
    member val Nest= Nest(code) with get
    member private __.Code3
        with get()= code
        and set v = code <- v
    member x.GetCode() = x.Code
    member x.GetCode i =
        match i with
        | 1 -> x.Code
        | 2 -> x.Code2
        | 3 -> x.Code3
        | i -> failwith "unknown Code index"
        |> sprintf "Code(%i)='%s'" i
    
let nakedTest = Test("hello")
let x = box <| nakedTest
    
module PublicPropertyAccess =
    let (?) (this : 'Source) (prop : string) : 'Result =
        let t = this.GetType()
        if isNull t then failwithf "bad getType"
        let p = t.GetProperty(prop)
        if isNull p then failwithf"could not find prop %s" prop
        p.GetValue(this, null) :?> 'Result
    let (?<-) (this: 'Source) (prop:string) (value: 'Value) =
        this.GetType().GetProperty(prop).SetValue(this,value,null)
    let runGetSample() :string =
        x?Code
    let runSetSample () =
        x?Code <- "dynamic"
        runGetSample()
     
module PrivatePropertyAccess =  
    let flags = BindingFlags.NonPublic |||
                BindingFlags.Instance
    let (?) (this: 'Source) (prop : string) : 'Result =
        let t = this.GetType()
        if isNull t then failwithf"bad getType"
        let prop' = t.GetProperty(prop,flags)
        if isNull prop' then failwithf"could not find prop %s" prop
        prop'.GetValue(this, null) :?> 'Result
    let (?<-) (this: 'Source) (prop:string) (value:'Value) =
        this.GetType().GetProperty(prop,flags).SetValue(this,value,null)
    let runGetSample() :string =
        x?Code2
    let runSetSample () =
        x?Code2 <- "dynamic"
        runGetSample()
    
module PublicMethodAccess =
    open Microsoft.FSharp.Reflection
    let (?) (this : 'Source) (member' : string) (args : 'Args) : 'Result =
        let argArray =
            if box args = null then null
            elif FSharpType.IsTuple (args.GetType()) then
              FSharpValue.GetTupleFields args
            else [|args|]
    
        let flags = BindingFlags.GetProperty ||| BindingFlags.InvokeMethod
        this.GetType().InvokeMember(member', flags, null, this, argArray) :?> 'Result
    let runMethodSample() =
        x?GetCode()
    let runMethodArgSample() =
        x?GetCode(1)
module DictionaryAccess =
    open System.Collections
    let (?) (this : #IDictionary) key =
        this.[key]
    let (?<-) (this: #IDictionary) key value =
        this.[key] <- value
    // this works with either string,obj or string,string
    let dict = System.Collections.Generic.Dictionary<string,string>()
    let runGetSample() =
        dict?Key :?> string
    let runSetSample() =
        dict?Key <- "Value"
        runGetSample()
        
// the dynamic access in here gives compiler warnings about get_Item and set_Item being special
// I also tried using Item property instead of special ops, and failed
module Indexing =
    let inline (?) this key =
        (^a:(member get_Item : _ -> _) (this,key))
    let inline (?<-) this key value =
        (^a:(member set_Item : _ -> _ -> unit) (this,key,value))
    type Indexed() =
        let d = Dictionary<string,string>()
        member self.Item
            with get key =
                d.[key]
            and set key value =
                d.[key] <- value
    let runIndexSample() =
        let i = Indexed()
        i?IndexedKey <- "IndexedValue"
        i?IndexedKey

    
module Ops =
    let inline (?) this (key:string) : 'Value = 
        let (?) =  PublicPropertyAccess.(?)
        // this?key would try to access this.key instead of the key passed in
        this?(key)
    let inline ( |? ) this key = 
        let (?) = PrivatePropertyAccess.(?)
        this?(key)
    type PubAndPri(value) =
        member __.Value = value
        member private __.Value2 = value
        
    let runSample() =
        let x = PubAndPri("dat")
        let v1:string= 
            try
                x?Value
            with _ ->
                printfn "Failed sample get"
                reraise()
        let v2:string= 
            try
                x|?"Value2"
            with _ -> printfn "Failed sample2 get"; reraise()
        sprintf "Pub:%s,Pri:%s" v1 v2
        
module DynamicComposition =
    let runSample() =
        let makePropAccess (flags:BindingFlags) =
            let (?) (this : 'Source) (prop : string) : 'Result =
                let t = this.GetType()
                if isNull t then failwithf "bad getType"
                let p = t.GetProperty(prop,flags)
                if isNull p then failwithf"could not find prop %s" prop
                p.GetValue(this, null) :?> 'Result
            (?)
            
        let inline (?) this key = 
            let (?) = makePropAccess (BindingFlags.Public ||| BindingFlags.Instance) 
            // parens are required for key to be what comes in, instead of looking for a prop named 'key'
            this?(key)
        let x = Test("Code")    
        let result = x?Code
        // alternative access method demonstrated, only the x?Code syntax allows the Magic 'string' access
        (?) x "Code" |> ignore<string>
        result
module Nesting =
    let (?) this key =
        let (?) = PublicPropertyAccess.(?)
        // parens are required for key to be what comes in, instead of looking for a prop named 'key'
        this?(key)
    let runSample() =
        let x:obj= upcast Test "Nesting"
        let n = x?Nest
        let v = n?Value
        v
[
    "PublicGet",PublicPropertyAccess.runGetSample()
    "PublicSet",PublicPropertyAccess.runSetSample()
    "PrivateGet",PrivatePropertyAccess.runGetSample()
    "PrivateSet",PrivatePropertyAccess.runSetSample()
    "PublicMethod",PublicMethodAccess.runMethodSample()
    "PublicMethodArg",PublicMethodAccess.runMethodArgSample()
    // dict samples order is important
    "DictSet",DictionaryAccess.runSetSample()
    "DictGet",DictionaryAccess.runGetSample()
    "IndexSetGet", Indexing.runIndexSample()
    "MyOps", Ops.runSample()
    "DynamicComposition", sprintf "Composed: %s" <| DynamicComposition.runSample()
    "Nested", sprintf "Nested: %s" <| Nesting.runSample()
] // |> List.map (fun (s,x) -> sprintf "%s:%s" s x)
//    x.GetType().GetProperties(PrivatePropertyAccess.flags)
|> Dump
|> ignore