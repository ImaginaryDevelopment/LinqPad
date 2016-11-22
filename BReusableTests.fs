module Pm.Tests.BReusableTests
open System
open System.Collections.Generic
open System.Collections.ObjectModel
let foo = ()

#if XUNIT
open global.Xunit
open global.Xunit.Abstractions
open FsCheck
open FsCheck.Xunit

open Pm.Schema.BReusable
open Pm.Schema.BReusable.Reflection
open Pm.Schema.BReusable.Debug

[<Fact>]
let ``getQuoteMemberName Static Method Name can work`` () = 
    let result = QuotationHelpers.getQuoteMemberName <@ System.String.IsNullOrEmpty("") @>
    Assert.Equal("IsNullOrEmpty", result)

[<Fact>]
let ``getQuoteMemberName InstanceMethodName can work`` () = 
    let result = QuotationHelpers.getQuoteMemberName <@ "".Clone() @>
    Assert.Equal("Clone", result)

[<Fact>]
let ``getQuoteMemberNameT InstanceMethodName can work`` () = 
    let result = QuotationHelpers.getQuoteMemberNameT<string>(<@ fun s -> upcast s.Chars(0) @>)
    Assert.Equal("Chars", result)

[<Fact>]
let ``getQuoteMemberName InstanceClosure`` () = 
    let x = String.Empty
    let result = QuotationHelpers.getQuoteMemberName <@@ x.Chars(0) @@>
    Assert.Equal("Chars", result)

[<Fact>]
let ``getQuoteMemberName PropertyName can work`` () = 
    let result = QuotationHelpers.getQuoteMemberNameT<string> <@ fun s -> s.Length :> obj @> 
    Assert.Equal("Length",result)

[<Fact>]
let ``castAs box ObservableCollection<string> -> ObservableCollection<string> are ref equal`` () = 
    let expected: obj = upcast ObservableCollection<string>() 
    let actualOpt = castAs<ObservableCollection<string>> expected
    Assert.NotNull actualOpt
    let actual = actualOpt.Value
    Assert.Same(expected,actual)

[<Fact>]
let ``castAs box ObservableCollection<string> -> IList<string> is not null`` () = 
    let expected: obj = upcast ObservableCollection<string>() 
    let actual = castAs<IList<string>> expected
    Assert.NotNull actual

// runner or xunit failed to find/run the test if the name was as follows:
//let ``ObservableCollection<String>() | TypeDef (isType:IList<_>)`` () =

[<Fact>]
let ``ObservableCollection<string>() | TypeDef IList<_>`` () = 
    let expected: obj = upcast ObservableCollection<string>()
    let actual = 
        match expected with 
        | TypeDef (isType:IList<_>) _ -> Some () 
        | _ -> None
    Assert.NotNull actual
    Assert.True (Option.isSome actual)

[<Fact>]
let ``ObservableCollection<string>() | TypeDef ObservableCollection<_> typeof<string>`` () = 
    let expected: obj = upcast ObservableCollection<string>()
    let actual = 
        match expected with 
        |TypeDef (isType:ObservableCollection<_>) [| t |] -> 
            if t = typeof<string> then Some () else None
        | _ -> None
    Assert.NotNull actual

//[<Theory>]
//[<InlineData()>]
//let fLineMapListenerTheories (inputs:string[])

[<Fact>]
let fLineMapListenerWriteLineRunsTheLineMap() = 
    let expected = [ "Hello World"]

    let f _ = expected.[0]
    let actualContainer = ObservableCollection<string>()
    use ll = new FLineListener(actualContainer, Some f)
    ll.WriteLine "bah"
    let actual = actualContainer |> List.ofSeq
    Assert.Equal(expected.[0], actual.[0])

[<Fact>]
let fLineMapListenerWriteDoesntRunTheLineMap() = 
    let expected = "unmapped" 

    let f _ = failwithf "bad listener"
    let actualContainer = ObservableCollection<string>()
    use ll = new FLineListener(actualContainer, Some f)
    ll.Write expected
    let actual = actualContainer |> List.ofSeq
    Assert.Equal(expected, actual.[0])

[<Fact>]
let fLineMapListenerWriteThenWriteLineIsMapped() = 
    let expected = "unmapped" 

    let f _ = expected
    let actualContainer = ObservableCollection<string>()
    use ll = new FLineListener(actualContainer, Some f)
    ll.Write "abcd"
    ll.WriteLine "efgh"
    let actual = actualContainer |> List.ofSeq
    Assert.Equal(1, actual.Length)
    Assert.Equal(expected, actual.[0])

let fLineMapListenerMaintainsOrder() = 
    let expected = ["0"; "1"; "2"]
    let actualContainer = ObservableCollection<string>()
    let mutable i = 0
    let f _ =
        let x = i
        i <- i + 1
        x |> string
    use ll = new FLineListener(actualContainer, Some f)
    ll.Write "Hello"
    ll.Write " "
    ll.WriteLine "World"
    ll.WriteLine "abcd"
    ll.Write "Herro"
    ll.WriteLine "idk"
    (int actualContainer.[0], int actualContainer.[1])
    |> Assert.NotEqual 
    Assert.Equal(expected.Length, actualContainer.Count)
    Assert.Equal(expected.[0], actualContainer.[0])
    Assert.Equal(expected.[1], actualContainer.[1])
    actualContainer
    |> List.ofSeq
    |> List.zip expected
    |> List.iter Assert.Equal


#endif


