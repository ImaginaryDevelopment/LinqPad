namespace Pm.Tests.BReusableTests
open System
open System.Collections.Generic
open System.Collections.ObjectModel

#if XUNIT
open global.Xunit
open global.Xunit.Abstractions
open FsCheck
open FsCheck.Xunit

open Pm.Schema.BReusable
open Pm.Schema.BReusable.Reflection
open Pm.Schema.BReusable.Debug

module StringHelpers =
    [<Literal>]
    let PascalCase = "Hello World"
    [<Fact>]
    let ``String.indexOf can work`` () =
        let actual = " Hello world" |> String.indexOf "Hello"
        Assert.Equal(1,actual)

    [<Theory>]
    [<InlineData(PascalCase, "Hello World", 0)>]
    [<InlineData("Hello World","World", 6)>]
    [<InlineData("Hello World"," World", 5)>]
    [<InlineData("Hello World","world", -1)>]
    let ``String.indexOf can work cases`` x sub expected =
        let actual = x |> String.indexOf sub
        Assert.Equal(expected,actual)

    [<Theory>]
    [<InlineData("Hello World", "hello world", 0)>]
    [<InlineData("Hello World", "Hello", 0)>]
    [<InlineData("Hello World","world", 6)>]
    [<InlineData("Hello World"," world", 5)>]
    [<InlineData("Hello World","World", 6)>]
    [<InlineData("Hello World","xorld", -1)>]
    let ``String.indexOfC cases`` x sub expected =
        let actual = x |> String.indexOfC sub String.defaultIComparison
        Assert.Equal(expected,actual)

    [<Fact>]
    let ``String.contains can work`` () =
        let actual = "Hello world" |> String.contains "world"
        Assert.Equal(true,actual)

    [<Theory>]
    [<InlineData(PascalCase, "Hello World",true)>]
    [<InlineData(PascalCase,"World",true)>]
    [<InlineData(PascalCase," World", true)>]
    [<InlineData(PascalCase,"xorld", false)>]
    [<InlineData(PascalCase,"world", false)>]
    let ``String.contains cases`` x sub expected =
        let actual = x |> String.contains sub
        Assert.Equal(expected,actual)

    [<Theory>]
    [<InlineData(PascalCase, "Hello World",true)>]
    [<InlineData(PascalCase,"World",true)>]
    [<InlineData(PascalCase,"world",true)>]
    [<InlineData(PascalCase, "hello world", true)>]
    [<InlineData(PascalCase,"world", true)>]
    [<InlineData(PascalCase," world", true)>]
    [<InlineData(PascalCase,"World", true)>]
    [<InlineData(PascalCase,"xorld", false)>]
    [<InlineData("Hello world","World", true)>]
    let ``String.containsC cases`` x sub expected =
        let actual = x |> String.containsC sub String.defaultIComparison
        Assert.Equal(expected,actual)

    open StringHelpers
    [<Theory>]
    [<InlineData("Hello World", "Hello World","")>]
    [<InlineData("Hello World","World","")>]
    [<InlineData("Hello World"," World", "")>]
    [<InlineData("Hello World","Hello", " World")>]
    [<InlineData("Hello World","l", "lo World")>]
    let ``String.after cases`` x sub expected =
        let actual = x |> after sub
        Assert.Equal(expected,actual)

    [<Theory>]
    [<InlineData("Hello World", 0, "Hello World")>]
    [<InlineData("Hello World",1,"ello World")>]
    [<InlineData("Hello World",6,"World")>]
    [<InlineData("Hello World",5," World")>]
    let ``String.substring cases`` input sub expected =
        let actual = input |> String.substring sub
        Assert.Equal(expected,actual)

    [<Theory>]
    [<InlineData("Hello World", 0, 1, "H")>]
    [<InlineData("Hello World",1,1,"e")>]
    [<InlineData("Hello World",6,2,"Wo")>]
    [<InlineData("Hello World",5,3," Wo")>]
    let ``String.substring2 cases`` input sub len expected =
        let actual = input |> String.substring2 sub len
        Assert.Equal(expected,actual)

module ReflectionTests =

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


