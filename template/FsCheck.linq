<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>FsCheck</NuGetReference>
</Query>

      
open FsCheck
open System
///Run this method as an FsCheck test.
//[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
//type PropertyAttribute() =
//    //inherit TestAttribute()
//
//    let mutable maxTest = Config.Default.MaxTest
//    let mutable maxFail = Config.Default.MaxFail    
//    let mutable startSize = Config.Default.StartSize
//    let mutable endSize = Config.Default.EndSize
//    let mutable verbose = false
//    let mutable quietOnSuccess = false
//    let mutable arbitrary = Config.Default.Arbitrary |> List.toArray
//
//    ///The maximum number of tests that are run.
//    member x.MaxTest with get() = maxTest and set(v) = maxTest <- v
//    ///The maximum number of tests where values are rejected, e.g. as the result of ==>
//    member x.MaxFail with get() = maxFail and set(v) = maxFail <- v
//    ///The size to use for the first test.
//    member x.StartSize with get() = startSize and set(v) = startSize <- v
//    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
//    member x.EndSize with get() = endSize and set(v) = endSize <- v
//    ///Output all generated arguments.
//    member x.Verbose with get() = verbose and set(v) = verbose <- v
//    ///The Arbitrary instances to use for this test method. The Arbitrary instances 
//    ///are merged in back to front order i.e. instances for the same generated type 
//    //at the front of the array will override those at the back.
//    member x.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v
//    ///If set, suppresses the output from the test if the test is successful. This can be useful when running tests
//    ///with TestDriven.net, because TestDriven.net pops up the Output window in Visual Studio if a test fails; thus,
//    ///when conditioned to that behaviour, it's always a bit jarring to receive output from passing tests.
//    ///The default is false, which means that FsCheck will also output test results on success, but if set to true,
//    ///FsCheck will suppress output in the case of a passing test. This setting doesn't affect the behaviour in case of
//    ///test failures.
//    member x.QuietOnSuccess with get() = quietOnSuccess and set(v) = quietOnSuccess <- v
//type Letters = 
//	static member Char() = 
//		Arb.Default.Char()
//		|> Arb.filter (fun c-> 'A' <= c && c <= 'Z')
//type DiamondPropertyAttribute() = 
//	inherit PropertyAttribute(
//		Arbitrary = [| typeof<Letters> |],
//		QuietOnSuccess = true)
type Diamond() = 
    static member public make letter = "Devil's advocate."
	
    member public x.make2 letter = Diamond.make letter
    member val MyProperty = (Diamond.make 'a') with get,set

//[<DiamondProperty>]
let ``Diamond is non-empty`` (letter :char) =
    let actual = Diamond.make letter
    not (String.IsNullOrWhiteSpace actual)

open FsCheck

Arb.register<Diamond> |> ignore
Check.All(FsCheck.Config.Verbose,typeof<Diamond>.DeclaringType)


