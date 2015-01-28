<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>FsCheck</NuGetReference>
  <NuGetReference>NUnitLite</NuGetReference>
  <Namespace>NUnit</Namespace>
  <Namespace>NUnit.Framework</Namespace>
  <Namespace>NUnit.Framework.Api</Namespace>
  <Namespace>NUnit.Framework.Builders</Namespace>
  <Namespace>NUnit.Framework.Constraints</Namespace>
  <Namespace>NUnit.Framework.Extensibility</Namespace>
  <Namespace>NUnit.Framework.Internal</Namespace>
  <Namespace>NUnit.Framework.Internal.Commands</Namespace>
  <Namespace>NUnit.Framework.Internal.Filters</Namespace>
  <Namespace>NUnit.Framework.Internal.WorkItems</Namespace>
  <Namespace>NUnitLite.Runner</Namespace>
</Query>

// files doesn't work because nUnit.Core.Extensibility isn't in nUnitLite
[<NUnitAddin(Description = "FsCheck addin")>]
type FsCheckAddin() =        
	interface IAddin with
		override x.Install host = 
			let tcBuilder = new FsCheckTestCaseBuider()
			host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder)
			true
					
module query =
	open NUnit.Framework
	open FsCheck
	
	///Run this method as an FsCheck test.
	[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
	type PropertyAttribute() =
		inherit TestAttribute()
	
		let mutable maxTest = Config.Default.MaxTest
		let mutable maxFail = Config.Default.MaxFail    
		let mutable startSize = Config.Default.StartSize
		let mutable endSize = Config.Default.EndSize
		let mutable verbose = false
		let mutable quietOnSuccess = false
		let mutable arbitrary = Config.Default.Arbitrary |> List.toArray
	
		///The maximum number of tests that are run.
		member x.MaxTest with get() = maxTest and set(v) = maxTest <- v
		///The maximum number of tests where values are rejected, e.g. as the result of ==>
		member x.MaxFail with get() = maxFail and set(v) = maxFail <- v
		///The size to use for the first test.
		member x.StartSize with get() = startSize and set(v) = startSize <- v
		///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
		member x.EndSize with get() = endSize and set(v) = endSize <- v
		///Output all generated arguments.
		member x.Verbose with get() = verbose and set(v) = verbose <- v
		///The Arbitrary instances to use for this test method. The Arbitrary instances 
		///are merged in back to front order i.e. instances for the same generated type 
		//at the front of the array will override those at the back.
		member x.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v
		///If set, suppresses the output from the test if the test is successful. This can be useful when running tests
		///with TestDriven.net, because TestDriven.net pops up the Output window in Visual Studio if a test fails; thus,
		///when conditioned to that behaviour, it's always a bit jarring to receive output from passing tests.
		///The default is false, which means that FsCheck will also output test results on success, but if set to true,
		///FsCheck will suppress output in the case of a passing test. This setting doesn't affect the behaviour in case of
		///test failures.
		member x.QuietOnSuccess with get() = quietOnSuccess and set(v) = quietOnSuccess <- v
		
		
	NUnitLite.Runner.TextUI().Execute([|"-noheader" |])
	
	type Diamond() = 
		static member make letter = "Devil's advocate."
	
	[<Property(QuietOnSuccess = true)>]
	let ``Diamond is non-empty`` (letter :char) =
		let actual = Diamond.make letter
		not (String.IsNullOrWhiteSpace actual)
	
	// Define other methods and classes here
	[<Test>]
	let SomeTest() = Assert.Pass()
