<Query Kind="Program">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>NUnitLite</NuGetReference>
  <Namespace>NUnit</Namespace>
  <Namespace>NUnitLite.Runner</Namespace>
  <Namespace>NUnit.Framework</Namespace>
</Query>

void Main()
{
	new NUnitLite.Runner.TextUI().Execute(new []{"-noheader"});
	
}

// Define other methods and classes here
	[Test]
	public void SomeTest()
	{
	  Assert.Pass();
	}