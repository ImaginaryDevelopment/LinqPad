<Query Kind="Program">
  <NuGetReference>FsCheck</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>xunit</NuGetReference>
  <Namespace>Xunit</Namespace>
</Query>

//http://www.weiwang.info/?p=351
void Main()
{

	XunitHelpers.LaunchXunitGui(Assembly.GetExecutingAssembly());
}
 
public class TestClass
{
	[Fact]
	public void Dummy()
	{
	 	Assert.Equal(4, 2 + 2);
		Console.WriteLine ("Dummy Test");
	}
 
	[Fact(Skip="Can't figure out where this is going wrong...")]
	public void BadMath()
	{
		Assert.Equal(5, 2 + 2);
	}
 
	[Fact(Timeout=50)]
	public void TestThatRunsTooLong()
	{
		System.Threading.Thread.Sleep(250);
	}
}
 
#region XUnit
public static class XunitHelpers
{
	// To Use it in LinqPad, add the following in Main method
	// XunitHelpers.LaunchXunitGui(Assembly.GetExecutingAssembly());
	// Also the test class should be public
	public static void LaunchXunitGui(Assembly assembly)
	{	
		// change the exe path to your xunit location
		string cmd = string.Format(@"""{0}"" ""{1}""", @"xunit.gui.clr4.exe", assembly.Location).Dump("cmd");		
		if(!File.Exists(Path.Combine(Path.GetDirectoryName(assembly.Location),"xunit.dll")))
		{
			// change the dll path to your xunit location
			//File.Copy(@"C:\SkyDrive\Tools\xunit-1.9.2\xunit.dll", Path.Combine(Path.GetDirectoryName(assembly.Location), "xunit.dll"));	
		}
		Util.Cmd (cmd);
	}	
}
#endregion