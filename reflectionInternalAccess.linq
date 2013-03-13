<Query Kind="Statements">
  <Reference>&lt;ProgramFiles&gt;\Microsoft Visual Studio 10.0\Team Tools\Static Analysis Tools\FxCop\Microsoft.VisualStudio.CodeAnalysis.Interop.dll</Reference>
</Query>

System.Reflection.Assembly.GetExecutingAssembly().GetReferencedAssemblies().Dump().First();
var assembly=System.Reflection.Assembly.UnsafeLoadFrom(@"C:\Program Files\Microsoft Visual Studio 10.0\Team Tools\Static Analysis Tools\FxCop\Microsoft.VisualStudio.CodeAnalysis.Interop.dll");
assembly.Dump();
var assemblyInfoType=assembly.GetType("Microsoft.VisualStudio.CodeAnalysis.Common.AssemblyInfo").Dump();
assemblyInfoType.GetMethods( BindingFlags.Public | BindingFlags.Static).Dump();
var method=assemblyInfoType.GetMethod("GetAssemblyInfo",BindingFlags.InvokeMethod | BindingFlags.Static | BindingFlags.Public,
	null,new Type[]{typeof(string)},null);
method.Invoke(null,
//System.Reflection.Assembly.GetExecutingAssembly().GetReferencedAssemblies().Dump();