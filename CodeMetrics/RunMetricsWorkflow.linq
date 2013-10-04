<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Activities.dll</Reference>
  <Reference>C:\Projects\CASE\HALOS\MSBuild.Tasks\BBuildTasks\TechnicalDebtTaskLib\TechnicalDebtTaskLib\bin\Debug\TechnicalDebtTaskLib.dll</Reference>
  <Namespace>System.Activities</Namespace>
</Query>

var metricsRelativePath=@"Microsoft Visual Studio 10.0\Team Tools\Static Analysis Tools\FxCop\metrics.exe";
var metricsExePath=System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), metricsRelativePath);
if(System.IO.File.Exists(metricsExePath)==false)
metricsExePath=System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), metricsRelativePath);

var metricsXmlBase=@"c:\users\nbkif5u\desktop\il\";
var technicalDebtTaskLibPath=@"C:\Projects\CASE\HALOS\MSBuild.Tasks\BBuildTasks\TechnicalDebtTaskLib\TechnicalDebtTaskLib\bin\Debug\TechnicalDebtTaskLib.dll";
var publishedTdPath=@"\\b002481060ae4\TFS_Build_Drop\TechnicalDebtTaskLib\TechnicalDebtTaskLib\TechnicalDebtTaskLib_20110419.8\BuildProcessTemplate.dll";
var item=publishedTdPath;
var metricsResultFilePath=System.IO.Path.Combine(metricsXmlBase, System.IO.Path.GetFileName(item) + "_" + "metrics.xml");
var activity=new TechnicalDebtTaskLib.Activities.MetricsWrapper(){ FileToAnalyzePath=item, MetricsExePath=metricsExePath,
 MetricsResultFile=metricsResultFilePath};
 
 //var output=new Dictionary<string,object>();
var outputs=WorkflowInvoker.Invoke(activity);
foreach(var output in outputs.Keys)
if(outputs[output]!=null)
 outputs[output].GetType().FullName.Dump(output);
outputs.Dump();
if(outputs["successText"]!=null)
outputs["successText"].ToString().Length.Dump();