<Query Kind="Statements">
  <Connection>
    <ID>4e94eacc-a31d-4687-947b-e4c9804c895a</ID>
    <Persist>true</Persist>
    <Server>(local)</Server>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>PmRewriteApplicationDatabase</Database>
    <ShowServer>true</ShowServer>
  </Connection>
</Query>

//sln reader
var slnPath = @"C:\TFS\XC\Source-development\BuildSolution.sln";
var slnText = System.IO.File.ReadAllLines(slnPath);
var projects = 
	slnText.SkipWhile(t => t.TrimStart().StartsWith("Project(\"{") == false).TakeWhile(t => t.StartsWith("Global")==false)
	.Where(t => t.TrimStart().StartsWith("Project(\"{"))
	.Select(t => t.After("Project(\""))
	.Select(t => new { SlnFolderGuid = Guid.Parse(t.Before("\"")), Remainder = t.After("\"") })
	.Select(t => new { t.SlnFolderGuid, Remainder = t.Remainder.After("\"") })
	.Select(t => new { t.SlnFolderGuid, Name = t.Remainder.Before("\""), Remainder = t.Remainder.After("\"")})
	.Where(t => t.Name != "Solution Items")
	.Select(t => new { t.SlnFolderGuid, t.Name, ProjectFilePath = t.Remainder.After("\"").Before("\""), Remainder = t.Remainder.After("\"").After("\"") })
	.Select(t => new { t.SlnFolderGuid, t.Name, t.ProjectFilePath, ProjectGuid = Guid.Parse(t.Remainder.After("\"").Before("\""))})
	.ToDictionary(t => t.ProjectGuid)
	;
	


var buildConfiguration =
	slnText
	.SkipWhile(f => f.Trim().StartsWith("GlobalSection(ProjectConfigurationPlatforms) = postSolution") == false)
	.Skip(1)
	.TakeWhile(f => f.TrimStart().StartsWith("{"))
	.Select(f => new { ProjectGuid = Guid.Parse(f.Before(".")), Remainder = f.After(".") })
	.Select(f => new { f.ProjectGuid, SlnConfiguration = f.Remainder.Before("|").Trim(), Remainder = f.Remainder.After("|") })
	.Select(f => new { f.ProjectGuid, f.SlnConfiguration, SlnPlatform = f.Remainder.Before("."), Remainder = f.Remainder.After(".") })
	.Select(f => new { f.ProjectGuid, f.SlnConfiguration, f.SlnPlatform, Action = f.Remainder.Before(" = "), Remainder = f.Remainder.After(" = ") })
	.Where(f => f.Action != "ActiveCfg")
	.Select(f => new { f.SlnConfiguration, f.SlnPlatform, f.ProjectGuid, f.Action, ProjectConfiguration = f.Remainder.Before("|"), ProjectPlatform = f.Remainder.After("|") })
	//.GroupBy(f => f.ProjectGuid,f=> f.Remainder)
	;
new { ProjectsCount = projects.Keys.Count(), ProjectCount = buildConfiguration.Select(bc => bc.ProjectGuid).Distinct().Count() }.Dump("stats");
projects.Dump();
buildConfiguration.Dump();

foreach (var platform in buildConfiguration.Select(c => c.SlnPlatform).Distinct())
{
	foreach (var configuration in buildConfiguration.Select(c => c.SlnConfiguration).Distinct())
		foreach (var project in buildConfiguration.Select(c => c.ProjectGuid).Distinct())
		{
			// make sure there is a build action for each combination
			var buildAction = buildConfiguration.FirstOrDefault(c => c.SlnPlatform == platform && c.SlnConfiguration == configuration && c.ProjectGuid == project);
			var projectInfo = projects[project];
			if (buildAction == null && projectInfo.Name != "Xpress.Interop.IO.Test")
			{
				new { platform, configuration, project,ProjectInfo=projectInfo }.Dump("no action found");
				throw new InvalidOperationException();
			}
		}
}
