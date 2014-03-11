<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Build.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.Build.Client</Namespace>
</Query>

void Main()
{
//unfinished? https://gist.github.com/jstangroome/6747950
	var tfs=new Microsoft.TeamFoundation.Client.TfsTeamProjectCollection(new Uri("https://tfs.oceansideten.com"));
	var vcs=tfs.GetService<VersionControlServer>();
	var tfsbuild = tfs.GetService<IBuildServer>(); //http://stackoverflow.com/questions/2909416/how-can-i-copy-a-tfs-2010-build-definition
	var collection =tfsbuild.TeamProjectCollection.Dump();
	var buildToDump=tfsbuild.GetBuildDefinition("Development","Cvr Dev Deploy").Dump();
	
	var buildDefinition= new {
			Uri=buildToDump.Uri, Id=buildToDump.Id, TeamProject=buildToDump.TeamProject,
			CollectionUri=tfsbuild.TeamProjectCollection.Uri,
			Name=buildToDump.Name,
			Description=buildToDump.Description,
			QueueStatus= buildToDump.QueueStatus,
			Trigger= new{
				TriggerType=buildToDump.TriggerType,
				ContinuousIntegrationType=buildToDump.ContinuousIntegrationType,
				ContinuousIntegrationQuietPeriodMinutes = buildToDump.ContinuousIntegrationQuietPeriod,
				BatchSize= buildToDump.BatchSize
			},
			Schedules = buildToDump.Schedules.Select (s =>new{DaysToBuild=s.DaysToBuild, StartTimeSecondsPastMidnight=s.StartTime, TimeZone = s.TimeZone, Type=s.Type} ).ToArray(),
			Workspace =new{ Mappings= buildToDump.Workspace.Mappings.Select (m =>new{m.MappingType, m.ServerItem, m.LocalItem, m.Depth}).ToArray()},
			BuildController = new{buildToDump.BuildControllerUri,buildToDump.BuildController.Name, buildToDump.BuildController.Description, buildToDump.BuildController.CustomAssemblyPath},
			buildToDump.DefaultDropLocation,
			Process= new {buildToDump.Process.Id, buildToDump.Process.Description,buildToDump.Process.ServerPath, buildToDump.Process.SupportedReasons, buildToDump.Process.TeamProject,
				buildToDump.Process.TemplateType, buildToDump.Process.Version
			},
			ProcessParameters= buildToDump.ProcessParameters,
			RetentionPolicies = buildToDump.RetentionPolicyList.Select (rpl => new{ rpl.BuildReason, rpl.BuildStatus, rpl.DeleteOptions, rpl.NumberToKeep}).ToArray()
		};
	
	buildDefinition.Dump("serialized?");
	
}
