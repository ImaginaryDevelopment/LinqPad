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
	var tfs=new Microsoft.TeamFoundation.Client.TfsTeamProjectCollection(new Uri("https://tfs.oceansideten.com"));
	var vcs=tfs.GetService<VersionControlServer>();
	var tfsbuild = tfs.GetService<IBuildServer>(); //http://stackoverflow.com/questions/2909416/how-can-i-copy-a-tfs-2010-build-definition
	var buildToCopy=tfsbuild.GetBuildDefinition("Development","Cvr Dev Deploy").Dump();
	var clone=CloneBuildDefinition(buildToCopy).Dump();
	clone.Name=clone.Name+"Clone";
	clone.Save();
}

// Define other methods and classes here
 public void MoveBuild(string fromTeamProject, string toTeamProject, string buildName, string newBuildName)
    {

        var _server = TfsTeamProjectCollectionFactory.GetTeamProjectCollection(new

        Uri("http://Mytfs:8080/defaultcollection"));

        IBuildServer _buildServer = _server.GetService<IBuildServer>();

        var buildDetails = _buildServer.QueryBuildDefinitions(fromTeamProject);

        foreach (var fromBuild in buildDetails)
        {
            if (fromBuild.Name != buildName) continue;
            var newBuildDefinition = _buildServer.CreateBuildDefinition(toTeamProject);
            newBuildDefinition.Name = !string.IsNullOrEmpty(newBuildName) ? newBuildName : fromBuild.Name;

            newBuildDefinition.BuildController = fromBuild.BuildController;

            // This finds the template to use 
            foreach (var mapping in fromBuild.Workspace.Mappings)
            {
                newBuildDefinition.Workspace.AddMapping(
                    mapping.ServerItem, mapping.LocalItem, mapping.MappingType, mapping.Depth);
            }
            newBuildDefinition.DefaultDropLocation = fromBuild.DefaultDropLocation;
            newBuildDefinition.Description = fromBuild.Description;
            
            newBuildDefinition.Process = _buildServer.QueryProcessTemplates(fromTeamProject)[2];

            newBuildDefinition.ProcessParameters = fromBuild.ProcessParameters;
            newBuildDefinition.Enabled = false;
            newBuildDefinition.Save();
        }//end of for each loop 
	}
	
	static IBuildDefinition CloneBuildDefinition(IBuildDefinition buildDefinition)
{
    var buildDefinitionClone = buildDefinition.BuildServer.CreateBuildDefinition(
        buildDefinition.TeamProject);

    buildDefinitionClone.BuildController = buildDefinition.BuildController;
    buildDefinitionClone.ContinuousIntegrationType = buildDefinition.ContinuousIntegrationType;
    buildDefinitionClone.ContinuousIntegrationQuietPeriod = buildDefinition.ContinuousIntegrationQuietPeriod;
    buildDefinitionClone.DefaultDropLocation = buildDefinition.DefaultDropLocation;
    buildDefinitionClone.Description = buildDefinition.Description;
    buildDefinitionClone.Enabled = buildDefinition.Enabled;
    buildDefinitionClone.Name = String.Format("Copy of {0}", buildDefinition.Name);
    buildDefinitionClone.Process = buildDefinition.Process;
    buildDefinitionClone.ProcessParameters = buildDefinition.ProcessParameters;

    foreach (var schedule in buildDefinition.Schedules)
    {
        var newSchedule = buildDefinitionClone.AddSchedule();
        newSchedule.DaysToBuild = schedule.DaysToBuild;
        newSchedule.StartTime = schedule.StartTime;
        newSchedule.TimeZone = schedule.TimeZone;
    }

    foreach (var mapping in buildDefinition.Workspace.Mappings)
    {
        buildDefinitionClone.Workspace.AddMapping(
            mapping.ServerItem, mapping.LocalItem, mapping.MappingType, mapping.Depth);
    }

    buildDefinitionClone.RetentionPolicyList.Clear();

    foreach (var policy in buildDefinition.RetentionPolicyList)
    {
        buildDefinitionClone.AddRetentionPolicy(
            policy.BuildReason, policy.BuildStatus, policy.NumberToKeep, policy.DeleteOptions);
    }

    return buildDefinitionClone;
}