<Query Kind="Program">
  <Connection>
    <ID>396ccfd9-66a7-4aa0-8385-105b0435cb18</ID>
    <Server>svrrbidevdb03</Server>
    <Database>Tfs_DefaultCollection</Database>
  </Connection>
</Query>

void Main()
{
	var rawQ = from bd in Tbl_BuildDefinitions.ToArray()
	//.Where(x=>x.LastSystemBuildStartTime>DateTime.Now.AddMonths(-2)) // only for system initiated (Scheduled?) builds

	let lastBuild = Tbl_Builds.Where(b=>b.DefinitionId==bd.DefinitionId).OrderByDescending(b=>b.StartTime).FirstOrDefault()
	where lastBuild !=null 
	select new{BuildDefinition= bd, lastBuild};
	
	var q= from raw in rawQ
	let lastBuild = raw.lastBuild
	let bd = raw.BuildDefinition
	let status = lastBuild==null? null:	lastBuild.BuildStatus
	let statusName = status==null? null:
			status==8? "Failed":
			status==2? "Success":
			status.ToString()
	let processParamsParsed= XElement.Parse(bd.ProcessParameters)
	let buildParamsParsed =lastBuild.ProcessParameters==null?null: XElement.Parse(lastBuild.ProcessParameters)
	let cleanWorkspaceOption = processParamsParsed.Elements().Where(e=>e.Name.LocalName=="CleanWorkspaceOption").Select(e=>e.Value).FirstOrDefault()
	let msBuildArgs = GetMsBuildArgs(processParamsParsed)
	let lastBuildArgs = buildParamsParsed==null?null:GetMsBuildArgs(buildParamsParsed)
	select new{ bd.DefinitionId, bd.DefinitionName, bd.ProcessTemplateId,
	StatusName=Util.HighlightIf(statusName,x=> x!="Success"),
	DefinitionArgs=msBuildArgs,BuildArgs=lastBuildArgs, StatusCode =status,cleanWorkspaceOption,
	RawProcessParameters =Util.OnDemand("Process Parameters",()=> new{
		LastBuildParams=lastBuild.ProcessParameters,
	DefinitionParams=bd.ProcessParameters
	}), lastBuild};
	
	q.Dump();
	
}

// Define other methods and classes here
string GetMsBuildArgs(XElement processParams){
	return processParams.Elements()
		.Where(e=>e.Name.LocalName=="String")
		.Where(e=> e.Attributes().Any(a=>a.Name.LocalName=="Key" && a.Value=="MSBuildArguments"))
		.Select(e=>e.Value).FirstOrDefault();
}