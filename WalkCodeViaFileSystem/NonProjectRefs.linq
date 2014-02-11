<Query Kind="Program" />

void Main()
{
	bool debug=false;
	var baseDir=Util.ReadLine("Directory?",@"C:\Development\");
	var projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);
	var ourCodeNamespaces=new[]{"Oceanside","CVS"};
	
	var csProjects=projects.Where(f=>f.EndsWith(".csproj"));//.Take(2);
	var filteredProjects=projects.Where(f=>f.EndsWith(".csproj") 
		&& f.Contains("test", StringComparison.InvariantCultureIgnoreCase)==false
		//not in solution
		&& f.EndsWith("WordsMatching.csproj")==false 
		&& f.Contains("LocalizationTool")==false
		&& f.EndsWith("CVS.Member.RestfulServices.csproj")==false
		&& f.EndsWith("CVS.Facebook.Web.csproj")==false
		&& f.EndsWith("CVS.Manage.Web.PanelBuilder.csproj")==false
		&& f.EndsWith(@"CVS\CVS.Services\CVS.Services.Twitter\CVS.Services.Twitter.csproj")==false
		);//.Take(2);
	var projectsWithGuid=filteredProjects.ToDictionary(a=>a.Dump(),a=>new{Path=a,Guid=GetProjectGuid(a).Dump()});
	
	var baseQuery=(from i in filteredProjects
		let doc=XDocument.Load(i)
		let rootns=doc.Root.Name.Namespace
		let proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,i+" has no project element")
		where proj!=null
		select new{ Path=i, Doc=doc,RootNs=rootns,ProjNode=proj}).ToArray();
	
	var commonNonProjectRefs = new string[]{
	//put these back for pointing at non-common dir stuff
	//	"Mortgageflex.Core",
//		"Mortgageflex.Common",
//		"Mortgageflex.Win",
//		"Mortgageflex.RuleEngine",
//		"Mortgageflex.Library.Base",
//		"Mortgageflex.LoanQuest.Shared",
	};
	var projectReferenceFormat=@"	<ProjectReference Include=""{0}"">
		  <Project>{1}</Project>
	      <Name>{2}</Name>
	</ProjectReference>";
	
	var references= from i in baseQuery
		let baseUri=new Uri(System.IO.Path.GetDirectoryName( i.Path)+"\\")
		
		from ig in i.ProjNode.Elements(i.RootNs+"ItemGroup")
		
		let refs=from r in ig.Nodes().OfType<XElement>()
			.Where(a=>a.Name.LocalName=="Reference")
			.Select(a=>new{Include=a.GetAttribValOrNull("Include").BeforeOrSelf(","),Node=a})
			.Where(a=>ourCodeNamespaces.Any (cn => a.Include.Contains(cn, StringComparison.InvariantCultureIgnoreCase)))
			.Where(a=>commonNonProjectRefs.Contains( a.Include)==false).DumpIf(a=>debug && a.Any(),"references of "+i.Path)
			let kvp= projectsWithGuid.FirstOrDefault (wg =>r.Include.EndsWith(System.IO.Path.GetFileNameWithoutExtension(wg.Key)))
			let csProj=kvp.Key!=null? projectsWithGuid[kvp.Key].Path:null
			
			let csProjGuid=kvp.Key!=null? projectsWithGuid[kvp.Key].Guid:null
			let refProjUri =csProj.IsNullOrEmpty()?null: new Uri(csProj)
			let relativePath=csProj.IsNullOrEmpty()?null: Uri.UnescapeDataString(baseUri.MakeRelativeUri(refProjUri).ToString().Replace('/',Path.DirectorySeparatorChar))
			
			select new{r.Include,
				CsProj=csProj,
				ProposedNode=csProjGuid.IsNullOrEmpty()?csProjGuid: String.Format(projectReferenceFormat,relativePath,csProjGuid,r.Include),
				r.Node
				}
		where refs.Any()
		select new{Project=i.Path,Condition=ig.Attribute("Condition"),Items=refs};
		if(debug)
		{
			
			references.Where (r => r.Items.Any (i => i.ProposedNode.IsNullOrEmpty()==false)).Dump("needs changed");
			references //.Take(2)
				.Dump();
				
		}	
	var failures= new List<object>();
	foreach(var p in references){
		p.Project.Dump("Updating:"+p.Items.Count(a=>a.ProposedNode.IsNullOrEmpty()==false));
		var text= System.IO.File.ReadAllText(p.Project);
		bool changed=false;
		foreach(var r in p.Items.Where(r=>r.ProposedNode.IsNullOrEmpty()==false)){
			
			var bad=r.Node.ToString();
			var toFind=bad.Before(" xmlns");
			var toReplace=toFind+bad.After("2003\"");
			var found=text.Contains(toReplace);
			var altFound=text.IndexOf(toFind);
			var endText="</Reference>";
			var altEnd=text.IndexOf(endText,altFound);
			if(!found && altFound>=0)
			new{ toFind,Index=altFound,badBlock=text.Substring(altFound,altEnd-altFound+endText.Length)}.Dump("alternate is "+r.ProposedNode);
			
			
			if(found)
			{
				var newText=text.Replace(toReplace,r.ProposedNode);
				if(text!=newText)
					text=newText;
					changed=true;
			}
			else if(altFound>=0)
			{
				text=text.Substring(0,altFound)+r.ProposedNode+text.Substring(altEnd+endText.Length);
				changed=true;		
			} else {
				failures.Add(new{p.Project,r});
			}
		}
		
		if(changed){
		
		//System.IO.File.WriteAllText(p.Project.Dump("Changed!"),text);
		} else {
		//p.Project.Dump("made no changes");
		}
	}
	failures.Dump("failures");
}

// Define other methods and classes here
public string GetProjectGuid(string path){
	if(path.IsNullOrEmpty())
	return path;
	
		var doc=XDocument.Load(path);
		var rootns=doc.Root.Name.Namespace;
		var proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,path+" has no project element");
		var projFull= new{ Path=path, Doc=doc,RootNs=rootns,ProjNode=proj};
	return projFull.ProjNode
		.Elements()
		.Where(e=>e.Name.LocalName=="PropertyGroup")
		.SelectMany(pg=>pg.Elements()
			.Where(e=>e.Name.LocalName=="ProjectGuid"))
		.Select(pg=>pg.Value).First();
}