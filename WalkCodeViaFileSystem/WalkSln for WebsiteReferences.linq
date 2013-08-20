<Query Kind="Program" />

const string slnFolderGuid= "2150E333-8FDC-42A3-9474-1A3956D46DE8";
const string websiteProjectGuid="E24C65DC-7377-472B-9ABA-BC803B73C61A";
void Main()
{
	var slnPath= @"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\AllApps.sln";
	
	var lines= System.IO.File.ReadAllLines(slnPath).ToArray();
	var q=  from l in lines.Select((l,i)=>new{ProjectLine=l,Index=i}).Where(l=>l.ProjectLine.StartsWith("Project(") && l.ProjectLine.Contains(slnFolderGuid)==false)
			let type= l.ProjectLine.After("{").Before("}")
			let isWebsiteProject = type==websiteProjectGuid
				//check for bin .dlls
			let slnRelativePathLine = l.ProjectLine.After(", \"").Before("\",")
			
			let projectLines=lines.Skip(l.Index+1).TakeWhile(pl=>pl!="EndProject")
			let slnRelativePath = slnRelativePathLine.StartsWith("http:")?
				projectLines.First(pl=>pl.Contains("PhysicalPath")).After("\"").Before("\"")
				:slnRelativePathLine
				
			let projectPath= System.IO.Path.Combine( System.IO.Path.GetDirectoryName(slnPath),slnRelativePath)
			
			let webSiteDlls= isWebsiteProject && projectPath.IsNullOrEmpty()==false?
				System.IO.Directory.EnumerateFiles(System.IO.Path.Combine(projectPath,"bin"),"*.dll")
					.Select(f=>System.IO.Path.GetFileName(f))
					.Where(f=>f.Contains("MortgageFlex", StringComparison.CurrentCultureIgnoreCase)) 
					:null
					
			let webSiteRefreshes = isWebsiteProject && projectPath.IsNullOrEmpty()==false?
				System.IO.Directory.EnumerateFiles(System.IO.Path.Combine(projectPath,"bin"),"*.refresh")//.Where(f=>f.Contains("MortgageFlex", StringComparison.CurrentCultureIgnoreCase))
					:null
			let projectReferenceLine= projectLines.FirstOrDefault(pl=>pl.TrimStart().StartsWith("ProjectReferences"))
			let projectReferences= projectReferenceLine.IsNullOrEmpty()?null: 
				projectReferenceLine
					.After("\"").Before("\"")
					.Split(new char[]{';'}, StringSplitOptions.RemoveEmptyEntries)
					.Select(pr=>pr.Split(new []{'|'}).Reverse().Delimit(Environment.NewLine))
			let pr=isWebsiteProject && projectPath.IsNullOrEmpty()==false? GetWebsiteProjectReferences(projectPath,projectReferenceLine):null
			where isWebsiteProject
			orderby isWebsiteProject descending
			
		select new{
		//Index=l.Index,
			Project=Util.HighlightIf(l.ProjectLine.After("= \"").Before("\""),p=>isWebsiteProject && projectReferenceLine.IsNullOrEmpty()),

			pr,

			};
		
		q.Dump();
}

// Define other methods and classes here
object GetWebsiteProjectReferences(string projectPath,string projectReferenceLine){
	//assume that it is a website project?
	var projReferences=projectReferenceLine.IsNullOrEmpty()?null: 
				projectReferenceLine
					.After("\"").Before("\"")
					.Split(new char[]{';'}, StringSplitOptions.RemoveEmptyEntries)
					.Select(pr=>pr.Split(new []{'|'}).Reverse().Take(1).Delimit(Environment.NewLine)).OrderBy(a=>a).ToArray();
					
	var refreshes= System.IO.Directory.EnumerateFiles(System.IO.Path.Combine(projectPath,"bin"),"*.refresh").Select(a=>System.IO.Path.GetFileNameWithoutExtension(a)).OrderBy(a=>a);
	var dlls=System.IO.Directory.EnumerateFiles(System.IO.Path.Combine(projectPath,"bin"),"*.dll")
					.Select(f=>System.IO.Path.GetFileName(f));
	var mfDlls=dlls.Where(f=>f.Contains("MortgageFlex", StringComparison.CurrentCultureIgnoreCase));
	
	//var notProjectReferences=mfDlls.Where(d=>projReferences==null || projReferences.Any(pr=>pr.Contains(d, StringComparison.CurrentCultureIgnoreCase))==false);
	var aggregateNames= refreshes.Union(dlls).Distinct().OrderBy(a=>a);
	return aggregateNames
		.Select(a=>new{name=a,
			refresh=refreshes!=null?refreshes.Any(r=>r==a):false,
			projReference=projReferences!=null?projReferences.Any(r=>r==a):false
		})
			//notProjectReference=notProjectReferences!=null? notProjectReferences.Any(r=>r==a):false})
			.Where(a=>!a.projReference && !a.refresh)
			;

}