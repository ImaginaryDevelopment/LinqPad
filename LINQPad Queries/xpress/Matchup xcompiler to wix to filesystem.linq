<Query Kind="Program">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Humanizer</NuGetReference>
  <Namespace>Humanizer</Namespace>
</Query>

void Main()
{
	var path = @"C:\TFS\XC-SourceDev\Source-development\ClientSetupWiX\ClientSetupWiX.wxs";
	var chartRoot = @"C:\TFS\XC-SourceDev\Source-development";
	var doc = XDocument.Load(path);
	var ids = doc.Descendants(doc.Root.Name.Namespace+"File").Where(e => e.GetAttribValOrNull("Id").IsNullOrEmpty()==false).Select(e => e.GetAttribValOrNull("Id")).ToArray();
	ids.CheckDuplicates("Found duplicate File Id");
	
	CheckBadIds(ids);
	doc.Descendants(doc.Root.Name.Namespace+"Component").ToList().ForEach(CheckDuplicateComponentSource);
	
	var xps= from c in doc.Descendants(doc.Root.Name.Namespace + "Component")
			from f in c.Descendants(doc.Root.Name.Namespace + "File")
		.Select(f => new{ComponentId=c.GetAttribValOrNull("Id"), Id=f.GetAttribValOrNull("Id"), Name= f.GetAttribValOrNull("Name"), Source=f.GetAttribValOrNull("Source")})
		.Where(f => f.Id != null && f.Id.EndsWith("xps"))
		//.Dump()
		select f
		;
	
	var targetX = @"C:\TFS\XC-SourceDev\Source-development\XCompiler\Xml\AppendedDocuments.xml";
	var xDoc = XDocument.Load(targetX);
	//xDoc.Dump();
	var targetDocItems = xDoc.Descendants(xDoc.Root.Name.Namespace + Path.GetFileNameWithoutExtension(targetX).Singularize().Camelize())
		.Select( x => new { Facilities = x.GetAttribValOrNull("targetFacilities").Split(new []{","}, StringSplitOptions.RemoveEmptyEntries), filename = x.GetAttribValOrNull("fileName")});
	targetDocItems.Sum(t => t.Facilities.Count()).Dump("targets");
	var q = from ad in targetDocItems
		from f in ad.Facilities
		join  lwix in xps on new{Name=ad.filename.ToLowerInvariant(),F=f} equals new {Name=lwix.Name.ToLowerInvariant(),F=lwix.ComponentId.Before("Charts")} into lWix
		from wix in lWix.DefaultIfEmpty()
		let relativePath = wix != null? wix.Source.After("\\\\") : Path.Combine(f+"Charts",ad.filename)
		let pathType = wix !=null ? relativePath.Before("\\"): relativePath
		let componentFileMatch = wix !=null ? pathType == wix.ComponentId : false
		let facilityMatch =  wix !=null ? pathType.StartsWith(f) : false
		let exists = File.Exists(Path.Combine(chartRoot, relativePath))
		let idPrefix = f =="UC"? f : string.Empty
		let suggestId = idPrefix+ ad.filename
		let shouldSuggest = wix == null && exists && ids.Contains(suggestId)==false
		// <File Id="UC2AET.xps" Name="x-2AET.xps" Vital="yes" Source="..\\UCCharts\x-2aet.xps" />
		let wixSuggestion = shouldSuggest? String.Format(@"<File Id=""{0}"" Name=""{1}"" Vital=""yes"" Source=""..\\{2}Charts\{3}"" />", 
			suggestId.Replace("-","_") /* no dashes allowed in Ids */,ad.filename,f,suggestId) : null
		
		orderby wix !=null,exists,facilityMatch && componentFileMatch, f, ad.filename
		select new {
			ad.filename,
			f,
			//ad.Facilities, wix.Source,
			ComponentId = wix!=null? wix.ComponentId:null,
			
			WixComponentFileMatch = Util.HighlightIf(componentFileMatch, d=>!d),
			WixFacilityMatch = Util.HighlightIf(facilityMatch, d=>!d),
			WixId=wix!=null? wix.Id: null,
			Exists = Util.HighlightIf(exists, d=>!d),
			wixSuggestion,
			relativePath
			};
		q.Dump("joined");
		
		q.Where(x=> x.wixSuggestion.IsNullOrEmpty()==false).GroupBy(x=>x.f, x=> x.wixSuggestion).Dump("to insert");
		
	var missing = 
		from wix in xps
		let relativePath = wix.Source.After("\\\\") 
		let pathType = relativePath.Before("\\")
		let exists = File.Exists(Path.Combine(chartRoot, relativePath))
		let componentFileMatch = pathType == wix.ComponentId
		where !exists || !componentFileMatch
		select new{ wix,exists,componentFileMatch};
		
		missing.Dump("bad wix");
}

static void CheckDuplicateComponentSource(XElement component){
	component
		.Descendants(component.Name.Namespace +"File")
		.Where(x => x.GetAttribValOrNull("Source") != null)
		.Select(x => x.GetAttribValOrNull("Source"))
		//.Dump("componentSources")
		.CheckDuplicates(StringComparer.InvariantCultureIgnoreCase,"Duplicate sources (case insensitive) within a component",true);
}

static void CheckBadIds(IEnumerable<string> ids){
	var badIds = ids.Where(i => i.Contains("-"));
	if(badIds.Any())
	{
		badIds.Dump("bad ids");
		throw new InvalidOperationException();
	}
}