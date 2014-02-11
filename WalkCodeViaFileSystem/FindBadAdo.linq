<Query Kind="Program">
  <Connection>
    <ID>282ab884-278b-4d31-a865-51e85288681f</ID>
    <Persist>true</Persist>
    <Server>SVRRBIDEVDB03</Server>
    <Database>CVS</Database>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

void Main()
{
	var path=@"C:\Development\Products\CVS\Common\CVS.DataAccess\";
	var doNotDescend = new[]{"$tf"};
	var adoReferences=from i in RecurseDirectory(path,doNotDescend,".cs")
		from line in System.IO.File.ReadAllLines(i)
		where line.Contains("\"[") && line.Contains("].[")
		let fullname=line.DumpIf(s=>s.Contains("]\"")==false,i).After("\"").Before("\"")
		where fullname.EndsWith("]")
		let schema=fullname.Before("].").After("[")
		let item=fullname.After(".[").DumpIf(s=>s.Contains("]")==false).Before("]")
		select new{file=i,line,fullname,schema,item};
	var joinedReferences= from a in adoReferences.ToArray().DistinctBy(a=>a.schema+a.item)
		let oSchema = sys.Schemas.FirstOrDefault(s=>s.Name==a.schema )
		let o=oSchema!=null?sys.Objects.FirstOrDefault (o => o.Name==a.item && o.Schema_id==oSchema.Schema_id):null
		//let isSproc = INFORMATION_SCHEMA.ROUTINES.Any (isr => isr.SPECIFIC_SCHEMA==schema && isr.SPECIFIC_NAME==item) //routine_type = 'PROCEDURE' and specific_name like 'member%fraud%'
		//let isView = INFORMATION_SCHEMA.VIEWS.Any (views => views.TABLE_SCHEMA==schema && views.TABLE_NAME==item)
		//let isTable =INFORMATION_SCHEMA.TABLES.Any (t => t.TABLE_SCHEMA==schema && t.TABLE_NAME==item)
		let exists=o!=null // isSproc || isView || isTable
		
		orderby exists
		select new{a,exists,Type=exists?o.Type:null,schemaId=exists?(int?)o.Schema_id:null,Modified=exists?(DateTime?)o.Modify_date:null};
		var missing=joinedReferences.Where (s => s.exists==false).ToArray();
		missing.Count ().Dump("distinct bad references");
		missing.Select (m => new{m.a.schema,m.a.item,m.a.file}).Dump();
		missing.GroupBy (s =>s.a.schema,s=>new{s.a.item,s.exists,s.Type,s.a.file}).Distinct().Dump();
}

// Define other methods and classes here
public IEnumerable<string> RecurseDirectory(string basePath,IEnumerable<string> folderBlacklist,string fileType){
	if(System.IO.Directory.Exists(basePath)==false)
		yield break;
	foreach(var f in System.IO.Directory.GetFiles(basePath))
	{
		if(f.EndsWith(fileType, StringComparison.InvariantCultureIgnoreCase))
		yield return f;
	}
	foreach(var d in System.IO.Directory.GetDirectories(basePath).Where (di => folderBlacklist.Any (b => di.EndsWith(b))==false)){
		//yield return d;
		foreach(var i in RecurseDirectory(d,folderBlacklist,fileType))
			yield return i;
	}
}