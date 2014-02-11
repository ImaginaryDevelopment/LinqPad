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
	var doNotDescend = new[]{"$tf",".git"};
	var fieldToFind="verified_bit_field";
	var adoReferences=from i in RecurseDirectory(path,doNotDescend,".cs")
		from line in System.IO.File.ReadAllLines(i).Select ((l,x) =>new{l,index=x} )
		where line.l.Contains("\"[") && line.l.Contains("].[")
		let fullname=line.l.DumpIf(s=>s.Contains("]\"")==false,i).After("\"").Before("\"")
		where fullname.EndsWith("]")
		let schema=fullname.Before("].").After("[")
		let item=fullname.After(".[").DumpIf(s=>s.Contains("]")==false).Before("]")
		select new{file=i,lineNumber=line.index,line=line.l,fullname,schema,item};
	var joinedReferences= from a in adoReferences.ToArray().DistinctBy(a=>a.schema+a.item)
		let oSchema = sys.Schemas.FirstOrDefault(s=>s.Name==a.schema )
		let o=oSchema!=null?sys.Objects.FirstOrDefault (o => o.Name==a.item && o.Schema_id==oSchema.Schema_id):null
		where o!=null
		let sprocInfo=INFORMATION_SCHEMA.ROUTINES.FirstOrDefault (isr => isr.SPECIFIC_SCHEMA==a.schema && isr.SPECIFIC_NAME==a.item) //routine_type = 'PROCEDURE' and specific_name like 'member%fraud%'
		where sprocInfo!=null
		where sprocInfo.ROUTINE_DEFINITION.Contains(fieldToFind, StringComparison.InvariantCultureIgnoreCase)
		select new{a,sprocInfo.ROUTINE_DEFINITION};
		var sprocs=joinedReferences.ToArray();
		sprocs.Count ().Dump("sprocs");
		sprocs.Select (m => new{m.a.schema,m.a.item,m.a.file,m.a.lineNumber}).Dump();
		//sprocs.GroupBy (s =>s.a.schema,s=>new{s.a.item,s.ROUTINE_DEFINITION}).Distinct().Dump();
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