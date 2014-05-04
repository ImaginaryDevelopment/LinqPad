<Query Kind="Program">
</Query>

void Main()
{
	//TODO: account for commented out code in sprocs
	var fieldsToFind=new[]{"bulk_int"};
	
	//var operationsOfInterest=new[]{""};
	
	var sprocs = from o in sys.Objects.Where (ob => ob.Type=="P" && ob.Is_ms_shipped==false)
		join oSchema in sys.Schemas on o.Schema_id equals oSchema.Schema_id
		join sprocInfo in INFORMATION_SCHEMA.ROUTINES.Where (isr => isr.ROUTINE_TYPE=="PROCEDURE" && isr.SPECIFIC_CATALOG==this.Connection.Database) on new{schema=oSchema.Name, o.Name} equals new{schema=sprocInfo.ROUTINE_SCHEMA,Name=sprocInfo.ROUTINE_NAME}
		
		select new{Object=o,SchemaName=oSchema.Name,sprocInfo};
		
		//sprocs.Where (s => s.SchemaName=="Project").OrderBy (s => s.Object.Name).Select (s => s.Object.Name).Dump();
		
	var joinedReferences= from s in sprocs.ToArray()
		//where s.SchemaName.IsIgnoreCaseMatch("project")
		let def=s.sprocInfo.ROUTINE_DEFINITION
		where fieldsToFind.Any (tf => def.Contains(tf, StringComparison.InvariantCultureIgnoreCase)) 
		//where operationsOfInterest.Any (oi =>Regex.IsMatch(def,oi+"[^\\w]", RegexOptions.IgnoreCase))
		orderby s.SchemaName, s.Object.Name
		select new{s.SchemaName,s.Object.Name, s.sprocInfo.ROUTINE_DEFINITION,s.sprocInfo.LAST_ALTERED};
		
	joinedReferences.Dump("sprocs that probably reference a field of interest");
	sprocs.Count ().Dump("sprocs");
		
}