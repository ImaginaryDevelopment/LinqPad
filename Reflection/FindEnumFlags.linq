<Query Kind="Statements" />

//find .net types that are Enum-Flags
var q = from ass in AppDomain.CurrentDomain.GetAssemblies()
		from t in ass.GetExportedTypes()
		where t.IsEnum && t.CustomAttributes.Any(ca=>ca.AttributeType==typeof(FlagsAttribute))
		select new{ass,t,t.Attributes};
		
q.Take(3).Dump();