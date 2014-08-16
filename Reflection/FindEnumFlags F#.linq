<Query Kind="FSharpProgram" />

let q = seq{
	for a in AppDomain.CurrentDomain.GetAssemblies() do
		for t in a.GetExportedTypes() do
			if t.IsEnum && t.CustomAttributes.Any(fun ca ->ca.AttributeType=typedefof<FlagsAttribute>) then yield (a,t)}
		
q.Take(3).Dump();