<Query Kind="Program" />

void Main()
{
	var edmx= @"C:\Projects\JobSystem.edmx";
	var xdoc=System.Xml.Linq.XDocument.Load(edmx);
	xdoc.Dump();
	var ns=xdoc.Root.Name.Namespace;
	
	//sort entity types?
	
	var storageModels=xdoc.Root.Element(ns+"Runtime").Element(ns+"StorageModels");
	var storeNs=storageModels.GetNamespaceOfPrefix("store");
	
	new {Root=ns,Default=xdoc.Root.GetDefaultNamespace(),DefaultLocal=storageModels.GetDefaultNamespace(), Store=storeNs}.Dump("namespaces");
	
	var schemas=storageModels.Elements();
	
	Debug.Assert(schemas.All (s => s.Name.LocalName=="Schema"));
	//ProcessSchemas(schemas); //not helpful or necessary
	var mappings = xdoc.Root.Element(ns+"Runtime").Element(ns+"Mappings").Elements();
	Debug.Assert(mappings.All (m => m.Name.LocalName=="Mapping"));
	ProcessMappings(mappings);
	var concept= xdoc.Root.Element(ns+"Runtime").Element(ns+"ConceptualModels");
	
	ProcessConceptual(concept);
	xdoc.Save(edmx);		
		
	
}

// Define other methods and classes here
void ProcessConceptual(XElement concept)
{
	var processed= new List<string>();
			var localNs=concept.Elements().First ().Name.Namespace;
		foreach(var s in concept.Elements())
		{
	
			localNs.Dump("Entity Type?");
			
			foreach(var et in s.Elements(localNs+"EntityType"))
			{
				foreach(var p in et.Elements(localNs+"Property"))
				{
					var pName=p.Attribute(XNamespace.None+"Name");	
					var existing=pName.Value;
					var proposed=existing.Pascalize();
					if(existing != proposed){
						processed.Add(et.Attribute(XNamespace.None+"Name").Value+":"+existing);
						pName.Value=proposed;
					}
				}
				
			}
			
		}
		var propRefs=concept.XPathSelectElements(".//*[local-name()='PropertyRef']");
		
		foreach(var propRef in propRefs)
		{
			var pName=propRef.Attribute(XNamespace.None+"Name");	
					var existing=pName.Value;
					var proposed=existing.Pascalize();
					if(existing != proposed){
						processed.Add(propRef.Parent.Name+":"+existing);
						pName.Value=proposed;
					}
		}
	
	processed.Dump("Conceptuals");
}
void ProcessMappings(IEnumerable<XElement> mappings)
{
	var processed=new List<string>();
	foreach(var m in mappings)
	{
		var localNs=m.Name.Namespace;
		foreach(var ecm in m.Elements(localNs+"EntityContainerMapping"))
		foreach(var esm in ecm.Elements(localNs+"EntitySetMapping"))
		foreach(var etm in esm.Elements(localNs+"EntityTypeMapping"))
		{
			var typeName=etm.Attribute(XNamespace.None+"TypeName");
			foreach(var mf in etm.Elements(localNs+"MappingFragment"))
			foreach(var sp in mf.Elements(localNs+"ScalarProperty"))
			{
				var spName=sp.Attribute(XNamespace.None+"Name");	
				var existing=spName.Value;
				var proposed=existing.Pascalize();
				if(existing != proposed){
					processed.Add(typeName+":"+existing);
					spName.Value=proposed;
				}
			}
		}
	}
	processed.Dump();
}