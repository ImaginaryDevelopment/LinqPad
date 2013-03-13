<Query Kind="Statements">
  <Namespace>Microsoft.Win32</Namespace>
</Query>

var snapIns=Registry.LocalMachine.OpenSubKey("SOFTWARE\\Microsoft\\MMC\\SnapIns");
var names=snapIns.GetSubKeyNames();
Func<RegistryKey,IEnumerable<string>,string,object> DumpKeys= (rKey,fieldNames,descKey)=> {
	if(rKey==null)
		return null;
	if(fieldNames==null)
		return null;
	var result=
	fieldNames
		.OrderBy (n => n)
		.Select (n => new KeyValuePair<string,string>(n,(rKey.GetValue(n!="null"?n:null)??string.Empty).ToString())).ToArray();
		return result
		.Dump((rKey.GetValue(descKey)??string.Empty).ToString());
};
foreach(var snap in names.OrderBy (n => n))
{
	
	var snapInKey=snapIns.OpenSubKey(snap);
	var clsidKey=Registry.ClassesRoot.OpenSubKey("CLSID\\"+snap);
	if(clsidKey!=null){
		var clsidName=(clsidKey.GetValue(null)??string.Empty).ToString();
		if(clsidName.HasValue())
			new{Name=clsidName,Subkeys=new Lazy<IEnumerable<RegistryKey>>(()=>clsidKey.GetSubKeyNames().Select (k => clsidKey.OpenSubKey(k)))}.Dump();
		//clsidName.Dump();
	}
	var fields=snapInKey.GetValueNames();
	if(fields.Contains("NameString"))
		DumpKeys(snapInKey,fields,"NameString");
		else if (fields.Contains("NameStringIndirect"))
		DumpKeys(snapInKey,fields,"NameStringIndirect");
//	if(snapInKey.GetValueNames().Contains("NameString"))
//		snapInKey.Dump();
}