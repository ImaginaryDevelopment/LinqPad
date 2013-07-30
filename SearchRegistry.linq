<Query Kind="Program">
  <Namespace>Microsoft.Win32</Namespace>
  <Namespace>System.Security</Namespace>
</Query>

IList<Tuple<RegistryKey,string>> failedOpenings=new List<Tuple<RegistryKey,string>>();
void Main()
{
	var allTrees=GetTrees();
	foreach(var tree in allTrees)
	{

	foreach(var result in SearchValues(tree,@"C:\MSOCache",FilterKeys))
	result.Dump();
	
	
	"tree finished".Dump(tree.Name);
	}
}
public static bool FilterKeys(string searchCriteria,RegistryKey key, string valueName)
{

	if(key.GetValueKind(valueName)== RegistryValueKind.String)
	{
		var value=(string)key.GetValue(valueName);
		if(value.Contains(searchCriteria))
		return true;
	}
	
	return false;
}


public static IEnumerable<Microsoft.Win32.RegistryKey> GetTrees()
{
	var roots=new []{Registry.ClassesRoot,  Registry.LocalMachine,Registry.CurrentConfig,
		Registry.CurrentUser, Registry.Users,};
		return roots;
}

IEnumerable<string> GetKeyValues(RegistryKey key)
{
string[] valueNames=null;
try
{	        
	valueNames=key.GetValueNames();
}
catch (Exception ex)
{
	ex.Dump("Failed to get key value names:"+key.Name);
	
}
return valueNames;

}

public  Tuple<string,RegistryKey,string> SearchKeyValue(RegistryKey baseKey,string valueName,string searchCriteria,Func<string,RegistryKey,string,bool> predicate)
{
if(valueName==null)
throw new NullReferenceException("valueName");
if(searchCriteria==null)
throw new NullReferenceException("searchCriteria");
if(baseKey==null)
throw new NullReferenceException("baseKey");
try
{	        
		if(predicate(searchCriteria,baseKey, valueName))
		{
		//"Found key!".Dump(baseKey.Name+" "+item);
		return new Tuple<string,RegistryKey,string>(valueName,baseKey,baseKey.GetValue(valueName).ToString());
		}
	
}
catch (SecurityException ex)
{
	//failedOpenings.Add(Tuple.Create(baseKey,item));
	ex.Dump("Failed to query keyValueNames:"+baseKey.Name);
}
catch (IOException iex)
{
	iex.Dump("Failed to query keyValueNames:"+baseKey.Name);
}
return null;
}

public RegistryKey OpenSubKey(RegistryKey key, string subKeyName)
{
if(key==null)
throw new NullReferenceException("key");
if(subKeyName==null)
throw new NullReferenceException("subKeyName");
RegistryKey subKey=null;
	try
	{	        
		subKey=key.OpenSubKey(subKeyName);
	}
	catch (SecurityException ex)
	{
		//failedOpenings.Add(Tuple.Create(key,subKeyName));
	}
	catch (IOException iex)
{
"about to query a keyName".Dump();
	iex.Dump("Failed to query keySubkeys:"+key.Name);
	"keyName queried".Dump();
}
return subKey;
}

public  IEnumerable<Tuple<string,RegistryKey,string>> SearchValues(RegistryKey baseKey,string searchCriteria,Func<string,RegistryKey,string,bool> predicate)
{
var results=new List<Tuple<string,RegistryKey,string>>();
if(searchCriteria==null)
throw new NullReferenceException("searchCriteria");
var valueNames=GetKeyValues(baseKey);
if(valueNames!=null&& valueNames.Any ())
foreach(var item in valueNames.Where (n => n!=null))
{

var result=SearchKeyValue(baseKey,item,searchCriteria,predicate);

if(result!=null)
results.Add(result);
}

	string[] subKeyNames=null;
	try
	{	        
		subKeyNames=baseKey.GetSubKeyNames();
	}
	catch (IOException iex)
	{
		
		//iex.Dump("Failed to query subKeyNames:"+baseKey.Name);
	}
	if(subKeyNames!=null && subKeyNames.Length>0)
		foreach(var subTree in subKeyNames)
		{
			if(subTree==null)continue;
			RegistryKey subKey=OpenSubKey(baseKey,subTree);
	
			if(subKey!=null)
			{
				var subResults=SearchValues(subKey,searchCriteria,predicate);
				if(subResults!=null)
					foreach(var subResult in subResults.Where (r => r!=null))
						results.Add( subResult);
			}
		}
	return results;
	
}