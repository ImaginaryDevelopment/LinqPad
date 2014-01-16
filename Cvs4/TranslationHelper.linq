<Query Kind="Program" />

void Main()
{
	var input=Util.ReadLine("Name");
	
	while(input!=string.Empty){
		var text= Util.ReadLine("Text");	
		string.Format("new Tr(\"{0}\",\"{1}\",null),",input.ToUpper(),text.Replace("\"","\\\"")).Dump();
		string.Format("@Html.Translate(Strings.{0})",input.ToUpper()).Dump();
		input=Util.ReadLine("Name",input);
	}
}

// Define other methods and classes here
