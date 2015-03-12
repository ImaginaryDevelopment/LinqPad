<Query Kind="Statements" />

var path =@"C:\TFS\XC-SourceDev\Source-development\Xpress.EDIS.Documents\PhysicianChartDocumentNEW.xaml.cs";
var text = File.ReadAllText(path);

var rawData =
new Regex(@"^\s*#region\s*\[?\s*([a-z]+)\s*.*\WINotifyPropertyChanged Property\W.*\s*",RegexOptions.IgnoreCase | RegexOptions.Multiline).Matches(text)
	.Cast<Match>()
	//.DumpProp(x=>x.Count(),"Count")
	
	.Select(r=>new{Name=r.Groups[1].Value,Whole= text.Substring(r.Index,text.IndexOf("#endregion",r.Index)-r.Index)})
	.Where(x=>Regex.IsMatch(x.Whole,@"\Wset\W"))
	.Where(x=>Regex.IsMatch(x.Whole,@"RaisePropertyChanged\("""));
	var cleanedText = text;
foreach(var raw in rawData){
	cleanedText= cleanedText.Replace(raw.Whole,string.Empty);
}
File.WriteAllText(path,cleanedText);
var data= rawData
	//.Take(50) 
	.Select(x=>
		new{
			Name=x.Name,
			RaiseCount = new Regex(@"RaisePropertyChanged")
				.Matches(x.Whole).Count,
			Properties=new Regex(@"RaisePropertyChanged\(\""(.*)""\)")
				.Matches(x.Whole).Cast<Match>().Select(m => m.Groups[1].Value),
				
			Meta= string.Join(" ",x.Whole.SplitLines().ToArray()).AfterOrSelf("private ").Before("public")
		})
	//.Dump()
	.Select(x=>
		new{
			x.Name,
			x.RaiseCount,
			x.Properties,
			Type= x.Meta.AfterOrSelf("private ").Before(" "),
			Initializer = x.Meta.BeforeOrSelf(";").AfterLast(" ")
		})
	//.Dump()
	.Select(x=> 
			new{
			x.Name,
			x.RaiseCount,
			x.Properties,
			x.Type,
			Initializer = x.Initializer.StartsWith("_")? string.Empty : x.Initializer
			})
	.OrderBy(x=>x.RaiseCount).ThenBy(x=>x.Name)
	.Select(r=>"{\""+r.Name+"\",new Notifiable{Type=\""+r.Type+"\"" +
		(r.Initializer.IsNullOrEmpty()? string.Empty: ",Initializer=\""+r.Initializer+"\"") + "}}")
	;
	String.Join(","+Environment.NewLine, data).Dump();
					