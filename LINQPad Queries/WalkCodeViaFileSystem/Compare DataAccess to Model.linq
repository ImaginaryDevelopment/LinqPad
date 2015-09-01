<Query Kind="Statements" />

var daFile = @"c:\tfs\xc-sourcedev\source-development\xpress.foundation\datamodels\cpoeorderdataaccess.cs";
var modelFile = @"C:\TFS\XC-SourceDev\Source-development\Xpress.Foundation\DataModels\CPOEOrderDataModel.cs";
var daText = File.ReadAllText(daFile);
var modelText = File.ReadAllText(modelFile);
//var TablePerTypeName = Path.GetFileNameWithoutExtension(modelFile).BeforeOrSelf("DataModel").Dump();
var typeName = Regex.Match(modelText,@"$\s*(?:public|internal)? class (\w+)\s*", RegexOptions.Multiline).Groups[1].Value.Dump();

Debug.Assert(typeName.Equals(Path.GetFileNameWithoutExtension(modelFile), StringComparison.InvariantCultureIgnoreCase));

var typeRegex = @"(?!new|public|class|enum|static\b)([\w_<>]+)";
//var noTrailingSemicolon= @"(?!.*;\s*)";
var methods = Regex.Matches(daText,@"$\s*(?:public|internal)? "+typeRegex +@" (\w+)\s*\(",RegexOptions.Multiline).Cast<Match>().Select(m=>new {m.Index,Type= m.Groups[1].Value, Name=m.Groups[2].Value}).ToArray()
	//.Dump("methods")
	;
var wholeMethods = methods
	.Select((m,i) => {
		var length = i < methods.Length-1 ? methods[i+1].Index - m.Index : daText.Length - m.Index;
		var body = daText.Substring(m.Index,length);
		var sproc = Regex.Match(body,@"""usp\w+""");
		return new{ m.Index, Length=length, m.Name, m.Type, Body = body, Sproc = sproc.Value};
	})
	//.Dump("whole")
	;
	var mapRegex = @"(?:public|internal) "+ typeName+@" \w+\(IDataRecord \w+\)";
	var mapMethod = wholeMethods.Single(m=> m.Body.IsMatch(mapRegex,false))
	//.Dump("maps")
	;
	
	var sprocCallMethods = wholeMethods.Where(m=>m.Sproc.IsNullOrEmpty()==false).Select(m=>
		{
			var sprocParams = Regex.Matches(m.Body,@"@(\w+)").Cast<Match>().Select(match=> match.Groups[1].Value);
			return new{m.Type, m.Name, m.Sproc, m.Body,Params=sprocParams,CallsMap = m.Body.Contains(mapMethod.Name)};
		})
	//.Dump("sproc calling methods")
	; 
	var interestingMethods = sprocCallMethods.Where(s=>s.Name.Contains("delete", StringComparison.InvariantCultureIgnoreCase)== false);

var props = Regex.Matches(modelText,@"$\s*(?:public|internal) " + typeRegex+@" (\w+)\s*", RegexOptions.Multiline).Cast<Match>().Select(m=>new{m.Index,Type= m.Groups[1].Value, Name=m.Groups[2].Value, m})
	//.Dump("props")
	;
var data = interestingMethods.Select(s=> 
{
	var propsMap = props.Select(p => new{p.Name,IsInvolved= s.Body.After("{").Contains(p.Name) || s.CallsMap && mapMethod.Body.Contains(p.Name) }).OrderBy(p=>p.IsInvolved);
	
	return new{ s.Name,s.Sproc,s.Params,Involved = propsMap.Count(p=>p.IsInvolved),NotReferenced=propsMap.Count(p=>!p.IsInvolved), Props = propsMap };
});

Util.HorizontalRun(false,data).Dump("propAccounting");

var byProp = props.Select(p=> new { p.Name,Total= data.Count(), Included = data.SelectMany(d=>d.Props).Where(dp => dp.Name==p.Name).Count(dp => dp.IsInvolved)}).Dump();