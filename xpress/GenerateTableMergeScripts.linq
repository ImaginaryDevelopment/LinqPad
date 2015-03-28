<Query Kind="Statements" />

// sql diff
var input1 = File.ReadAllText(@"C:\TFS\XC-SourceDev\Source-development\XPEncounter\Schema Objects\Tables\dbo.CPOEOrders.table.sql");
var input2 = File.ReadAllText(@"C:\TFS\XC-SourceDev\Source-development\XPEncounter\Schema Objects\Tables\dbo.CPOEMedications.table.sql");

var skip = new[]{"CPOEMedicationID"};

var namePart = @"$\s*\[(\w+)\]\s*";
var typePart = @"(\w+)\s*";
var lengthPart = @"(?:\((\d+|MAX)\))?\s*";
var optionsPart = @".*,?$";
var regex = new Regex(namePart + typePart + lengthPart+optionsPart, RegexOptions.Multiline);
//regex.Matches(input1).Dump();

var table1Matches = regex.Matches(input1.Before("-- cpoe")).Cast<Match>();
var table2Matches = regex.Matches(input2).Cast<Match>();

var shared = table1Matches.Where(tm => table2Matches.Select(t2 => t2.Groups[1].Value).Contains( tm.Groups[1].Value)).Dump("shared");

var toAdd = table2Matches.Where(t2 =>skip.Contains(t2.Groups[1].Value)==false &&  !shared.Select(tm => tm.Groups[1].Value).Contains(t2.Groups[1].Value)).OrderBy(t2 => t2.Groups[1].Value);
String.Join(string.Empty, toAdd.Select(t2=> t2.Value)).Dump("addTo1");
string.Join(Environment.NewLine, toAdd.Select( ta => "alter table [dbo].[CPOEOrders] drop column " + ta.Groups[1].Value)).Dump("drop columns");
string.Join(Environment.NewLine, toAdd.Select( ta => "alter table [dbo].[CPOEOrders] add " + ta.Groups[1].Value +" " + ta.Groups[2].Value + (ta.Groups[3].Success? "(" +ta.Groups[3].Value+")" : string.Empty) +" NULL")).Dump("add columns");
string.Join(","+Environment.NewLine, toAdd.Select( ta => "\t"+ta.Groups[1].Value + " = m." + ta.Groups[1].Value)).Dump("set body");
	
	
	input1.Dump();