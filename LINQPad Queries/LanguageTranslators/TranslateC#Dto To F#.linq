<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//C# to F#
// http://json2csharp.com/

//var input=LINQPad.Util.ReadLine<string>("What shall we reformat?");

let translate (input:string) = 
	[
	for l in input.SplitLines() do 
		if l.TrimStart().StartsWith ("class ") || l.Contains(" class ") then
			yield sprintf "type %s = {" <| l.After("class ").BeforeOrSelf(":").Trim()
		if l.Contains(";") then 
			let cleaned = l.AfterOrSelf("public ").BeforeOrSelf("get;")
			let itemType = cleaned.Before(" ").Trim()
			let name = cleaned.After(" ").Trim().BeforeOrSelf("{").Trim()
			yield name+":"+itemType
	]	
	|> fun l -> String.Join("\r\n",l)

let input ="""
public class AddressComponent
{
    public string long_name { get; set; }
    public string short_name { get; set; }
    public List<string> types { get; set; }
}

public class Location
{
    public double lat { get; set; }
    public double lng { get; set; }
}

public class Northeast
{
    public double lat { get; set; }
    public double lng { get; set; }
}

public class Southwest
{
    public double lat { get; set; }
    public double lng { get; set; }
}

public class Viewport
{
    public Northeast northeast { get; set; }
    public Southwest southwest { get; set; }
}

public class Geometry
{
    public Location location { get; set; }
    public string location_type { get; set; }
    public Viewport viewport { get; set; }
}

public class Result
{
    public List<AddressComponent> address_components { get; set; }
    public string formatted_address { get; set; }
    public Geometry geometry { get; set; }
    public List<string> types { get; set; }
}

public class RootObject
{
    public List<Result> results { get; set; }
    public string status { get; set; }
}
"""
translate input