<Query Kind="Statements" />

var progFilesx86 = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86);
var gacUtils = System.IO.Directory.EnumerateFiles(progFilesx86, "gacutil.exe", SearchOption.AllDirectories).Dump();
var gacUtil = gacUtils.First();
var output = Util.Cmd(gacUtil,"/l",quiet:true); // for much more info do /lr
var search = Util.ReadLine("search for?(empty for all)");

var assemblies = from o in output //.Where(o=>o.Contains("telerik",StringComparison.InvariantCultureIgnoreCase)).Dump("telerik");
	where o.Contains(",")
	where search.IsNullOrEmpty() || o.Contains(search,StringComparison.InvariantCultureIgnoreCase)
	let csv = o.Split(new[]{","}, StringSplitOptions.None)
	let name = csv[0]
	let version = csv[1].AfterOrSelf("=")
	let culture = csv[2].AfterOrSelf("=")
	let pkt = csv[3].AfterOrSelf("=")
	let pa = csv.Length>4 ? csv[4].AfterOrSelf("=") : string.Empty
	orderby name, version descending
	select new{Name=name,Version = version, Culture = culture, PublicKeyToken = pkt, ProcessorArchitecture = pa,FullText=Util.OnDemand("full text",()=> o)};
	
	assemblies.Dump();