<Query Kind="Statements">
  <Namespace>System.Net</Namespace>
</Query>

var sb= new StringBuilder();
using(var client= new WebClient()){
for (int i = 1; i < 7; i++)
{
sb.AppendLine(
	client.DownloadString("https://forums.marvelheroes.com/discussion/42635/interactive-hero-roster-2-0/p"+i.ToString())
	//.Dump()
	);	
}
}
var urls=Regex.Matches(sb.ToString(),@"([^>]*\w+)\s*: <a href=""http://tinyurl.com/(\w+)", RegexOptions.Multiline)
	.Cast<Match>()
	.Select (m =>new{Name= m.Groups[1].Value,m.Groups[2].Value,line=m.Value}).Distinct()
//.Dump()
;
var buckets= new[]{
	new []{"BlackPanther","BLKP","BP"},
	new []{"BlackWidow","BW","BLKW"},
	new []{"Cable","CBL","CABLE"},
	new []{"CaptainAmerica","CAA","CAP","CAN","CAR","CAT","CAE"},
	new []{"Colossus","CO"},
	new []{"Cyclops","CY"},
	new[]{"Daredevil","DD"},
	new[]{"DrStrange"},
	new []{"Deadpool","DP","LDY"},
	new[]{"EmmaFrost","EF"},
	new[]{"Gambit","GMBT"},
	new[]{"GhostRider","GR"},
	new[]{"Hawkeye","HKY","HAWK"},
	new[]{"Hulk","HLK"},
	new[]{"HumanTorch","HT"},
	new[]{"Ironman","IM"},
	new[]{"JeanGrey","JG","JNG"},
	new[]{"Juggernaut"},
	new[]{"Loki"},
	new[]{"LukeCage","LC"},
	new[]{"Magneto"},
	new[]{"MoonKnight"},
	new[]{"MsMarvel","MM"},
	new[]{"Nightcrawler"},
	new[]{"Nova","futnova"},
	new[]{"Psylocke"},
	new[]{"Punisher","PUN"},
	new[]{"RocketRacoon","RR"},
	new[]{"ScarletWitch","SW"},
	new[]{"SilverSurfer"},
	new[]{"Spiderman","SM"},
	new[]{"SquirrelGirl","SG"},
	new[]{"StarLord"},
	new[]{"Storm","STRM"},
	new[]{"SueStorm"},
	new[]{"Thing"},
	new[]{"Thor"},
	new[]{"Venom"},
	new[]{"Wolverine","WOLV"},
	};
	
var q= from b in buckets

		select new { b,urls= urls.Where (ur =>b.Any (x => ur.Value.StartsWith(x, StringComparison.CurrentCultureIgnoreCase)))};
var eager= q.ToArray();
eager.SelectMany (x => x.urls).Count ().Dump("Costumes");
var main=eager.Select (x => new {Name=x.b.First (),Urls=x.urls.Select(u=>u.Value).Distinct(),Count=x.urls.Select (u => u.Value).Distinct().Count ()}).Dump();	
main.Select (m =>"new character('"+m.Name+"',["+(m.Urls.Any () ? m.Urls.Select (u =>"'"+u+"'").Aggregate ((s1,s2) =>s1+","+s2 ):"'GENERPH'")+"])").Aggregate ((s1,s2)=>s1+","+Environment.NewLine+s2).Dump();

		