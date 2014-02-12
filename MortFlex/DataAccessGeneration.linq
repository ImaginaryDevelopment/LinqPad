<Query Kind="Statements" />

var file=@"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS\LoanQuest Origination\Library\Setup\Common\Lender\InvestComm\InvestorCommitment.cs";
var lines=System.IO.File.ReadAllLines(file);
var formattedLines=from mi in  lines.Where(fl=>fl.Contains("[MapInfo"))
	let baseData = mi.After("(").Before(")")
	let baseformatted= new{ Dbfield= mi.After("\"").Before("\""), cField= mi.After(", \"").Before("\"")}
	join f in lines.Where(fl=>fl.Contains("private") && fl.Trim().EndsWith(";")) .Select(fl=> fl.After("private ")).Select(fl=>new{ type=fl.Before(" "), name= fl.After(" ").Before(";")})
	 on baseformatted.cField equals f.name
	//join pub in lines
	
	//where .Contains("public")
	
	where mi.Contains("MapInfo")
	
	let caseId = mi.Contains("case")?'"'+ mi.After("case ")+'"':"null"
	select "new Mapping(" + baseData+","+f.type.WrapWith("\"")+"," +caseId+"),";
	
	formattedLines.Dump();