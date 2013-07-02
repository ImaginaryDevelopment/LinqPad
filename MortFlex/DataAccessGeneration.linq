<Query Kind="Statements">
  <Connection>
    <ID>c889faef-d2e9-4289-8165-1a9d4232951d</ID>
    <Persist>true</Persist>
    <Server>rpsql2008r2dev\dev</Server>
    <SqlSecurity>true</SqlSecurity>
    <UserName>winrls</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAA1WLKs9qc4USFiwcJ5tmkhgAAAAACAAAAAAADZgAAwAAAABAAAACEKZyPDjj1jpZroQ8fb+q3AAAAAASAAACgAAAAEAAAAN6vwGVht+wq/UdaxUso2eoIAAAAf2dnjdkWnhwUAAAAeojVz05mzbG5SM3YwihG/GWjFao=</Password>
    <Database>STD08RLSD</Database>
    <ShowServer>true</ShowServer>
  </Connection>
</Query>


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