<Query Kind="Statements" />

//extract the relevant column information to transform into a derived class
var targetFile=@"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS\LoanQuest Origination\Presentation\Forms\Setup\Lender\FrmInvestComm.Designer.cs";


var lines= System.IO.File.ReadAllLines(targetFile).ToArray();
var q = from i in Enumerable.Range(0,lines.Length)
	let line= lines[i]
	where line.Contains(".TabIndex")
	
	let tIndex=int.Parse( line.After("= ").Before(";"))
	let control= line.After("this.").Before(".")
	let newIndex=control=="cmbLoanType"?72: tIndex<=71?tIndex:tIndex+2 
	orderby tIndex
select new{i,tIndex,newIndex,control,IsLabel= line.Contains("Label",StringComparison.CurrentCultureIgnoreCase) ,line=lines[i]};

q.Dump();