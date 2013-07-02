<Query Kind="Statements" />

//extract the relevant column information to transform into a derived class
var targetFile=@"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS\LoanQuest Origination\Presentation\Forms\Setup\Lender\FrmInvestComm.Designer.cs";

//based off dfManualAdjustments
//appearance 58?
var targetProperty="dfFlexPricing";

var lines= System.IO.File.ReadAllLines(targetFile);

var q= from bands in lines.Where(a=>a.Contains(targetProperty) && a.Contains("BandsSerializer.Add"))
	
	
	
	//from l in lines.Where(a=>a.Contains(targetProperty))
	//where l.Contains("BeginInit()")==false && l.Contains("ISupportInitialize")==false
	let b = bands.After("(").Before(")")
	let previous=string.Empty
	let ugb =lines.SkipWhile(a=>a.Contains(b+".Columns.AddRange")==false).Skip(1).TakeWhile(a=>a.Contains(".")==false) .Aggregate((s1,s2)=>s1+s2).After("{").Before("}") //.Count() //
	from ugc in ugb.Split(',').Select(a=>a.Trim())
	let cp =lines.Select(a=>a.Trim()).Where(a=>a.Contains( ugc) && a.Length>ugc.Length+1)
	let caption= cp.Where(a=>a.Contains(ugc+".Header.Caption =")).Select(a=>a.After("\"").Before("\"")).FirstOrDefault()
	let key= cp.Where(a=>a.Contains(" = new ")).Select(a=>a.After("\"").Before("\"")).FirstOrDefault()
	let order= cp.Where(a=>a.Contains("VisiblePosition")).Select(a=>a.After("=").Before(";").Trim()).FirstOrDefault()
	let width= cp.Where(a=>a.Contains(".Width = ")).Select(a=>a.After("=").Before(";").Trim()).FirstOrDefault()
	let hidden= cp.Where(a=>a.Contains(".Hidden = true;")).Select(a=>"true").FirstOrDefault()
	
	//ultraGridColumn2.CellActivation = Infragistics.Win.UltraWinGrid.Activation.ActivateOnly;
	let activation= cp.Where(a=>a.Contains(".CellActivation")).Select(a=>a.After("=").Before(";").Trim()).FirstOrDefault() 
	where hidden!="true"
	orderby b,order
	
	select new{band=b,column=ugc,key,caption,order,activation,width,hidden,cp};
	
	var code=new StringBuilder();
	code.Append("switch (col.Key)\r\n\t{ \r\n");
	var columnSettingTabs="\t\t\t";
	foreach(var c in q)
	{
	
		code.AppendLine("\t\tcase \""+c.key+"\":");
		code.AppendLine(columnSettingTabs+ "SetColPosition(col,"+c.order+");");
		code.AppendLine(columnSettingTabs+ "col.Hidden = " + (c.hidden??"false")+";");
		if(c.width.IsNullOrEmpty()==false)
			code.AppendLine(columnSettingTabs+"col.Width = "+c.width+";");
		if(c.caption.IsNullOrEmpty()==false)
			code.AppendLine(columnSettingTabs+ "col.Header.Caption = \""+c.caption+"\";");
		
		
		if(c.activation.IsNullOrEmpty()==false)
			code.AppendLine(columnSettingTabs+"col.CellActivation = "+c.activation);
		
		code.AppendLine(columnSettingTabs+"break;");
	}
	code.AppendLine("\t\tdefault:");
	code.AppendLine("\t\t\tcol.Hidden = true;");
	code.AppendLine("\t\t\tbreak;");
	code.AppendLine("\t}");
	code.ToString().Dump();
	
q.Dump();