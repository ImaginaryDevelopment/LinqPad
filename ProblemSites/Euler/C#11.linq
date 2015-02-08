<Query Kind="Program" />

void Main()
{
	
	
	var data = from l in GetData().SplitLines().Select((l,i)=> new{l,i})
			from n in l.l.Trim().Split(' ').Select((n,i)=> new{n,i})
			select new Coordinate(){Row=l.i, Column = n.i,Val=int.Parse(n.n)};
	var columns=data.Select(a=>a.Column);
	var rows= data.Select(a=>a.Row);
	new{ CMax= columns.Max(), CMin=columns.Min()}.Dump();
	new{ RMax= rows.Max(), RMin=rows.Min()}.Dump();
	data.Count().Dump("Count");
	var horiz=SearchHorizontal(data).Dump("Horizontal");
	var vert= SearchVertical(data).Dump("Vertical");
	var diag=SearchDiagonal(data).Dump("Diag");
	new{ Horizontal=horiz.ToString("C"), Vertical=vert.ToString("C"), Diag=diag.ToString("C"), Largest=Math.Max( Math.Max(horiz,vert),diag)}.Dump();
}

public class Coordinate{
	public int Row{get;set;}
	public int Column{get;set;}
	public int Val{get;set;}
}


string GetData(){
	var numbers = @"08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
	49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
	81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
	52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
	22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
	24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
	32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
	67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
	24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
	21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
	78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
	16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
	86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
	19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
	04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
	88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
	04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
	20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
	20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
	01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48";
	return numbers;
}

 static long SearchHorizontal(IEnumerable<Coordinate> data){
 	long largest=0;
 for(var r=0; r<20;r++)
	for(var c =0;c< 17;c++){ //no wrap around
		var values=data.Where(a=>a.Row==r).Skip(c).Take(4);
		var product= values.Select(a=>(long)a.Val).Aggregate((a,b)=>a * b);
		if(product>largest)
		{
			largest=product;
			//new{r,c,product}.Dump();
		}
	}
	return largest;
}
 static long SearchVertical(IEnumerable<Coordinate> data){
 	long largest=0;
 for(var c=0; c<20;c++)
	for(var r =0;r< 17;r++){ //no wrap around
		var values=data.Where(a=>a.Column==c && a.Row>=r).Take(4);//.Dump("Verticals");
		var product= values.Select(a=>(long)a.Val).Aggregate((a,b)=>a * b);
		if(product>largest)
			largest=product;
	}
	return largest;
}

static long SearchDiagonal(IEnumerable<Coordinate> data){
 	long largest=0;
 foreach(var d in data.Where(a=>a.Row<17)){
 	// backslash diag
	if(d.Column<17)
	{
		long value=d.Val;
		var range= Enumerable.Range(1,3);
		var diags= from otherD in data
			join r in range on new{x=otherD.Column,y=otherD.Row} equals new{x=d.Column+r,y=d.Row+r}
			select otherD;
			
		if(diags.Any()==false || diags.Count()<3)
		{
			
			d.Dump("failed to find related points");
			range.Dump("range");
			throw new InvalidOperationException();
		}
		
			value*= diags.Select(a=>(long)a.Val).Aggregate((x,y)=>x * y);	
		
		
		if(value>largest)
		{
			
		
			largest=value;
		}
	}
	// frontslash diag
	if(d.Column>=3){
		long value=d.Val;
		var range= Enumerable.Range(1,3);
		var rawDiags= from otherD in data.Where(rd=>rd.Row>d.Row && rd.Column<d.Column && rd.Row<d.Row+4 && rd.Column>d.Column-4)
			from r in range
			let pt= new{Row=d.Row+r,Column=d.Column-r}
			let oPt= new{Row=otherD.Row,Column=otherD.Column}
			orderby pt.Row, pt.Column descending
			select new{d,r,pt,oPt,Equal=pt.Column==oPt.Column && pt.Row==oPt.Row,otherD};
			//diags.Concat(new[]{d}).Dump("backward diags");
		var diags=rawDiags.Where(rd=>rd.Equal);
		if(diags.Any()==false || diags.Count()<3)
		{
			if(diags.Any())
				diags.Dump("related");
				rawDiags.GroupBy(g=>g.r).Where(rd=>rd.Any(r=>r.Equal==true)==false).Dump();
			d.Dump("failed to find related points");
			range.Dump("range");
			
			throw new InvalidOperationException();
		}
		
			value*= diags.Select(a=>(long)a.otherD.Val).Aggregate((x,y)=>x * y);	
		
		
		if(value>largest)
		{
			//new{ d, diags}.Dump();
		
			largest=value;
		}
	}
	
 }
	return largest;	
}