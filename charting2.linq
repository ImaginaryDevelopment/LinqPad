<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.DataVisualization.dll</Reference>
  <Reference>C:\Projects\CASE\TechnicalDebtAccountant\TechnicalDebtTaskLib\TechnicalDebtTaskLib\bin\Debug\TechnicalDebtTaskLib.dll</Reference>
  <Namespace>TechnicalDebtTaskLib.Charts</Namespace>
</Query>

void Main()
{

	var data=MetricCharter. GenerateInput();
	var data2= MetricCharter.GenerateRandomTrending(data,4,new Random());
	
	var sample1=new Sample(
	data.First().Select(x=>x.Key),data.Select(d=>new DbContainer{Values=d.Select(x=>x.Value)}));
	var sample2=new Sample(
	data2.First().Select(x=>x.Key),data2.Select(d=>new DbContainer{Values=d.Select(x=>x.Value)}));
	var randoms=MetricCharter.GenerateRandomSamples(2,28,4,data2);				
	var charter=new TechnicalDebtTaskLib.Charts.MetricCharter((new List<Sample>{sample1,sample2}).Concat(randoms));
		//q.Dump();
		var charts=charter.GetCharts(new System.Drawing.Size(800,600)).Take(8).ToList();
		using(var form=new TechnicalDebtTaskLib.ChartIteratorForm())
		{
			form.ShowDialog(charts);
		}
}

// Define other methods and classes here
IEnumerable<double> Deltas(IEnumerable<double> data)
{
var last=(double?)null;
		foreach(var item in data)
		{
			if(last==null)
			{
			last=item;
			yield return 0;
			}
			else 			{
			var result= item-last.Value;
			last=result;
			yield return result;
			}
		}
}

