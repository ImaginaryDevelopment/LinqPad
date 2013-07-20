<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.DataVisualization.dll</Reference>
  <Namespace>System.Windows.Forms.DataVisualization.Charting</Namespace>
</Query>

void Main()
{
	var headers=new[]{"db","num_procs","proc_len","joins","wheres","ors"};
	var data=new[]{"db1","2","400","1","6","4","5"};
	var data2=new[]{"db1","2","400","4","2","4","5"};
	var data3=new[]{"db1","4","400","3","4","4","5"};
	var data4=new[]{"db1","4","40000","30","4","4","5"};
	var data5=new[]{"db1","4","40000","1","4","4","5"};
	var allSamples=new[]{data,data2,data3,data4,data5};
	var chart=new Chart();
	chart.Size=new System.Drawing.Size(800,600);
	
	var chartArea=new ChartArea("area1");
	chart.ChartAreas.Add(chartArea);
	var chartType=SeriesChartType.Line;
	chartArea.AxisX.Minimum=1;
	double minimum=0;
	double maximum=0;
	var series=new Series("total deltas");
	series.BorderWidth=2;
	
	chartArea.AxisX.Maximum=allSamples.Count ( );
	for (int i = 3; i < headers.Count (); i++)
	{
	
		headers[i].Dump();
		var metricSeries=new Series(headers[i]);
		metricSeries.ChartType=chartType;
		var dataPoints=Deltas(allSamples.Select (s =>Double.Parse(s[i])),1);
		metricSeries.BorderDashStyle= ChartDashStyle.Dash;
		minimum=Math.Min( dataPoints.Min ( ),minimum);
		maximum=Math.Max(dataPoints.Max ( ),maximum);
		dataPoints.ToList().ForEach(p=>metricSeries.Points.Add(p));
		 chart.Series.Add(metricSeries);
	}
	var totalDataPoints=Deltas(allSamples.Select (s =>s.Skip(3).Select (x => double.Parse(x)).Sum () ),1).Dump("total");
			minimum=Math.Min( totalDataPoints.Min ( ),minimum);
		maximum=Math.Max(totalDataPoints.Max ( ),maximum);
		totalDataPoints.ToList().ForEach(p=>series.Points.Add(p));
	
	series.ChartType=chartType;
	series.Points.Count ( ).Dump("series points");
	chart.Series.Add(series);
	
	
	//chart.Series.ToList().ForEach(s=>s.IsValueShownAsLabel=true);
	chartArea.AxisY.Minimum=minimum.Dump();
	chartArea.AxisY.Maximum=maximum.Dump();
	
	//series.Points.AddY(data[i]
	
	chart.Series.Count ( ).Dump("series Count");
	chart.ChartAreas.Count ().Dump("ChartAreasCount");
	chartArea.Visible=true;
	ShowChart(chart);
	
}

// Define other methods and classes here

IEnumerable<double> Deltas(IEnumerable<double> source,double scale)
{
	double? last=null;
	foreach(var item in source)
	{
		if(last.HasValue==false)
		{
		last=item;
		yield return 0;
		}
		else
		{
		var result=scale*(last.Value-item);
		last=item;
		yield return result;
		}
	
	}
}

void ShowChart(Chart chart)
{
	using(var form=new System.Windows.Forms.Form())
	{
	chart.Legends.Add("k");
	chart.Dock= System.Windows.Forms.DockStyle.Fill;
		form.Controls.Add(chart);
		form.WindowState= System.Windows.Forms.FormWindowState.Maximized;
		form.ShowDialog();
	}
}