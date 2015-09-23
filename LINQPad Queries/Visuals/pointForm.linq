<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.DataVisualization.dll</Reference>
  <Namespace>System.Windows.Forms.DataVisualization.Charting</Namespace>
</Query>

//geometry
void Main()
{
	var p1 = new { X = 0.0 , Y=0.0 };
	var p2 = new { X = 757.0 , Y=417.0 };
	var maxX = 800;
	var minX = -800;
	var minY = -600;
	var maxY = 600;
	var slope = (p2.Y - p1.Y)/(p2.X - p1.X);
	// given an x, and closing over the slope, what is y?
	Func<float,float> pointSlope = x => (float)(slope * (x - p1.X) + p1.Y);
	
	var twoPointForm = "y - "+ p1.Y + " = " + slope +  " * (x - "+p1.X+")";
	
	var yIntercept =  pointSlope(0).Dump("yintercept (also b)");
	var slopeIntercept= "y = " + slope + "x + "+ yIntercept;
	twoPointForm.Dump();
	slopeIntercept.Dump();
	var northeastQuadrant = new { A=(float) 100.0, B= pointSlope(100.0)};
	
	var headers=new[]{"x","y"};
	var range = maxX+Math.Abs(minX);
	
	var series=new Series("values") { ChartType = SeriesChartType.Line};
	series.BorderWidth=2;
	
	for (int i = 0; i < 10; i++)
	{
		var x = i*(range / 10) - (range/2);
		series.Points.AddXY(x,pointSlope(x));
	}
	var chart=new Chart();
	chart.Size=new System.Drawing.Size(800,600);
	
	var chartArea=new ChartArea("area1");
	chart.ChartAreas.Add(chartArea);
	chartArea.AxisX.Minimum=minX;
	chartArea.AxisX.Maximum=maxX;
	
	chartArea.AxisY.Minimum=minY;
	chartArea.AxisY.Maximum=maxY;
	chart.Series.Add(series);
	
	series.Points.Count ( ).Dump("series points");
	
	//series.Points.AddY(data[i]
	
	chart.Series.Count ( ).Dump("series Count");
	chart.ChartAreas.Count ().Dump("ChartAreasCount");
	chartArea.Visible=true;
	ShowChart(chart);
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