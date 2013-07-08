<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.DataVisualization.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Framework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Utilities.v4.0.dll</Reference>
  <Reference>C:\Projects\CASE\TechnicalDebtAccountant\TechnicalDebtTaskLib\TechnicalDebtTaskLib\bin\Debug\TechnicalDebtTaskLib.dll</Reference>
  <Namespace>System.Windows.Forms</Namespace>
  <Namespace>System.Windows.Forms.DataVisualization.Charting</Namespace>
</Query>

void Main()
{
	
					
					
					
			
			var chartTask=new TechnicalDebtTaskLib.SqlMetricChartTask();
			chartTask.Input=GenerateInput();
			chartTask.Execute();
			
		ShowChart(chartTask.Chart);
			
			
	
	
}
void ShowChart(Chart chartControl)
{

			using(var form=new Form())
			{
			//chartControl.ChartAreas.Add("db");
			chartControl.Series.ToList().ForEach(x=> x.ChartArea=chartControl.ChartAreas.First ().Name);
			form.Controls.Add(chartControl);
			chartControl.Dock= DockStyle.Fill;
			//chartControl.Legends.Add("db");
			form.ShowDialog();
			}
}
// Define other methods and classes here
IEnumerable<IEnumerable<KeyValuePair<string,string>>> GenerateInput()
{
var data=@"db,num_procs,len_procs,cursors_refs,tt_refs,ifs,cases,where,join,ands,ors
	master,1,,,,,,,,,
	msdb,19,,,,,,,,,
	LINX_INIT1_DEV,102,173755,84,16,904,80,625,160,585,3514
	CreditSource_Init1_SIT,255,306586,96,112,1320,576,1545,692,1098,4490
	GUS_Init1_SIT,1214,3164502,426,484,13712,2308,11690,5387,10750,55332
	GMS_Init1_SIT,1167,3295064,660,2682,13274,2716,23873,4925,15768,42194
	GMS_GLM_Init1_SIT,227,558090,84,97,2238,808,3905,1356,2796,7808
	GMS_ODM_Init1_SIT,291,749058,42,171,3614,1032,4880,2820,4480,10158";
	
	
	var csv= from d in data.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries).AsEnumerable()		
			select d.Split(',').AsEnumerable();
			
			foreach(var row in csv.Skip(1))
			{
			var list=new List<KeyValuePair<string,string>>();
			foreach(var datapoint in row.Zip(csv.First (),(x,header)=> new KeyValuePair<string,string>(header,x)))
				list.Add(datapoint);
				yield return list;
			}
			
			
				
			
}