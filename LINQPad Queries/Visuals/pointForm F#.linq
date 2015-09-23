<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.DataVisualization.dll</Reference>
  <Namespace>System.Windows.Forms.DataVisualization.Charting</Namespace>
</Query>

//geometry

let dumpc a = a.Dump();a
let dumpct (t:string) a =  a.Dump(t);a
let dumpt (t:string) a = a.Dump(t)

type Vector = { X:float32;Y:float32}
type RightTriangle = { A:float32;B:float32;C:float32}

let p1,p2 =  { X = 0.0f ; Y=0.0f },{ X = 757.0f ; Y=417.0f }
let minX,maxX,minY,maxY = -800,800,-600,600
let slope = (p2.Y - p1.Y)/(p2.X - p1.X)

// given an x; and closing over the slope; what is y?
let  pointSlope x = float (slope * (x - p1.X) + p1.Y)

let twoPointForm = sprintf "y - %f = %f * (x - %f)"p1.Y slope p1.X

let yIntercept =  
	pointSlope(0.0f)
	|> dumpct "yintercept (also b)"
let slopeIntercept= sprintf "y = %fx + %f" slope yIntercept
twoPointForm.Dump()
slopeIntercept.Dump()
let northeastQuadrant = 
	let a = float32 100.0f
	let b = pointSlope 100.0f |> float32
	let c = 
		let c = a ** 2.0f + b ** 2.0f |> float
		let c = Math.Sqrt c
		let c = float32 c
		c
	{ A=a; B=b; C=c}
northeastQuadrant.Dump("quad")
let headers= [|"x";"y"|]
let range = maxX + Math.Abs(minX)

let series=new Series(Name="values",ChartType=SeriesChartType.Line)
series.BorderWidth <- 2
let points = 
	seq {
		for i in 0..10 do
			let x = (float32 i) * (float32 range / 10.0f) - (float32 range / 2.0f)
			let y = pointSlope x
			series.Points.AddXY(x,y) |> ignore
			yield (x,y) 
		}
points |> dumpt "points"
pointSlope 100.0f |> dumpt "100 x, y="
let chart= new Chart(Size = new System.Drawing.Size(800,600))

let chartArea=new ChartArea("area1")
chart.ChartAreas.Add(chartArea)
chartArea.AxisX.Minimum <- float minX
chartArea.AxisX.Maximum <- float maxX

chartArea.AxisY.Minimum <- float minY
chartArea.AxisY.Maximum <- float maxY
chart.Series.Add(series)

series.Points.Count |> dumpt "series points"

//series.Points.AddY(data[i]

chart.Series.Count |> dumpt "series Count"
chart.ChartAreas.Count |> dumpt "ChartAreasCount"
chartArea.Visible <-true
let ShowChart (chart:Chart)=
	use form=new System.Windows.Forms.Form()
	chart.Legends.Add("k") |> ignore
	chart.Dock <- System.Windows.Forms.DockStyle.Fill
	form.Controls.Add(chart)
	form.WindowState <- System.Windows.Forms.FormWindowState.Maximized
	form.ShowDialog()
ShowChart chart |> ignore



	
