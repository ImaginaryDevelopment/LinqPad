<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>FSharp.Charting</NuGetReference>
  <Namespace>FSharp.Charting</Namespace>
  <Namespace>Microsoft.FSharp.Reflection</Namespace>
  <Namespace>System.Data.OleDb</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

//required installation of http://download.microsoft.com/download/2/4/3/24375141-E08D-4803-AB0E-10F2E3A07AAA/AccessDatabaseEngine.exe
//alternative for 64 bit apps : http://download.microsoft.com/download/2/4/3/24375141-E08D-4803-AB0E-10F2E3A07AAA/AccessDatabaseEngine_x64.exe
let RedirectAssembly shortName (targetVersion:Version) publicKeyToken = // http://blog.slaks.net/2013-12-25/redirecting-assembly-loads-at-runtime/
	//https://dotnetfiddle.net/UnS2vU
	let rec onResolveEvent = new ResolveEventHandler( fun sender evArgs ->
		let requestedAssembly = AssemblyName(evArgs.Name)
		printfn "trying to resolve requestedAssembly: %A" requestedAssembly
		if requestedAssembly.Name <> shortName then
			Unchecked.defaultof<Assembly>
		else
			printfn "Redirecting assembly load of %s ,\tloaded by %s" evArgs.Name (if evArgs.RequestingAssembly = null then "(unknown)" else evArgs.RequestingAssembly.FullName)
			requestedAssembly.Version <- targetVersion
			printfn "Redirecting to %A" requestedAssembly.Version
			requestedAssembly.SetPublicKeyToken(AssemblyName(sprintf "x, PublicKeyToken=%s" publicKeyToken).GetPublicKeyToken())
			requestedAssembly.CultureInfo <- System.Globalization.CultureInfo.InvariantCulture
			AppDomain.CurrentDomain.remove_AssemblyResolve(onResolveEvent)
			Assembly.Load(requestedAssembly)
			)
	AppDomain.CurrentDomain.add_AssemblyResolve(onResolveEvent)
RedirectAssembly "FSharp.Core" (Version("4.3.1.0")) "b03f5f7f11d50a3a"
//charting in linqpad issues (so far no go) http://blog.ploeh.dk/2014/01/30/how-to-use-fsharpcore-430-when-all-you-have-is-431/

let fullHomePath = Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%")
let xlPath = System.IO.Path.Combine(fullHomePath, "SkyDrive","salary history.xlsx")
if System.IO.File.Exists(xlPath) = false then failwith "File not found"
use dt = new DataTable()
// http://stackoverflow.com/a/12996342/57883
let xlsxCs = sprintf """Provider=Microsoft.ACE.OLEDB.12.0;Data Source=%s;Extended Properties="Excel 12.0 Xml;HDR=YES";"""
let sheetName = "Sheet1$"
(
	use conn = new OleDbConnection(xlsxCs xlPath)
	conn.Open()
	(
		use schema = conn.GetOleDbSchemaTable(OleDbSchemaGuid.Tables, null)
		schema.Dump()
		schema.Dispose()
	)

	let rowQuery = sprintf "select * from [%s]" sheetName
	( 
		use sheetAdapter = new OleDbDataAdapter(rowQuery,conn)
		let rowCount = sheetAdapter.Fill(dt)
		dt.Dump("dt!")
		sheetAdapter.Dispose()
	)
	
	conn.Close()
	conn.Dispose()
)

type Row = { Date: Nullable<DateTime>; pay:decimal; ``job code``:string; band:string;``change %``:double;change:decimal;reason:string;year:double}

let dataTableToRecords (table:DataTable) : 'T list = 
	let fields = FSharpType.GetRecordFields(typeof<'T>)
	[for r in dt.Rows ->
		let values = 
			[| for fld in fields -> r.[fld.Name] |]
			|> Array.map (fun fld -> 
				if fld=(System.DBNull.Value :> obj)
				then 
					//printfn "found a null! setting as None"
					None :> obj 
				else fld
				)
		FSharpValue.MakeRecord(typeof<'T>, values) :?> 'T ]
	//values.Dump()
let rows:Row list = dataTableToRecords dt

type FSharp.Charting.ChartTypes.GenericChart with 
    member this.CreateForm ()= 
        let frm = new Form(Visible = true, TopMost = true, Width = 700, Height = 500, WindowState=FormWindowState.Maximized)
        let ctl = new FSharp.Charting.ChartTypes.ChartControl(this, Dock = DockStyle.Fill)
        frm.Controls.Add(ctl)
        frm.Show()
        ctl.Focus() |> ignore
        frm
AppDomain.CurrentDomain.GetAssemblies()  //.Dump()
	|> Seq.filter (fun a -> a.FullName.Contains("FSharp"))
	|> Dump
	
let doCharting () = 
	let chartable = rows |> Seq.filter (fun r -> r.Date.HasValue)
	use chartForm = Chart.FastLine([ for row in chartable -> row.Date.Value, row.pay ], Name="Netflix").CreateForm()
	//use chartForm =(Chart.Line [ for x in 1.0 .. 100.0 -> (x, x ** 2.0) ]).CreateForm()
	System.Windows.Forms.Application.Run(chartForm)
	
doCharting()