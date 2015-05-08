<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>Newtonsoft.Json.Bson</Namespace>
  <Namespace>Newtonsoft.Json.Converters</Namespace>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
  <Namespace>Newtonsoft.Json.Schema</Namespace>
  <Namespace>Newtonsoft.Json.Serialization</Namespace>
</Query>

let dumpRaw (xhtml:string) = Util.RawHtml(xhtml).Dump()
type Script =
	|Inline of string
	|Href of string // * (string list option) todo?
let scriptInjector (script:Script) = 
	match script with
	| Href src -> dumpRaw <| sprintf """<script src="%s"></script>""" src
	| Inline text ->
		dumpRaw <| sprintf """<script>
		//<![CDATA[ 
		%s 
		//]]></script>"""  text


		
// inject a css stylesheet link into the head element
let cssInjector href = 
	sprintf """
	<script>
		var link = document.createElement("link");
		link.href = "%s";
		link.type = "text/css";
		link.rel = "stylesheet";
		document.getElementsByTagName("head")[0].appendChild(link);
	</script>""" href
	
// add classes to the body tag via injected javascript	
let htmlClassInjector className = 
	sprintf """
	<script>
		var target = document.getElementsByTagName("body")[0];
		
		if(target.hasAttribute("class")){
			var att = target.getAttributeNode("class");
			if(att.Contains("%s")==false){
					att.value = att.value + " %s";
			}
		} else {
			var att=document.createAttribute("class");
			att.value = "%s"
			target.setAttributeNode(att);
		}
	</script>""" className className className
// add a css class marker to the body tag so the script can decide how to do things
dumpRaw <| htmlClassInjector "linqPad"


// start charting

scriptInjector <| Href "http://code.highcharts.com/adapters/standalone-framework.js"
scriptInjector <| Href "https://code.highcharts.com/highcharts.js"

// generic application?
let valueOrEmpty (s:string option) = if s.IsSome then s.Value else String.Empty

type TextElement =
	| Json of string
	| Numeric of string
	| Escape of string
	
let renderElement (el:TextElement) : string = 
	let result = match el with | Json s | Numeric s -> s | Escape d -> sprintf "'%s'" d 
	result

type SeriesItem = { Name:string; Data: (TextElement seq)} with
	override y.ToString() = 
		let dataItemsRendered:string seq = Seq.map renderElement y.Data
		let data:string =  String.Join(",",dataItemsRendered)
		let name = y.Name
		sprintf """{name: '%s', data: [%s]}""" name data
type SeriesElement = 
	| Json of string
	| Series of SeriesItem

type HighChart = { RenderTo: string; Title: string option; Subtitle:string option; XAxis: string option; YAxis: string option; Series: SeriesElement seq}

// {Name="hello series"; Data = [Escape "hello series 1"; Escape "hello series2"]}.ToString().Dump("series to string!")


let renderChart (hc:HighChart) = 
	dumpRaw <| sprintf "<div id='%s'></div>" hc.RenderTo
	
	let chartSection = hc.RenderTo |> sprintf """ chart: { renderTo: '%s' }"""
	let titleSection = valueOrEmpty hc.Title |> sprintf """ title: { text: '%s' }"""
	let seriesSection = 
		hc.Series 
		|> Seq.map(fun ser -> match ser with |Json j -> j | Series s -> s.ToString())
		|> (fun items -> "series:["+ String.Join(",",items)+"]")
	let varWrapper s = sprintf "var chart = new Highcharts.Chart(%s);" s
	let objectFormat = 
		
		sprintf """{%s,
        
        tooltip: {
            valueSuffix: '°C'
        },
        legend: {
            layout: 'vertical',
            align: 'right',
            verticalAlign: 'middle',
            borderWidth: 0
        }
		}"""
	
	let scriptFormat s : string = 
		// http://codebork.com/2011/08/17/pretty-printing-json-jsonnet.html
		// alternatives: http://stackoverflow.com/questions/4580397/json-formatter-in-c
		let deserialize s = JsonConvert.SerializeObject(s, Formatting.Indented)
		let serializePretty (s: JObject) = s.ToString(Formatting.Indented)
		let pipe1 = objectFormat >>  Newtonsoft.Json.Linq.JObject.Parse >> serializePretty >>  varWrapper
		let pipe2 = objectFormat >> JsonConvert.DeserializeObject >> deserialize >> varWrapper
		let notPretty = objectFormat >> varWrapper
		Util.OnDemand("Prettified", Func<string>(fun () -> pipe1 s)).Dump()
		notPretty s
	let scriptText = 
		String.Join(",", 
			[
				chartSection
				titleSection
				valueOrEmpty hc.XAxis
				valueOrEmpty hc.YAxis
				seriesSection 
			] |> Seq.filter( fun e -> e <> String.Empty))
		|> scriptFormat
	
    
	printfn "%s" scriptText
	scriptInjector <| Inline scriptText
let series = 
	let seriesTransform (arr: 'a seq) = 
		arr
		|> Seq.map (fun (m:'a) -> Numeric(m.ToString()))
	[
            { Name = "Tokyo" ;Data=seriesTransform  [ 7.0; 6.9; 9.5; 14.5; 18.2; 21.5; 25.2; 26.5; 23.3; 18.3; 13.9; 9.6]}
            { Name ="New York" ; Data =seriesTransform [-0.2; 0.8; 5.7; 11.3; 17.0; 22.0; 24.8; 24.1; 20.1; 14.1; 8.6; 2.5]}
        	{ Name ="Berlin" ;Data =  seriesTransform [-0.9; 0.6; 3.5; 8.4; 13.5; 17.0; 18.6; 17.9; 14.3; 9.0; 3.9; 1.0]}
        	{ Name ="London"; Data = seriesTransform [3.9; 4.2; 5.7; 8.5; 11.9; 15.2; 17.0; 16.6; 14.2; 10.3; 6.6; 4.8]}
	]
	|> Seq.map (fun s -> Series s)
	
// http://jsfiddle.net/gh/get/jquery/1.9.1/highslide-software/highcharts.com/tree/master/samples/highcharts/demo/line-basic/
renderChart { 
	RenderTo= "container"
	Title= Some "Monthly Average Temperature"
	Subtitle=Some """subtitle: {
            text: 'Source: WorldClimate.com',
            x: -20
        }""" 
	XAxis= None
	YAxis= Some """yAxis: {
            title: {
                text: 'Temperature (°C)'
            },
            plotLines: [{
                value: 0,
                width: 1,
                color: '#808080'
            }]
        }"""
	Series= series}
