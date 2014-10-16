<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
</Query>

let cssInjector href = 
	sprintf """
	<script>
		var link = document.createElement("link");
		link.href = "%s";
		link.type = "text/css";
		link.rel = "stylesheet";
		document.getElementsByTagName("head")[0].appendChild(link);
	</script>""" href
	
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

let bootstrapInjector = cssInjector "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
Util.RawHtml(bootstrapInjector + """<span class="text-info"> css injected</span>""").Dump()
Util.RawHtml(htmlClassInjector "linqPad").Dump()
let headLines,bodyLines = 
	let raw = new HtmlDocument()
	raw.Load(file)
	// raw.Dump()
	let head,bodies=raw.DocumentNode.SelectNodes("//head/*"),raw.DocumentNode.SelectNodes("//body/*")|> Seq.map(fun e-> e.OuterHtml)
	// bodies |> (fun e->e.Dump("bodies!"))
	head,bodies
	
for b in bodyLines do
	//h.Dump("a body line!")
	Util.RawHtml(b).Dump()