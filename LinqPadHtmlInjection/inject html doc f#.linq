<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
</Query>

let fileName = Util.ReadLine("html path?")

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

Util.RawHtml(htmlClassInjector "linqPad").Dump()

let makeLinqPadable (file:string) = 
	let getAttribValOrEmpty (node:HtmlNode) (attribName:string) = 
		let attrib = node.Attributes.[attribName]
		if  attrib = null || attrib.Value = null then
			System.String.Empty
		else attrib.Value
		
	let headLines,bodyLines = 
		let raw = new HtmlDocument()
		raw.Load(file)
		// raw.Dump()
		let head,bodies=raw.DocumentNode.SelectNodes("//head/*"),raw.DocumentNode.SelectNodes("//body/*")
		// bodies |> (fun e->e.Dump("bodies!"))
		//(head,bodies).Dump()
		head,bodies
	[
		for h in headLines do
			match h with 
			| script when script.OuterHtml.StartsWith("<script") -> yield script.OuterHtml // these shouldn't require head injection
			| link when link.Name="link" && (getAttribValOrEmpty link "rel") = "stylesheet" && (getAttribValOrEmpty link "href") <> System.String.Empty ->
				let href = link.Attributes.["href"].Value
				printfn "css injecting into head %s" href
				yield cssInjector href
			| _ ->
				printfn "unknown head element type caught injecting to body %s with attrs %A" h.Name (h.Attributes |> Seq.map (fun a -> a.Name,a.Value))
				yield h.OuterHtml // hope it will work outside of head
		yield! bodyLines |> Seq.map(fun e->e.OuterHtml)
	]

for toDump in (makeLinqPadable fileName) do
	Util.RawHtml(toDump).Dump()


