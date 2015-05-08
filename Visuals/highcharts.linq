<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
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
dumpRaw "<div id='container'></div>"
scriptInjector <| Href "http://code.highcharts.com/adapters/standalone-framework.js"
scriptInjector <| Href "https://code.highcharts.com/highcharts.js"
scriptInjector <| Inline """
	var chart = new Highcharts.Chart({
    chart: {
        renderTo: 'container'
    },

    xAxis: {
        categories: ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    },

    series: [{
        data: [29.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4]
    }]

});
"""
