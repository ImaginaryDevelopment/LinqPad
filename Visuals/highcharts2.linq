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

// start charting
dumpRaw "<div id='container'></div>"
scriptInjector <| Href "http://code.highcharts.com/adapters/standalone-framework.js"
scriptInjector <| Href "https://code.highcharts.com/highcharts.js"
scriptInjector <| Inline """
	var chart = new Highcharts.Chart({
    chart: {
        renderTo: 'container'
    },
 title: {
            text: 'Monthly Average Temperature',
            x: -20 //center
        },
        subtitle: {
            text: 'Source: WorldClimate.com',
            x: -20
        },
        xAxis: {
            categories: ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        },
        yAxis: {
            title: {
                text: 'Temperature (°C)'
            },
            plotLines: [{
                value: 0,
                width: 1,
                color: '#808080'
            }]
        },
        tooltip: {
            valueSuffix: '°C'
        },
        legend: {
            layout: 'vertical',
            align: 'right',
            verticalAlign: 'middle',
            borderWidth: 0
        },
        series: [{
            name: 'Tokyo',
            data: [7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6]
        }, {
            name: 'New York',
            data: [-0.2, 0.8, 5.7, 11.3, 17.0, 22.0, 24.8, 24.1, 20.1, 14.1, 8.6, 2.5]
        }, {
            name: 'Berlin',
            data: [-0.9, 0.6, 3.5, 8.4, 13.5, 17.0, 18.6, 17.9, 14.3, 9.0, 3.9, 1.0]
        }, {
            name: 'London',
            data: [3.9, 4.2, 5.7, 8.5, 11.9, 15.2, 17.0, 16.6, 14.2, 10.3, 6.6, 4.8]
        }]
});
"""
