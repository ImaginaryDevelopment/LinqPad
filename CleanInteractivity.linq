<Query Kind="FSharpExpression">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
</Query>

// clean Html for display elsewhere

let baseLoc =
	"""http://foo.com/Pages/Projects/EditProject.aspx"""
let input =
	let clip =System.Windows.Forms.Clipboard.GetText()
	if clip.StartsWith("<html")=false then failwithf "expected an html document instead of %s..." (clip.Substring(0,5))
	"<!DOCTYPE html>"+clip
printfn "was %i" input.Length
let doc = HtmlAgilityPack.HtmlDocument()
doc.LoadHtml(input)
let selectNodes selector = doc.DocumentNode.SelectNodes(selector)
// removal
let removalElements = ["//script";"//bl";"//input[@type='hidden']"];
for re in removalElements do
	for e in selectNodes(re) do
		e.ParentNode.RemoveChild(e) |> ignore
let removalAttrs =["onmouseover";"onmouseout";"onclick"]
for attr in removalAttrs do
	for e in selectNodes(sprintf "//*[@%s]" attr) do
		e.Attributes.Remove(attr)
		
// updates
let baseRelative u = baseLoc
let getElementsWithAttr e attr = doc.DocumentNode.SelectNodes(sprintf "//%s[@%s]" e attr)
let getElementsWithAttrs vals = 
	seq{
		for (e,attr) in vals do
			yield e,attr,(getElementsWithAttr e attr)
	}
let relativeElements = ["img","src";"link","href";"input[@type='image']","src"]

for (e,attr,nodes) in getElementsWithAttrs relativeElements do
	for node in nodes do
		let u = node.Attributes.[attr]
		if u.Value.StartsWith("http")=false && u.Value.StartsWith("data")=false then u.Value <- Uri(Uri(baseLoc),u.Value).AbsoluteUri

//printfn "is %i" doc.DocumentNode.OuterHtml.Length
//doc.DocumentNode.OuterHtml

//for s in doc.DocumentNode.SelectNodes("//script").ToArray() do
//	s.ParentNode.RemoveChild(s) |> ignore 

//doc.DocumentNode.SelectNodes("//input[@type='hidden']") |>
//	Seq.iter (fun x-> printfn "%s" x.OuterHtml)

//printfn "scripts removed: %i" doc.DocumentNode.OuterHtml.Length
doc.DocumentNode.OuterHtml.Length
doc.DocumentNode.OuterHtml.Dump("oh hai")