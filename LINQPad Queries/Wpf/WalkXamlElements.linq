<Query Kind="FSharpProgram" />

let reverseDump = 
    let dc = DumpContainer()
    let mutable items = List.empty
    dc.Content<- items
    dc.Dump()
    fun x ->
        dc.Content <- null
        items <- (box x)::items
        dc.Content <- items
        x
        
let getAttrVal (name:string) (x:XElement) =
    x.Attribute(XNamespace.None + name)
    |> Option.ofObj
    |> Option.map(fun a -> a.Value)
let xDoc = XDocument.Load(@"C:\tfs\practicemanagement\trunk\PracticeManagement\Billing\ConsentFormFlowDocument.xaml")

xDoc.Descendants()
|> Seq.map(fun x -> x.Name <- XNamespace.None + x.Name.LocalName; x)
|> Seq.filter(fun xe -> xe.Name.LocalName = "TextBlock")
|> reverseDump
|> Seq.filter(getAttrVal "FontFamily" >> Option.isNone)
|> reverseDump
|> ignore
