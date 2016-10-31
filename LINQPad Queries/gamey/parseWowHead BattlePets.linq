<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Jint</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
  <Namespace>Microsoft.FSharp</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>


[<AutoOpen>]
module Helpers =
    let dumpt (t:string) x = x.Dump(t); x
    let dumpBlacklist blacklist t x = x.Dump(description=t,exclude=blacklist)
    let after (delimiter:string) (text:string) = text.Substring(text.IndexOf(delimiter) + delimiter.Length)
    let before (delimiter:string) (text:string) = text.Substring(0,text.IndexOf(delimiter))
    let startsWith d (text:string) = text.StartsWith(d)
    let prettifyJsonObj o = JsonConvert.SerializeObject(o, Formatting.Indented)
    let prettifyJson s = s |> JsonConvert.DeserializeObject |> prettifyJsonObj
    let deserialize<'t> s = JsonConvert.DeserializeObject<'t>(s) 
    // case sensitive
    let deserializePartial propName s = Newtonsoft.Json.Linq.JObject.Parse(s).[propName]
    
module Option = 
    let ofObj x = 
        if isNull <| box x then 
            None
        else Some x
module HtmlElement = 
    let getElement name (x:HtmlNode) = 
        x.Element name
        |> Option.ofObj
    let getElements name (x:HtmlNode) = 
        x.Elements name
        |> Option.ofObj
    let getDescendants (name:string) (x:HtmlNode) = 
        x.Descendants(name)
module JEngine = 
    let toJson defines (s:string)= 
        let engine = Jint.Engine()
        let define = defines |> Seq.map (sprintf "var %s = {};") |> Seq.fold (fun s next -> s + next) String.Empty
        //engine.Execute(sprintf "var LANG = {}; var myTabs={}; JSON.stringify(%s)" s |> dumpt "script").GetCompletionValue()
        engine.Execute(sprintf "%sJSON.stringify(%s)" define s).GetCompletionValue()
        |> string
module HttpClient = 
    let tryGetUrl headers (url:string) = 
        use hc = new System.Net.Http.HttpClient()
        hc.DefaultRequestHeaders.Add("Accept", "application/json")
        
        try
            hc.GetStringAsync(url).Result
            |> Some
        with ex ->
            url.Dump()
            ex.Dump()
            None
    let tryGetHtml (url:string) = 
        use hc = new System.Net.Http.HttpClient()
        try
            hc.GetStringAsync(url).Result
            |> Some
        with ex ->
            url.Dump()
            ex.Dump()
            None
        
    let getToken key secret =
        tryGetUrl ["client_id",key; "client_secret", secret; "grant_type", "client_credentials"] 
module WowHead = 
    open HtmlElement
    open HtmlAgilityPack
    type PetSpeciesInfo = { Health:decimal; Location: int list; Power:decimal; Speed: decimal; Type:int; Name:string;}
    //type PetSpeciesGallery = { Template:string; Id:string;}
    let getBattleTameablePets () =

        let rawHtml = Util.Cache((fun () -> HttpClient.tryGetHtml "http://www.wowhead.com/battle-pets?filter=9:5;2:5;0:0"),"PetSpecies")
        //rawHtml.Dump()
        rawHtml
        //|> Option.map (fun s-> s.Substring(s.IndexOf("<html>",StringComparison.InvariantCultureIgnoreCase)))
        |> Option.map (fun s -> 
            let doc = HtmlAgilityPack.HtmlDocument()
            doc.LoadHtml(s)
            let html = doc.DocumentNode |> getElement "html" |> Option.get
            let scripts = 
                html 
                |> getDescendants "script" 
                |> Seq.map (fun x -> x.InnerText) 
                |> Seq.filter (fun x -> x.Contains("new Listview")) 
                |> Seq.head
                
                |> after "new Listview"
                |> after "("
                //|> dumpt "before the before"
                |> before ";"
                |> before ")"
                //|> dumpt "object?"
                |> JEngine.toJson ["LANG"; "myTabs"]
                |> deserializePartial "data"
                |> Seq.map string
                |> Seq.map deserialize<PetSpeciesInfo>
                |> Seq.toList
            scripts //, s
        )
        |> Option.get
    let pets = getBattleTameablePets()
    let path = Util.CurrentQueryPath |> Path.GetDirectoryName |> fun p -> Path.Combine(p,"PetSpecies.json")
    pets 
    |> Newtonsoft.Json.JsonConvert.SerializeObject
    |> fun x -> File.WriteAllText(path, x)
    pets.Dump("written to " + path)
    