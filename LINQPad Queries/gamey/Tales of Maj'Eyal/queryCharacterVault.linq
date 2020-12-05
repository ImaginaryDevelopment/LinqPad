<Query Kind="FSharpExpression">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
</Query>

let path = "characters-vault"
let flip f x y = f y x
let encodeQueryValue (x:string) =
    Uri.EscapeDataString x
let httpClient () =
    let c = new System.Net.Http.HttpClient()
    c.BaseAddress <- Uri "https://te4.org/"
    c
let cache (key:string) (f:unit -> 't) =
    Util.Cache(Func<_> f, key)
let parseHtml (x:string) =
    let d = HtmlAgilityPack.HtmlDocument()
    d.LoadHtml x
    d
let getElementById x (d:HtmlAgilityPack.HtmlDocument) =
    d.GetElementbyId x
let getOuterHtml (x:HtmlAgilityPack.HtmlNode) =
    x.OuterHtml
let toQueryValues k items =
    items
    |> Seq.map(encodeQueryValue >>  sprintf "%s=%s" k)
    
let getCharacters (kvs:Map<string,string list>) =
    let query = (List.empty,kvs) ||> Map.fold(fun s k -> toQueryValues k >> String.concat "&" >> fun v -> v::s) |> String.concat "&"
    async{
        use c = httpClient()
        let fullpath = sprintf "%s?%s" path query
        fullpath.Dump("query uri")
        
        let! chars = Async.AwaitTask <| c.GetStringAsync fullpath
        return parseHtml chars |> getElementById "characters" 
    }
let getPage () =
    async {
        use c = httpClient()
        return! Async.AwaitTask <| c.GetStringAsync path 
    }
let setupInputs () =
    let getForm (d:HtmlAgilityPack.HtmlDocument) =
        d
        |> getElementById "selectors"
        |> fun el -> el.OuterHtml
    cache "formpage" <| fun () ->
            getPage()
            |> Async.RunSynchronously
    |> parseHtml
    |> getForm
    
let alwaysInputs =
    Map [
        "tag_official_addons", ["1"] // only querying characters using only official addons
        "tag_winner", ["winner"] // only query winners
    ]
setupInputs()
|> ignore


alwaysInputs
|> getCharacters
|> Async.RunSynchronously
|> getOuterHtml 