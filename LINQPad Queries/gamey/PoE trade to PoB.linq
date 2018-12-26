<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

// purpose: take a poe.trade page and make items that can be pasted into the create custom of poe.trade
// wip: start with weapons

let debug = true
module Helpers =
    let replace (d:string) r =
        function
        | null | "" as x -> x
        | x -> x.Replace(d,r)
    let rReplace (d:string) (r:string) =
        function
        | null | "" as x -> x
        | x -> Regex.Replace(x,d,r)
open Helpers
module IO =
    let combine y x =
        Path.Combine(x,y)
    let combine' items x = x::items |> Array.ofList |> Path.Combine
let truncate i x =
    match x with
    | null | "" -> x
    | x -> x.[0..i]
module Html =
    open HtmlAgilityPack
    let prettify (x:HtmlNode) =
        let toRemove = ResizeArray()
        x.DescendantsAndSelf()
        |> Seq.iter(fun x ->
            if x.NodeType = HtmlNodeType.Text && String.IsNullOrWhiteSpace x.InnerText then
                toRemove.Add x
        )
        toRemove
        |> Seq.iter(fun x -> x.Remove())
        x
        
    module Impl =
        let removeComments (x:HtmlNode) =
            let i' = ref 0
            x.SelectNodes(".//comment()")
            |> function
                |null -> ()
                | x -> x
                    |> Seq.iteri(fun i x ->
                        i' := i
                        x.Remove())
            printfn "removed %i comments" (!i' + 1)
            x
        open IO
        // not exactly cache, not exactly perm
        let getRawPath targetDirPath =
            if debug then
                printfn "getting cachie"
            let p = targetDirPath |> combine "page.html"
            if Path.GetDirectoryName p |> Directory.Exists |> not then
                p |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
                printfn "Created dir %s" <| Path.GetDirectoryName p
            p
        let getDoc text =
            let hd = HtmlDocument()
            hd.LoadHtml text
            hd
        let getDescendant (name:string) (node:HtmlNode) =
            node.Descendants name
            |> Seq.tryHead
        let removeNodes title (items:HtmlNode seq) = 
            match items with
            | null -> ()
            | items -> 
                let items = List.ofSeq items
                if debug then printfn "found %i %s" items.Length title
                items
                |> List.iter(fun x -> x.Remove())
        let getAll (doc:HtmlDocument) (nodeType:string) =
            doc.DocumentNode.Descendants nodeType
        let getMungePath targetDirPath =
            IO.combine "munged.html" targetDirPath
        let targetFolder =
            Environment.GetFolderPath Environment.SpecialFolder.ApplicationData
            |> combine "poe.trade"
        let rawPath = lazy(getRawPath targetFolder)
        let mungedPath = lazy(getMungePath targetFolder)
        if debug then printfn "made lazy cachie"
    open Impl
    let munge() =
        let cachie= rawPath.Value
        cachie.Dump("target file")
        let mungePath = Path.GetDirectoryName cachie |> getMungePath
        let doc,hDump =
            let html = 
                File.ReadAllText cachie
            getDoc html, fun () -> html.Dump("raw")
        try
            getAll doc "script" |> removeNodes "script"
            getAll doc "iframe" |> removeNodes "iframe"
            doc.DocumentNode.SelectNodes("//comment()")
            |> Seq.iter(fun x -> x.Remove())
            doc.DocumentNode |> prettify |> ignore
            getDescendant "head" doc.DocumentNode
            |> function
                |None ->
                    if debug then eprintfn"Headless?"
                | Some h ->
                    getDescendant "base" h
                    |> function
                        |None ->
                            let baseNode=HtmlNode(HtmlNodeType.Element,h.OwnerDocument,h.ChildNodes.Count)
                            baseNode.Name<-"base"
                            baseNode.Attributes.Add("href","http://poe.trade/search/foobar/")
                            baseNode.Attributes.Add("target","_blank")
                            h.AppendChild baseNode
                            |> ignore<HtmlNode>
                        |Some b ->
                            if debug then printfn "base: %A" b.Attributes
            doc.Save mungePath
        with _ ->
            hDump()
            reraise()
            
        doc
    let clearMunge() = mungedPath.Value |> File.Delete
    let getDocOrMunge() =
        let doc = 
            if File.Exists mungedPath.Value |> not then
                munge()
            else mungedPath.Value |> File.ReadAllText |> getDoc
        if debug then 
            Process.Start mungedPath.Value |> ignore
            doc
            |> fun x -> x.DocumentNode
            |> removeComments
            |> getDescendant "body"
            |> Option.map prettify
            |> function
                |Some x -> x.OuterHtml
                |None -> doc.DocumentNode.OuterHtml
            |> truncate 255
            |> fun x -> x.Dump("result")
            |> ignore
            
            match doc.DocumentNode.InnerHtml with
            | _ -> ()
        doc
Html.clearMunge()
Html.getDocOrMunge()

|> ignore