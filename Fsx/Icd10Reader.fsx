#r "System.Xml"
#r "System.Xml.Linq"
open System
open System.Xml
open System.Xml.Linq

module Helpers = 
    let delimit delimiter (values:string seq) = String.Join(delimiter,values)
    let replace (item:string) replace (s:string) = s.Replace(item,replace)
    let combine basePath path = System.IO.Path.Combine(basePath,path)
    let wrap wrapper (s:string) = wrapper + s + wrapper
    let wrap2 left right s = left + s + right
    let nullToOpt x = if x = null then None else Some x
    let (|StartsWith|_|) delimiter (s:string) = if s.StartsWith(delimiter) then Some () else None
    let (|StringEquals|_|) text (s:string) = if s = text then Some() else None
    let (|ExtensionNote|_|) text (s: string list) = if s |> Seq.length = 1 && s |> Seq.head |> (=) text then Some() else None
    let (|ExtensionNoteStartsWith|_|) text (s: string list) = if s |> Seq.length = 1 && s |> Seq.head |> (fun s-> s.StartsWith(text)) then Some() else None
    let (|AllowedExtensionNote|_|) rawCode = 
        function
        | Some s -> 
            s
            |> function
                | StringEquals (sprintf "The appropriate 7th character is to be added to all codes from category %s" rawCode)
                | StringEquals (sprintf "The appropriate 7th character is to be added to each code from category %s" rawCode) 
                | StringEquals (sprintf "The appropriate 7th character is to be added to each code from subcategory %s" rawCode) 
                | StringEquals (sprintf "One of the following 7th characters is to be assigned to each code in subcategory %s to designate the stage of glaucoma" rawCode) 
                | StringEquals (sprintf "The appropriate 7th character is to be added to each code from subcategory %s:" rawCode) 
                | StringEquals (sprintf "The appropriate 7th character is to be added to each code in subcategory %s" rawCode) 
                | StringEquals (sprintf "The appropriate 7th character is to be added to all codes in subcategory %s" rawCode) 
                | StringEquals (sprintf "The appropriate 7th character is to be added to each code in subcategory  %s" rawCode) // yes there is actually a code with two spaces as the only difference for the note
                | StartsWith (sprintf "One of the following 7th characters is to be assigned to each code under subcategory %s" rawCode) 
                | StartsWith (sprintf "The following appropriate 7th character is to be added to subcategory %s" rawCode)
                    -> Some ()
                | _ -> None
        | _ -> None

open Helpers

module Seq = 
    let any (items:#seq<_>) = Seq.exists( fun _ -> true) items

open System.Diagnostics

let runProc filename args startDir = 
    let procStartInfo = 
        ProcessStartInfo(
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            FileName = filename,
            Arguments = args
        )
    match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

    let outputs = System.Collections.Generic.List<string>()
    let errors = System.Collections.Generic.List<string>()
    let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
    let p = new Process(StartInfo = procStartInfo)
    p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
    p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))
    let started = p.Start()
    if not started then
        failwithf "Failed to start process %s" filename
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()
    outputs,errors

let tf startDir args = 
    runProc @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\TF.exe" args startDir

let tfAdd basePath items =
    let addItem item = sprintf "add %s" item |> tf basePath
    let readOutputs (o,e) = 
        if e|> Seq.exists (fun e' -> String.IsNullOrWhiteSpace( e' ) = false) then
            o |> Seq.iter (printfn "%s")
            e |> Seq.iter (printfn "error:%s")
        else
            o |> Seq.iter (printfn "%s")
    items |> Seq.iter (addItem >> readOutputs)

let tfCheckout basePath items = 
    let checkout item = sprintf "checkout %s" item |> tf basePath
    let readOutputs (o,e) = 
        if e|> Seq.exists (fun e' -> String.IsNullOrWhiteSpace( e' ) = false) then
            o |> Seq.iter (printfn "%s")
            e |> Seq.iter (printfn "error:%s")
        else
            o |> Seq.iter (printfn "%s")
    items |> Seq.iter (checkout >> readOutputs)

type Diagnosis = {Code:string;Desc:string;IsBillable:bool; Unextended:string}


module Diags = 
    let getSectionalizedDiags (icd10Path:string) = 
        let xDoc = XDocument.Load(icd10Path)
        let rootNs = xDoc.Root.Name.Namespace
        let getElementValue name (parent : XElement) = parent.Element(rootNs + name).Value |> nullToOpt 
        let getElements name (parent:XElement) = parent.Elements(rootNs + name)
        let getDiagName = getElementValue "name"
        let getAttrValue name (parent:XElement) = 
            let attr = parent.Attribute(XNamespace.None + name)
            if attr = null then None else Some attr.Value

        let getAttrValueOrNull name parent = 
            match getAttrValue name parent with 
            | None -> null
            | Some x -> x

        rootNs,getElements "chapter" xDoc.Root
        |> Seq.map (fun c -> 
            let walkchapter node =c |> getElements "section" |> Seq.collect (getElements "diag")
            match getElementValue "name" c,getElementValue "desc" c with 
            | Some name, Some desc -> name,desc, walkchapter c
            | _ -> 
                let elementNames = c.Elements() |> Seq.map(fun e -> e.Name.LocalName) |> Seq.iter( printfn "unnamed chapter node: %s")
                failwithf "Chapter was unnamed or had no desc"
            )

    let getIcd10Diags icd10Path = 
        let rootNs,diags = getSectionalizedDiags(icd10Path)
        let getElement name (parent:XElement) = parent.Element(rootNs + name) |> nullToOpt 
        let getElementValue name (parent : XElement) = parent.Element(rootNs + name).Value

        let getElements name (parent:XElement) = 
            let result = parent.Elements(rootNs + name) |> List.ofSeq
            match result with 
            | [] -> None
            | _ -> Some result

        let getDiagName = getElementValue "name"
        let getAttrValue name (parent:XElement) = 
            let attr = parent.Attribute(XNamespace.None + name)
            if attr = null then null else attr.Value

        let rec descend parentExtensions node =
            let childDiagnoses = getElements "diag" node

            let rawCode = node |> getDiagName
            let cleanedCode = rawCode |> replace "." String.Empty
            let baseDiagnosis = {Code= cleanedCode ;Desc= node|> getElementValue "desc";IsBillable = Option.isNone childDiagnoses; Unextended=cleanedCode}
            //printfn "checking for extensions on node %A" baseDiagnosis
            let extensionElements = 
                node.Element(rootNs + "sevenChrDef")
                |> nullToOpt 
                |> Option.bind (getElements "extension")

            let extensionNote() = 
                node
                |> getElement "sevenChrNote"
                |> Option.bind( getElement "note" >> Option.bind (fun e -> e.Value |> Some))

            let descendChildren children extensions = 
                //if baseDiagnosis.Code="V00" then failwithf "found V00! hasExtension=%A, extensionElements= %A,childDiags=%A" hasExtension extensionElements childDiagnoses
                match extensions with
                | Some extensionElements -> 
                    children |> Seq.map (descend (Some extensionElements)) |> Seq.collect id
                | None -> 
                    if Option.isSome parentExtensions then failwith "descend children called with parentExtensions"
                    children |> Seq.map (descend None) |> Seq.collect id

            let walkWithExtensions (extensions:XElement seq) = 
                seq{
                            for x in extensions do
                                let value = x.Value
                                let attr = getAttrValue "char" x
                                yield { baseDiagnosis with Code= baseDiagnosis.Code.PadRight(6,'X') + attr;Desc = sprintf "%s (%s - %s)" baseDiagnosis.Desc attr value }
                        } |> List.ofSeq |> Seq.ofList
            //printfn "matching on node %A" baseDiagnosis
            let allowedExtensionCodes = ["M1A";"M80";"O31";"O32";"O35";"O36";"O40";"O41";"O64";"O69";"S00";"S01";"S02";"S42"]
            let walkExtensionParent extensions children = 
                let limitMap = 
                    [
                    "S12",[0..6]
                    "S49",[0..1]
                    "S59",[0..2]
                    "S79",[0..1]
                    "S89",[0..3]
                    ] |> Map.ofSeq
                if allowedExtensionCodes |> Seq.contains rawCode then 
                    descendChildren children (Some extensions)
                elif Map.containsKey rawCode limitMap then
                    let useExtensions = limitMap.[rawCode] |> Seq.map (sprintf "%s.%i" rawCode) |> List.ofSeq
                    children
                    |> Seq.map ( fun c -> if useExtensions |> Seq.contains (getDiagName c) then descend (Some extensions) c else descend None c )
                    |> Seq.collect id
                else
                    match extensionNote() with 
                    | AllowedExtensionNote rawCode -> descendChildren children (Some extensions) // must have children
                    | Some note -> failwithf "1:Failed on node %s, hasDiagChildren, hasExtension with note %A" rawCode note
                    | _ -> failwithf "2:Failed on node %s, hasDiagChildren, hasExtension with no note" rawCode

            match parentExtensions,extensionElements,childDiagnoses with
            | None,                 None,               None ->  seq { yield baseDiagnosis }
            | None,                 None,               Some children -> seq{ yield baseDiagnosis; yield! children |> Seq.map (descend None) |> Seq.collect id}
            | None,                 Some extensions,    None -> walkWithExtensions extensions
            | None,                 Some extensions,    Some children -> walkExtensionParent extensions children
            | Some parentExtensions, None,              None -> walkWithExtensions parentExtensions
            | Some parentExtensions, None,              Some children -> 
                let results = 
                    seq {
                        yield! walkWithExtensions parentExtensions
                        yield! children |> Seq.map (descend None) |> Seq.collect id
                    }
//                if rawCode.StartsWith("V00") && rawCode <> "V00" then failwithf "code=%s, yielding:%i, hasExtension=%A, hasDiagChildren=%A parentExtensions= %A" rawCode (Seq.length results) hasExtension hasDiagChildren parentExtensions
                results
            | Some _, Some extensions, None -> walkWithExtensions extensions
            | Some _, Some extensions, Some children -> 
                // parent has extensions, but we have our own!
                match extensionNote() with 
                    | AllowedExtensionNote rawCode -> descendChildren children (Some extensions) // must have children
                    | Some note -> failwithf "3:Failed on node %s, hasDiagChildren, hasExtension with note %A" rawCode note
                    | _ -> failwithf "4:Failed on node %s, hasDiagChildren, hasExtension with no note" rawCode
            
            //| Some parentExtensions, Some extensions, _ -> failwithf "5:Failed on node %s, hasDiagChildren: %A,hasParentExtensions, hasExtension %A with note(s) %A" rawCode hasDiagChildren hasExtension extensionNotes

        diags
        |> Seq.map (fun (sectionName,desc,diags) ->
        sectionName,desc,diags |> Seq.collect (descend None)
        )
