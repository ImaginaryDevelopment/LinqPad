<Query Kind="FSharpProgram" />

// Path Monad?
[<AutoOpen>]
module Helpers = 
    let trimEnd1 items (s:string) = s.TrimEnd items
    
    //         // both of the following are taken from http://stackoverflow.com/questions/6497058/lazy-cartesian-product-of-multiple-sequences-sequence-of-sequences
    let cartesian_product sequences =
        let step acc sequence = seq {
            for x in acc do
            for y in sequence do
            yield seq { yield! x; yield y}}
        Seq.fold step (Seq.singleton Seq.empty) sequences
    
// should this be a DU?

//type WrappedPath = 
//    | DirPath of string
//    | FilePath of string

// from my old extensions file at https://github.com/ImaginaryDevelopment/config/blob/master/My%20Documents/LINQPad%20Plugins/MyExtensions.FW40.linq
// PathWrapper can wrap any path, not specifically dir or file
module PathWrapping = 
    let seperators = [Path.AltDirectorySeparatorChar;Path.DirectorySeparatorChar]
    let normalizePath = 
        seperators 
        |> Array.ofList
        |> trimEnd1 
        //>> replace (Path.AltDirectorySeparatorChar|> string) (Path.DirectorySeparatorChar |> string)
    //let getRelativePathTo otherPath path =             
open PathWrapping
// validating a path seems to be a huge problem: http://stackoverflow.com/questions/6198392/check-whether-a-path-is-valid
type PathWrapper(rawPath) = 
    member x.RawPath = rawPath
    static member Normalize = normalizePath
    member x.Normalized = rawPath |> PathWrapper.Normalize
    member x.GetRelativePathTo otherPath = 
        Uri(x.Normalized).MakeRelativeUri(Uri(PathWrapper.Normalize otherPath))
        |> string
        |> replace "%20" " "
    member x.Combine segment = 
        PathWrapper(Path.Combine(rawPath,segment))
    member x.GetSegments() = 
        
        let first,path = if rawPath |> startsWith @"\\" then @"\\", rawPath.Substring 2 else String.Empty, rawPath
        let splits = path.TrimEnd(seperators |> Array.ofList).Split(seperators |> Array.ofList)
        if String.IsNullOrEmpty first then 
            splits |> List.ofArray
        else 
            splits |> Seq.head |> (+) first
            |> fun head -> head::(splits |> Seq.skip 1 |> List.ofSeq)


    
            
#if LINQPAD    
type PathWrapper with
    member x.ToAHref() = 
        LINQPad.Util.RawHtml(sprintf """<a href="%s" title="%s">link</a>""" this.RawPath this.RawPath ) //.Dump(path.Key.ToString());
    member x.AsExplorerSelectLink text =
        let args = sprintf "/select,%s" x.RawPath
        let result = Hyperlinq(QueryLanguage.Expression, sprintf "Process.Start(\"Explorer.exe\",@\"%s\")" arguments, text= text)
#endif

module Tests = 
    // Debug.Assert nor assert actually throw or fail in any way when run from linqpad it seems
    let assertEqual expected actual  =
        Debug.Assert((expected = actual))
        assert (expected = actual)
        if expected = actual then () else failwithf "assertion failed expected:%A actual:%A" expected actual
    do
        let projectsBaseVariants =
            // get every possible combination of list 1 joined by an item from list 2, optionally trailing
            let mateSeperators pathStart = 
                let result = 
                    seperators
                    |> List.map string
                    |> (@) [String.Empty]
                    |> Seq.map((+) pathStart)
                    |> List.ofSeq
                //printfn "input: %s, result: %A" pathStart result
                result
                
                
                
            ["C:";"projects"]
            |> Seq.fold(fun (paths: string list) (segment:string) ->
                match paths with
                | [] -> mateSeperators segment
                | paths -> 
                    paths
                    |> Seq.choose (fun pathStart ->
                        let keeper= seperators |> Seq.exists (fun sep -> pathStart.[pathStart.Length - 1] = sep)
                        if keeper then
                            pathStart + segment 
                            |> mateSeperators
                            |> Some
                        else 
                            None
                    )
                    |> Seq.collect id
                    |> List.ofSeq
            ) List.empty
            |> Dump

        let getSegmentsTests = 
            projectsBaseVariants
            |> Seq.map (fun x -> PathWrapper(x).GetSegments())
            |> Seq.iter (assertEqual ["C:";"projects"])
            
        let getRelativeToTests =
            projectsBaseVariants
            |> Seq.map (PathWrapper >> fun x -> x.GetRelativePathTo @"C:\projects\hello world")
            |> Seq.iter (assertEqual "projects/hello world")
            @"\projects\hello world" |> PathWrapper
            PathWrapper(@"c:\projects").GetRelativePathTo @"C:/projects/hello world" |> assertEqual "projects/hello world"

        ()
        

//        Debug.Assert( @"C:\program files\".AsFilePath().GetSegments().Count()=2);
//        Debug.Assert( @"C:\program files".AsFilePath().GetSegments().Count()=2);
//        Debug.Assert( @"\\vbcdapp1\c$".AsFilePath().GetSegments().Count()=2,"GetSegments on a network path");
//        Debug.Assert( @"\\vbcdapp1\c$\".AsFilePath().GetSegments().Count()=2,"GetSegments on a network path");