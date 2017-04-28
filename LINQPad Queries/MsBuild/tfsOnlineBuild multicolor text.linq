<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Drawing</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

// purpose: display an msbuild log file with line coloring and other nice to have features
// nice to have:
//  get tfs online build logs with help from : http://stackoverflow.com/questions/39715860/get-zipped-tfs-2015-vnext-build-output-logs-through-powershell-just-like-the

let doWinForms = true
[<AutoOpen>]
module StringExtensions = 
    let private sc = StringComparison.InvariantCultureIgnoreCase
    type System.String with
        static member startsWithI (delim:string) (x:string) = 
            x.StartsWith(delim, sc)
        static member endsWith (delim:string) (x:string) : bool =
            x.EndsWith(delim)
        static member containsI (delim:string) (x:string) =
            x.Contains(delim, comparison=sc)
        static member trim (x:string) =
            x.Trim()
    let before delim (x:string) = 
        x.Before(delim)
    let beforeWithDelim delim x :string = 
        x
        |> before delim
        |> fun x -> sprintf "%s%s" x delim
        
    let after (delim:string) (x:string) = 
        x.After(delim)
    let afterI delim (x:string) = 
        x.After(delim, Nullable sc)
    let afterIAnyOf delims (x:string) = 
        delims
        |> Seq.map Regex.Escape
        |> delimit "|"
        |> fun delimiters -> Regex.Match(x,sprintf "^(?:%s)(.*)" delimiters).Groups.[1].Value
        
        
        
    let boolToUnitOption b = if b then Some() else None
    let (|StartsWithI|_|) delim x = 
        String.startsWithI delim x
        |> boolToUnitOption
    let (|EndsWith|_|) delim x = 
        String.endsWith delim x
        |> boolToUnitOption
    let (|ContainsI|_|) delim x = 
        String.containsI delim x
        |> boolToUnitOption
        
    let (|ContainsIX|_|) delim x = 
        String.containsI delim x
        |> function 
            | true -> Some delim
            | false -> None


module Colored = 
    let private appendLine (box:RichTextBox) text color = 
        box.SelectionStart <- box.TextLength
        box.SelectionLength <- 0
        box.SelectionColor <- color
        if String.endsWith "\r\n" text then
            text
        else
            sprintf "%s\r\n" text
        |> fun x -> box.AppendText x
        box.SelectionColor <- box.ForeColor
    let rt = 
        if doWinForms then new RichTextBox(ReadOnly = true,BackColor = Color.Black) else null
    rt
    |> Option.ofObj
    |> Option.iter (fun rt ->
        rt.Dump()
    )
    
    let append text color = 
        if doWinForms then
            appendLine rt text color
        else ()

let text = Util.Cache((fun () -> System.Windows.Forms.Clipboard.GetText()), "clipText")

type BuildLine = 
    | LogLine of DateTime * string
    | Other of string

[<AutoOpen>]
module Regexing =     
    let (|RMatch|_|) (pattern:string) (text:string) = 
        match Regex.Match(text,pattern) with
        | r when r.Success -> Some r
        | _ -> None
    let (|RMatches|_|) (pattern:string) (text:string) = 
        match Regex.Matches(text,pattern) with
        | r when r.Count > 0 -> Some r
        | _ -> None
    
    [<Literal>]
    let msBuildPattern = @"\s*##\[command\]""*\w:.*msbuild.exe""\s+(?<args>.*)"
    let flag = @"(?<flag>/(\w+))(?: |$)"
    let argdParam = @"(?<argdParam>/(\w+):(?:(\w+)(?: |$)|""(?<quotedArg>[^""]+)""(?: |$)))"
    let quotedParam = @"""(?<quotedArg>[^""]+)""(?: |$)"
    let plainWord = @"/(?<plainWord>\w+)"
    // /p:x=y;c=d
    let propParam = @"/p(?:roperty)?:(?<propParam>(""[^""]+""(?: |$)))"
    let multiArgdParam = 
        let starter = @"(?<starter>\w+)[:=]"
        // one arg was quoted followed by *ForwardingLogger (ending with ,)
        let quotedArg = @"""[^""]+""(?:\w|\*)*"
        // can include a filepath with no spaces (unquoted) so \w or : or \\
        let unquoted = @"[\w:\\]+"
        let multiFinisher = "(?:,|;)?"
        sprintf "/(?<multiArgdParam>%s(?<arg>%s%s|%s)+)(?: |$)" starter multiFinisher quotedArg unquoted
        //@"((?:,|;|)?(?:\w+|""[^""]+""(?:\w|\*)*))+)(?: |$)" // should we not attempt to ensure what's next is space or end of line, may not be safe to do so?
    let msBuildArgPieces = [ propParam; argdParam; quotedParam; plainWord; multiArgdParam]
    let msBuildArgPieceNames = msBuildArgPieces |> Seq.map (fun p -> Regex.Matches(p,"\(\?<(\w+)>") |> Seq.cast<Match> |> Seq.map (fun m -> m.Groups.[1].Value)) |> Seq.concat
    let msBuildArgPieceMeal = 
        msBuildArgPieces
        |> fun x -> String.Join("|", Array.ofList x)
    let spaceOrStringLiteral = @"(?<arg>(?:""[^""]+?""|[^ ])+(?: |$))"
module Slicing = 
    // this one encodes . when it doesn't need to, but should work fine
    let escape = System.Security.SecurityElement.Escape // or we could reference System.Web and use HttpUtility.HtmlEncode t 
    let sliceMsBuildArg (t:string) =
        match t with
        | StartsWithI "/p:"
        | StartsWithI "/property:" ->
            t
            |> String.split [";"]
            |> List.ofSeq
            |> function
                | h::tl ->
                    h
                    |> afterIAnyOf ["/p:";"/property"]
                    |> fun newHead -> newHead::tl
                | x -> x
            |> Seq.map (escape >> sprintf "<li>%s</li>")
            |> delimit "\r\n  "
            |> sprintf "<div>property:<ul>\r\n  %s</ul></div>"
            
        | _ -> 
            escape t // or we could reference System.Web and use HttpUtility.HtmlEncode t
        |> sprintf "<div>%s</div>"
        |> Util.RawHtml
    
    let sliceMsBuildArgs (t:string) =
        
        //Regex.Matches(msg, @"(?<arg>(?:""[^""]+?""|/|\w|-|\.|_|;|,)+(?: |$))+")
        Regex.Matches(t, spaceOrStringLiteral)
        |> Seq.cast<Match>
        |> Seq.map (fun m ->
            //printfn "found arg? %s" m.Value
            sliceMsBuildArg m.Value
        )
        |> Dump
        |> ignore
        
        
    let sliceCompilerArg surround (t:string) :string = 
        match t with
        // handle subsplits here if there are any
//            |> Seq.map (escape >> sprintf "<li>%s</li>")
//            |> delimit "\r\n  "
//            |> sprintf "<div>property:<ul>\r\n  %s</ul></div>"
        | x -> escape x
        |> fun x -> sprintf "<%s>%s</%s>" surround x surround
//                
//    let sliceFsc t = 
//        t
//        |> after "fsc.exe"
//        |> function
//            | RMatches spaceOrStringLiteral m ->
//                m
//                |> Seq.cast<Match>
//                |> Seq.map (fun m -> m.Value)
//                
//                |> Seq.fold (fun state arg ->
//                    // going to have to use reverse at the end
//                    match arg,state with
//                    // if there's a reference arg, and the head list item is a reference arg, mate, other wise start new list
//                    | ReferenceArg as a, ((ReferenceArg as prevRa)::otherRefs) :: tl ->
//                        // more refs, mate them
//                        [[a::otherRefs];
//                ) List.empty
//                |> List.ofSeq
//                |> List.rev
//                |> Seq.map (sliceCompilerArg "li")
//                ), t |> beforeWithDelim "fsc.exe"
//            | args -> failwithf "how did we get here in compiler args? %s" args

    let sliceCompilation t =
        match t|> String.trim with
        | (ContainsIX "fsc.exe" c as x)
        | (ContainsIX "csc.exe" c as x) ->
            
            let compilerFullPath = x |> beforeWithDelim c
            x
            |> after c
            |> function
                | RMatches spaceOrStringLiteral m ->
                    let x1 = 
                        m
                        |> Seq.cast<Match>
                        |> Seq.map (fun m -> 
                            m.Value |> sliceCompilerArg "li"
                        )
                        |> List.ofSeq
                    x1
                | args -> failwithf "how did we get here in compiler args? %s" x
            |> delimit "\r\n    "
            |> sprintf "<div title=\"%s\">%s\r\n  <ul>\r\n    %s</ul>\r\n</div>" (escape x) compilerFullPath
        | x -> failwithf "compilationSlice not implemented:%s" x 
        |> Util.RawHtml
        |> Dump
        |> ignore

        
open Slicing
text
|> String.split [ "\r\n"; "\n";"\r"]
|> Seq.iter (
    function
    | RMatch "(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})T(?<hour>\d{1,2})[^ ]+ (?<msg>.*)$" r ->
        let msg = r.Groups.["msg"].Value
        match msg with
        | (RMatch @"\s*##\[command\]""*\w:.*msbuild.exe""\s+(?<args>.*)" r) as x ->
            //print msbuild lines in green
            Colored.append x Color.Green
            // also dump the command with arguments sliced up to the results pane
            
            let argsUnsplit = r.Groups.["args"].Value
            
            printfn "msbuild argsUnsplit:%s" argsUnsplit
            sliceMsBuildArgs argsUnsplit
            
        | (ContainsI "error fs" as x)
        | (ContainsI "error cs" as x)
        | (ContainsI "error msb" as x) ->
            Colored.append x Color.Blue
        | (ContainsI "warning fs" as x)
        | (ContainsI "warning cs" as x) ->
            Colored.append x Color.DarkGoldenrod
        | (ContainsI "fsc.exe" as x)
        | (ContainsI "csc.exe" as x) ->
            Colored.append x Color.DarkGray
            sliceCompilation x
        | x -> Colored.append x Color.White
    | x -> 
        // if anything doesn't match the pattern, red it
        Colored.append x Color.Red
        
    
)