<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//var input=LINQPad.Util.ReadLine<string>("What shall we encode?");
let input=System.Windows.Forms.Clipboard.GetText()
module Helpers =
    let delimit (delimiter:string) (items:#seq<string>) = String.Join(delimiter,items)
    let after (delimiter:string) (x:string) =
        match x.IndexOf delimiter with
        | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
        | i -> x.[i + delimiter.Length ..]
    let replace (target:string) (replacement) (str:string) = if String.IsNullOrEmpty target then invalidOp "bad target" else str.Replace(target,replacement)
        
    let dumpt title x = x.Dump(description = title); x
    let trimEnd (delim:char) (x:string) = 
        x.TrimEnd(delim)
    let splitLines(x:string) = x.Split([| "\r\n";"\n"|], StringSplitOptions.None)
    module Option =
        let getOrDefault y =
            function
            | Some x -> x
            | None -> y

    // works but won't compile if we don't have a usage for it
    //let dumpLen x = (^T:(member Length:int) x).Dump("length"); x
    let addLeadingSpaceIfValue s = 
        match s with 
        | null
        | "" -> s
        | x -> sprintf " %s" x
    let prependIfValue prep s = 
        match s with
        | null
        | "" -> s
        | x -> sprintf "%s%s" prep s
    let appendIfValue postfix s = 
        match s with
        | null
        | "" -> s
        | x -> sprintf "%s%s" s postfix
open Helpers     

module Scriptify = 
    let getScriptRefText srcType src = 
        sprintf """<script src="%s"%s></script>""" src (srcType |> prependIfValue " type=\"" |> appendIfValue "\"")
        
module Scripting = 

    let clipInjectText = Scriptify.getScriptRefText "text/javascript" "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/1.6.1/clipboard.min.js"
    // cdata tags were necessary
    let useClipText ="""
        <script type="text/javascript">
            //<![CDATA[
          Clipboard && new Clipboard('.btn');
          
          //]]>
        </script>"""
    Util.RawHtml(clipInjectText).Dump()
    Util.RawHtml(useClipText).Dump()    
module Html = 

    type TagClosure = 
        |Content of string
        |SelfClose
        |NoSelfClose
        |EitherClosing
        // Input tag isn't supposed to be closed as it is a 'void' element, but we're going to close it here, browser is ok with it
        // |NoClosingAllowed
    // not really for direct exposure to other modules, but not making private
    
    let htmlElement name attributeMap closing = 
        attributeMap
        |> Map.toSeq
        |> Seq.map(fun (k,v) -> sprintf "%s=\"%s\"" k (System.Web.HttpUtility.HtmlAttributeEncode v))
        |> delimit " "
        |> prependIfValue " "
        |> (fun a ->
            match closing with
            | NoSelfClose -> sprintf "<%s%s></%s>" name a name
            | EitherClosing | SelfClose -> sprintf "<%s%s/>" name a
            | Content c -> sprintf "<%s%s>%s</%s>" name a c name
        )
    let button attribMap content = 
        Content content 
        |> htmlElement "button" attribMap
    // if html should we be encoding it?
    let headerify title textOrHtml = 
        sprintf """<table class="headingpresenter">
            <tr>
                <th class="headingpresenter">%s</th>
            </tr>
            <tr>
                <td class="headingpresenter">%s</td>
            </tr>
        </table>""" 
        <|title 
        <|textOrHtml        
module Copying = 
    
    type CopyButtonTargeting = 
        | Id of string
        | DataAttrib of string
        | Other
    let makeCopyButton targeting = 
        match targeting with
        | Id x -> 
            Map[
                "class","btn"
                "data-clipboard-target", sprintf "#%s" x
            ]
            |> fun m -> Html.button m "Copy to clipboard"
        | DataAttrib text -> 
            sprintf """<button class="btn" data-clipboard-text="%s">Copy to clipboard</button>"""
            <| System.Web.HttpUtility.HtmlAttributeEncode text
        | Other ->
            sprintf """<button class="btn">Copy to clipboard</button>"""
    let getCopyableHtml encoder id' (x:obj) = 
        let value = 
            match x with
            | :? string as s -> s
            | x -> Util.ToHtmlString(true, noHeader=true, objectsToDump = [| box x|])
        
        //value |> string |> (fun x-> x.Dump("raw toHtmlString value")) |> ignore
        // if there are tags surrounding the element escape them
        let reencoded = encoder value
        sprintf """<div><pre id="%s">%s</pre>%s</div>""" id' reencoded (makeCopyButton (Id id'))
    let dumpCopyable id' (x:obj) = 
        getCopyableHtml System.Net.WebUtility.HtmlEncode id' x
        |> Util.RawHtml
        |> Dump
        |> ignore
    let dumpCopyableHighlighted titling id' (x:obj) = 
        let innerHtml = getCopyableHtml System.Net.WebUtility.HtmlEncode id' x |> string
        //innerHtml.Dump("getCopyableHighlighted")
        Util.RawHtml(Html.headerify titling innerHtml).Dump()
        
input.Dump("Raw input from clipboard");
input
|> Seq.map (fun x -> x,int x)
|> fun x -> x.Dump("Chars",maximumDepth=Nullable 0); x
|> ignore
let modern rawInput = // would be nice to move this to using a table of copyables
    printfn "starting main block ----------------------------\r\n\r\n"
    let tableCopyable tagId encodedInput = Copying.getCopyableHtml System.Net.WebUtility.HtmlEncode tagId encodedInput
    let htmlEncodedPre x = sprintf "%s%s%s" "<pre class='brush: csharp'>\r\n"(System.Net.WebUtility.HtmlEncode x) "\r\n</pre>\r\n"
    let htmlEncodedCode x = sprintf "%s%s%s" "<code>"(System.Net.WebUtility.HtmlEncode x) "</code>"
    let xHtmlEncode x = 
        use sw = new StringWriter()
        use xtw = new XmlTextWriter(sw)
        xtw.WriteAttributeString("attr",x)
        sw.GetStringBuilder()
        |> string
        |> after "\""
        |> trimEnd '"'
        
    [
        "webUtilityHtmlEncodedCode", htmlEncodedCode, None
        "webUtilityHtmlEncodedPre", htmlEncodedPre, None
        // this one is supposed to be nearly equal to WebUtility.HtmlEncode
        "securityElement", System.Security.SecurityElement.Escape, (Some "System.Security.SecurityElement.Escape") // reverse it with s => SecurityElement.FromString s |> fun x -> x.Text
        "htmlEncoded", System.Net.WebUtility.HtmlEncode, (Some "System.Net.WebUtility.HtmlEncode")
        "httpUtilityHtmlEncode", System.Web.HttpUtility.HtmlEncode,(Some "System.Web.HttpUtility.HtmlEncode")
        
        "urlEncoded", System.Net.WebUtility.UrlEncode, (Some "System.Net.WebUtility.UrlEncode")
        "httpUtilityUrlEncode", System.Web.HttpUtility.UrlEncode, (Some "System.Web.HttpUtility.UrlEncode")
        "escapeUriString", System.Uri.EscapeUriString, (Some "System.Uri.EscapeUriString")
        
        "httpUtilityAttributeEncode", System.Web.HttpUtility.HtmlAttributeEncode, (Some "System.Web.HttpUtility.HtmlAttributeEncode")
        "escapedDataString", System.Uri.EscapeDataString, (Some "System.Uri.EscapeDataString")
        
        "jsEncoded", System.Web.HttpUtility.JavaScriptStringEncode, (Some "System.Web.HttpUtility.JavaScriptStringEncode")
        "xHtmlEncoded", xHtmlEncode, (Some "XmlTextWriterAttributeString")
        "regexEncoded", System.Text.RegularExpressions.Regex.Escape, (Some "System.Text.RegularExpressions.Regex.Escape")
        // none of the items below appear to properly handle escapes ['\t';'\r'] etc.. =(
        // Start bad code section----
        
        //not sure if this properly handles multi-line strings
        "VerbatimString", replace "\"" "\"\"" >> sprintf "@\"%s\"", None
        // this doesn't properly handle things like tab
        "LiteralString", replace "\"" "\\\"" >> sprintf "\"%s\"", None
        // limitations, string can not start with triple-quotes already, and can't end with a quote or double quote
        "FString", sprintf "\"\"\"%s\"\"\"", None
        "FStripLead", (fun s -> s |> splitLines |> delimit "\\\r\n" |> sprintf "\"\"\"%s\"\"\""), None
    ]
    |> Seq.map (fun (nameId,f, nameOverrideOpt) ->
        
        let v = f rawInput
        nameOverrideOpt |> Option.getOrDefault nameId, Util.RawHtml(tableCopyable nameId v) // checksum with v if desired
    )
    |> Dump
    |> ignore

printfn "System.Net and System.Security don't appear to require an outside dll"

//legacy()
modern input
printfn "test data for copying to test"
let copyIt () = 
    "hello \t tab" |> System.Windows.Forms.Clipboard.SetText
printfn "hello\\ttab"