<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//var input=LINQPad.Util.ReadLine<string>("What shall we encode?");
let input=System.Windows.Forms.Clipboard.GetText()
module Scriptify = 
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
module Copying = 
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
    type CopyButtonTargeting = 
        | Id of string
        | DataAttrib of string
        | Other
    let makeCopyButton targeting = 
        match targeting with
        | Id x -> 
            sprintf """<button class="btn" data-clipboard-target="#%s">Copy to clipboard</button>""" x
        | DataAttrib text -> 
            sprintf """<button class="btn" data-clipboard-text="%s">Copy to clipboard</button>"""
            <| System.Web.HttpUtility.HtmlAttributeEncode text
        | Other ->
            sprintf """<button class="btn">Copy to clipboard</button>"""
    
    let dumpCopyable id' (x:obj) = 
        let value = 
            match x with
            | :? string as s -> s
            | x -> Util.ToHtmlString(true, noHeader=true, objectsToDump = [| box x|])
        x.Dump("dumpCopyable.x")
        value |> string |> (fun x-> x.Dump("raw toHtmlString value")) |> ignore
        Util.RawHtml(sprintf """<div><span id="%s">%s</span>%s</div>""" id' value (makeCopyButton (Id id')))
        |> Dump
        |> ignore
    
Util.RawHtml(Scripting.clipInjectText).Dump()
Util.RawHtml(Scripting.useClipText).Dump()

printfn "%s%s%s" "<pre class='brush: csharp'>"(System.Net.WebUtility.HtmlEncode(input)) "</pre>"
printfn "%s%s%s" "<code>" (System.Net.WebUtility.HtmlEncode(input)) "</code>"

input.Dump("Raw");
System.Net.WebUtility.HtmlEncode(input).Dump("System.Net.WebUtility.HtmlEncode")
System.Net.WebUtility.HtmlEncode(input)
|> Copying.dumpCopyable "WebUtilityHtmlEncode"
System.Net.WebUtility.UrlEncode(input).Dump("UrlEncode")
System.Uri.EscapeDataString(input).Dump("EscapeDataString")
System.Uri.EscapeUriString(input).Dump("EscapeUriString")
System.Web.HttpUtility.HtmlEncode(input).Dump("System.Web.HttpUtility.HtmlEncode")
System.Web.HttpUtility.UrlEncode(input).Dump("System.Web.HttpUtility.UrlEncode")
System.Web.HttpUtility.HtmlAttributeEncode(input).Dump("HtmlAttribute")
