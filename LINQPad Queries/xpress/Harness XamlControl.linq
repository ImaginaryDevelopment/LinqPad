<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
</Query>

// load a xaml screen for stand-alone viewing/interaction
// parts borrowed from the LoadXamlDragAndDrop script
open System.Windows
open System.Windows.Controls

let debug = true
let path = Environment.ExpandEnvironmentVariables(@"%devroot%\Source-dev-rewrite\PracticeManagement\PracticeManagement\Intellidox\PrintIntellidoxControl.xaml")
type System.String with
    static member defaultComparison = StringComparison.InvariantCultureIgnoreCase
    static member equalsI (x:string) (x2:string) = not <| isNull x && not <| isNull x2 && x.Equals(x2, StringComparison.InvariantCultureIgnoreCase)
    
    
[<AutoOpen>]
module Helpers = 
    let dumpt (t:string) x = x.Dump(t); x
    let flip f y x = f x y
    let getType x = x.GetType()
    let toLower (s:string) = s.ToLower()
    let tee f x = x, f x
    let splitLines (x:string) = x.Split([| "\r\n";"\n"|], StringSplitOptions.None)
    let delimit delimiter (values:#seq<string>) = String.Join(delimiter, Array.ofSeq values)
    let (|StringEqualsI|_|) s1 (toMatch:string) = if String.equalsI toMatch s1 then Some() else None
    let regMatches (pattern:string) (text:string) = Regex.Matches(text,pattern) |> Seq.cast<Match> |> List.ofSeq
    let regIsMatch (pattern:string) (text:string) = Regex.IsMatch(text, pattern)
    let regReplace (pattern:string) (replacement:string) (text:string) = Regex.Replace(text,pattern,replacement)
    let regReplaceM (pattern:string) (replacement:string) (text:string) = Regex.Replace(text,pattern,replacement, RegexOptions.Multiline)
    
    let regRemove x t = regReplace x String.Empty t
    let regRemoveM isRequired p t =
        if not <| regIsMatch p t then
            if isRequired || debug then
                failwithf "Regex did not match %s" p
        regReplace p String.Empty t
    let replace (delimiter:string) (replacement:string) (text:string) = text.Replace(delimiter,replacement)
    let contains (delimiter:string) (text:string) = text.Contains(delimiter)
    let after (delimiter:string) (x:string) =  
            match x.IndexOf delimiter with
            | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
            | i -> x.Substring(i + delimiter.Length)
    let before (delimiter:string) (x:string) = x.Substring(0, x.IndexOf delimiter)
    let remove d (s:string) = if s |> contains d then s |> before d |> flip (+) (s|> after d) else s
    module Tuple2 = 
        let fromCurry x y = (x,y)
    
    let getLine i s = s|> splitLines |> Seq.mapi Tuple2.fromCurry |> Seq.find(fun (lineIndex,l) -> lineIndex = i)
    let getLines indexes s = s|> splitLines |> Seq.mapi Tuple2.fromCurry |> Seq.filter (fun (lineIndex,_) -> indexes |> Seq.contains lineIndex)
()



    
type TagAlteration =
    | TagRemoval
    | TagReplacement of string
let replaceResourceDictionaries basePath fGetSourceMapOpt xamlText =
        let pairs = xamlText |> regMatches "<ResourceDictionary.*Source=\"(.*)\".* />" |> List.map (fun m -> m.Groups.[1].Value, m.Value)  
        let anyDictionariesWithMoreThan1Attrib = pairs |> Seq.map snd |> Seq.map XElement.Parse |> Seq.exists (fun xe -> xe.Attributes() |> Seq.length > 1)
        assert (not anyDictionariesWithMoreThan1Attrib)
        let replaceResourceDictionaries (basePath:string) (tagPairs:(string*string) seq) (xamlText:string) : string = 
            let mapSource text (source,tag) = 
                match fGetSourceMapOpt tag source,source with
                | Some TagRemoval, _ ->         remove tag text
                | Some (TagReplacement r), _ -> replace tag r text
                | None, StringEqualsI "ApplicationResources.xaml" as target ->
                    File.ReadAllText(Path.Combine(basePath, source))
                    |> fun r -> replace tag r text
                | _ -> text
                    
            tagPairs 
            |> Seq.fold mapSource xamlText
            
        xamlText
        |> replaceResourceDictionaries (Path.GetDirectoryName(path)) pairs 

let text =
	File.ReadAllText path
	|> regRemove "x:Class=\".+\""
	// replace behaviors namespace,static resources
	|> regRemove "xmlns:b=\".+\""
	|> regRemove "<b:.+>"
	//|> replace "xmlns:bc=\"clr-namespace:Pm.UI.WpfCustomControls;assembly=Pm.UI\"" localTag
	// replace converters namespace, static resources
	|> regRemove "xmlns:converter=\".+\""
	|> regRemove "<converter:.+>"
	// remove converter calls
	|> regRemove ",\s*Converter={StaticResource.*?}"
	|> regRemove "Converter=\"{StaticResource.*?}\""
    |> regRemove "Style=\"{StaticResource [\w_]+}\""
    
    |> regRemove "Click=\"[\w_]+\"\s*"
	|> replaceResourceDictionaries path (fun _tag _sourceAttribValue -> None)

let parse<'t> x =  System.Windows.Markup.XamlReader.Parse x :?> 't
type DisplayItem = {DisplayName:string}
let items = [{DisplayName="test"}]
try
    let r = parse<UserControl> text
    text.Dump("raw text")
    r
with ex when ex.Message.Contains("Line number") ->
    text.Dump("raw text")
    let line = Regex.Match(ex.Message,@"Line number '(\d+)'").Groups.[1].Value |> int
    text |> String.split ["\r\n"] |> fun x -> x.[line - 1] |> Dump |> ignore
    //printfn "error on line '%s'" line
    reraise()
|> fun r -> 
    r.DataContext <- null
    let lf = r.FindName("listForms") :?> ListBox
    lf.ItemsSource <- items
    r
|> Dump
|> ignore
