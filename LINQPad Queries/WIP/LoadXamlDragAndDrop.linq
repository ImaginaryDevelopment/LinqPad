<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <NuGetReference Version="5.0.0">FSharp.Compiler.Service</NuGetReference>
  <NuGetReference Version="1.4.1-beta-24227-04" Prerelease="true">System.Reflection.Metadata</NuGetReference>
</Query>

// test the drag/drop control type

// problem is we are letting Wpf create the control, so it creates DragDropList<object>, our items are _<string>
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Markup
open System.Windows.Media
type MyListBox () = 
    inherit ListBox()
    member x.Foo() = 
        x.OnItemsChanged( Specialized.NotifyCollectionChangedEventArgs(Specialized.NotifyCollectionChangedAction.Move))
let debug = true
type System.String with
    static member defaultComparison = StringComparison.InvariantCultureIgnoreCase
    static member equalsI (x:string) (x2:string) = not <| isNull x && not <| isNull x2 && x.Equals(x2, StringComparison.InvariantCultureIgnoreCase)
    
//debug.GetType().GenericTypeArguments.[0].Name
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
    
let lb = ListBox()

    
let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Pm.UI\Diagnostic.xaml"
module DynamicAssemblies = 
    let srcCode ns = replace "%%ns%%" ns """
namespace %%ns%%
open System
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Markup
open System.Windows.Media
[<AutoOpen>]
module Reflection = 
    open System.Reflection
    open Microsoft.FSharp.Reflection
        // some parts of this may be a translation of BMore.linq
//    let (|TypeDefOf|_|) (_:'a) t = 
//        if t = typedefof<'a> then Some() else None

    /// for when you need to see if something matches and expected Generic Type Definition ( you don't know "'t" but don't care)
    /// Sample (tested good) usage:
    /// match list with
    /// | TypeDefOf (isType:List<_>) typeArgs -> sprintf "Yay matched1 : %A" typeArgs
    /// | _ -> "boo"
    /// Also works for some types: 
    /// | TypeDefOf (null:List<_>) typeArgs -> sprintf "Yay matched: %A" typeArgs
    let (|TypeDef|_|) (_:'a) (value:obj) = 
        let typeDef = typedefof<'a>
        if obj.ReferenceEquals(value, null) then 
            None
        else
            let typ = value.GetType()
            if typ.Name = "RuntimeType" then failwithf "Can not GetGenericTypeDefinition of System.Type"
//            let gtd = if typ.IsGenericType then typ.GetGenericTypeDefinition() |> Some else None
            if typ.IsGenericType && typ.GetGenericTypeDefinition() = typeDef then 
                Some(typ.GetGenericArguments())
            else None
    // instead of null in TypeOf or TypeDef matches for types that don't allow null
    let isType<'a> = Unchecked.defaultof<'a>

module WpfHelpers =
    Trace.WriteLine("Hello WpfHelpers.Trace")
    Debug.WriteLine("Hello WpfHelpers.Debug")
    Console.WriteLine("Hello WpfHelpers.Console")
    printfn "Hello WpfHelpers.PrintFn"
    let rec findVisualParent<'t when 't :> DependencyObject> child =
        let parentObject = VisualTreeHelper.GetParent child
        match box parentObject with
        | null -> None
        | :? 't as t -> Some t
        | :? DependencyObject as x -> findVisualParent<'t> x
        | _ -> failwithf "Unexpected parent type %A" (parentObject.GetType())
        

type DragDropListBox() as self =
    inherit ListBox()

    let mutable dragStartPointOpt : Point option = None
    let mutable t : Type option = None
    let mutable insertF : Option< int*obj -> unit > = None
    let mutable removeAtF: Option< int -> unit > = None

    let listBoxPreviewMouseMove (* sender *) (e:MouseEventArgs) = 
        let point = e.GetPosition(null)
        let diff = defaultArg dragStartPointOpt (Point()) - point
        if e.LeftButton = MouseButtonState.Pressed && Math.Abs diff.X > SystemParameters.MinimumHorizontalDragDistance ||
            Math.Abs diff.Y > SystemParameters.MinimumVerticalDragDistance then
            match WpfHelpers.findVisualParent<ListBoxItem>(e.OriginalSource :?> DependencyObject) with
            | Some lbi -> 
                let dde = DragDrop.DoDragDrop(lbi, lbi.DataContext, DragDropEffects.Move)
                dde |> ignore
                ()
            | None -> ()
            
            ()

    let listBoxPreviewMouseLeftButtonDown _ (e:MouseButtonEventArgs) =
        dragStartPointOpt <- Some <| e.GetPosition(null)

    let listBoxItemDrop (x:obj) (sender:obj) (e:DragEventArgs) =
        match sender with
        | :? ListBoxItem as lbi ->
            let source = e.Data.GetFormats() |> Seq.head |> e.Data.GetData
            //assert (box itemContext |> isNull |> not)
            match removeAtF, insertF with
            | Some removeAt, Some insert -> 
                let target = lbi.DataContext
                let sourceIndex = self.Items.IndexOf source
                let targetIndex = self.Items.IndexOf target
                Debug.WriteLine(sprintf "Moving items around in: %A" self.ItemsSource)
                removeAt sourceIndex
                insert(targetIndex, source)
                Debug.WriteLine(sprintf "AfterMove:%A" self.ItemsSource)
                self.Items.Refresh()
            | _ ->
                let isSet x = if Option.isSome x then "set" else "unset"
                failwithf "removeAtF (%A) or insertF (%A) most both be set" (isSet removeAtF) (isSet insertF)
            ()
        | _ -> ()
        ()

    do
        self.PreviewMouseLeftButtonDown.Add listBoxPreviewMouseMove //.AddHandler (MouseButtonEventHandler previewMouseMove)
        let style =
            let s = Style(typeof<ListBoxItem>)
            s.Setters.Add (Setter(ListBoxItem.AllowDropProperty, true))
            s.Setters.Add (EventSetter(ListBoxItem.PreviewMouseLeftButtonDownEvent, MouseButtonEventHandler listBoxPreviewMouseLeftButtonDown))
            s.Setters.Add (EventSetter(ListBoxItem.DropEvent, DragEventHandler (listBoxItemDrop self)))
            s
        self.ItemContainerStyle <- style

    override x.OnItemsSourceChanged(oldValue, newValue) =
        // call this first in case anything from here on out depends on the results
        base.OnItemsSourceChanged(oldValue, newValue)
        
        // get the type underneath, and cache method calls for drag/drop 
        let _collectionType = newValue |> Option.ofObj |> Option.map (fun c -> c.GetType())
        
        match newValue with
        | null -> t <- None
        | TypeDef (isType:List<_>) typeArgs ->
            let targetType = typedefof<List<_>>.MakeGenericType(typeArgs)
            let insertMethod = targetType.GetMethod("Insert") //.MakeGenericMethod(typeArgs)
            let removeAtMethod = targetType.GetMethod("RemoveAt") //.MakeGenericMethod(typeArgs)
            
            insertF <- Some (fun (i,o) -> insertMethod.Invoke(x.ItemsSource, [| box i; o|]) |> ignore<obj>)
            removeAtF <- Some (fun i -> removeAtMethod.Invoke(x.ItemsSource, [| box i |] ) |> ignore<obj>)
            ()
        | x ->
            failwithf "Unexpected itemsSource value: %A" x
            ()

"""
    open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
    let compileDragDrop ns = 
        let scs = SimpleSourceCodeServices()
        let srcCode = srcCode ns
        let src,dll = 
            let fn = Path.GetTempFileName()
            let fn2 = Path.ChangeExtension(fn, ".fs")
            let fn3 = Path.ChangeExtension(fn, ".dll")
            fn2,fn3
        File.WriteAllText(src,srcCode)
        let errors, exitCode = scs.Compile [| "fsc.exe"; "-o"; dll; "-a";src; "-d"; "DEBUG"; "-r"; "WindowsBase"; "-r" ;"PresentationCore"; "-r"; "PresentationFramework" |]
        let mapError (er:Microsoft.FSharp.Compiler.FSharpErrorInfo) = 
            let lines = srcCode |> splitLines 
            lines |> Seq.skip (er.StartLine - 1) |> Seq.take 3 |> delimit "\r\n", er.Message, er.Subcategory
        
        errors |> Seq.map mapError |> Dump |> ignore
        match exitCode with
        | 0 -> 
            Some dll
        | _ -> 
            (errors,exitCode).Dump("Compilation issues")
            File.Delete src
            File.Delete dll
            None
            
let ns ="LinqPadDynamic"
let assemblyName = 
    match DynamicAssemblies.compileDragDrop ns with
    | Some dllPath -> 
        let a = Assembly.LoadFrom(dllPath)
        a.GetName().Name
    | None -> failwithf "Failed to compile"
    
//if isNull Application.ResourceAssembly then
//    Application.ResourceAssembly <- Assembly.GetExecutingAssembly()
//Application.ResourceAssembly.add_ModuleResolve(ModuleResolveEventHandler((fun sender e -> e.Name.Dump("resource model resolution attempting"); null)))
let wpfSetup () = 
    if isNull Application.Current then
        System.Windows.Application() |> ignore
    if isNull Application.ResourceAssembly then
        Application.ResourceAssembly <- Assembly.GetExecutingAssembly()
        
    if debug then
        Application.ResourceAssembly.add_ModuleResolve(ModuleResolveEventHandler((fun sender e -> e.Name.Dump("resource model resolution attempting"); null)))
        Application.ResourceAssembly.GetManifestResourceNames().Dump("manifest resources")    

wpfSetup()

type DerivedResourceDictionary() = 
    inherit ResourceDictionary()
    override x.OnGettingValue(key, value, canCache) =
        key.Dump("getting value by key")
        base.OnGettingValue(key, &value, &canCache)

let addAppResource path = 
    
    let assWithResources = @"C:\TFS\PracticeManagement\dev\PracticeManagement\bin\PracticeManagement.exe"
    let rd = ResourceDictionary(Source=Uri(path))
    let filename = Path.GetFileName path |> toLower
    let isEmpty = Application.Current.Resources.Count = 0
    if not isEmpty then failwithf "resourceDictionary to be replaced had items"
    Application.Current.Resources <- DerivedResourceDictionary()
    Application.Current.Resources.Add(filename, rd)
    Application.Current.Resources.MergedDictionaries.Add rd

addAppResource "C:\TFS\PracticeManagement\dev\PracticeManagement\Pm.UI\ApplicationResources.xaml"
type TagAlteration =
    | TagRemoval
    | TagReplacement of string
//type Tag = |Tag of string
//type CompositionFailureSourceTag = 
//    | ResourceDictionary of Tag*source:string
//   

let rootElement = 
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
        
    let localTag = 
        let r = sprintf "xmlns:bc=\"clr-namespace:%s;assembly=%s\"" ns assemblyName  // try it with just namespace first
        r.Dump()
        r
        
    let text = 
        File.ReadAllText path
        |> regRemove "x:Class=\".+\"" 
        // replace behaviors namespace,static resources
        |> regRemove "xmlns:b=\".+\"" 
        |> regRemove "<b:.+>" 
        |> replace "xmlns:bc=\"clr-namespace:Pm.UI.WpfCustomControls;assembly=Pm.UI\"" localTag
        // replace converters namespace, static resources
        |> regRemove "xmlns:converter=\".+\""
        |> regRemove "<converter:.+>"
        // remove converter calls
        |> regRemove ",\s*Converter={StaticResource.*?}"
        |> regRemove "Converter=\"{StaticResource.*?}\""
        |> remove ", Converter={x:Static local:PatientFaceSheet.PhoneFormattingConverter}"
        |> replaceResourceDictionaries path (fun _tag _sourceAttribValue -> None)
        |> regRemove "<ImageBrush ImageSource.*/>"
        //|> regRemoveM true "<ResourceDictionary>.*</ResourceDictionary>"
     
    Util.OnDemand("raw xaml", fun () -> text).Dump() |> ignore
    Util.OnDemand("formatted xml", fun () ->            
            try
                XDocument.Parse(text).DumpFormatted() |> ignore
            with ex -> ex.Dump("Xml exception")
    ).Dump() |> ignore
    
    let pc = 
        let pc = ParserContext()
        pc.XamlTypeMapper <-
            XamlTypeMapper(
                [|
                    Assembly.GetExecutingAssembly().GetName().Name
                    assemblyName
                |]
            )
        pc
    //pc.XamlTypeMapper.SetAssemblyPath("PracticeManagement")
    pc.Dump("pc")

    try
        XamlReader.Parse(text, pc)
    with ex ->
        if ex.Message.Contains("Line number '") then
            let lineNumber = ex.Message |> after "Line number '" |> before "'" |> int
            text
            |> getLines [lineNumber-1 .. lineNumber + 1]
            |> dumpt (sprintf "errant line:%i" lineNumber)
            |> ignore
        reraise()
   
let window = rootElement :?> Window
type DcHost = {Items: List<string>}
let dcHost = {Items= List<string>()}
[0..5]
|> Seq.map string
|> Seq.iter (dcHost.Items.Add >> ignore)
type Listener(created:DateTime) = 
    inherit TraceListener("DataBindingErrorListener")
    override x.Write (msg:string) = printf "%s" msg
    override x.WriteLine (msg:string) = 
        printfn "%s" msg
        msg.Dump("traced!")
    member __.Created= created

let closeOnEscape (w:Window) = 
        w.KeyDown.Add (fun e -> 
            if e.Key = Key.Escape then
                w.Close()
        )
window.DataContext <- dcHost
closeOnEscape window
window.ShowDialog()