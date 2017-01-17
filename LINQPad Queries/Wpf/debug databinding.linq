<Query Kind="FSharpProgram">
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\bin\Pm.Dal.dll</Reference>
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\bin\Pm.UI.exe</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\ReachFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>System.Collections.ObjectModel</Namespace>
</Query>

// face sheet
// things attempted:
    // runtime exception in http://stackoverflow.com/questions/1267046/wpf-flowdocument-scale-to-fit-page (attempt commented out in previous commit)

//http://stackoverflow.com/questions/910814/loading-xaml-at-runtime
//https://blogs.msdn.microsoft.com/ashish/2007/08/14/dynamically-loading-xaml/

open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Markup

[<AutoOpen>]
module Helpers = 
    let dumpt (t:string) x = x.Dump(t); x
    let flip f y x = f x y
    let getType x = x.GetType()
    let regReplace (delimiter:string) (replacement:string) (text:string) = Regex.Replace(text,delimiter,replacement)
    let regRemove x t = regReplace x String.Empty t
    let indexOf (d:string) (t:string) = t.IndexOf(d)
    let replace (delimiter:string) (replacement:string) (text:string) = text.Replace(delimiter,replacement)
    
    let after (delimiter:string) (x:string) =  
            match x.IndexOf delimiter with
            | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
            | i -> x.Substring(i + delimiter.Length)
    let inline before (delimiter:string) (x:string) = x.Substring(0, x.IndexOf delimiter)
    let remove d (s:string) = if s.Contains(d) then s |> before d |> flip (+) (s|> after d) else s
    let replaceAll d r t = 
        t 
        |> Seq.unfold(fun t -> 
            if isNull t then 
                None
            elif t |> indexOf d |> flip (>) 0 then 
                Some(t |> before d |> flip (+) r, t |> after d) 
            else 
                Some (t, null)
            ) 
        |> delimit String.Empty
    let splitLines (x:string) = x.Split([| "\r\n";"\n"|], StringSplitOptions.None)
    module Tuple2 = 
        let fromCurry x y = (x,y)
    
    let getLine i s = s|> splitLines |> Seq.mapi Tuple2.fromCurry |> Seq.find(fun (lineIndex,l) -> lineIndex = i)
    let getLines indexes s = s|> splitLines |> Seq.mapi Tuple2.fromCurry |> Seq.filter (fun (lineIndex,_) -> indexes |> Seq.contains lineIndex)
    
module Cereal = 
    open Newtonsoft.Json
    type private Deserializer<'t> =
        static member deserializeObject<'t> x = JsonConvert.DeserializeObject<'t>(x)
    let serialize usePretty x =
        JsonConvert.SerializeObject(x, if usePretty then Formatting.Indented else Formatting.None)
    let deserialize<'t> x = 
        JsonConvert.DeserializeObject<'t> x

let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\Configuration\UsersPopup.xaml"
let stripTelerik = true
let knownEventTypeRegexs = [
    "MouseDoubleClick=\"[\w_]+\""
]
[<RequireQualifiedAccess>]
type RootElementType =
    |Window of Window
    |Other of obj
let usersPopupSpecificReplacements =
    [
        // root element binding
        remove " DataContext=\"{Binding ElementName=UsersPopupRoot}\""
        // command binding
        replace "{Binding UnlockCommand,ElementName=UsersPopupRoot}" "{Binding Path='Data.UnlockCommand', Source={StaticResource dgProxy}}"
        // commandParameterBinding
        replace "CommandParameter=\"{Binding Path='Data.DataContext.Items/', Source={StaticResource dgProxy}, diag:PresentationTraceSources.TraceLevel=High}\"" 
            "CommandParameter=\"{Binding Source={RelativeSource Self}}\""
    ]
let rootElement = 
    let specificReplacements =  usersPopupSpecificReplacements
    let text = 
        File.ReadAllText path
        |> fun x -> specificReplacements |> Seq.fold (fun t f -> f t) x
        |> regRemove "x:Class=\".+\"" 
        // replace behaviors namespace,static resources
        |> regRemove "xmlns:b=\".+\"" 
        |> regRemove "<b:.+>" 
        // replace converters namespace, static resources
        |> regRemove "xmlns:converter=\".+\""
        |> regRemove "<converter:.+>"
        // remove converter calls
        |> regRemove ",\s*Converter={StaticResource.*?}"
        |> regRemove "Converter=\"{StaticResource.*?}\""
        |> regReplace "{lang:Translate ([\w_]+)}" "$1"
        |> replaceAll "notify:NotifyWindow" "Window"
        |> fun x -> knownEventTypeRegexs |> Seq.fold(fun t regexp -> t |> regRemove regexp) x
        |> fun x -> if stripTelerik then replaceAll "telerik:RadButton" "Button" x else x
        
    Util.OnDemand("raw xaml", fun () -> text).Dump() |> ignore
        
    XDocument.Parse(text).DumpFormatted() |> ignore
    try
        let result = XamlReader.Parse text
        if text |> startsWith "<Window" then
            RootElementType.Window (result :?> Window)
        else RootElementType.Other result
    with ex ->
        if ex.Message.Contains("Line number '") then
            let lineNumber = ex.Message |> after "Line number '" |> before "'" |> int
            text
            |> getLines [lineNumber-1 .. lineNumber + 1]
            |> dumpt (sprintf "errant line:%i" lineNumber)
            |> ignore
        reraise()

//let inline toFormatString (f:string) (a:^a) = ( ^a : (member ToString:string -> string) (a,f))
let inline showDialog x = (^a: (member ShowDialog:unit -> bool Nullable) x)

let display vm =
    let window = 
        match rootElement with
        | RootElementType.Window w -> w
        | RootElementType.Other o -> Window(Content= rootElement)

    window.DataContext <- vm
    window

type User = {UserId:int;LastFirstMiddleName:string; UserFacilityID: int; UserLevel:int}
type ViewModel = { 
    Permission:string; 
    Users: User ObservableCollection; 
    [<JsonIgnore>]
    UnlockCommand: ICommand }
let withDispatcher f = 
    System.Windows.Threading.Dispatcher.CurrentDispatcher.Invoke(Func<_>(f))
    
let unlockCommand : ICommand = 
    let canExecute (obj:obj) = 
        withDispatcher (fun _ ->
            //MessageBox.Show("in can excecute!") |> ignore
            obj.Dump("can execute")
            isNull obj
        )
    let execute (obj:obj) = 
        withDispatcher (fun _ ->
            obj.Dump("executed")
        )
    upcast Pm.UI.ViewModels.Command.Create( 
        (Func<_,_>(canExecute)), 
        execute)
        
let vm = 
    let savePath = Path.Combine (Path.GetDirectoryName path, sprintf "%s.vm.json" (Path.GetFileNameWithoutExtension path))
    let rawTextOpt = if File.Exists savePath then File.ReadAllText savePath |> Some else None
    let defaultUsers = Lazy( fun _ ->  
            [ 
                {UserId=1; LastFirstMiddleName="World, Hello"; UserFacilityID=1;UserLevel=11}
                {UserId=2; LastFirstMiddleName="World2, Hello"; UserFacilityID=2;UserLevel=12}
            ])
    let deserialized:ViewModel = 
        
        let getDefault() = {Permission = "a few"; Users = defaultUsers.Value |> ObservableCollection; UnlockCommand=unlockCommand}
        match rawTextOpt with 
        |Some rawText -> 
            try
                Cereal.deserialize rawText 
            with ex ->
                ex.Dump()
                getDefault()
        | None -> getDefault()
        
    // make any updates/property additions here
    // ....
    // 
    let vm = {deserialized with Permission = "lots"; Users=defaultUsers.Value |> ObservableCollection; UnlockCommand=unlockCommand}
    
    let reserialized = Cereal.serialize true vm
    match rawTextOpt with
    | None -> 
        printfn "Writing vm update to filesystem"
        File.WriteAllText(savePath,reserialized)
    | Some rawText ->
        if rawText <> reserialized then
            printfn "Writing vm update to filesystem"
            File.WriteAllText(savePath,reserialized)    
    vm
// https://spin.atomicobject.com/2013/12/11/wpf-data-binding-debug/    
type Listener(created:DateTime) = 
    inherit TraceListener("DataBindingErrorListener")
    do 
        Listener.CreationCount <- Listener.CreationCount + 1
    override x.Write (msg:string) = 
        withDispatcher (fun () -> printf "%s" msg)
    override x.WriteLine (msg:string) = 
        withDispatcher (fun () -> printfn "%s" msg)
    member __.Created= created
    static member val CreationCount = 0 with get,set
    
// sometimes this works, sometimes it doesn't
let listenForBindingErrors() = 
    let getTypeName x = x.GetType().Name
    // when debugger isn't attached this has to be manually called to enable listening
    PresentationTraceSources.Refresh()
    let alreadyAdded = PresentationTraceSources.DataBindingSource.Listeners |> Seq.cast<TraceListener> |> Seq.exists (getTypeName >> (=) typeof<Listener>.Name)
    if not alreadyAdded then
        new Listener(DateTime.Now)
        |> PresentationTraceSources.DataBindingSource.Listeners.Add
        |> Some
    else
        printfn "Trace listener(s) already attached (%i)" PresentationTraceSources.DataBindingSource.Listeners.Count
        PresentationTraceSources.DataBindingSource.Listeners 
        |> Seq.cast<TraceListener> 
        |> Seq.map getTypeName 
        |> List.ofSeq
        |> printfn "TraceListenerTypes: %A"
        None
        
listenForBindingErrors() |> ignore


let displayForShow() = 
    let window = display vm
    let closeOnEscape (w:Window) = 
        w.KeyDown.Add (fun e -> 
            if e.Key = Key.Escape then
                w.Close()
        )
    closeOnEscape window
//    window.KeyDown.Add(fun k -> 
//        if k.Key = Key.Escape then
//            window.Close())
    
    //// show to query via code, then hide and show dialog to interact and view
    //window.Show()
    //
    //window.Hide()
    
    window.ShowDialog()
    |> ignore<bool Nullable>

displayForShow()
//consider adding printing/print preview to this script
// http://stackoverflow.com/questions/4263204/wpf-printing-flowdocument-not-centering