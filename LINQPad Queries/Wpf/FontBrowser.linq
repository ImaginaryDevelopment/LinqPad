<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\System.Windows.Presentation.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Namespace>System.Collections.ObjectModel</Namespace>
  <Namespace>System.Windows</Namespace>
  <Namespace>System.Windows.Controls</Namespace>
  <Namespace>System.Windows.Data</Namespace>
</Query>

// font viewer

module DebuggingWpf =

    type DebugTraceListener(?breakOnAll) =
        inherit TraceListener()
        let mutable breakOnAll:bool = defaultArg breakOnAll false
        override __.Write (_msg:string) = ()
        override __.WriteLine (msg:string) =
            let toIgnorePatterns = [
//                @"BindingExpression path error: 'Title' property not found on 'object' ''String' \(HashCode=-[0-9]+\)'. BindingExpression:Path=Title; DataItem='String' \(HashCode=-[0-9]+\); target element is 'ContentPresenter' \(Name='Content'\); target property is 'ResourceKey' \(type 'String'\)"
                ]
            let regMatch p =
                let m = Text.RegularExpressions.Regex.Match(msg,p)
                if m.Success then
                    Some p
                else
                    None

            let matchedIgnorePattern = toIgnorePatterns |> Seq.choose regMatch |> Seq.tryHead
            match matchedIgnorePattern with
            | Some _ -> ()
            | None ->
                if breakOnAll && Debugger.IsAttached then
                    Debugger.Break()
                else ()

    type Listener(created:DateTime, name) =
        inherit TraceListener(name)

        new(created) = new Listener(created, null)

        override __.Write (msg:string) = printf "%s" msg
        override __.WriteLine (msg:string) =
            printfn "%s" msg
        member __.Created= created

let stayAwhileAndListen() =
    PresentationTraceSources.Refresh();
    let l = new DebuggingWpf.Listener(DateTime.Now)
    PresentationTraceSources.DataBindingSource.Listeners.Add l |> ignore
    PresentationTraceSources.DataBindingSource.Switch.Level <- SourceLevels.Warning;
    PresentationTraceSources.DataBindingSource.Flush()
    l

let text = "The quick brown fox jumped over the lazy dog."
let buildCb (items:_ seq) =
    let cb = ComboBox()
    items
    |> Seq.iter (cb.Items.Add>>ignore)
    cb
    
let buildCbBinder items =
    let cb = buildCb items
    let b = Binding("SelectedItem",Source=cb)
    cb,b

let buildFontCombo prop =
    //let fonts = System.Drawing.FontFamily.Families
    let fonts = Media.Fonts.SystemFontFamilies
    
    //cb.DisplayMemberPath<-"Name"
    let toMediaFont (f:System.Drawing.FontFamily) = 
        let f' = Media.FontFamily(f.Name)
        f'
    let cb,b = buildCbBinder fonts
    cb.SelectedIndex <- 1
    cb,b
let buildStretchCombo() =
    let stretches = 
        let t = typeof<System.Windows.FontStretches>
        let props = t.GetProperties()
        props
        |> Seq.map(fun p -> p.GetValue(null) :?> FontStretch)
    let cb,b = buildCbBinder stretches
    cb,b
let bindOptBinding (target:Control) prop (b:Binding option) =
    b
    |> Option.map(fun (b:Binding) ->
        let bi = target.SetBinding(prop, b)
        bi
    )
    
let buildTextInput fontBindingOpt stretchBindingOpt =
    let tb = TextBox(Text=text)
    tb.AcceptsReturn <- true
    tb.MinHeight <- 70.0
    fontBindingOpt
    |> bindOptBinding tb TextBox.FontFamilyProperty
    |> ignore
    stretchBindingOpt
    |> bindOptBinding tb TextBox.FontStretchProperty
    |> ignore
    
    tb
let inline addChild child x =
    let cc = (^a:(member Children:UIElementCollection) x)
    cc.Add child |> ignore
    x
let buildDisplay() = 
    
    let sp = StackPanel()
    let sp' = StackPanel(Orientation =Orientation.Horizontal)
    let cbF,bFont = buildFontCombo()
    let cbStretch,bStretch= buildStretchCombo()
    
    sp'
    |> addChild cbF
    |> addChild cbStretch
    |> ignore
    
    let tb = buildTextInput (Some bFont) (Some bStretch)
    sp
    |> addChild sp'
    |> addChild tb
    |> ignore
    sp

stayAwhileAndListen |> ignore
let sp = buildDisplay()
sp.Dump()
