<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <NuGetReference Prerelease="true">Elmish.WPF</NuGetReference>
  <Namespace>Elmish</Namespace>
</Query>

open System.Windows
open System.Windows.Controls
open System.ComponentModel
open System.Dynamic

type ClockMsg =
    | Tick of DateTime

type ClockModel =
    { Time: DateTime }

type Msg =
    | ClockMsg of ClockMsg
    | Increment
    | Decrement
    | SetStepSize of int

type Model = 
    { Count: int
      StepSize: int
      Clock: ClockModel }
      
let init() = { Count = 0; StepSize = 1; Clock = { Time = DateTime.Now }}      
let clockUpdate (msg:ClockMsg) (model:ClockModel) =
    match msg with
    | Tick t -> { model with Time = t }

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> { model with Count = model.Count + model.StepSize }
    | Decrement -> { model with Count = model.Count - model.StepSize }
    | SetStepSize n -> { model with StepSize = n }
    | ClockMsg m -> { model with Clock = clockUpdate m model.Clock }
    
let timerTick dispatch =
    let timer = new System.Timers.Timer 1.
    timer.Elapsed.Subscribe (fun _ -> dispatch (System.DateTime.Now |> Tick |> ClockMsg)) |> ignore
    timer.Enabled <- true
    timer.Start()

let subscribe model =
    Cmd.ofSub timerTick
    
type Getter<'model> = 'model -> obj
type Setter<'model,'msg> = obj -> 'model -> 'msg
type ValidSetter<'model,'msg> = obj -> 'model -> Result<'msg,string>
type Execute<'model,'msg> = obj -> 'model -> 'msg
type CanExecute<'model> = obj -> 'model -> bool

open System.Windows.Input
open System.Windows.Data

type Command(execute, canExecute) as this =
    let canExecuteChanged = Event<EventHandler,EventArgs>()
    let handler = EventHandler(fun _ _ -> this.RaiseCanExecuteChanged()) 
    do CommandManager.RequerySuggested.AddHandler(handler)
    // CommandManager only keeps a weak reference to the event handler, so a strong handler must be maintained
    member private x._Handler = handler
    member x.RaiseCanExecuteChanged () = canExecuteChanged.Trigger(x,EventArgs.Empty)
    interface ICommand with
        [<CLIEvent>]
        member x.CanExecuteChanged = canExecuteChanged.Publish
        member x.CanExecute p = canExecute p
        member x.Execute p = execute p

type ViewBinding<'model,'msg> = string * Variable<'model,'msg>
and ViewBindings<'model,'msg> = ViewBinding<'model,'msg> list
and Variable<'model,'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model,'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model,'msg>
    | BindCmd of Execute<'model,'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model,'msg>
    | BindMap of Getter<'model> * (obj -> obj)    

[<RequireQualifiedAccess>]
module Binding =
    
    // Maps a set of view bindings to its parent view bindings
    let rec private mapViewBinding<'model,'msg,'_model,'_msg> toModel toMsg (viewBinding: ViewBindings<'_model,'_msg>) : ViewBindings<'model,'msg> =
        let mapVariable =
            function
            | Bind getter ->
                toModel >> getter
                |> Bind
            | BindTwoWay (getter,setter) -> 
                (toModel >> getter, fun v m -> (toModel m) |> (setter v >> toMsg))
                |> BindTwoWay
            | BindTwoWayValidation (getter,setter) -> 
                (toModel >> getter, fun v m -> (toModel m) |> (setter v >> Result.map toMsg))
                |> BindTwoWayValidation
            | BindCmd (exec,canExec) ->
                ((fun v m -> (toModel m) |> exec v |> toMsg), (fun v m -> (toModel m) |> canExec v))
                |> BindCmd
            | BindModel (getter,binding) ->
                (toModel >> getter, binding |> mapViewBinding toModel toMsg)
                |> BindModel
            | BindMap (getter,mapper) ->
                ((toModel >> getter), mapper)
                |> BindMap

        viewBinding
        |> List.map (fun (n,v) -> n, mapVariable v)

    // Helper functions that clean up binding creation

    ///<summary>Source to target binding (i.e. BindingMode.OneWay)</summary>
    ///<param name="getter">Gets value from the model</param>
    ///<param name="name">Binding name</param>
    let oneWay (getter: 'model -> 'a) name : ViewBinding<'model,'msg> = 
        name, Bind (getter >> unbox)
    ///<summary>Either source to target or target to source (i.e. BindingMode.TwoWay)</summary>
    ///<param name="getter">Gets value from the model</param>
    ///<param name="setter">Setter function, returns a message to dispatch, typically to set the value in the model</param>
    ///<param name="name">Binding name</param>
    let twoWay (getter: 'model -> 'a) (setter: 'a -> 'model -> 'msg) name : ViewBinding<'model,'msg> = 
        name, BindTwoWay (getter >> unbox, fun v m -> setter (v :?> 'a) m)
    
    ///<summary>Either source to target or target to source (i.e. BindingMode.TwoWay) with INotifyDataErrorInfo implementation)</summary>
    ///<param name="getter">Gets value from the model</param>
    ///<param name="setter">Validation function, returns a Result with the message to dispatch or an error string</param>
    ///<param name="name">Binding name</param>
    let twoWayValidation (getter: 'model -> 'a) (setter: 'a -> 'model -> Result<'msg,string>) name : ViewBinding<'model,'msg> = 
        name, BindTwoWayValidation (getter >> unbox, fun v m -> setter (v :?> 'a) m)
        
    ///<summary>Command binding</summary>
    ///<param name="exec">Execute function, returns a message to dispatch</param>
    ///<param name="name">Binding name</param>
    let cmd exec name : ViewBinding<'model,'msg> = 
        name, BindCmd (exec, fun _ _ -> true)
        
    ///<summary>Conditional command binding</summary>
    ///<param name="exec">Execute function, returns a message to dispatch</param>
    ///<param name="canExec">CanExecute function, returns a bool</param>
    ///<param name="name">Binding name</param>
    let cmdIf exec canExec name : ViewBinding<'model,'msg> = 
        name, BindCmd (exec, canExec)
        
    ///<summary>Sub-view binding</summary>
    ///<param name="getter">Gets the sub-model from the base model</param>
    ///<param name="viewBinding">Set of view bindings for the sub-view</param>
    ///<param name="toMsg">Maps sub-messages to the base message type</param>
    ///<param name="name">Binding name</param>
    let model (getter: 'model -> '_model) (viewBinding: ViewBindings<'_model,'_msg>) (toMsg: '_msg -> 'msg) name : ViewBinding<'model,'msg> = 
        name, BindModel (getter >> unbox, viewBinding |> mapViewBinding getter toMsg)
        
    ///<summary>One-way binding that applies a map when passing data to the view.
    /// Should be used for data that a view needs wrapped in some view-specific type. 
    /// For example when graphing a series, the data can be stored as a plain array in the model, 
    /// and then mapped to a SeriesCollection for the view.</summary>
    ///<param name="getter">Gets the value from the model</param>
    ///<param name="mapper">Maps the value for consumption by the view</param>
    ///<param name="name">Binding name</param>
    let oneWayMap (getter: 'model -> 'a) (mapper: 'a -> 'b) name : ViewBinding<'model,'msg> =
        name, BindMap (getter >> unbox, unbox >> mapper >> unbox)
        
      
let view _ _ = 
        let clockViewBinding : ViewBindings<ClockModel,ClockMsg> =
            [ "Time" |> Binding.oneWay (fun m -> m.Time) ]

        [ Binding.cmd (fun _ _ -> Msg.Increment) "Increment"
          "Decrement" |> Binding.cmdIf (fun m _ -> 
            printfn "Decrement"
            Msg.Decrement) (fun _ m -> 
            m.StepSize = 1)
          "Count" |> Binding.oneWay (fun m -> m.Count)
          "StepSize" |> Binding.twoWay (fun m -> (double m.StepSize)) (fun v m -> v |> int |> SetStepSize)
          "Clock" |> Binding.model (fun m -> m.Clock) clockViewBinding ClockMsg ]

type PropertyAccessor<'model,'msg> =
    | Get of Getter<'model>
    | GetSet of Getter<'model> * Setter<'model,'msg>
    | GetSetValidate of Getter<'model> * ValidSetter<'model,'msg>
    | Cmd of Command
    | Model of ViewModelBase<'model,'msg>
    | Map of Getter<'model> * (obj -> obj)

and ViewModelBase<'model, 'msg>(m:'model, dispatch, propMap: ViewBindings<'model,'msg>, debug:bool) as this =
    inherit DynamicObject()

    let log msg = if debug then console.log msg
    
    // Store all bound properties and their corresponding accessors
    let props = new System.Collections.Generic.Dictionary<string, PropertyAccessor<'model,'msg>>()
    // Store all errors
    let errors = new System.Collections.Generic.Dictionary<string, string list>()

    // Current model
    let mutable model : 'model = m

    // For INotifyPropertyChanged

    let propertyChanged = Event<PropertyChangedEventHandler,PropertyChangedEventArgs>()
    let notifyPropertyChanged name = 
        log <| sprintf "Notify %s" name
        propertyChanged.Trigger(this,PropertyChangedEventArgs(name))
    let notify (p:string list) =
        p |> List.iter notifyPropertyChanged
        let raiseCanExecuteChanged =
            function
            | Cmd c ->
                if isNull Application.Current then ()
                elif isNull Application.Current.Dispatcher then () else
                fun _ -> c.RaiseCanExecuteChanged()
                |> Application.Current.Dispatcher.Invoke
            | _ -> ()
        //TODO only raise for cmds that depend on props in p
        props |> List.ofSeq |> List.iter (fun kvp -> raiseCanExecuteChanged kvp.Value)
    

    // For INotifyDataErrorInfo

    let errorsChanged = new DelegateEvent<System.EventHandler<DataErrorsChangedEventArgs>>()


    // Initialize bindings
    do
        let toCommand (exec, canExec) = Command((fun p -> exec p model |> dispatch), fun p -> canExec p model)
        let toSubView propMap = ViewModelBase<_,_>(model, dispatch, propMap, debug)
        let rec convert = 
            List.map (fun (name,binding) ->
                match binding with
                | Bind getter -> name, Get getter
                | BindTwoWay (getter,setter) -> name, GetSet (getter,setter)
                | BindTwoWayValidation (getter,setter) -> name, GetSetValidate (getter,setter)
                | BindCmd (exec,canExec) -> name, Cmd <| toCommand (exec,canExec)
                | BindModel (_, propMap) -> name, Model <| toSubView propMap
                | BindMap (getter,mapper) -> name, Map <| (getter,mapper)
            )
        
        convert propMap |> List.iter (fun (n,a) -> props.Add(n,a))

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member __.PropertyChanged = propertyChanged.Publish
        
    interface INotifyDataErrorInfo with
        [<CLIEvent>]
        member __.ErrorsChanged = errorsChanged.Publish
        member __.HasErrors = errors.Count > 0
        member __.GetErrors propName = 
            log <| sprintf "Getting errors for %s" propName
            match errors.TryGetValue propName with
            | true, errs -> errs
            | false, _ -> []
            :> System.Collections.IEnumerable
    
    member __.UpdateModel other =
        log <| sprintf "UpdateModel %A" (props.Keys |> Seq.toArray)
        let propDiff name =
            function
            | Get getter | GetSet (getter,_) | GetSetValidate(getter,_) | Map (getter,_) ->
                if getter model <> getter other then Some name else None
            | Model m ->
                m.UpdateModel other
                None
            | _ -> None

        let diffs = 
            props
            |> Seq.choose (fun (kvp) -> propDiff kvp.Key kvp.Value)
            |> Seq.toList
        
        model <- other
        notify diffs


    // DynamicObject overrides

    override __.TryGetMember (binder, r) = 
        log <| sprintf "TryGetMember %s" binder.Name
        if props.ContainsKey binder.Name then
            r <-
                match props.[binder.Name] with 
                | Get getter 
                | GetSet (getter,_)
                | GetSetValidate (getter,_) -> getter model
                | Cmd c -> unbox c
                | Model m -> unbox m
                | Map (getter,mapper) -> getter model |> mapper
            true
        else false

    override __.TrySetMember (binder, value) =
        log <| sprintf "TrySetMember %s" binder.Name
        if props.ContainsKey binder.Name then
            match props.[binder.Name] with 
            | GetSet (_,setter) -> try setter value model |> dispatch with | _ -> ()
            | GetSetValidate (_,setter) -> 
                let errorsChanged() = errorsChanged.Trigger([| unbox this; unbox <| DataErrorsChangedEventArgs(binder.Name) |])
                try 
                    match setter value model with
                    | Ok msg -> 
                        if errors.Remove(binder.Name) then errorsChanged()
                        dispatch msg 
                    | Error err ->
                        match errors.TryGetValue binder.Name with
                        | true, errs -> errors.[binder.Name] <- err :: errs
                        | false, _ -> errors.Add(binder.Name, [err])
                        errorsChanged()
                with | _ -> ()
            | _ -> invalidOp "Unable to set read-only member"
        false         
let private _run debug (window:Window) (programRun:Program<'t, 'model, 'msg, ViewBindings<'model,'msg>> -> unit) (program: Program<'t, 'model, 'msg, ViewBindings<'model,'msg>>) =
    let mutable lastModel = None

    let setState model dispatch = 
        match lastModel with
        | None -> 
            let mapping = program.view model dispatch
            let vm = ViewModelBase<'model,'msg>(model, dispatch, mapping, debug)
            window.DataContext <- vm
            lastModel <- Some vm
        | Some vm ->
            vm.UpdateModel model
                  
    // Start Elmish dispatch loop  
    { program with setState = setState } 
    |> programRun
    
    // Start WPF dispatch loop
    let app = Application()
    app.Run(window) //blocking          
/// Blocking function.
/// Starts both Elmish and WPF dispatch loops.
let runWindow window program = _run false window Elmish.Program.run program

let parse<'t> x =  System.Windows.Markup.XamlReader.Parse x :?> 't
let window = //x:Class="Elmish.CounterViews.MainWindow"
// xmlns:local="clr-namespace:Elmish.Views"
    let clock = 
        """<UserControl
             xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
             xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
             xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
             xmlns:d="http://schemas.microsoft.com/expression/blend/2008" 
             mc:Ignorable="d" 
             d:DesignHeight="300" d:DesignWidth="300">
    <Grid>
        <Grid.RowDefinitions>
            <RowDefinition Height="Auto"/>
            <RowDefinition Height="Auto"/>
        </Grid.RowDefinitions>
        <TextBlock Text="{Binding Time, StringFormat=yyyy-MM-dd HH:mm:ss:fff}"/>
    </Grid>
</UserControl>"""
        |> parse<UserControl>
    """<Window 
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
        xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
        mc:Ignorable="d"
        Title="MainWindow" Height="350" Width="525">
    <Grid HorizontalAlignment="Center">
        <Grid.RowDefinitions>
            <RowDefinition Height="Auto"/>
            <RowDefinition Height="Auto"/>
            <RowDefinition Height="Auto"/>
            <RowDefinition Height="Auto"/>
            <RowDefinition Height="*"/>
            <RowDefinition Height="Auto"/>
        </Grid.RowDefinitions>
        <TextBox Text="{Binding Count}"/>
        <StackPanel Grid.Row="1" Orientation="Horizontal" HorizontalAlignment="Center">
            <Button Command="{Binding Decrement}" Width="30" Content="-"/>
            <Button Command="{Binding Increment}" Width="30" Content="+"/>
        </StackPanel>
        <Slider Grid.Row="2" TickFrequency="1" TickPlacement="Both" Width="200"
                Maximum="10" Minimum="1" IsSnapToTickEnabled="True"
                Value="{Binding StepSize}"/>
        <TextBlock Grid.Row="3" Text="{Binding StepSize}"/>
        <Grid Name="grdClock" Grid.Row="5" HorizontalAlignment="Center">
            <!-- <local:Clock DataContext="{Binding Clock}"/> -->
        </Grid>
    </Grid>
</Window>"""
    //|>System.Windows.Markup.XamlReader.Parse
//    |> fun x -> x :?> Window
    |> parse<Window>
    |> fun x -> 
        x.FindName "grdClock" :?> Grid
        |> fun grd -> grd.Children.Add clock
        x

Program.mkSimple init update view
//        |> Program.withConsoleTrace
        |> Program.withSubscription subscribe
        |> runWindow (window)
        