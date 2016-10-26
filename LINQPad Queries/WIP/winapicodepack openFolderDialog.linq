<Query Kind="FSharpProgram" />

// attempt to translate the windowsapicodepack's nicer openfolderdialog


module WindowsAPICodePack = 
    module TaskDialogNativeMethods = //https://github.com/ljvankuiken/WindowsAPICodePack/blob/master/WindowsAPICodePack/Core/Interop/TaskDialogs/TaskDialogNativeMethods.cs
        type TaskDialogCommonButtonReturnIds =
            | Ok = 1
            | Cancel = 2
            | Abort = 3
            | Retry = 4
            | Ignore = 5
            | Yes = 6
            | No = 7
            | Close = 8
            
    module DialogsDefaults = //https://github.com/ljvankuiken/WindowsAPICodePack/blob/master/WindowsAPICodePack/Core/Dialogs/Common/DialogsDefaults.cs
        let minimumDialogControlId = int TaskDialogNativeMethods.TaskDialogCommonButtonReturnIds.Close + 1
        
    
    let mutable private nextId = DialogsDefaults.minimumDialogControlId
    [<AllowNullLiteral>]
    type IDialogControlHost = //https://github.com/ljvankuiken/WindowsAPICodePack/blob/master/WindowsAPICodePack/Core/Dialogs/Common/IDialogControlHost.cs
        abstract IsCollectionChangeAllowed: unit -> bool
        abstract IsControlPropertyChangeAllowed: string -> DialogControl -> bool
        abstract ApplyCollectionChanged: unit -> unit
    and 
        [<AbstractClass>]
        DialogControl(name) = //https://github.com/ljvankuiken/WindowsAPICodePack/blob/master/WindowsAPICodePack/Core/Dialogs/Common/DialogControl.cs
            let identifier = nextId
            
            do
                nextId <-
                    match nextId with
                    | Int32.MaxValue -> DialogsDefaults.minimumDialogControlId
                    | x -> x + 1
            
            //new() = DialogControl(null)
            /// The native dialog that is hosting this control. This property is null is
            /// there is not associated dialog
            member val HostingDialog:IDialogControlHost = null with get,set
            //member val Name = name with get,set
            member __.Name = name 
            member __.Id = identifier
            
            member x.CheckPropertyChangeAllowed propName = 
                Debug.Assert(not <| String.IsNullOrEmpty propName, "Property to change was not specified")
                match isNull x.HostingDialog with
                | true -> ()
                | false -> 
                    // This will throw if the property change is not allowed.
                    x.HostingDialog.IsControlPropertyChangeAllowed propName x |> ignore<bool>
            override x.Equals o = 
                match o with
                | :? DialogControl as control -> x.Id = control.Id
                | _ -> false
            override x.GetHashCode() = 
                match name with
                | null -> x.ToString().GetHashCode()
                | name -> name.GetHashCode()

    //and 't : null
    type CommonFileDialogControlCollection< 't when 't :> DialogControl  >(host:IDialogControlHost) = //https://github.com/dbarros/WindowsAPICodePack/blob/master/WindowsAPICodePack/Shell/CommonFileDialogs/CommonFileDialogControlCollection.cs
        inherit Collection<'t>()
        override x.InsertItem(index,control) =
            if x.Items.Contains control then
                invalidOp "DialogControlCollectionMoreThanOneControl"
            if control.HostingDialog |> isNull |> not then
                invalidOp "DialogControlRemoveControlFirst"
            if host.IsCollectionChangeAllowed() |> not then
                invalidOp "DialogControlCollectionModifyingControls"
            // unless needed, not seeking out/converting CommonFileDialogMenuItem which is at https://github.com/ljvankuiken/WindowsAPICodePack/blob/master/WindowsAPICodePack/Shell/CommonFileDialogs/CommonFileDialogMenu.cs
//            match control with
//            | :? CommonFileDialogMenuItem -> 
//                invalidOp "DialogControlCollectionMenuItemControlsCannotBeAdded"
//            | _ -> ()
            
            control.HostingDialog <- host
            base.InsertItem(index,control)
            
            host.ApplyCollectionChanged()
            
        override __.RemoveItem index = raise <| NotSupportedException "DialogControlCollectionCannotRemoveControls"
        
        member x.Item
            with get (name:string): _ = 
                match String.IsNullOrEmpty name with
                | true -> raise <| ArgumentException("DialogControlCollectionEmptyName","name")
                | false -> 
                    x.Items 
                    |> Seq.choose (fun c -> 
                        match name with
                        | n when c.Name = n -> Some c
                        | _ -> 
                            match c with
//                            | :? CommonFileDialogGroupBox as groupBox ->
//                                groupBox.Items |> Seq.tryFind (fun subControl -> subControl.Name = name)
                            | _ -> None
                    )
                    |> Seq.tryHead
                    //|> function | Some c -> c | None -> null
    module DialogsComInterfaces = //https://github.com/ljvankuiken/WindowsAPICodePack/blob/master/WindowsAPICodePack/Shell/Interop/Dialogs/DialogsCOMInterfaces.cs
        [<ComImport>]
        [<Guid("42F85136-DB7E-439C-85F1-E4075D135FC8")>]
        [<InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>]
        type IFileDialog = 
            interface IModalWindow with
                member x.Show([<In>] IntPtr parent):int
        
        
    [<AbstractClass>]
    type CommonFileDialog(title) = // https://github.com/dbarros/WindowsAPICodePack/blob/master/WindowsAPICodePack/Shell/CommonFileDialogs/CommonFileDialog.cs
        let mutable title = title
        let filenames = List<string>()
        let nativeDialog: IFileDialog = null
        member __.FileNameCollection = filenames
        
        new () = CommonFileDialog(null)
        
        member x.Title 
            with get() = title
            and set v =
                title <- v
                if x.NativeDialogShowing then
                    nativeDialog.SetTitle v
        
    type CommonOpenFileDialog() =
        inherit CommonFileDialog()
        let mutable openDialogCoClass: NativeFileOpenDialog = null
        do
            base.EnsureReadOnly <- true