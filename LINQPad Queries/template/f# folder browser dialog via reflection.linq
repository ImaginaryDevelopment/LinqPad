<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

//translated from question http://stackoverflow.com/questions/15368771/show-detailed-folder-browser-from-a-propertygrid
// using answer: http://stackoverflow.com/a/33836106/57883
module Reflections = 
    let c_flags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic
    let windowsFormsAssembly = typeof<FileDialog>.Assembly
    let fosPickFoldersBitFlag = windowsFormsAssembly.GetType("System.Windows.Forms.FileDialogNative+FOS").GetField("FOS_PICKFOLDERS").GetValue(null) :?> uint32
    let iFileDialogType = windowsFormsAssembly.GetType("System.Windows.Forms.FileDialogNative+IFileDialog")
    let private createVistaDialogMethodInfo = typeof<OpenFileDialog>.GetMethod("CreateVistaDialog", c_flags)
    let createVistaDialog o = createVistaDialogMethodInfo.Invoke(o, Array.empty)
    let private onBeforeVistaDialogMethodInfo = typeof<OpenFileDialog>.GetMethod("OnBeforeVistaDialog", c_flags)
    let onBeforeVistaDialog o iFileDialog = onBeforeVistaDialogMethodInfo.Invoke(o, [| iFileDialog |]) |> ignore<obj>
    let private getOptionsMethodInfo = typeof<FileDialog>.GetMethod("GetOptions", c_flags)
    let private setOptionsMethodInfo = iFileDialogType.GetMethod("SetOptions", c_flags)
    let getOptions o = getOptionsMethodInfo.Invoke(o, Array.empty) :?> uint32
    let setOptions iFileDialog (pickFoldersBitFlag: uint32) = setOptionsMethodInfo.Invoke(iFileDialog, [| pickFoldersBitFlag|]) |> ignore<obj>
    let private adviseMethodInfo = iFileDialogType.GetMethod("Advise")
    let advise iFileDialog adviseParametersWithOutputConnectionToken = adviseMethodInfo.Invoke(iFileDialog, [| adviseParametersWithOutputConnectionToken; 0u|]) |> ignore<obj>
    let private vistaDialogEventsConstructorInfo = windowsFormsAssembly.GetType("System.Windows.Forms.FileDialog+VistaDialogEvents").GetConstructor(c_flags, null, [| typeof<FileDialog> |] , null)
    let vistaDialogEvents o = vistaDialogEventsConstructorInfo.Invoke([| o |])
    let show x (owner:IntPtr) = iFileDialogType.GetMethod("Show").Invoke(x, [| owner|]) :?> int
    
open Reflections 
let showOpenFolderDialog owner initialDirectory title = 
    use ofd = new OpenFileDialog(AddExtension = false, CheckFileExists = false, DereferenceLinks=true, Filter = "Folders|\n", InitialDirectory = initialDirectory, Multiselect=false, Title=title)
    let iFileDialog = createVistaDialog ofd
    //iFileDialog.GetType().Dump() <- returned on win10 System.Windows.Forms.FileDialogNative+FileOpenDialogRCW
    onBeforeVistaDialog ofd iFileDialog
    (getOptions ofd ||| fosPickFoldersBitFlag)
    |> setOptions iFileDialog

    let adviseParametersWithOutputConnectionToken = vistaDialogEvents ofd
    advise iFileDialog adviseParametersWithOutputConnectionToken
    let result = 
        try
            let retVal = show iFileDialog owner
            Some((retVal = 0), ofd.FileName)
        with ex ->
            None
    //ofd.ShowDialog()
    result
    
showOpenFolderDialog IntPtr.Zero null null 
|> Dump
|> ignore