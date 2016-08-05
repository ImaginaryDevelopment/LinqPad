<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

module Reflections = 
    let c_flags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic
    
    
    let windowsFormsAssembly = typeof<FileDialog>.Assembly
    let fosPickFoldersBitFlag = windowsFormsAssembly.GetType("System.Windows.Forms.FileDialogNative+FOS").GetField("FOS_PICKFOLDERS").GetValue(null) :?> uint32
    let iFileDialogType = windowsFormsAssembly.GetType("System.Windows.Forms.FileDialogNative+IFileDialog")
    let createFolderDialog = typeof<OpenFileDialog>.GetMethod("CreateVistaDialog", c_flags)
    let onBeforeVistaDialogMethodInfo = typeof<OpenFileDialog>.GetMethod("OnBeforeVistaDialog", c_flags)
    let getOptionsMethodInfo = typeof<FileDialog>.GetMethod("GetOptions", c_flags)
    let setOptionsMethodInfo = iFileDialogType.GetMethod("SetOptions", c_flags)
    let adviseMethodInfo = iFileDialogType.GetMethod("Advise")
    //let createVistaDialogMethodInfo = typeof(OpenFileDialog).GetMethod("CreateVistaDialog", c_flags)
    let vistaDialogEventsConstructorInfo = windowsFormsAssembly.GetType("System.Windows.Forms.FileDialog+VistaDialogEvents").GetConstructor(c_flags, null, [| typeof<FileDialog> |] , null)
    let show x (owner:IntPtr) = iFileDialogType.GetMethod("Show").Invoke(x, [| owner|]) :?> int
open Reflections 
let showOpenFolderDialog owner initialDirectory title = 
    use ofd = new OpenFileDialog(AddExtension = false, CheckFileExists = false, DereferenceLinks=true, Filter = "Folders|\n", InitialDirectory = initialDirectory, Multiselect=false, Title=title)
    let iFileDialog = createFolderDialog.Invoke(ofd, Array.empty)
    //onBeforeVistaDialogMethodInfo.Dump();
    onBeforeVistaDialogMethodInfo.Invoke(ofd, [| iFileDialog |]) |> ignore<obj>
    //s_setOptionsMethodInfo.Invoke(iFileDialog, new object[] { (uint) s_getOptionsMethodInfo.Invoke(openFileDialog, new object[] { }) | s_fosPickFoldersBitFlag });
    getOptionsMethodInfo.Invoke(ofd, Array.empty) :?> uint32  ||| fosPickFoldersBitFlag
    |> fun args -> setOptionsMethodInfo.Invoke(iFileDialog, [| args |])
    |> ignore<obj>
    
//    var adviseParametersWithOutputConnectionToken = new[] { s_vistaDialogEventsConstructorInfo.Invoke(new object[] { openFileDialog }), 0U };
//    s_adviseMethodInfo.Invoke(iFileDialog, adviseParametersWithOutputConnectionToken);

    let adviseParametersWithOutputConnectionToken = vistaDialogEventsConstructorInfo.Invoke([| ofd |])
    adviseMethodInfo.Invoke(iFileDialog, [| adviseParametersWithOutputConnectionToken; 0u|])
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