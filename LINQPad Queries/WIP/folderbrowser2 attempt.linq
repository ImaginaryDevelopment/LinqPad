<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Runtime.InteropServices</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>


module FolderBrowsing = 
    //might help: 
    // https://blogs.msdn.microsoft.com/jomo_fisher/2009/11/17/f-scripting-net-4-0-and-mixed-mode-assemblies/
    //   System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeInterfaceAsObject(
    // http://stackoverflow.com/questions/10383041/comimport-in-f
//    [<ComImport>]
//    [<Guid("DC1C5A9C-E88A-4dde-A5A1-60F82A20AEF7")>]
//    type private FileOpenDialog() = class end
    let createComObject (guid:Guid) = 
        let ty = System.Type.GetTypeFromCLSID guid
        Activator.CreateInstance ty
        
    [<Flags>]
    type private FOS = 
        |FOS_ALLNONSTORAGEITEMS = 0x80
        |FOS_ALLOWMULTISELECT = 0x200
        |FOS_CREATEPROMPT = 0x2000
        |FOS_DEFAULTNOMINIMODE = 0x20000000
        |FOS_DONTADDTORECENT = 0x2000000
        |FOS_FILEMUSTEXIST = 0x1000
        |FOS_FORCEFILESYSTEM = 0x40
        |FOS_FORCESHOWHIDDEN = 0x10000000
        |FOS_HIDEMRUPLACES = 0x20000
        |FOS_HIDEPINNEDPLACES = 0x40000
        |FOS_NOCHANGEDIR = 8
        |FOS_NODEREFERENCELINKS = 0x100000
        |FOS_NOREADONLYRETURN = 0x8000
        |FOS_NOTESTFILECREATE = 0x10000
        |FOS_NOVALIDATE = 0x100
        |FOS_OVERWRITEPROMPT = 2
        |FOS_PATHMUSTEXIST = 0x800
        |FOS_PICKFOLDERS = 0x20
        |FOS_SHAREAWARE = 0x4000
        |FOS_STRICTFILETYPES = 4
        
        type private SIGDN =
        |NORMALDISPLAY = 0u
        |PARENTRELATIVEPARSING = 0x80018001u
        |PARENTRELATIVEFORADDRESSBAR = 0x8001c001u
        |DESKTOPABSOLUTEPARSING = 0x80028000u
        |PARENTRELATIVEEDITING = 0x80031001u
        |DESKTOPABSOLUTEEDITING = 0x8004c000u
        |FILESYSPATH = 0x80058000u
        |URL = 0x80068000u
//        |SIGDN_DESKTOPABSOLUTEEDITING = 0x8004c000u
//        |SIGDN_DESKTOPABSOLUTEPARSING = 0x80028000u
//        |SIGDN_FILESYSPATH = 0x80058000u
//        |SIGDN_NORMALDISPLAY = 0u
//        |SIGDN_PARENTRELATIVE = 0x80080001u
//        |SIGDN_PARENTRELATIVEEDITING = 0x80031001u
//        |SIGDN_PARENTRELATIVEFORADDRESSBAR = 0x8007c001u
//        |SIGDN_PARENTRELATIVEPARSING = 0x80018001u
//        |SIGDN_URL = 0x80068000u
        
    [<ComImport>]
    [<Guid("43826D1E-E718-42EE-BC55-A1E261C37BFE")>]
    [<InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>]
    [<AllowNullLiteral>]
    type private IShellItem =
        abstract BindToHandler: unit -> unit // not fully defined
        abstract GetParent: unit -> unit // not fully defined
        abstract GetDisplayName: sigdnName:SIGDN * [<Out;MarshalAs(UnmanagedType.LPWStr)>] ppszName:string -> unit
        //abstract GetDisplayName: sigdnName:SIGDN * [<Out>] ppszName:IntPtr  -> unit
        abstract GetAttributes: unit -> unit  // not fully defined
        abstract Compare: unit -> unit  // not fully defined
    
    [<ComImport>]
    [<Guid("42f85136-db7e-439c-85f1-e4075d135fc8")>]
    [<InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>]
    type private IFileOpenDialog =
        [<PreserveSig>]
        abstract Show: [<In>] parent:IntPtr -> System.UInt32; // IModalWindow
        abstract SetFileTypes: unit -> unit  // not fully defined
        abstract SetFileTypeIndex: [<In>] iFileType:System.UInt32 -> unit
        abstract GetFileTypeIndex:   [<Out>] piFileType: System.UInt32 -> unit
        abstract Advise: unit -> unit // not fully defined
        abstract Unadvise: unit -> unit
        abstract SetOptions:[<In>] fos:FOS  -> unit
        abstract GetOptions:[<Out>] pfos:FOS -> unit ;
        abstract SetDefaultFolder:psi:IShellItem -> unit 
        abstract SetFolder:psi:IShellItem -> unit
        abstract GetFolder: [<Out>]  ppsi:IShellItem -> unit
        abstract GetCurrentSelection:[<Out>] ppsi :IShellItem -> unit
        abstract SetFileName:[<In; MarshalAs(UnmanagedType.LPWStr)>] pszName:string -> unit
        abstract GetFileName:[<MarshalAs(UnmanagedType.LPWStr)>] [<Out>]  pszName:string-> unit
        abstract SetTitle:[<In; MarshalAs(UnmanagedType.LPWStr)>] pszTitle:string-> unit
        abstract SetOkButtonLabel:[<In; MarshalAs(UnmanagedType.LPWStr)>] pszText:string-> unit
        abstract SetFileNameLabel:[<In; MarshalAs(UnmanagedType.LPWStr)>] pszLabel:string -> unit
        abstract GetResult: [<Out;MarshalAs(UnmanagedType.Interface)>]ppsi:IShellItem -> unit
        abstract AddPlace:psi:IShellItem*alignment:int -> unit
        abstract SetDefaultExtension:[<In; MarshalAs(UnmanagedType.LPWStr)>] pszDefaultExtension:string -> unit
        abstract Close:hr:int -> unit
        abstract SetClientGuid: unit -> unit  // not fully defined
        abstract ClearClientData: unit -> unit
        abstract SetFilter:[<MarshalAs(UnmanagedType.Interface)>]  pFilter:IntPtr -> unit
        abstract GetResults:[<MarshalAs(UnmanagedType.Interface)>] [<Out>]  ppenum:IntPtr -> unit // not fully defined
        abstract GetSelectedItems:[<MarshalAs(UnmanagedType.Interface)>] [<Out>]  ppsai:IntPtr  -> unit // not fully defined
        
    module private PInvoke = 
        // pinvoke help for F#: http://stackoverflow.com/questions/1689460/f-syntax-for-p-invoke-signature-using-marshalas
        [<DllImport("user32.dll")>]
        extern IntPtr GetActiveWindow();
        [<DllImport("shell32.dll")>]
        extern int SHILCreateFromPath([<MarshalAs(UnmanagedType.LPWStr)>] string pszPath, [<Out>]IntPtr ppIdl, [<Out;In>]uint32 rgflnOut);
        [<DllImport("shell32.dll")>]
        extern int SHCreateShellItem(IntPtr pidlParent, IntPtr psfParent, IntPtr pidl, [<Out>] IShellItem ppsi);
    
    let private ERROR_CANCELLED = 0x800704C7u
    
    type FolderBrowser2() = 
        member val  DirectoryPath:string = null with get,set
    
        member x.ShowDialog(owner:IWin32Window) =
            let hwndOwner = if owner |> isNull |> not then owner.Handle else PInvoke.GetActiveWindow()
    
            let dialog = createComObject (Guid("DC1C5A9C-E88A-4dde-A5A1-60F82A20AEF7")) :?> IFileOpenDialog
            
            try
                let item:IShellItem = null
                //let mutable item:byref<IShellItem> =  &ishell // createComObject (Guid "43826D1E-E718-42EE-BC55-A1E261C37BFE") :?> IShellItem
                //TODO: this commented out section translation
//                if not <| String.IsNullOrEmpty x.DirectoryPath then
//                
//                    let mutable idl= IntPtr.Zero
//                    let mutable atts= 0u
//                    if (PInvoke.SHILCreateFromPath(x.DirectoryPath, idl, atts) = 0) then
//                        if (PInvoke.SHCreateShellItem(IntPtr.Zero, IntPtr.Zero, idl, item) = 0) then
//                            dialog.SetFolder(item);

                dialog.SetOptions(FOS.FOS_PICKFOLDERS ||| FOS.FOS_FORCEFILESYSTEM)
                let hr = dialog.Show(hwndOwner);
                if (hr = ERROR_CANCELLED) then
                    DialogResult.Cancel
                else    
                    if hr <> 0u then
                        DialogResult.Abort
                    else 
                        try
                            dialog.GetResult(item)
                            //let mutable path:IntPtr = IntPtr.Zero
                            let mutable path:string= null
                            
//                            Enum.GetValues(typeof<SIGDN>) 
//                            |> Seq.cast<int>
//                            |> Seq.map (fun i -> Enum.ToObject(typeof<SIGDN>,i) :?> SIGDN)
//                            |> Seq.choose(fun v ->
//                                try 
//                                    item.Getdis
//                                )
                                
                            item.GetDisplayName(SIGDN.FILESYSPATH, path)
                            x.DirectoryPath <- path |> string
                            DialogResult.OK
                        with ex ->
                            //Marshal.query
                            ex.Data.Add("Win32Error",Marshal.GetLastWin32Error())
                            reraise()
                            
            finally
                Marshal.ReleaseComObject(dialog) |> ignore<int>


let fb = FolderBrowsing.FolderBrowser2()
//fb.DirectoryPath <- Environment.ExpandEnvironmentVariables("%devroot%")
let result = fb.ShowDialog(null)

let path = fb.DirectoryPath

(result,path).Dump()