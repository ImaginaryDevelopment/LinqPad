<Query Kind="FSharpProgram">
  <Namespace>Microsoft.Win32.SafeHandles</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

// check for junctions/hard linked files
// based somewhat on http://stackoverflow.com/questions/4510269/c-sharp-count-and-list-hardlink-locations-of-a-file
// based loosely on https://blogs.msdn.microsoft.com/vbteam/2008/09/23/to-compare-two-filenames-lucian-wischik/

type System.String with
    static member startsWithI (toMatch:string) (x:string) = not <| isNull x && not <| isNull toMatch && toMatch.Length > 0 && x.StartsWith(toMatch, StringComparison.InvariantCultureIgnoreCase)
    static member containsI (sub:string) (x:string) = if isNull x then false elif sub = "" then failwithf "bad contains call" else x.IndexOf(sub, StringComparison.InvariantCultureIgnoreCase) >= 0

let (|StartsWithI|_|) s1 (toMatch:string) = if toMatch <> null && toMatch.StartsWith(s1, StringComparison.InvariantCultureIgnoreCase) then Some () else None
/// take a function that expects 2 arguments and flips them before applying to the function
let inline flip f x y = f y x

[<RequireQualifiedAccess>]
module PInvoke = 
    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]    
    type BY_HANDLE_FILE_INFORMATION =
            val mutable FileAttributes:UInt32 
            val mutable CreationTime:ComTypes.FILETIME 
            val mutable LastAccessTime:ComTypes.FILETIME 
            val mutable LastWriteTime:ComTypes.FILETIME 
            val mutable VolumeSerialNumber:UInt32
            val mutable FileSizeHigh:UInt32
            val mutable FileSizeLow:UInt32
            val mutable NumberOfLinks:UInt32
            val mutable FileIndexHigh:UInt32
            val mutable FileIndexLow:UInt32
        with 
            member x.HasHardLink = x.NumberOfLinks > 1u
            
//    [<Struct>]
//    [<StructLayout(LayoutKind.Sequential)>]
//    type FILETIME =
//        val mutable dwLowDateTime:UInt32;
//        val mutable dwHighDateTime:UInt32;
    [<Literal>]
    let MAX_PATH = 260
    [<Literal>]
    let MAX_ALTERNATE = 14
    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, CharSet=CharSet.Unicode)>] 
    type WIN32_FIND_DATA =
        val mutable dwFileAttributes:FileAttributes
        val mutable ftCreationTime:ComTypes.FILETIME 
        val mutable ftLastAccessTime:ComTypes.FILETIME 
        val mutable ftLastWriteTime:ComTypes.FILETIME 
        val mutable nFileSizeHigh:UInt32 //changed all to UInt32, otherwise you run into unexpected overflow
        val mutable nFileSizeLow:UInt32  //|
        val mutable dwReserved0:UInt32   //|
        val mutable dwReserved1:UInt32   //v
        [<MarshalAs(UnmanagedType.ByValTStr, SizeConst=MAX_PATH)>] 
        val mutable cFileName:string 
        [<MarshalAs(UnmanagedType.ByValTStr, SizeConst=MAX_ALTERNATE)>] 
        val mutable cAlternate:string 
    
    //http://www.pinvoke.net/default.aspx/kernel32/GetFileInformationByHandle.html
    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool GetFileInformationByHandle(IntPtr hFile,BY_HANDLE_FILE_INFORMATION& lpFileInformation);
    // http://www.pinvoke.net/default.aspx/kernel32/CreateFile.html
    [<DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)>]
    extern IntPtr CreateFile(
                                            string lpFileName,
                                            UInt32 dwDesiredAccess, //[<MarshalAs(UnmanagedType.U4)>] FileAccess dwDesiredAccess,
                                            UInt32 dwShareMode, //[<MarshalAs(UnmanagedType.U4)>] FileShare dwShareMode,
                                            IntPtr lpSecurityAttributes,
                                            UInt32, //[<MarshalAs(UnmanagedType.U4)>] FileMode dwCreationDisposition,
                                            UInt32, //[<MarshalAs(UnmanagedType.U4)>] FileAttributes dwFlagsAndAttributes,
                                            IntPtr hTemplateFile);
                                            
    [<DllImport("Kernel32.dll", CharSet= CharSet.Unicode, SetLastError= true)>]
    extern IntPtr FindFirstFileNameW(string lpFileName, UInt32 dwFlags, UInt32& stringlength, StringBuilder fileName);
//    [<DllImport("Kernel32.dll", CharSet= CharSet.Unicode, SetLastError= true)>]
//    extern IntPtr FindFirstFileNameTransactedW(string lpFileName, UInt32 dwFlags, 
    [<DllImport("Kernel32.dll", CharSet= CharSet.Unicode, SetLastError= true)>]
    extern bool FindNextFileNameW(IntPtr hFindStream,UInt32& stringLength,StringBuilder fileName);
    [<DllImport("kernel32.dll")>]
    extern bool GetVolumePathName(string lpszFileName,StringBuilder& lpszVolumePathName, UInt32 cchBufferLength);
    [<DllImport("shlwapi.dll", CharSet = CharSet.Auto)>]
    extern bool PathAppend(StringBuilder& pszPath, string pszMore);
    
module Wrapper = 
    let fileAttributes:UInt32 = 33554560u //&H2000080
    let getHandle (filename1:string) = PInvoke.CreateFile(filename1, Unchecked.defaultof<_>, 7u, IntPtr.Zero, 3u,fileAttributes ,IntPtr.Zero)
    let getFileInformationByHandle handle = 
        let mutable info:PInvoke.BY_HANDLE_FILE_INFORMATION = Unchecked.defaultof<_>
        let result = PInvoke.GetFileInformationByHandle(handle, &info)
        result,info
        
    // find out if this only works for files, or if it works for directories also
    let getFileHardLinks fileName = 
        let mutable stringLength = 256u
        let mutable sb = new StringBuilder(int stringLength)
        let volume = Path.GetPathRoot fileName //sb |> string
        let findHandle = PInvoke.FindFirstFileNameW(fileName, 0u, &stringLength, sb)
        let sequencer volume =
            if not <| String.IsNullOrEmpty volume then
                
                let fullPath = Path.Combine(volume, sb.ToString().TrimStart('\\'))

                sb.Length <- 0
                stringLength <- 256u
                let hasNext = PInvoke.FindNextFileNameW(findHandle, &stringLength, sb)
                Some (fullPath,if hasNext then volume else null)
            else None
            
        if findHandle.ToInt32() <> -1 then
            volume |> Seq.unfold sequencer
            |> List.ofSeq
            |> Seq.ofList
        else Seq.empty
        
let getFileInfoSample filename = 
    let handle = Wrapper.getHandle filename
    let success,info = Wrapper.getFileInformationByHandle handle
    (success,info)
    |> Dump

// want to make sure all files in this directory are hard linked to the actual usage spots
// check that all usages of the filename are linked to here.


let directoryToCheck = @"C:\projects\LinqPad\LinqPad\T4\"
let devBlackLists = [
    //String.containsI "Pm.DbAutomation"
    
    ]
    
directoryToCheck
|> Directory.GetFiles
|> Seq.map (fun f -> 
    let links = Wrapper.getFileHardLinks f
    let startDir = Environment.ExpandEnvironmentVariables("%devroot%")
    let extension = Path.GetExtension(f)
    let isBlackListed filePath = devBlackLists |> Seq.exists (fun f -> f filePath)
    let devLocations = 
        Directory.GetFiles(startDir, Path.GetFileName(f), SearchOption.AllDirectories)
        |> Seq.filter (isBlackListed>>not)
    f,links,devLocations  //Util.HighlightIf(links, fun l -> Seq.length l < 2)
    )
|> List.ofSeq
|> Dump


