<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.InteropServices.dll</Reference>
  <Namespace>Microsoft.Win32</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
  <Namespace>Microsoft.Win32.SafeHandles</Namespace>
</Query>

void Main()
{
	bool debug=false;
	
	
	HandleEnvironmentVariables(debug);
	
	HandleApps(debug);
	HandleGitBash(debug);
	
	HandleSites(_payspanRoot, HealthMode.Interactive,debug);
//	Util.RawHtml("<script src='http://cdnjs.cloudflare.com/ajax/libs/knockout/2.1.0/knockout-min.js'></script>").Dump();
//	Util.RawHtml(@"<script type='text/javascript'>var healthModel= function HealthModel(){
//		this.env=ko.observable(false);
//		this.apps=ko.observable(false);
//		this.bash=ko.observable(false);
//	};
//	ko.applyBindings(new healthModel());</script>").Dump();
}
public enum HealthMode{
Interactive,
True,
False,
}

#region EnvironmentVars
string _payspanRoot=@"C:\Projects\psh\hpx";
string _payspanEndpoint;
const string PayspanEnv="PSH_ENVIRONMENT";
const string PayspanConfigEndpoint="PSH_CONFIG_ENDPOINT";
const string PayspanConfigDb="PSH_CONFIG_DB_CONNECTION";

// Define other methods and classes here
public  void HandleEnvironmentVariables(bool debug)
{
	var paths=Environment.GetEnvironmentVariable("PATH").Split(new[]{';'}, StringSplitOptions.RemoveEmptyEntries);
	if(paths.Any (p => p.Contains("\"")))
		Util.Highlight("Quotes cause errors for VS command prompt in Windows 7").Dump();
	if(paths.Any (p => p.Contains("SysInternals"))==false)
		Util.Highlight("SysInternals not in path");
	foreach(var path in paths)
	{
		if(System.IO.Directory.Exists(path)==false)
			Util.Highlight(path).Dump("path entry not found");
		
	}
	this._payspanEndpoint=Environment.GetEnvironmentVariable(PayspanConfigEndpoint);
	HandlePshConfigEp(_payspanEndpoint);
	
	
}

public static void HandlePshConfigEp(string endpoint)
{
	if(endpoint.IsNullOrEmpty()){
		"endpoint not found".Dump(PayspanConfigEndpoint);
		return;
	}
	var epUri=new Hyperlinq(endpoint);
	epUri.Dump("Config Endpoint");
}


#endregion

public static void HandleSites(string payspanRoot,HealthMode mode,bool debug)
{
	//http://stackoverflow.com/questions/1619308/how-do-i-change-the-physical-path-of-web-site-in-iis7-with-appcmd
	const string path=@"C:\Windows\System32\inetsrv\appcmd.exe"; //http://blogs.msdn.com/b/sukeshak/archive/2006/05/24/605911.aspx
	if(System.IO.File.Exists(path)==false)
	{
		Util.Highlight(path).Dump("Not found");
		return;
	}
	using(var ps=new Process())
	{
	ps.StartInfo.UseShellExecute=false;
		ps.StartInfo.RedirectStandardOutput=true;
		ps.StartInfo.RedirectStandardError=true;
		ps.StartInfo.FileName=path; //appcmd list app /config
		var outputs=ps.RunProcessRedirected("list apps /config /xml"); // for debug consider adding /path:/jms
		if(outputs.Errors.IsNullOrEmpty()==false)
		{
			Util.Highlight(outputs.Errors).Dump("Error running appcmd");
		}
		
		var config=XDocument.Parse(outputs.Output);
		if(config.Root.IsEmpty)
			throw new InvalidOperationException();
		if(debug) config.DumpFormatted();
		var isJunction=payspanRoot.IsNullOrEmpty()==false && Exists(payspanRoot);
		
		var q= from siteApp in config.XPathSelectElements("appcmd/APP")
			let appName=siteApp.Attribute(XNamespace.None+"APP.NAME").Value
				from app in siteApp.XPathSelectElements("application")
			let appPath=app.Attribute(XNamespace.None+"path").Value
			let pool=app.GetAttribValOrNull("applicationPool")
			let vd=app.XPathSelectElements("virtualDirectory[@path]")
			let virtuals=vd.Select (v => new{VirDir=v.Attribute(XNamespace.None+"path").Value,PhysicalPath=v.Attribute(XNamespace.None+"physicalPath").Value})
			let xvirtuals=virtuals.Select (v => new{ VirDir=v.VirDir,
				PhysicalPath=v.PhysicalPath,
				DisplayPhysicalPath=Util.HighlightIf(v.PhysicalPath,f=>f.StartsWith(payspanRoot)==false),
				ValidRoot=(isJunction && v.PhysicalPath.StartsWith(payspanRoot)),
				Exists=Directory.Exists(v.PhysicalPath)}) //valid meaning consistent with team standards
			select new{AppName=appName,AppPath=appPath, Pool=pool,Virtuals=xvirtuals};
		var allEntries=q.ToList();
		if(debug) allEntries.Dump("all entries");
		
		var suspects=allEntries.Where (e => e.Virtuals.Any (v =>v.ValidRoot==false));
		
		if(payspanRoot.IsNullOrEmpty()){
			suspects.Dump("suspects");
			"unable to process app paths, no payspanRoot variable found".Dump();
			return;
		}
		//items.Dump();
		var payspanUri= new Uri(payspanRoot);
		if(suspects.Any()){	
				foreach(var badItem in suspects)
				foreach(var virt in badItem.Virtuals.Where (v =>v.ValidRoot==false))
				{
					if(virt.PhysicalPath.ToLower().Contains("projects")==false)
					{
						Util.Highlight(new{badItem.AppPath,badItem.Pool,virt.DisplayPhysicalPath,virt.VirDir}).Dump("unable to determine adjustment for path");
						continue;
					}
					//all items before projects
					var payspanBase= payspanRoot.Before("projects", StringComparison.CurrentCultureIgnoreCase);
					var payspanSuffix=payspanRoot.After("projects", StringComparison.CurrentCultureIgnoreCase);
					var proposedUri=payspanUri.MakeRelativeUri(new Uri(virt.PhysicalPath));
					var segments=SegmentPath(proposedUri.ToString()).ToArray();
					
					if(System.IO.Path.GetDirectoryName(proposedUri.ToString())!=System.IO.Path.GetDirectoryName(payspanRoot))
					segments=segments.Skip(1).ToArray();
					var basedProposed=segments.Prepend(payspanRoot).ToArray();
					
					var proposedPath=System.IO.Path.Combine(basedProposed);
					
					var display=new{badItem.AppName,AppPath=badItem.AppPath,ProposedPath=proposedPath,Proposed=segments.Aggregate (System.IO.Path.Combine ).ToString(),virt.VirDir,
						virt.PhysicalPath,ProposedExists=System.IO.Directory.Exists(proposedPath)};
					display.Dump();
					var relPath=segments.Aggregate (System.IO.Path.Combine);
					
					var args="set vdir \""+badItem.AppName+virt.VirDir
						+"\" -physicalPath:\""+ System.IO.Path.Combine(payspanRoot,relPath)+"\"";
							
					
					if(display.ProposedExists)
					{
							StreamOuts outs;
							if (System.IO.Directory.Exists(virt.PhysicalPath)==false)
							{
								args.Dump("running appcmd");
								outs=ps.RunProcessRedirected(args);
								outs.Dump("ran appcmd");
							}
							else{
								
								args.Dump("proposed appcmd");
								if(mode== HealthMode.True || (mode== HealthMode.Interactive && Util.ReadLine<bool>("Run update?"))){
									outs=ps.RunProcessRedirected(args);
									outs.Dump("ran appcmd");
								}
							}
							
						
					} else{
						Util.Highlight(new{badItem.AppPath,badItem.Pool,virt.DisplayPhysicalPath,virt.VirDir}).Dump("proposed does not exist");
					}
					
				
				}
		
		}
	}
	
}

public static IEnumerable<string> SegmentPath(string input)
{
	return input.Split(new[]{System.IO.Path.AltDirectorySeparatorChar,System.IO.Path.DirectorySeparatorChar });
}

public static void HandleGitBash(bool debug)
{
	var profilePath=System.Environment.GetFolderPath( System.Environment.SpecialFolder.UserProfile);
	var gitrc=System.IO.Path.Combine(profilePath,".bashrc");
	if(File.Exists(gitrc)==false)
	{
		gitrc.Dump("Not found");
		return;
	}
	var rcLines=File.ReadAllLines(gitrc);
	if(rcLines.Any (l => l.StartsWith("alias "))==false)
		("Suggested aliases:"+Environment.NewLine+
		"alias dir='ls -l'"+Environment.NewLine+
		"alias gr='git remote -v'").Dump();
	
}

public static void HandleApps(bool debug){

	var uninstallKey=Registry.LocalMachine.OpenSubKey("Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall");
	var installed=uninstallKey.GetSubKeyNames()
	.Select (k => uninstallKey.OpenSubKey(k))
	.Where (appKey=>appKey.GetValueNames().Contains("DisplayName"))
	.Select (appKey => appKey.GetValue("DisplayName").ToString());
	var vs2012=installed.FirstOrDefault (i =>Regex.IsMatch(i,"Microsoft Visual Studio \\w+ 2012" ) );
	if(vs2012==null)
		"Visual Studio 2012 not found".Dump();
		else if (debug)
		vs2012.Dump();
		if(installed.FirstOrDefault (i => i.StartsWith("MagicDisc "))==null)
			"MagicDisc not found".Dump();
			if(debug)
	installed.Dump();
}


#region PInvoke //http://www.codeproject.com/script/Articles/ViewDownloads.aspx?aid=15633

          [Flags]
          private enum EFileAccess : uint
          {
              GenericRead = 0x80000000,
              GenericWrite = 0x40000000,
              GenericExecute = 0x20000000,
              GenericAll = 0x10000000,
          }
		  
		  [Flags]
         private enum EFileAttributes : uint
         {
             Readonly = 0x00000001,
             Hidden = 0x00000002,
             System = 0x00000004,
             Directory = 0x00000010,
             Archive = 0x00000020,
             Device = 0x00000040,
             Normal = 0x00000080,
             Temporary = 0x00000100,
             SparseFile = 0x00000200,
             ReparsePoint = 0x00000400,
             Compressed = 0x00000800,
             Offline = 0x00001000,
             NotContentIndexed = 0x00002000,
             Encrypted = 0x00004000,
             Write_Through = 0x80000000,
             Overlapped = 0x40000000,
             NoBuffering = 0x20000000,
             RandomAccess = 0x10000000,
             SequentialScan = 0x08000000,
             DeleteOnClose = 0x04000000,
             BackupSemantics = 0x02000000,
             PosixSemantics = 0x01000000,
             OpenReparsePoint = 0x00200000,
             OpenNoRecall = 0x00100000,
             FirstPipeInstance = 0x00080000
         }
		[Flags]
          private enum EFileShare : uint
          {
              None = 0x00000000,
              Read = 0x00000001,
              Write = 0x00000002,
              Delete = 0x00000004,
          }  
		  private enum ECreationDisposition : uint
          {
              New = 1,
              CreateAlways = 2,
              OpenExisting = 3,
              OpenAlways = 4,
              TruncateExisting = 5,
          }
/// <summary>
/// Determines whether the specified path exists and refers to a junction point.
/// </summary>
/// <param name="path">The junction point path</param>
/// <returns>True if the specified path represents a junction point</returns>
/// <exception cref="IOException">Thrown if the specified path is invalid
/// or some other error occurs</exception>
public static bool Exists(string path)
{
  if (! Directory.Exists(path))
      return false;

  using (SafeFileHandle handle = OpenReparsePoint(path, EFileAccess.GenericRead))
  {
      string target = InternalGetTarget(handle);
      return target != null;
  }
}
/// <summary>
/// The file or directory is not a reparse point.
/// </summary>
private const int ERROR_NOT_A_REPARSE_POINT = 4390;
/// <summary>
 /// Command to get the reparse point data block.
 /// </summary>
private const int FSCTL_GET_REPARSE_POINT = 0x000900A8;

 /// <summary>
 /// Reparse point tag used to identify mount points and junction points.
 /// </summary>
private const uint IO_REPARSE_TAG_MOUNT_POINT = 0xA0000003;
/// This prefix indicates to NTFS that the path is to be treated as a non-interpreted
/// path in the virtual file system.
/// </summary>
private const string NonInterpretedPathPrefix = @"\??\";
 [StructLayout(LayoutKind.Sequential)]
private struct REPARSE_DATA_BUFFER
{
   /// <summary>
   /// Reparse point tag. Must be a Microsoft reparse point tag.
   /// </summary>
   public uint ReparseTag;

   /// <summary>
   /// Size, in bytes, of the data after the Reserved member. This can be calculated by:
   /// (4 * sizeof(ushort)) + SubstituteNameLength + PrintNameLength + 
   /// (namesAreNullTerminated ? 2 * sizeof(char) : 0);
   /// </summary>
   public ushort ReparseDataLength;

   /// <summary>
   /// Reserved; do not use. 
   /// </summary>
   public ushort Reserved;

   /// <summary>
   /// Offset, in bytes, of the substitute name string in the PathBuffer array.
   /// </summary>
   public ushort SubstituteNameOffset;

   /// <summary>
   /// Length, in bytes, of the substitute name string. If this string is null-terminated,
   /// SubstituteNameLength does not include space for the null character.
   /// </summary>
   public ushort SubstituteNameLength;

   /// <summary>
   /// Offset, in bytes, of the print name string in the PathBuffer array.
   /// </summary>
   public ushort PrintNameOffset;

   /// <summary>
   /// Length, in bytes, of the print name string. If this string is null-terminated,
   /// PrintNameLength does not include space for the null character. 
   /// </summary>
   public ushort PrintNameLength;

   /// <summary>
   /// A buffer containing the unicode-encoded path string. The path string contains
   /// the substitute name string and print name string.
   /// </summary>
   [MarshalAs(UnmanagedType.ByValArray, SizeConst = 0x3FF0)]
   public byte[] PathBuffer;
}

 private static void ThrowLastWin32Error(string message)
{
   throw new IOException(message, Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error()));
}
 private static string InternalGetTarget(SafeFileHandle handle)
{
    int outBufferSize = Marshal.SizeOf(typeof(REPARSE_DATA_BUFFER));
    IntPtr outBuffer = Marshal.AllocHGlobal(outBufferSize);

    try
    {
        int bytesReturned;
        bool result = DeviceIoControl(handle.DangerousGetHandle(), FSCTL_GET_REPARSE_POINT,
            IntPtr.Zero, 0, outBuffer, outBufferSize, out bytesReturned, IntPtr.Zero);

        if (!result)
        {
            int error = Marshal.GetLastWin32Error();
            if (error == ERROR_NOT_A_REPARSE_POINT)
                return null;

            ThrowLastWin32Error("Unable to get information about junction point.");
        }

        REPARSE_DATA_BUFFER reparseDataBuffer = (REPARSE_DATA_BUFFER)
            Marshal.PtrToStructure(outBuffer, typeof(REPARSE_DATA_BUFFER));

        if (reparseDataBuffer.ReparseTag != IO_REPARSE_TAG_MOUNT_POINT)
            return null;

        string targetDir = Encoding.Unicode.GetString(reparseDataBuffer.PathBuffer,
            reparseDataBuffer.SubstituteNameOffset, reparseDataBuffer.SubstituteNameLength);

        if (targetDir.StartsWith(NonInterpretedPathPrefix))
            targetDir = targetDir.Substring(NonInterpretedPathPrefix.Length);

        return targetDir;
    }
    finally
    {
        Marshal.FreeHGlobal(outBuffer);
    }
}

private static SafeFileHandle OpenReparsePoint(string reparsePoint, EFileAccess accessMode)
          {
              SafeFileHandle reparsePointHandle = new SafeFileHandle(CreateFile(reparsePoint, accessMode,
                  EFileShare.Read | EFileShare.Write | EFileShare.Delete,
                  IntPtr.Zero, ECreationDisposition.OpenExisting,
                  EFileAttributes.BackupSemantics | EFileAttributes.OpenReparsePoint, IntPtr.Zero), true);
  
              if (Marshal.GetLastWin32Error() != 0)
                  ThrowLastWin32Error("Unable to open reparse point.");
  
              return reparsePointHandle;
          }
  
[DllImport("kernel32.dll", SetLastError = true)]
private static extern IntPtr CreateFile(
 string lpFileName,
 EFileAccess dwDesiredAccess,
 EFileShare dwShareMode,
 IntPtr lpSecurityAttributes,
 ECreationDisposition dwCreationDisposition,
 EFileAttributes dwFlagsAndAttributes,
 IntPtr hTemplateFile);  
       [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
 private static extern bool DeviceIoControl(IntPtr hDevice, uint dwIoControlCode,
   IntPtr InBuffer, int nInBufferSize,
   IntPtr OutBuffer, int nOutBufferSize,
    out int pBytesReturned, IntPtr lpOverlapped);

#endregion