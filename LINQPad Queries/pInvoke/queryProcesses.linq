<Query Kind="Program">
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

static bool keepEnumerating = true;
const int limit = 150; // seems the while loop doesn't finish otherwise, or maybe I didn't wait long enough?
static int count = 0;
// Pinvoke SetFocus to a particular control: http://stackoverflow.com/a/9547099/57883


void Main()
{
	keepEnumerating = true;
	count = 0;
	pid2ToWindows.Clear();
	pidToWindows.Clear();
	var blacklist = new[]{
		"chrome",
		"LINQPad.UserQuery",
		"explorer",
		"winlogon",
		"svchost"
	};
	Util.OnDemand("GetWindows",() =>
		  {
		  	Util.OnDemand("Stop enumerating", 
				() => { 
					keepEnumerating = false; 
					return new {pidToWindows, pid2ToWindows};
					}
				).Dump();
			  while (EnumWindows((EnumWindowsProc)WithWindows, IntPtr.Zero)) { }
			  count.Dump("windows enumerated");
			  return Util.OnDemand("GetProcesses()", GetProcesses);
		  }).Dump();
	
}
static IEnumerable<object> GetProcesses()
{
	var blacklist = new[]{
		"chrome",
		"LINQPad.UserQuery",
		"explorer",
		"winlogon",
		"svchost"
	};

	return Process.GetProcesses()
	.Where(p => !blacklist.Contains(p.ProcessName))
	.Select(SelectProcesses);
	
}
static Dictionary<uint, HashSet<IntPtr>> pidToWindows = new Dictionary<uint, HashSet<IntPtr>>();
static IDictionary<uint, HashSet<IntPtr>> pid2ToWindows = new Dictionary<uint, HashSet<IntPtr>>();

static object SelectProcesses(Process p)
{
	var windows = pid2ToWindows.ContainsKey(Convert.ToUInt32(p.Id)) ?
		pid2ToWindows[Convert.ToUInt32(p.Id)].Select(hWnd => new { WindowText = Util.OnDemand("TryGetWindowText", () => hWnd.TryGetWindowText()), ClassName = Util.OnDemand("TryGetClassName", () => hWnd.TryGetClassName()), hWnd}).ToArray() : null;
	return new
	{
		name = p.ProcessName,
		id = p.Id,
		MainModule = Util.OnDemand("TryGetMainModule",() => p.TryGetMainModule()),
		Details = p,
		Windows = windows,
		Kill = new Hyperlinq(() => p.Kill(), "Kill")
	};
}

static bool WithWindows(IntPtr hWnd, IntPtr lParam)
{
	uint pIdOut = 0;

	var pId = GetWindowThreadProcessId(hWnd, out pIdOut);

	if (lParam != IntPtr.Zero)
		new {pId, pIdOut, hWnd, lParam }.Dump();
	
	if (pidToWindows.ContainsKey(pId) == false)
		pidToWindows.Add(pId, new HashSet<IntPtr>());
	pidToWindows[pId].Add(hWnd);
	if (pid2ToWindows.ContainsKey(pIdOut) == false)
		pid2ToWindows.Add(pIdOut, new HashSet<IntPtr>());
	pid2ToWindows[pIdOut].Add(hWnd);
	System.Threading.Thread.Sleep(100);
	count++;
	
	return keepEnumerating && count < limit;
}

public static class Extensions
{
	public static object TryGetWindowText(this IntPtr hWnd)
	{
		try
		{
			var sb = new StringBuilder();
			var result = UserQuery.GetWindowText(hWnd, sb, 1024);
			return sb.ToString();
		}
		catch (Exception ex)
		{
			int errorCode = Marshal.GetLastWin32Error();
			errorCode.Dump();
			return ex;
		}
	}
	
	public static object TryGetClassName(this IntPtr hWnd)
	{
		try
		{

			var sb = new StringBuilder();
			var rt = UserQuery.GetClassName(hWnd, sb, 1024);
			return sb.ToString();
		}
		catch (Exception ex)
		{
			int errorCode = Marshal.GetLastWin32Error();
			errorCode.Dump();
			return ex;
		}
	}
	
	public static object TryGetMainModule(this Process p)
	{
		try
		{
			return p.MainModule;
		}
		catch (Exception ex)
		{
			return ex;
			//return null;
		}
	}
}

delegate bool EnumWindowsProc(IntPtr hWnd, IntPtr lParam);

// Define other methods and classes here
[DllImport("user32.dll")]
[return: MarshalAs(UnmanagedType.Bool)]
static extern bool EnumWindows(EnumWindowsProc lpEnumFunc, IntPtr lParam);

// When you don't want the ProcessId, use this overload and pass IntPtr.Zero for the second parameter
[DllImport("user32.dll")]
static extern uint GetWindowThreadProcessId(IntPtr hWnd, out uint lpdwProcessId);

[DllImport("user32.dll", CharSet = CharSet.Auto)] //  SetLastError = true,
public static extern int GetClassName(IntPtr hWnd, StringBuilder lpClassName, int nMaxCount);

[DllImport("user32.dll", CharSet = CharSet.Auto)] //  SetLastError = true,
public static extern int GetWindowText(IntPtr hWnd, StringBuilder lpString, int nMaxCount);