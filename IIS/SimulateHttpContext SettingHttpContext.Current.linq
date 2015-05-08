<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Framework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.DataAnnotations.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Caching.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.ApplicationServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.Services.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Utilities.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.Protocols.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.EnterpriseServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Design.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Tasks.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ServiceProcess.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.RegularExpressions.dll</Reference>
  <Namespace>System.Web</Namespace>
  <Namespace>System.Web.Hosting</Namespace>
</Query>

void Main()
{
	SetHttpContextWithSimulatedRequest("localhost","app");
	HttpContext.Current.Dump("simulated");
}

// http://haacked.com/archive/2005/06/11/simulating_httpcontext.aspx/
/// <summary>
/// Used to simulate an HttpRequest.
/// </summary>
public class SimulatedHttpRequest : SimpleWorkerRequest
{
    string _host;
 
    /// <summary>
    /// Creates a new <see cref="SimulatedHttpRequest"/> instance.
    /// </summary>
    /// <param name="appVirtualDir">App virtual dir.</param>
    /// <param name="appPhysicalDir">App physical dir.</param>
    /// <param name="page">Page.</param>
    /// <param name="query">Query.</param>
    /// <param name="output">Output.</param>
    /// <param name="host">Host.</param>
    public SimulatedHttpRequest(string appVirtualDir, string appPhysicalDir, string page, string query, TextWriter output, string host) : base(appVirtualDir, appPhysicalDir, page, query, output)
    {
        if(host == null || host.Length == 0)
            throw new ArgumentNullException("host", "Host cannot be null nor empty.");
        _host = host;
    }
 
    /// <summary>
    /// Gets the name of the server.
    /// </summary>
    /// <returns></returns>
    public override string GetServerName()
    {
        return _host;
    }
 
    /// <summary>
    /// Maps the path to a filesystem path.
    /// </summary>
    /// <param name="virtualPath">Virtual path.</param>
    /// <returns></returns>
    public override string MapPath(string virtualPath)
    {
        return Path.Combine(this.GetAppPath(), virtualPath);
    }
}
// Define other methods and classes here
public static void SetHttpContextWithSimulatedRequest(string host, string application)
{
    string appVirtualDir = "/";
    string appPhysicalDir = @"c:\projects\SubtextSystem\Subtext.Web\";
    string page = application.Replace("/", string.Empty) + "/default.aspx";
    string query = string.Empty;
    TextWriter output = null;
 
    SimulatedHttpRequest workerRequest = new SimulatedHttpRequest(appVirtualDir, appPhysicalDir, page, query, output, host);
    HttpContext.Current = new HttpContext(workerRequest);
 
    Console.WriteLine("Request.FilePath: " + HttpContext.Current.Request.FilePath);
    Console.WriteLine("Request.Path: " + HttpContext.Current.Request.Path);
    Console.WriteLine("Request.RawUrl: " + HttpContext.Current.Request.RawUrl);
    Console.WriteLine("Request.Url: " + HttpContext.Current.Request.Url);
    Console.WriteLine("Request.ApplicationPath: " + HttpContext.Current.Request.ApplicationPath);
    Console.WriteLine("Request.PhysicalPath: " + HttpContext.Current.Request.PhysicalPath);
}