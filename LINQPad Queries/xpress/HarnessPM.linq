<Query Kind="Program">
  
  <Reference>&lt;RuntimeDirectory&gt;\Accessibility.dll</Reference>
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\bin\Pm.Dal.dll</Reference>
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\bin\Pm.Schema.dll</Reference>
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\bin\PracticeManagement.exe</Reference>
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\bin\PracticeManagement.Foundation.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationUI.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\ReachFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Deployment.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\System.Printing.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\UIAutomationProvider.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\UIAutomationTypes.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Namespace>System.Windows</Namespace>
  <Namespace>System.Windows.Automation.Peers</Namespace>
</Query>

// wip can't seem to load PracticeManagement.exe
void Main()
{
    var debug = false;
    var targetAssembly = typeof(PracticeManagement.MainWindow).Assembly;
    var currentAssembly = this.GetType().Assembly;

    Application.ResourceAssembly = targetAssembly;
    if (debug)
        Application.ResourceAssembly.Dump("resource assembly");

    var rd = (ResourceDictionary)Application.LoadComponent(new Uri("/PracticeManagement;component/CommonUIStylesDictionary.xaml", UriKind.Relative));
    if (debug)
        rd.Dump("rdloaded!");

    if (Application.Current == null)
    {
        var x = new System.Windows.Application(); // magically is assigned to application.current behind the scenes it seems
        if (debug)
            x.Dump("application");
    }

    Application.Current.Dump("app current");
    Application.Current.Resources = rd;
    Application.Current.Resources.Dump("resources");
    // works but doesn't do what we want in this case:
    //var loginPopup = Application.LoadComponent(new Uri("/PracticeManagement;component/login/LoginPopup.xaml", UriKind.Relative));
    //var mw = Application.LoadComponent(new Uri("/PracticeManagement;component/MainWindow.xaml", UriKind.Relative));
    //System.Windows.Application.LoadComponent(new Uri("PracticeManagement.exe", UriKind.RelativeOrAbsolute));

    var mainWindow = new PracticeManagement.MainWindow(this.Connection.ConnectionString);
    if(debug)
        mainWindow.GetType().Assembly.GetManifestResourceNames().Dump();
    mainWindow.Show();

    // peer section doesn't work, the rest did
    var peer = UIElementAutomationPeer.FromElement(mainWindow);
    peer.Dump("peer?");
}

// Define other methods and classes here
public class PMControlAutomationPeer : FrameworkElementAutomationPeer
{
    public PMControlAutomationPeer(FrameworkElement owner) : base(owner) { }
    protected override string GetNameCore() => "MainWindow";
    protected override AutomationControlType GetAutomationControlTypeCore() => AutomationControlType.Window;
    protected override List<AutomationPeer> GetChildrenCore()
    {
        var peers = base.GetChildrenCore();
        var peer = UIElementAutomationPeer.CreatePeerForElement((Window)base.Owner);
        if (peer != null) peers.Add(peer);
        return peers;
    }

}