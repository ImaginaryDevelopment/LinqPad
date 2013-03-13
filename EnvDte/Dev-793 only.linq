<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE80.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE90.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 11.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v2.0\Microsoft.VisualStudio.OLE.Interop.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 11.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v4.0\Microsoft.VisualStudio.Shell.11.0.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 11.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v2.0\Microsoft.VisualStudio.Shell.Interop.10.0.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 11.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v2.0\Microsoft.VisualStudio.Shell.Interop.8.0.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 11.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v2.0\Microsoft.VisualStudio.Shell.Interop.9.0.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 11.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v2.0\Microsoft.VisualStudio.Shell.Interop.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.InteropServices.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>Microsoft.VisualStudio</Namespace>
  <Namespace>Microsoft.VisualStudio.Shell</Namespace>
  <Namespace>Microsoft.VisualStudio.Shell.Interop</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

const string SolutionFolder="{66A26720-8FB5-11D2-AA7E-00C04F688DDE}";
const string ProjectGuid="{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}";

void Main()
{
	var keeps=new []{"ProviderPortal.csproj",
		"PaySpan.Apps.Contracts.csproj",
		"PaySpan.CareFirst.Contracts.csproj",
		"PaySpan.Provider.Services.csproj",
		"PaySpan.Entities.csproj",
		"Payformance.Core.csproj",
		"Payformance.PaySpan.Contracts.csproj",
		"Payformance.PaySpan.DataAccess.csproj",
		"Payformance.PaySpan.Logging.csproj",
		"Payformance.PaySpan.Security.csproj",
		"Payformance.PaySpan.ServiceContracts.csproj",
		"Payformance.PaySpan.ServiceContracts.ObjectMapper.csproj",
		"QueryBroker.Core.csproj",
		"dtsinterface.csproj",
		"NPS.WebServices.Interface.csproj",
		"PaySpan.Hpx.Web.csproj","PaySpan.Core.csproj","PaySpan.Web.csproj"};
	bool topdown=false;
	const string SolutionExplorerWindow="{3AE79031-E1BC-11D0-8F78-00A0C9110057}";
	
	
	//EnvDTE.DTE
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE");
	Microsoft.VisualStudio.Shell.Interop.IVsUIHierarchyWindow hierarchy;
	ServiceProvider sp = new ServiceProvider((Microsoft.VisualStudio.OLE.Interop.IServiceProvider)dte);
	IVsSolution sol = (IVsSolution)sp.GetService(typeof(SVsSolution));
	//var uih=dte.ActiveWindow.Object as UIHierarchy;
	//hierarchy=(IVsUIHierarchyWindow) sp.GetService(typeof(IVsUIHierarchyWindow));
	
	foreach (ProjInfo info in GetProjectInfo(sol))
	{
    	info.Dump();
		
	}
	return;
	dte.FileName.Dump();
	dte.Solution.FullName.Dump();
	dte.Windows.Item(SolutionExplorerWindow).Activate();
	
	//Microsoft.VisualStudio.Shell.Interop.IVsUIHierarchyWindow hierarchy;
	
	SuspendResharper(dte);
	
	
	
	
	var slnName=dte.Solution.Properties.Item("Name").Value.ToString();
	slnName.Dump("sln name");
	var slnUI=uih.GetItem(slnName).Dump("uih getItem");
	var slnFolderUI=slnUI.UIHierarchyItems.Item("1. Web Sites").Dump("child ui item");
	var unloadedProject=slnFolderUI.UIHierarchyItems.Item("AdProxy").Dump("unloaded item?!?");
	unloadedProject.Select( vsUISelectionType.vsUISelectionTypeSelect);
	((object[])uih.SelectedItems).Cast<UIHierarchyItem>().Select (uihi => ComHelper.GetTypeName(uihi.Object)).Dump();
	unloadedProject.Object.Dump("unloaded item object");
	
		
}

internal sealed class ProjInfo
{
    public ProjInfo(Guid guid, string name)
    {
        Guid = guid;
        Name = name;
    }

    public Guid Guid { get; private set; }
    public string Name { get; private set; }
}

static IEnumerable<ProjInfo> GetProjectInfo(IVsSolution sol)
{
    Guid ignored = Guid.Empty;
    IEnumHierarchies hierEnum;
    if (ErrorHandler.Failed(sol.GetProjectEnum((int)__VSENUMPROJFLAGS.EPF_ALLPROJECTS, ref ignored, out hierEnum)))
    {
        yield break;
    }

    IVsHierarchy[] hier = new IVsHierarchy[1];
    uint fetched;
    while ((hierEnum.Next((uint)hier.Length, hier, out fetched) == VSConstants.S_OK) && (fetched == hier.Length))
    {
        int res = (int)VSConstants.S_OK;
		
        Guid projGuid;
        if (ErrorHandler.Failed(res = sol.GetGuidOfProject(hier[0], out projGuid)))
        {
            Debug.Fail(String.Format("IVsolution::GetGuidOfProject returned 0x{0:X}.", res));
            continue;
        }
		
        string uniqueName;
        if (ErrorHandler.Failed(res = sol.GetUniqueNameOfProject(hier[0], out uniqueName)))
        {
            Debug.Fail(String.Format("IVsolution::GetUniqueNameOfProject returned 0x{0:X}.", res));
            continue;
        }
		if( System.IO.Path.GetInvalidPathChars().Any (p =>uniqueName.Contains(p) ))
		{
			uniqueName.Dump("invalid filename found");
			yield return new ProjInfo(projGuid,uniqueName);
		} 
		else {
        	yield return new ProjInfo(projGuid, Path.GetFileName(uniqueName).BeforeOrSelf("{"));
		}
    }
}

public void SuspendResharper(DTE dte)
{
	try
	{	        
		dte.ExecuteCommand("ReSharper_Suspend");
	}
	catch (COMException ex)
	{
		ex.Dump("Failed to suspend resharper");
	}
}

public object GetDisplay(UIHierarchyItem item)
{

	object display;
	var itemObject=item.Object;
	var comType=ComHelper.GetTypeName(itemObject);
	display=new{ComType=comType,Display=item.ToString()};
			if(comType=="Project")
			{
				var p = item.Object as Project;
				
				if(p!=null)
					display=new{p.Kind, p.UniqueName,p.FileName,p.FullName,};
				
				
			} else if(comType=="_UIHierarchyItemMarshaler")
			{
				if(object.ReferenceEquals(item.Object,item))
					return display;
				var uihi=(UIHierarchyItem) item.Object;
				ComHelper.GetTypeName(uihi.Object).Dump("Found child of uihi");
			} else {
				comType.Dump("Failed to unload");
			}
	return display;
}

//http://weblogs.asp.net/soever/archive/2007/02/20/enumerating-projects-in-a-visual-studio-solution.aspx
public IEnumerable<Project> GetProjects(Project projParent){
	if(projParent.Kind == ProjectGuid)
	{
		yield return projParent;
		new{ projParent.UniqueName, projParent.FullName, projParent.FileName}.Dump("parent found");
	} else if(projParent.Kind != SolutionFolder)
	{
		new{ projParent.UniqueName, projParent.FullName, projParent.FileName,projParent.Kind}.Dump("odd kind found");
	} else {
		projParent.UniqueName.Dump("searching solution folder");
		foreach(ProjectItem projItem in projParent.ProjectItems)
		{
			
			var childProject=projItem.SubProject as Project;
			//childProject = childProject ?? projItem as Project;
			if(childProject !=null)
			{
				foreach(var child in GetProjects(childProject))
				{
					new{ child.UniqueName, child.FullName, child.FileName}.Dump("child found");
					//childProject.FullName.Dump("child found");
					yield return child;
			
				}
			} else {
				
				if(projItem.SubProject ==null)
				{
					//maybe unloaded projects fall here?
					projItem.Kind.Dump("subproject null");
				} else {
					
					ComHelper.GetTypeName( projItem.SubProject).Dump("projectItem unhandled");
				}
			}
		}
		
	}
}

    public class ComHelper 
    { 
        /// <summary> 
        /// Returns a string value representing the type name of the specified COM object. 
        /// </summary> 
        /// <param name="comObj">A COM object the type name of which to return.</param> 
        /// <returns>A string containing the type name.</returns> 
        public static string GetTypeName(object comObj) 
        { 
 
            if (comObj == null) 
                return String.Empty; 
 
            if (!Marshal.IsComObject(comObj)) 
                //The specified object is not a COM object 
                return String.Empty; 
 
            IDispatch dispatch = comObj as IDispatch; 
            if (dispatch == null) 
                //The specified COM object doesn't support getting type information 
                return String.Empty; 
 
            System.Runtime.InteropServices.ComTypes.ITypeInfo typeInfo = null; 
            try 
            { 
                try 
                { 
                    // obtain the ITypeInfo interface from the object 
                    dispatch.GetTypeInfo(0, 0, out typeInfo); 
                } 
                catch (Exception ex) 
                { 
				ex.Dump();
                    //Cannot get the ITypeInfo interface for the specified COM object 
                    return String.Empty; 
                } 
 
                string typeName = ""; 
                string documentation, helpFile; 
                int helpContext = -1; 
 
                try 
                { 
                    //retrieves the documentation string for the specified type description 
                    typeInfo.GetDocumentation(-1, out typeName, out documentation, 
                        out helpContext, out helpFile); 
                } 
                catch (Exception ex) 
                { 
                    // Cannot extract ITypeInfo information 
                    return String.Empty; 
                } 
                return typeName; 
            } 
            catch (Exception ex) 
            { 
                // Unexpected error 
                return String.Empty; 
            } 
            finally 
            { 
                if (typeInfo != null) Marshal.ReleaseComObject(typeInfo); 
            } 
        } 
    } 
 
    /// <summary> 
    /// Exposes objects, methods and properties to programming tools and other 
    /// applications that support Automation. 
    /// </summary> 
    [ComImport()] 
    [Guid("00020400-0000-0000-C000-000000000046")] 
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)] 
    interface IDispatch 
    { 
        [PreserveSig] 
        int GetTypeInfoCount(out int Count); 
 
        [PreserveSig] 
        int GetTypeInfo( 
            [MarshalAs(UnmanagedType.U4)] int iTInfo, 
            [MarshalAs(UnmanagedType.U4)] int lcid, 
            out System.Runtime.InteropServices.ComTypes.ITypeInfo typeInfo); 
 
        [PreserveSig] 
        int GetIDsOfNames( 
            ref Guid riid, 
            [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPWStr)] 
            string[] rgsNames, 
            int cNames, 
            int lcid, 
            [MarshalAs(UnmanagedType.LPArray)] int[] rgDispId); 
 
        [PreserveSig] 
        int Invoke( 
            int dispIdMember, 
            ref Guid riid, 
            uint lcid, 
            ushort wFlags, 
            ref System.Runtime.InteropServices.ComTypes.DISPPARAMS pDispParams, 
            out object pVarResult, 
            ref System.Runtime.InteropServices.ComTypes.EXCEPINFO pExcepInfo, 
            IntPtr[] pArgErr); 
    }