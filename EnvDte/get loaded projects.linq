<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE80.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.InteropServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

void Main()
{
	bool topdown=false;
	
	const string SolutionFolder="{66A26720-8FB5-11D2-AA7E-00C04F688DDE}";
	const string ProjectGuid="{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}";
	//EnvDTE.DTE
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.12.0");
	dte.FileName.Dump();
	dte.Solution.FullName.Dump();
	
	var dte2=(EnvDTE80.DTE2)dte;
	
	var uih= GetSolutionExplorerSln(dte2);
	
	
	
	var solutionItem=uih.UIHierarchyItems.Cast<UIHierarchyItem>().Single ();
	solutionItem.UIHierarchyItems.Cast<UIHierarchyItem>().Select(a=>GetDisplay(a)).Dump("sln");
	ComHelper.GetTypeName(solutionItem).Dump("solutionItem Type");
	//does not traverse solution folders?
	var q= from uiItem in GetChildItems(solutionItem,topdown)
		let project = uiItem.Object as Project
		where project !=null && project.Kind == ProjectGuid
		select new{project.FullName,project.FileName,project.UniqueName};
	
	var items=q.ToArray();
	if(items.Any()==false){
		"No items found".Dump();
		return;
	}
	var fileNames=("var fileNames=new []{"+items
		.Select (l => '"'+l.FileName+'"')
		.Delimit(",")+"};").Dump();
	var uniqueNames=("var uniqueNames=new []{"+items.Select (l => '"'+l.UniqueName+'"').Delimit(",")+"};").Dump();
//System.Windows.Forms.Clipboard.SetText(statement);

}

public static UIHierarchy GetSolutionExplorerSln(EnvDTE80.DTE2 dte2){
	var slnExplorer=(UIHierarchy)dte2.ToolWindows.SolutionExplorer;
	return slnExplorer;
}

[Obsolete()]
public static UIHierarchy GetSolutionExplorerSln( EnvDTE.DTE dte){
	const string SolutionExplorerWindow="{3AE79031-E1BC-11D0-8F78-00A0C9110057}";
	dte.Windows.Item(SolutionExplorerWindow).Activate();
	var uih=dte.ActiveWindow.Object as UIHierarchy;
	return uih;
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
			} else if(comType=="ProjectItem"){
				var pi = item.Object as ProjectItem;
				if(pi!=null)
					display= new {pi.Kind,UniqueName=(string)null,FileName= pi.FileNames[0],FullName= pi.FileNames[0]};
			} else if (comType=="Solution4"){
				var sln = item.Object as Solution;
				if(sln!=null)
					display= new {Kind=comType, UniqueName=(string) null, FileName= sln.FileName, FullName= sln.FullName};
			}else {
				comType.Dump("Failed to read type");
			}
	return display;
}
public IEnumerable<UIHierarchyItem> GetChildItems(UIHierarchyItem parent,bool topdown)
{
	
	if(parent.Collection.Count>0)
		foreach(var item in parent.UIHierarchyItems.Cast<UIHierarchyItem>())
		{//.Where (uihi => uihi.Object is Project))
			if(topdown)
				yield return item;
			foreach(var ic in GetChildItems(item,topdown))
				yield return ic;
			if(!topdown)
				yield return item;
		}
	yield break;
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
					ex.Dump("failed to read typeInfo");
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
					ex.Dump("failed to get documentation");
                    // Cannot extract ITypeInfo information 
                    return String.Empty; 
                } 
                return typeName; 
            } 
            catch (Exception ex) 
            { 
                // Unexpected error 
				ex.Dump("unexpected error");
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