<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE80.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE90.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

#define LINQPAD
void Main()
{
	const string SolutionExplorerWindow="{3AE79031-E1BC-11D0-8F78-00A0C9110057}";
	const string SolutionFolder="{66A26720-8FB5-11D2-AA7E-00C04F688DDE}";
	//EnvDTE.DTE
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE");
	dte.Dump();
	dte.FileName.Dump();
	dte.Solution.FullName.Dump();
	dte.Windows.Item(SolutionExplorerWindow).Activate();
	var uih=dte.ActiveWindow.Object as UIHierarchy;
	var failures=new List<object>();
	foreach(var solutionItem in uih.UIHierarchyItems.Cast<UIHierarchyItem>().Single ().UIHierarchyItems.Cast<UIHierarchyItem>())
	{
		solutionItem.Select(vsUISelectionType.vsUISelectionTypeSelect);
		var comType=ComHelper.GetTypeName(solutionItem.Object);
		try
		{	        
			dte.ExecuteCommand("Project.UnloadProject");
			new{Object= solutionItem.Object.ToString(),ComTypeName=comType}.Dump("unloaded");
		}
		catch (COMException ex)
		{
		object display=new{Object= solutionItem.Object.ToString(),ComTypeName=comType};
			if(comType=="Project")
			{
				var p = solutionItem.Object as Project;
				
				if(p!=null)
					display=new{p.Kind, p.UniqueName,p.FileName,p.FullName,};
				
				
			}
			failures.Add(Util.HorizontalRun(false, display,ex));
		}
	}
	if(failures.Any ( ))
			failures.Dump("failures");
	
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
					#if LINQPAD
					ex.Dump();
					#endif
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