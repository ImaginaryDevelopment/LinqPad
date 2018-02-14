<Query Kind="FSharpExpression">
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

// decrypt encrypted string
let decryptSecureString f (ss:System.Security.SecureString) =
    let mutable insecureString = String.Empty
    let gcHandler = GCHandle.Alloc(insecureString, GCHandleType.Pinned)
    let mutable insecureStringPtr = IntPtr.Zero
    try
        insecureStringPtr <-Marshal.SecureStringToGlobalAllocUnicode ss
        insecureString <- Marshal.PtrToStringUni insecureStringPtr
        f insecureString
    finally
        insecureString <- null
        gcHandler.Free()
        Marshal.ZeroFreeGlobalAllocUnicode insecureStringPtr
let pwd = "test123"
       
pwd
|> Seq.fold(fun (ss:System.Security.SecureString) c ->
    ss.AppendChar c
    ss
) (new System.Security.SecureString())
|> decryptSecureString (printfn "pwd?:%s")

