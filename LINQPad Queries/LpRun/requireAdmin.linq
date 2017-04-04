<Query Kind="FSharpProgram" />

module Sec = 
        open System.Security.Principal
        let getIsAdmin() =
            WindowsIdentity.GetCurrent()
            |> WindowsPrincipal
            |> fun wp -> wp.IsInRole(WindowsBuiltInRole.Administrator)
        let requireAdmin () = 
            let runningAsAdmin = getIsAdmin()
            if not runningAsAdmin then
                failwithf "Requested feature is not known to work without administrator permissions"
                
Sec.requireAdmin()
printfn "printfn: success!"
Console.WriteLine("console:success! admin did not throw")
Console.Error.WriteLine("console error stream: success? captured this error text?")