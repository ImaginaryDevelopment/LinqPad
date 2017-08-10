<Query Kind="FSharpProgram" />

// model machine setup
// consider adding already installed detection
// consider adding menu to select installs to run
let install cmd args = 
    Util.Cmd(cmd,args=args)
module Helpers =
    let dumpLinq uriOrPath = 
        Hyperlinq(uriOrPath=uriOrPath)
        |> Dump
        |> ignore
    ()
open Helpers

// https://www.linqpad.net/licensing/ListActivations.aspx
module Chocolatey = 
    type ChocoInstalls = 
        | Choco
        | Chrome
        | Linqpad
        | Git
        | GitKraken
        | VisualStudioCode
        | KDiff3
    let install = 
        let chocoInstall args= install "choco" (sprintf "-y %s" args)
        function
        | Choco -> // requires admin, as do some of the other installs (maybe only if c:\programData\ isn't created yet)
            install @"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" """-NoProfile -InputFormat None -ExecutionPolicy Bypass -Command "iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" && SET "PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin" """
        | x ->
            
            let result = chocoInstall  (sprintf "%A" x)
            match x with
            | LinqPad ->
                printfn "setup license 7/2/09, setup custom my queries and extensions folder, dark theme, db connection(s)"
                dumpLinq "https://www.linqpad.net/licensing/ListActivations.aspx"
            | Git -> printfn "setup SSH key, if not letting Kraken handle\r\nsetup push url git remote set-url --push origin git@github.com:ImaginaryDevelopment/LinqPad.git"
            | GitKraken -> printfn "setup SSH key if not done"
            | VisualStudioCode -> printfn "install vim"
            | _ -> ()
            result
            // print extra steps
    let getChocoPage =
        let getChocoLink relPath = Hyperlinq(sprintf "https://chocolatey.org/%s" relPath)
        function
        | Choco -> String.Empty
        
        | x -> sprintf "packages/%A" x
        >> getChocoLink

Chocolatey.getChocoPage Chocolatey.GitKraken
|> Dump
|> ignore

// other steps

printfn """install visual studio 2017, update
    install vs2017's vsvim 
    install vsCode's vim (settings -> vim.useCtrlKeys : false)
    install justdecompile or something similar
    """