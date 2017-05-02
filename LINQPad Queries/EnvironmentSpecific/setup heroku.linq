<Query Kind="FSharpProgram" />

// heroku tutorial mapping
// assumes heroku is installed.
// expects you have created the director you want the heroku app to live in, and have set it on the next line
let projectPath = @"c:\projects\helloheroku\linqpadHeroku"

Environment.CurrentDirectory <- projectPath
// heroku login
// not sure how to automate/script this part
Process.Start("cmd", "/k heroku login")

Util.Cmd("node -v",quiet = true) |> delimit Environment.NewLine |> printfn "Node version:%s"
Util.Cmd("npm -v", quiet = true) |> delimit Environment.NewLine |> printfn "Npm version:%s"
Util.Cmd("git --version", quiet = true) |> delimit Environment.NewLine |> printfn "git version:%s"

printfn "you can clone app source code in now, or it could have been done before the previous steps"

if(Directory.Exists(".git") |> not) then
    Util.ReadLine("I'll just wait here for your to confirm your code is in the directory and ready to go",String.Empty) |> ignore

