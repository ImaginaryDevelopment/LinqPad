<Query Kind="FSharpProgram" />

// agent setup

// download agent from subdomain 

let getUrl subdomain project =
    sprintf "https://%s.visualstudio.com/%s/_admin/_AgentQueue?queueId=1&_a=agents" subdomain project
// extract to c:\builds\agent

// run ./config.cmd from the agent folder

let createConfigCmd subdomain patToken agentName domain user password =
    sprintf ".\config.cmd --unattended --url https://%s.visualstudio.com --auth pat --token %s --pool default --agent %s --runAsService --windowsLogonAccount %s\%s --windowsLogonPassword %s"
        subdomain patToken agentName domain user password

let patToken = Util.GetPassword("patToken")

createConfigCmd "..." patToken "..." "..." "..." String.Empty
|> Dump
|> ignore