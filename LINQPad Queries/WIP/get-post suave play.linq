<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

open System.Net.Http
open System.Net

module Helpers = 
    let delimit (d:string) (x:string seq) =
        String.Join(d, x)
    let before (d:string) (x:string) = 
        x.Before(d)
    let after (d:string) (x:string) =
        x.After(d)
        
    module Option =
        let getOrDefault y= 
            function
            | Some x -> x
            | None -> y
    let dumpt t x = 
        x.Dump(description=t)
        x
open Helpers

module Net = // https://stackoverflow.com/questions/4015324/how-to-make-http-post-web-request
    let auth = "Authorization"
    let encodeAuth (u:string) (p:string) = 
        sprintf "%s:%s" u p
        |> Encoding.ASCII.GetBytes
        |> Convert.ToBase64String
        |> sprintf "Basic %s"
    let toUrlParams =
        Seq.map (|KeyValue|)
        >> Seq.map (fun (n,v) -> sprintf "%s=%s" n v) 
        >> delimit "&"
    let getResponseText (hResp:HttpWebResponse) =
        use rStream = hResp.GetResponseStream()
        use r = new StreamReader(rStream,true)
        r.ReadToEnd()
        
        
    let tryWeb (url:string) contentOpt fReq =
        let contentData = 
            contentOpt
            |> Option.map toUrlParams
        let req = WebRequest.Create(url) :?> HttpWebRequest
        fReq req
        contentData
        |> Option.iter(Encoding.ASCII.GetBytes >> fun data ->
            req.ContentType <- "application/x-www-form-urlencoded"
            req.ContentLength <- int64 data.Length
            try
                use stream = req.GetRequestStream()
                stream.Write(data, 0, data.Length)
            with ex ->
                ex.Dump()
        )
        
        
        try
            use resp = req.GetResponse()
            use hResp = resp :?> HttpWebResponse
            // should we assert the response is a 200 OK?
            let isGood = int hResp.StatusCode = 200
            let output = (hResp.StatusCode,getResponseText hResp, hResp.Cookies)
            if isGood then Choice1Of3 output
            else Choice2Of3 output
            
        with :? WebException as e ->
            use resp = e.Response
            use hResp = resp :?> HttpWebResponse
            let rt = 
                try
                    getResponseText hResp
                    |> Some
                with ex ->
                    ex.Dump("getResponseText fail")
                    None
            printfn "in WebEx %A %A" (isNull e) (isNull e.Response)
            resp.Headers.Dump()
            Choice3Of3(hResp.StatusCode |> int, hResp.StatusCode, e.Status, rt |> Option.getOrDefault null)
           
    let tryWithAuth u p (url:string) contentOpt fReq  =
        tryWeb url contentOpt (fun req ->
            req.Headers.Add(auth,encodeAuth u p)
            fReq req
        )
        
    let tryPostWithBasicAuth u p (url:string) fReq dic =
        tryWithAuth u p url (Some dic) (fun req ->
            req.Method <- "POST"
            fReq req
        )
    let tryPostWithWindowsAuth d u p url fReq dic = 
        dic.Dump("content sans d u p")
        let content = dic |> Seq.map (|KeyValue|) |> List.ofSeq |> (@) ["domain",d;"username",u;"password",p] |> dict
        tryWeb url (Some content) (fun req ->
            req.Method <- "POST"
            fReq req
        )

open Net
let winLogonKey = "winlogonPwd"
let d,u,p = Environment.UserDomainName, Environment.UserName, Util.GetPassword(winLogonKey)
let getter,inserter, updater, login = 
    let ``base`` = "http://localhost:8080/"
    sprintf "%sresult/client/55/participant/45440" ``base``,
    sprintf "%sresult" ``base``,
    sprintf "%sresult/%i" ``base``,
    sprintf "%sauthentication/login" ``base``

    
let fReq (req:HttpWebRequest)=
    req.Accept <- "application/json"

tryWithAuth u p getter None fReq
|> dumpt "Auth result"
|> ignore

dict["HTInches_Total", "72"; "WT", "160";"EventId","22647"]
|> tryPostWithWindowsAuth d u p inserter ignore 
|> Dump
|> ignore
//Util.SetPassword(winLogonKey, null)