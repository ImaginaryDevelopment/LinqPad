<Query Kind="FSharpProgram" />

// unshorten urls
open System.Net
let urls = [
    "http://goo.gl/zdf2n"
    "http://tinyurl.com/8xc9vca"
    "http://x.co/iEup"
    "http://is.gd/vTOlz6"
    "http://bit.ly/FUA4YU"
]
type unshortenResult = 
    | Success of status:HttpStatusCode*url:string 
    | Failure of HttpStatusCode
    | FailureStatus of WebExceptionStatus
    
let unshorten (url:string) = // with help from http://stackoverflow.com/a/9761793/57883
    let req = WebRequest.Create(url) :?> HttpWebRequest
    req.AllowAutoRedirect <- false
    try 
        use resp = req.GetResponse()
        use hResp = resp :?> HttpWebResponse
        match hResp.StatusCode with
        | HttpStatusCode.MovedPermanently // they should all be returning this code if they are shortened (also seems to return this to redirect to the same url in https)
        | _ -> 
            let realUrl = resp.Headers.["Location"]
            Success (hResp.StatusCode,realUrl) // unless it redirects to another shortened link
    with :? WebException as e ->
        match e.Status = WebExceptionStatus.ProtocolError with
        | true ->
            use resp = e.Response 
            use hResp = resp :?> HttpWebResponse
            Failure hResp.StatusCode   
        | false -> 
            FailureStatus e.Status
        
    
urls
|> Seq.map (fun x -> x,unshorten x)
|> Dump
