<Query Kind="FSharpProgram" />

open System.Net.Http
open System.Security.Cryptography

let client = new HttpClient()
type SSOPost = {
    rcopia_portal_system_name:string
    rcopia_practice_user_name:string
    rcopia_user_id:string
//    rcopia_user_external_id:string
    service:string
    action:string
    time:string
//    MAC:string
    // start non-required
//    startup_screen:string
//    rcopia_patient_id:string
//    rcopia_patient_system_name:string
//    limp_mode:string
}
let defaultPost = {
    rcopia_portal_system_name="xvendor"
    rcopia_practice_user_name="xp9898"
    rcopia_user_id="xprovider"
//    rcopia_user_external_id=null
    service="rcopia"
    action="login"
    time=DateTime.UtcNow.ToString("MMddyyhhmmss")
//    MAC="1234"
//    startup_screen=
//    limp_mode="n"
}
defaultPost.Dump()
let readParams dp =
    dp.GetType().GetProperties()
    |> Seq.map(fun prop ->
        let v = prop.GetValue(dp) :?> string
        sprintf "%s=%s" prop.Name v
    )
    |> String.concat "&"
let md5 (paramString:string)  =
    let md5Crypto = new MD5CryptoServiceProvider()
    let data = Encoding.UTF8.GetBytes(paramString);
    let hasheddata = md5Crypto.ComputeHash(data);
    BitConverter.ToString(hasheddata).Replace("-", "").ToUpper();
let createfullUrl key url =
    let keyed = sprintf "%s%s" url key
    let mac = md5 keyed
    sprintf "%s&MAC=%s" keyed mac
    
    
let result =
    let key = Util.GetPassword("drfirst_secret_key")
    let url:string =readParams defaultPost |> sprintf "https://web4.staging.drfirst.com/sso/portalServices?%s" |> createfullUrl key
    printfn "Url is %s" url
    client.PostAsync(url, content=null)
    |> Async.AwaitTask
    |> Async.RunSynchronously
    
result.StatusCode.Dump()
result.Content.ReadAsStringAsync()
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> fun x -> x.Dump("content")
result.Content.Headers.Dump("headers")
result.Headers.Dump("headers2")