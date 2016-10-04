<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

// connect to twitter, save all my liked/favorited tweets in case the owner deletes their account or the tweet.
// unimplmented: save 
// desired feature: unshorten any urls found
open System.Globalization
open System.Net
open System.Net.Http
open Newtonsoft.Json

// for OAuth isntead of this app only thing see: https://dev.twitter.com/oauth/reference/post/oauth2/token

let isDebug = false
let consumerKey = Util.GetPassword "TwitterApiKey"
let consumerSecret = 
    //Util.SetPassword("TwitterSecret",null); 
    Util.GetPassword "TwitterSecret"

if consumerKey.StartsWith(" ") then 
    if isDebug then printfn "Key %s" consumerKey
    failwith "Copy paste failed on consumer key"
if consumerSecret.StartsWith(" ") then 
    if isDebug then printfn "Secret %s" consumerSecret
    failwith "Copy paste failed on consumer secret"
let dumpt (t:string) x = x.Dump(t); x
let getType x = x.GetType()
let getGtProp (gtOpt:Type option) = gtOpt |> Option.map (fun gt -> gt.GetProperty "Value")
let rec getValueOpt (genTypeOpt:Type option) (typeOpt:Type option)  (o:obj) = 
    match o,genTypeOpt, typeOpt with
    | null, _, _ -> None
    | _ , Some gt ,_  -> 
        // based on http://stackoverflow.com/a/13367848/57883
        match gt.GetProperty "Value" with
        | null -> None
        | prop ->
            let v = prop.GetValue(o,null)
            Some v
    | _, _,Some t -> 
        match t.IsGenericType with
        | true -> getValueOpt typeOpt (t.GetGenericTypeDefinition() |> Some) o
        | false -> Some o
    | _, _, None ->
        getValueOpt None (o |> getType |> Some) o
        
let (|NullableValue|NullableNull|) (nv: _ Nullable) = match nv.HasValue with | true -> NullableValue nv.Value | false -> NullableNull

// Nullish covers actual null, NullableNull, and None
let (|Nullish|NullableObj|SomeObj|GenericObj|NonNullObj|) (o:obj) =
    // consider including empty string in nullish?
    Debug.Assert(Nullable<int>() |> box |> isNull)
    Debug.Assert(None |> box |> isNull)
    match isNull o with
    | true -> Nullish
    | false -> 
        let t = o |> getType
        // a more direct translation would have been t |> Nullable.GetUnderlyingType|> isNull |> not
        match t.IsGenericType with
        | false -> NonNullObj
        | true ->
            let genericType = t.GetGenericTypeDefinition()
            if genericType = typedefof<Nullable<_>> then
                NullableObj genericType
            elif genericType = typedefof<Option<_>> then
                SomeObj genericType
            else GenericObj genericType
        
[<AutoOpen>]
module StringHelpers = 
    let toLowerInvariant (s:string) = s.ToLowerInvariant()
    let format fmt ([<ParamArray>] arr) = 
        String.Format(fmt, arr )

    let contains d (s:string) = s.Contains(d)
    let endsWith d (s:string) = s.EndsWith d
    let endsWithC (c:char) (s:string) = s |> endsWith (c |> string)
    let getIndexOf (d:string) (s:string) = match s.IndexOf d with | -1 -> None | x -> Some x
    let prettifyJsonObj o = JsonConvert.SerializeObject(o, Formatting.Indented)
    let prettifyJson s = s |> JsonConvert.DeserializeObject |> prettifyJsonObj
    let delimit delimiter (values:#seq<string>) = String.Join(delimiter, Array.ofSeq values)
    
module Array =
    let ofOne x = [| x |]
module Seq = 
    let any x = x |> Seq.exists(fun _ -> true)
// failed to write this: kept on compiling as ints only
//module Option =
//    let add<'t when 't (member: (+))> (x:'t) (vOpt: 't option) = 
//        match vOpt with
//        | Some v -> x + v
//        | None -> x
let buildParam name value = 
    sprintf "%s=%s" name value
    
let addParams query queryParams = 
    match queryParams |> Seq.any with
    | true ->
        queryParams
        |> delimit "&"
        |> sprintf "%s?%s" query
    | false -> query
    
let baseUrl = "https://api.twitter.com/"
module Bearer = 

    type BearerTokenResponse = {token_type:string; access_token:string;errors:IDictionary<string,obj>[]}
    type Bearer =
        | Token of string
        | Error of IDictionary<string,obj>[]
        
    let sampleToken = """{"token_type":"bearer","access_token":"somebearerToken"}"""
    let sampleError = """{"errors":[{"code":99,"message":"Unable to verify your credentials","label":"authenticity_token_error"}]}"""
    let getBearer useCache = 
        // also didn't work: 
        //let encodedPair = sprintf "userName=%s&password=%s" consumerKey consumerSecret |> Encoding.UTF8.GetBytes |> Convert.ToBase64String
        let getRawText () = 
            if isDebug then 
                printfn "Key %s" consumerKey
                printfn "Secret %s" consumerSecret
            let encodedPair = sprintf "%s:%s" consumerKey consumerSecret |> dumpt "before encoding" |> Encoding.UTF8.GetBytes |> Convert.ToBase64String
            encodedPair.Dump("encoded pair")
            use client = new HttpClient()
            client.Timeout.Dump("default timeout")
            use content = new StringContent("grant_type=client_credentials")
            let url = sprintf "%s%s" baseUrl "oauth2/token"
            url.Dump("url")
            use req = new HttpRequestMessage(HttpMethod.Post, url, Content= content)
            if isNull req then failwithf "Request was null"
            if isNull req.Content then failwithf "Content was null"
            
            req.Content.Headers.ContentType <- System.Net.Http.Headers.MediaTypeWithQualityHeaderValue("application/x-www-form-urlencoded",CharSet = "UTF-8")
            
            if not <| req.Headers.TryAddWithoutValidation("Authorization", sprintf "Basic %s" encodedPair) then failwithf "adding auth header failed"
            let response = client.SendAsync(req).Result
            let bearerData = response.Content.ReadAsStringAsync().Result
            bearerData.Dump("raw response")
            bearerData
        let bearerData = if useCache then Util.Cache(Func<_>(getRawText)) else getRawText()
        let bearerData = 
            JsonConvert.DeserializeObject<BearerTokenResponse>(bearerData)
            //|> dumpt "response"
        if isNull bearerData.errors || bearerData.errors |> Seq.length = 0 then
            if bearerData.token_type <> "bearer" then Console.Error.WriteLine(sprintf "Warning: token type was '%s', expected '%s'" bearerData.token_type "bearer")
            Bearer.Token bearerData.access_token
        else
            Bearer.Error bearerData.errors
        
//    ()
open Bearer    

let cacheBearer = true

let bearer = 
    match getBearer cacheBearer with
    | Bearer.Token t -> t
    | Bearer.Error errors -> errors.Dump("bearer errors"); failwith "bearer errors"


let getApi apiPath = 
    printfn "Getting %s" apiPath
    //let sampleFailure = """{"request":"\/1.1\/statuses\/user_timeline.json","error":"Not authorized."}"""
    use req = new HttpRequestMessage(HttpMethod.Get, baseUrl + apiPath)
    printfn "using bearer %s" bearer
    if not <| req.Headers.TryAddWithoutValidation("Authorization", sprintf "Bearer %s" bearer) then failwith "validation failed to add to request"
    use client = new HttpClient()
    let response = client.SendAsync(req).Result
    response.Content.ReadAsStringAsync().Result

let runExampleGet() = 
    getApi "1.1/statuses/user_timeline.json?count=2&screen_name=twitterapi"
    |> dumpt "api call success?"  
// type AppRateLimitStatus = {rate_limit_context: string; resources: AppRateLimitStatusResource list}
let getAppRateLimitStatus() = 
    getApi "1.1/application/rate_limit_status.json"
    |> dumpt "Raw app rate limit status"
    |> prettifyJson 
    |> dumpt "rate limit status"
    
// https://dev.twitter.com/rest/reference/get/favorites/list
type User = { name:string; profile_image_url:string;id:Int64 Nullable; screen_name:string; location:string; description:string; url:string;}
type Media = { Id:Int64 Nullable; Media_Url:string;Expanded_Url:string; Type:string; Display_Url:string}
type Url = {Url:string; Expanded_url:string; Display_url:string}
type Entity = { Media: Media[]; Urls: Url[]}
type FavoriteResult = {
    Coordinates:string 
    Id:Int64 Nullable 
    Truncated:bool Nullable
    Favorited:bool Nullable
    Created_at:string
    Id_str:string
    In_reply_to_user_id_str:Int64 Nullable
    Text:string
    User:User
    Entities:Entity}

let getFavorites useCache userIdOpt screenNameOpt countOpt since_idOpt max_idOpt _include_entities =
    let getFavoritesRaw () = 
        let url = "1.1/favorites/list.json"
        
        let getIntParamOpt name (vOpt:int option) = match vOpt with | Some i -> sprintf "%s=%i" name i |> Some | None -> None
        let getStrParamOpt name (vOpt:string option) = match vOpt with | Some v -> sprintf "%s=%s" name v |> Some | None -> None
        let paramsToAdd = 
            [
                getIntParamOpt "user_id" userIdOpt
                getStrParamOpt "screen_name" screenNameOpt
                getIntParamOpt "count" countOpt
                getIntParamOpt "since_id" since_idOpt
                getIntParamOpt "max_id" max_idOpt
            ]
            |> Seq.choose id
            |> List.ofSeq
        let rawOutput = 
            if Seq.any paramsToAdd then
                paramsToAdd |> delimit "&" |> sprintf "%s?%s" url
            else url
            |> getApi 
        rawOutput
    let rawOutput = 
        if useCache then Util.Cache(Func<_>(getFavoritesRaw),"favoritesRaw") else getFavoritesRaw()
        //|> dumpt "Raw favorites output"
    let items =
        rawOutput
        |> fun t -> JsonConvert.DeserializeObject<obj[]>(t)
    let prettyPrintHead() = 
        items
        |> Seq.head 
        |> prettifyJsonObj
        |> dumpt "first favorite"
        |> ignore
    if isDebug then // turning this on while I work out the format of a single favorite object
        prettyPrintHead()
    let favoriteResults = 
        items
        |> Seq.map JsonConvert.SerializeObject
        |> Seq.map (fun t -> JsonConvert.DeserializeObject<FavoriteResult>(t))
        |> List.ofSeq
        |> dumpt "Deserialized"
    prettyPrintHead()
    favoriteResults


module Unshortening =
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
                
let cacheResult = true
getFavorites cacheResult None (Some "maslowjax") (Some 2) None None false