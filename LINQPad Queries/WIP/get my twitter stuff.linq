<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

// connect to twitter, save all my liked/favorited tweets in case the owner deletes their account or the tweet.
// desired feature: unshorten any urls found
// JSON.net note: if deserializing into T casing is ignored, if deserializing by property name it is not
open System.Globalization
open System.Net
open System.Net.Http
open Newtonsoft.Json

let isDebug = false
let savePathOpt = 
    Util.GetPassword("saveMachineNamesCommaDelimited")
    |> String.split [","]
    |> Seq.exists ((=) Environment.MachineName)
    |> function 
        | false -> None
        | true -> Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
    |> Option.map (fun myDocs ->
        if not <| Directory.Exists myDocs then
            failwithf "My Documents did not exist"
        let targetPath = Path.Combine(myDocs, "Twitter exports")
        if not <| Directory.Exists targetPath then
            Directory.CreateDirectory targetPath
        targetPath
    )    
            

[<AutoOpen>]
module Helpers =
    let getLine i (s:string) = s.Split (["\r\n";"\n"] |> Array.ofList,StringSplitOptions.None) |> fun items -> items.[i - 1]
    let getNear i (s:string) = 
        let start = Math.Max(i - 20, 0)
        let e = Math.Min(s.Length, i + 20) - start
        try
            s |> String.subString2 start e
        with ex ->
            s.Dump(sprintf "Length=%i. String.subString2(start=%i;e=%i)" s.Length start e)
            reraise()
    
    let toDo = ResizeArray<unit -> unit>()
        
    let (|Matched|_|) (m:Match)= if m.Success then Some m else None
    let flip f x y = f y x
    
    let containsIAnyOf (delimiters:string seq) (x:string) = delimiters |> Seq.exists(flip containsI x)
    
    let dumpt (t:string) x = x.Dump(t); x
    let dumpBlack (t:string) blacklist x = 
        let blacklist = blacklist |> delimit ", "
        x.Dump(description=t,exclude=blacklist)
    let dumpc (t:string) (x : _ list) = x.Length.Dump(t); x
    let dumpLaterOrDebug (t:string) onDemand x = 
        match isDebug,onDemand with
        | true,true -> Util.OnDemand(t, fun () -> x).Dump()
        | false,true -> toDo.Add(fun () -> Util.OnDemand(t,fun () -> x |> dumpt t |> ignore).Dump() |> ignore)
        | true,false
        | false,false -> toDo.Add(fun () -> x |> dumpt t |> ignore)
        
        x
        
    let containsI (delimiter:string) (x:string) = if isNull x then false elif isNull delimiter || delimiter = "" then failwithf "bad contains call" else x.IndexOf(delimiter, String.defaultComparison) >= 0
    
    let stringEqualsI s1 (toMatch:string)= not <| isNull toMatch && toMatch.Equals(s1, StringComparison.InvariantCultureIgnoreCase)
                    
    let getType x = x.GetType()
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
            getValueOpt None (o.GetType() |> Some) o
            
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
        
    let deserializePartial propName s = Newtonsoft.Json.Linq.JObject.Parse(s).[propName]    
    let deserializeT<'T> x = JsonConvert.DeserializeObject<'T> x
    // use prettifyJsonObj
    //let serialize x = JsonConvert.SerializeObject(x, Formatting.PrettyPrint)
    let tryDeserializeT<'T> x = 
        try
            deserializeT<'T> x
        with ex ->
            match Regex.Match(ex.Message, "line (\d+), position (\d+)") with
            | Matched m ->
                x
                |> getLine (int m.Groups.[1].Value)
                |> getNear (int m.Groups.[2].Value)
                |> dumpt "context"
                |> ignore
            | _ -> ()
            prettifyJson x
            |> dumpt "Failed to deserialize"
            |> ignore
            reraise()    
            
    module Array =
        let ofOne x = [| x |]
    module Seq = 
        let any x = x |> Seq.exists(fun _ -> true)        
    let buildParam name value = sprintf "%s=%s" name value
    
    let addParams query queryParams = 
        match queryParams |> Seq.any with
        | true ->
            queryParams
            |> delimit "&"
            |> sprintf "%s?%s" query
        | false -> query
    
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
                    
    ()
()
// for OAuth isntead of this app only thing see: https://dev.twitter.com/oauth/reference/post/oauth2/token



type Cache =
    |LinqPad of key:string // Util.Cache(... key)
    |AppDomain of key:String // AppDomain.CurrentDomain.SetData
    |TempKeyed of key:string // so the file can be found from run to run // get the temp folder and use the key to make the filename
    
type DataLifecycle = //could be named cache, but one of the entries is to save it to somewhere
    |Caching of Cache
    |JsonFile of path:string // save to some file location
    
let cacheBearer:Cache option = Cache.LinqPad "bearerData" |> Some
// from https://apps.twitter.com/
let consumerKey = Util.GetPassword "TwitterApiKey"
let consumerSecret = Util.GetPassword "TwitterSecret"

if consumerKey.StartsWith(" ") then 
    if isDebug then printfn "Key %s" consumerKey
    failwith "Copy paste failed on consumer key"
if consumerSecret.StartsWith(" ") then 
    if isDebug then printfn "Secret %s" consumerSecret
    failwith "Copy paste failed on consumer secret"
    
let baseUrl = "https://api.twitter.com/"

module Bearer = 
    type BearerTokenResponse = {token_type:string; access_token:string;errors:IDictionary<string,obj>[]}
    type Bearer =
        | Token of string
        | Error of IDictionary<string,obj>[]
        
    let sampleToken = """{"token_type":"bearer","access_token":"somebearerToken"}"""
    let sampleError = """{"errors":[{"code":99,"message":"Unable to verify your credentials","label":"authenticity_token_error"}]}"""
    let getBearer caching = 
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
        let bearerData = 
            match caching with
            | Some (Cache.LinqPad key) -> Util.Cache(getRawText,key)
            | Some (Cache.AppDomain key) -> 
                match AppDomain.CurrentDomain.GetData key with
                | null ->
                    let data = getRawText()
                    AppDomain.CurrentDomain.SetData(key, data)
                    data
                | x -> x :?> string
            | Some (TempKeyed key) ->
                let tmp=
                    let tp = Path.GetTempPath()
                    Path.Combine(tp, sprintf "%s.json" key)
                
                match File.Exists tmp with
                | true -> File.ReadAllText tmp |> deserializeT 
                | false -> 
                    let data = getRawText()
                    data |> prettifyJsonObj |> fun data -> File.WriteAllText(tmp,data)
                    data
            | None -> getRawText()
        let bearerData = 
            JsonConvert.DeserializeObject<BearerTokenResponse>(bearerData)
            //|> dumpt "response"
        if isNull bearerData.errors || bearerData.errors |> Seq.length = 0 then
            if bearerData.token_type <> "bearer" then Console.Error.WriteLine(sprintf "Warning: token type was '%s', expected '%s'" bearerData.token_type "bearer")
            Bearer.Token bearerData.access_token
        else
            Bearer.Error bearerData.errors
        

open Bearer    

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

type User = { Name:string; profile_image_url:string;Id:Int64 Nullable; screen_name:string; location:string; description:string; url:string;}
type Media = { Id:Int64 Nullable; Media_Url:string;Expanded_Url:string; Type:string; Display_Url:string}
type Url = {Url:string; Expanded_url:string; Display_url:string}
type Entity = { Media: Media[]; Urls: Url[]}
type Tweet = {(* Coordinates:obj; *) Id:Int64 Nullable; Truncated:bool Nullable ; Favorited:bool Nullable; Created_at:string; Id_str:string; In_reply_to_user_id_str:Int64 Nullable; Text:string; User:User; Entities:Entity}
    with static member getText (x:Tweet) = x.Text
type SearchMetadata = {Query:string;RefreshUrl:string}
type SearchResult = {Statuses:Tweet list; search_metadata:SearchMetadata}
type UserIdentifier = |UserId of int |ScreenName of string
let getIntParamOpt name (vOpt:int option) = match vOpt with | Some i -> sprintf "%s=%i" name i |> Some | None -> None
let getStrParamOpt name (vOpt:string option) = match vOpt with | Some v -> sprintf "%s=%s" name v |> Some | None -> None
let getBoolParamOpt name vOpt = 
    vOpt
    |> Option.map (function |true -> "true" | false -> "false")
    |> Option.map (sprintf "%s=%s" name)

let getApiFromParams useCache title uri p =
    let p = p |> Seq.choose id |> List.ofSeq
    let getRaw() =
        let rawOutput =
            if Seq.any p then
                p |> delimit "&" |> sprintf "%s?%s" uri
            else uri
            |> getApi
        rawOutput
    if useCache then Util.Cache(getRaw, title) else getRaw()
        
// https://dev.twitter.com/rest/reference/get/favorites/list
let getFavorites useCache userIdOpt screenNameOpt countOpt since_idOpt max_idOpt _include_entities =
    let url = "1.1/favorites/list.json"
    let rawOutput = 
        [
            getIntParamOpt "user_id" userIdOpt
            getStrParamOpt "screen_name" screenNameOpt
            getIntParamOpt "count" countOpt
            getIntParamOpt "since_id" since_idOpt
            getIntParamOpt "max_id" max_idOpt
        ]    
        |> getApiFromParams useCache "favoritesRaw" url
        //|> dumpt "Raw favorites output"
        
    let items =
        rawOutput
        |> fun t -> JsonConvert.DeserializeObject<obj[]>(t)
    let prettyPrintHead() = 
        items
        |> Seq.head 
        |> prettifyJsonObj
        |> dumpLaterOrDebug "first favorite" true
        |> ignore
    if isDebug then // turning this on while I work out the format of a single favorite object
        prettyPrintHead()
    let favoriteResults = 
        items
        |> Seq.map JsonConvert.SerializeObject
        |> Seq.map (tryDeserializeT<Tweet>)
        |> List.ofSeq
        |> dumpLaterOrDebug "Deserialized" true
    prettyPrintHead()
    favoriteResults
    
//https://dev.twitter.com/rest/reference/get/users/show
let getUsersShow useCache userIdentifierOpt includeEntities =
    let url = "1.1/users/show.json"
    [
                match userIdentifierOpt with
                | UserId uId -> 
                    yield getIntParamOpt "user_id" (Some uId)
                | ScreenName sn ->
                    yield getStrParamOpt "screen_name" (Some sn)
                yield getBoolParamOpt "include_entities" includeEntities
    ]
    |> getApiFromParams useCache "usersShowRaw" url
    |> fun x -> JsonConvert.DeserializeObject<User> x
    //|> JsonConvert.DeserializeObject<obj[]>
    
let searchUserTweets useCache userId countOpt =  
// appears to require more than a bearer token
    let url = 
        sprintf "from:%s" userId
        |> Uri.EscapeDataString
        // currently leaving result_type off results in no results
        |> sprintf "1.1/search/tweets.json?q=%s&result_type=mixed"
    [
        getIntParamOpt "count" countOpt
    ]
    |> getApiFromParams useCache "searchTweets" url
    //|> dumpt "searchTweetsRaw"
    |>  fun raw -> // for data exploring/debug to look at the fields not in the tweets section
        let o = JsonConvert.DeserializeObject(raw) :?> Newtonsoft.Json.Linq.JObject
        o.Property("statuses").Remove()
        o |> string |> dumpt "without statuses" |> ignore
        raw
    |> tryDeserializeT<SearchResult>
    |> fun sr -> sr.Statuses
    
module NeedMoreAuth =
    // requires more than just a bearer token    
    let getMyRetweets useCache countOpt trimUser : Tweet list =
        // this was for getting retweets of a tweet, not a users' retweets 
        //let url = sprintf "1.1/statuses/retweets/%s.json" (userId |> string)
        //this was for searching for tweets
        //let url = sprintf "1.1/search/tweets.json?q=from:%s" userId
        // https://dev.twitter.com/rest/reference/get/statuses/home_timeline
        let url = sprintf "1.1/statuses/home_timeline.json"
        [
            //getStrParamOpt "user_id" (Some userId)
            getIntParamOpt "count" countOpt
            getBoolParamOpt "trim_user" trimUser
        ]
        |> getApiFromParams useCache "retweetsRaw" url
        |> tryDeserializeT<obj[]>
        |> Seq.map (string >> tryDeserializeT<Tweet>)
        |> List.ofSeq

let cacheResult = true
let countOpt = Some 200 // Some 2
let userId = "maslowjax"
// save to file, appending new unique results if one already exists
let saveAppendish savePathOpt key (data:Tweet list) = 
    match savePathOpt with
    | None -> ()
    | Some savePath ->
        let oldData = if File.exists savePath then File.ReadAllText savePath |> deserializeT<Tweet list> else List.empty
        // replace ones that match in case the read algorithm has improved/changed?
try
    let userInfo = 
        getUsersShow cacheResult (UserIdentifier.ScreenName userId) None
//    |> dumpt "getUsersShow"
//    |> ignore
    let myTweets = 
        searchUserTweets cacheResult userId countOpt
        |> dumpc "search tweets"
        //|> dumpt "search tweets results"
    getFavorites cacheResult None (Some userId) countOpt None None false
    |> dumpc "favorites results"
    |> saveAppendish savePathOpt "twitterFavorites"
    |> Seq.append myTweets
    |> Seq.filter(Tweet.getText >> containsIAnyOf ["parenthesis"; "semicolon" ; "java";"finally"] )
    |> dumpBlack "main results" ["Id_str";"Id";"id"]
    |> ignore
finally
    toDo
    |> Seq.iter(fun f -> f())