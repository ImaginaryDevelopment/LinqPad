<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

// connect to twitter, save all my liked/favorited tweets in case the owner deletes their account or the tweet.
// WIP: nothing works yet
open System.Globalization
open System.Net.Http
open Newtonsoft.Json

//// trying to follow: https://dev.twitter.com/overview/api/tls
//let myHandle = "MaslowJax"
//let myApiPath = "https://api.twitter.com/1.1/yourpathgoeshere"

let consumerKey = Util.GetPassword("TwitterApiKey")
let consumerSecret = 
    //Util.SetPassword("TwitterSecret",null); 
    Util.GetPassword("TwitterSecret")
let cacheBearer = false
printfn "Key %s" consumerKey
printfn "Secret %s" consumerSecret
if consumerKey.StartsWith(" ") then failwith "Copy paste failed on consumer key"
if consumerSecret.StartsWith(" ") then failwith "Copy paste failed on consumer secret"
let dumpt (t:string) x = x.Dump(t); x
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
    // translation of https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Core/Core/Extensions/StringExtension.cs#L264
    // does not return query, so that strings or stringBuilder can be used
    let buildParameter query name value = 
        if String.IsNullOrEmpty name || String.IsNullOrEmpty value then
            null
        else
            let rawText = format "{0}={1}" [| name;value|]
            let result = 
                let leadingText = 
                    if query |> contains "?" |> not then 
                        "?" 
                    elif query |> endsWithC '?' |> not && query |> endsWithC '&' |> not then 
                        "&" 
                    else 
                        String.Empty
                leadingText + rawText
            result


    // translation of https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Core/Core/Extensions/StringExtension.cs#L308   
    let buildParameterForQuery<'t> query name (value:'t) =
        // valuable is for generics that you should be able to get the value of
        let (|IgnorePair|InvalidNullable|Valueable|OtherParam|) (name,value:'t) =
            // handles NullableNulls
            match String.IsNullOrEmpty name || isNull (box value) with
            | true -> IgnorePair
            | _ ->
                let v:obj = upcast value
                //match Nullable.GetUnderlyingType typeof<'t> |> isNull |> not with
                match v with
                | Nullish -> IgnorePair
                | NullableObj gt -> 
                    // no idea why this is in the source, but leaving it
                    match value |> getType |> string = (v |> string) with 
                    | true -> InvalidNullable
                    | false -> Valueable (gt,v)
                |SomeObj gt ->
                    Valueable (gt,v)
                | _ -> OtherParam v
                    
        match (name,value) with
        | IgnorePair 
        | InvalidNullable
            -> None
        | Valueable (gt,v) ->
            getValueOpt (Some gt) None v
        | OtherParam v ->
            Some v
        |> Option.map 
            (fun v -> 
                v
                |> function
                    | :? Double as v ->
                        v.ToString(CultureInfo.InvariantCulture)
                        |> buildParameter query name
                    | _ -> v |> string |> toLowerInvariant |> buildParameter query name 
            )

module Array =
    let ofOne x = [| x |]
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
            |> dumpt "response"
        if isNull bearerData.errors || bearerData.errors |> Seq.length = 0 then
            if bearerData.token_type <> "bearer" then Console.Error.WriteLine(sprintf "Warning: token type was '%s', expected '%s'" bearerData.token_type "bearer")
            Bearer.Token bearerData.access_token
        else
            Bearer.Error bearerData.errors
        
//    ()
open Bearer    
let bearer = 
    match getBearer cacheBearer with
    | Bearer.Token t -> t
    | Bearer.Error errors -> errors.Dump("bearer errors"); failwith "bearer errors"


let getApi apiPath = 
    let sampleFailure = """{"request":"\/1.1\/statuses\/user_timeline.json","error":"Not authorized."}"""
    use req = new HttpRequestMessage(HttpMethod.Get, baseUrl + apiPath)
    printfn "using bearer %s" bearer
    if not <| req.Headers.TryAddWithoutValidation("Authorization", sprintf "Bearer %s" bearer) then failwith "validation failed to add to request"
    use client = new HttpClient()
    let response = client.SendAsync(req).Result
    response.Content.ReadAsStringAsync().Result
    
getApi "1.1/statuses/user_timeline.json?count=2&screen_name=twitterapi"
|> dumpt "api call success?"
    
//
//// try to use the code exposed by the repo: https://github.com/linvi/tweetinvi/wiki/Introduction
//// DI map @ https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Core/TweetinviCoreModule.cs
////module UserQueryParameterGenerator = // https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Controllers/User/UserQueryParameterGenerator.cs
////    let 
////module User = // https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Controllers/User/UserController.cs
//    
////// ITweetController.PublishTweet
////let tweet consumerKey consumerSecret access token accessTokenSecret words =
////    let isSetupForAppAuthentication = not <| String.IsNullOrEmpty consumerKey && not <| String.IsNullOrEmpty consumerSecret // https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Core/Public/Models/Authentication/ConsumerCredentials.cs
////    let isSetupForUserAuthentication = isSetupForAppAuthentication && not <| String.IsNullOrEmpty accessToken && accessTokenSecret // https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Core/Public/Models/Authentication/TwitterCredentials.cs
////    ()
//
//type TweetMode = 
//    | Extended
//    | Compat
//    with override x.ToString() = sprintf "%A" x
//type UserIdentifier = 
//    | UserId of Int64
//    | ScreenName of string
//type THttpMethod =
//    | Get
//    | Post of HttpContent
//    | Delete
//    
//type HttpRequestParameters = {Query:string; HttpMethod: THttpMethod }
//
//type ConsumerCredentials = {ConsumerKey:string; ConsumerSecret:string}
//type TwitterCredentials = {AccessToken:string; AccessTokenSecret:string ; ConsumerCredentials: ConsumerCredentials}
//type OAuthQueryParameter = {Key:string; Value:string; RequiredForSignature:bool; RequiredForHeader:bool; IsPartOfOAuthSecretKey: bool}
//type TwitterQuery = {HttpRequestParameters:HttpRequestParameters; Proxy:string; Timeout:TimeSpan;TwitterCredentials:TwitterCredentials;QueryParameters: OAuthQueryParameter list}
//
//
//
//// Tweetinvi.WebLogic.HttpClientWebHelper
//// not currently supporting anything but get
//let getHttpResponse (twitterQuery:TwitterQuery) = 
//    use client = new HttpClient()
//    client.Timeout <- twitterQuery.Timeout
//    match twitterQuery.HttpRequestParameters.HttpMethod with
//    | Get -> 
//        use request = new HttpRequestMessage(HttpMethod.Get, twitterQuery.HttpRequestParameters.Query)
//        client.SendAsync(request).Result
//    | Post content -> client.PostAsync(twitterQuery.HttpRequestParameters.Query, content).Result
//    
////    
////    
////// Tweetinvi.WebLogic.WebRequestExecutor.cs 
////let executeQuery twitterQuery = 
////    getHttpResponse twitterQuery
////// Tweetinvi.WebLogic.TwitterRequestHandler.cs 
////let executeQuery httpMethod handlerOpt (credentials:TwitterCredentials) url = 
////    let url = 
////        match url |> getIndexOf "?" with
////        | None -> url
////        | Some index -> 
////            match url |> endsWith "?" with
////            | true -> url.TrimEnd([| '?' |])
////            | false -> 
////                if url.Length > index && url.[index + 1] = '&' then
////                    url.Remove(index + 1, 1)
////                else 
////                    url
////    //let rateLimitTrackOpt = rateLimitTrackerMode
////    // prepare request
////    // TwitterQueryFactor.Create
////    let create() = 
////        let queryUrlParam = (Uri url).AbsoluteUri // "queryURL", 
////        
////        let requestParams = {Query= queryUrlParam; HttpMethod = httpMethod}
////        printfn "Uri:%s" queryUrlParam
////        // we are assuming proxies in this case are actual proxies, not something OAuth relies on
////        let twitterQuery = {HttpRequestParameters = requestParams; Proxy = null; Timeout = TimeSpan.FromMinutes(1.); TwitterCredentials = credentials; QueryParameters = list.Empty}
////    //let credentials = defaultArg credentialsOpt currentThreadCredentials
////        ()
////    create()
////    
////    ()
////// Tweetinvi.Credentials.TwitterAccessor.cs #378
////let executeCredentialsQuery query tMethod =
////    if String.IsNullOrEmpty query then failwithf "invalid query"
////    executeQuery tMethod query
////let getUserFromIdentifier credentials = 
////    function 
////    | ScreenName s -> 
////        let url = 
////            s
////            |> box
////            |> Array.ofOne
////            |> format "https://api.twitter.com/1.1/users/show.json?user_id={0}"
////        url
////        |> executeQuery THttpMethod.Get None credentials
////        |> Convert.ToInt64
////    | UserId l ->
////        l
////    //|> ExecuteGETQuery<IUserDTO>
////// IUserController.GetFavoriteTweets
////let getUserFavorites userIdentifier = 
////    
////    // https://github.com/linvi/tweetinvi/blob/master/Examplinvi/Program.cs#L645
////    let user = getUserFromIdentifier userIdentifier
////    //https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Controllers/User/UserQueryGenerator.cs#L62
////    let getFavoriteTweetsQuery tweetModeOpt formattedCustomQueryParametersOpt = 
////        // translated via https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Controllers/Properties/Resources.cs
////        let query = StringBuilder("https://api.twitter.com/1.1/favorites/list.json?" + (user |> string)) // resources_user_getFavorites + userIdentifierParameter)
////        let inline addParameterToQuery name (value:'t) = 
////            let paramTextOpt = buildParameterForQuery (query|> string) name value
////            paramTextOpt
////            |> Option.iter (query.Append >> ignore<StringBuilder>)
////        //guessing on parameter types for now
////        addParameterToQuery "include_entities" true
////        addParameterToQuery "since_id" 0
////        addParameterToQuery "max_id" -1
////        addParameterToQuery "count" 10
////        tweetModeOpt |> Option.iter (string >> toLowerInvariant >> query.Append >> ignore<StringBuilder>)
////        formattedCustomQueryParametersOpt |> Option.iter (Array.ofOne >> format "&{0}" >> query.Append >> ignore<StringBuilder>)
////        query |> string
////    let userIdentifierParameter (userId:string) = 
////        //_userQueryParameterGenerator.GenerateIdOrScreenNameParameter(favoriteParameters.UserIdentifier);
////        [| "user_id";userId |] |> Array.map box |> format "{0}={1}"
////    let query = getFavoriteTweetsQuery None None 
////    let result = _twitterAccessor.ExecuteGETQuery<IEnumerable<ITweetDTO>>(query)
////    // involves : https://github.com/linvi/tweetinvi/blob/master/Tweetinvi.Core/Public/Parameters/GetUserFavoritesParameters.cs
////    ()