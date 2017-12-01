<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Net</Namespace>
</Query>

let delimit (d:string) (x:string seq) = String.Join(d, x |> Array.ofSeq)

module Option = 
    let ofObj x = 
        match x with
        | null -> None
        | x -> Some x

module Cereal = 
    open Newtonsoft.Json
    let deserialize<'T> (x:string) = 
        JsonConvert.DeserializeObject<'T>(x)
open Cereal

type RequestType = 
    | GetPostCards
    | GetTemplates
    | CreateTemplate
    | CreateLetter
    | UpdateTemplate of tmplId:string
    | DeleteTemplate of tmplId:string
let getUrl = 
    function
    | GetPostCards -> "https://api.lob.com/v1/postcards"
    | CreateTemplate
    | GetTemplates -> "https://api.lob.com/v1/templates"
    | CreateLetter -> "https://api.lob.com/v1/letters"
    | UpdateTemplate tmplId -> sprintf "https://api.lob.com/v1/templates/%s" tmplId
    | DeleteTemplate tmplId -> sprintf "https://api.lob.com/v1/templates/%s" tmplId

module Net = 
    let auth = "Authorization"
    let encodeAuth (u:string) (p:string) = 
        sprintf "%s:%s" u p
        |> Encoding.ASCII.GetBytes
        |> Convert.ToBase64String
        |> sprintf "Basic %s"
        
    //let getWithAuth u p (url:string) =
    //    use wc = new System.Net.WebClient()
    //    let encodedAuth = encodeAuth u p
    //    wc.Headers.Add(auth, encodedAuth)
    //    wc.DownloadString url
    
    let tryGetWithAuth u p (url:string) =
        let req = WebRequest.Create(url) :?> HttpWebRequest
        req.Headers.Add(auth,encodeAuth u p)
        try
            use resp = req.GetResponse()
            use hResp = resp :?> HttpWebResponse
            use rStream = hResp.GetResponseStream()
            use r = new StreamReader(rStream,true)
            let rawResults =
                let toDump = r.ReadToEnd()
                toDump
            Choice1Of2 (hResp.StatusCode,rawResults) // unless it redirects to another shortened link
        with :? WebException as e ->
            use resp = e.Response 
            use hResp = resp :?> HttpWebResponse
            Choice2Of2(hResp.StatusCode, e.Status)
            
    type UriAddress =
        | Uri of System.Uri
        | Raw of String
        
    let inline preparePost (formParams:IDictionary<string,string>) = 
        formParams |> Seq.map (|KeyValue|) |> Seq.map (fun (k,v) -> sprintf "%s=%s" k (System.Uri.EscapeDataString v)) |> delimit "&"
    let createReq =
        function 
        | Uri x -> WebRequest.Create x, x.ToString()
        | Raw s -> WebRequest.Create s, s
    let addAuth (req:WebRequest) u p = req.Headers.Add(auth,encodeAuth u p) 
    let getResp (req:WebRequest) =
        let resp = 
            match req.GetResponse() with
            | :? HttpWebResponse as x -> x
            | x -> failwithf "response type %s is unexpected" (x.GetType().Name)
        (int resp.StatusCode, resp.StatusCode, resp.StatusDescription)
        |> Dump
        |> ignore
        let content = 
            use stream = resp.GetResponseStream()
            use reader = new StreamReader(stream, Encoding.UTF8)
            reader.ReadToEnd()
        match int resp.StatusCode with
        | 200 -> content
        | _ -> 
            resp.Headers.Dump("headers")
            content.Dump("Failed content")
            resp.Cookies.Dump("cookies?!?")
            failwith "Unexpected response" 
    let inline deleteIt url u p = 
        let req, url = createReq url
        addAuth req u p
        req.Method <- "DELETE"
        getResp req
        
    let inline postIt url u p (formParams:IDictionary<string,string>) = 
        // https://stackoverflow.com/questions/930807/login-to-website-via-c-sharp
        let formParams = preparePost formParams
        let req,url = createReq url
        req.ContentType <- "application/x-www-form-urlencoded"
        req.Headers.Add(auth,encodeAuth u p) 
        req.Method <- "POST"
        let bytes = Encoding.ASCII.GetBytes(formParams)
        
        req.ContentLength <- int64 bytes.Length
        printfn "Posting to %s" url
        (
            use os = req.GetRequestStream()
            os.Write(bytes, 0, bytes.Length)
            os.Flush()
        )

        getResp req

open Net


let u = 
    Util.GetPassword "lobTestKey"
    //Util.GetPassword "lobLiveKey"
module Templates = 
    let postIt url = postIt url u String.Empty
    let deleteIt url = deleteIt url u String.Empty
    
    type CreatedTemplate = {Id:string;Description:string;Date_Created:DateTime; Date_Modified:DateTime;Html:string}
    type Template = {Id:string; Description:string}
    type TemplateList = {Data: Template[]; Count:int}
    let createTemplate description html : CreatedTemplate = 
        Map [
            "description", description
            "html",html
        ]
        |> postIt (getUrl RequestType.CreateTemplate |> Raw)
        |> deserialize
        |> fun x -> {x with Html = html}

    // WIP, not working
    let updateTemplate tmplId description html = 
        Map [
            if not <| String.IsNullOrWhiteSpace description then
                yield "description", description
            yield "html",html
        ]
        |> postIt (getUrl (RequestType.UpdateTemplate tmplId) |> Raw)
        
    let getTemplates () : Template list = 
        tryGetWithAuth u String.Empty (getUrl RequestType.GetTemplates)
        |> function
            |Choice1Of2 (_,content) -> 
                content
                |> deserialize<TemplateList>
                |> fun x -> x.Data |> List.ofArray
            | Choice2Of2 x ->
                x.Dump()
                failwith "Bad get response"

    let deleteTemplate tmplId =
        deleteIt (getUrl (RequestType.DeleteTemplate tmplId) |> Raw)

    // based on https://github.com/lob/examples/blob/master/letters/8.5x11-letter.html
    let html = """<html>
<head>
<meta charset="UTF-8">
<link href="https://fonts.googleapis.com/css?family=Open+Sans" rel="stylesheet" type="text/css">
<title>Lob.com Sample 8.5x11 Letter</title>
<style>
  *, *:before, *:after {
    -webkit-box-sizing: border-box;
    -moz-box-sizing: border-box;
    box-sizing: border-box;
  }
  body {
    width: 8.5in;
    height: 11in;
    margin: 0;
    padding: 0;
  }
  .page {
    page-break-after: always;
    position: relative;
    width: 8.5in;
    height: 11in;
  }
  .page-content {
    position: absolute;
    width: 8.125in;
    height: 10.625in;
    left: 0.1875in;
    top: 0.1875in;
    font-size: .11in;
    font-style: normal;
    font-family: "Courier New", Courier, "Lucida Sans Typewriter", "Lucida Typewriter", monospace;
    background-color: rgba(0,0,0,0.2);
  }
  .text {
    position: relative;
    left: 20px;
    top: 20px;
    width: 6in;
    font-family: 'Open Sans';
//    font-size: 30px;
  }
  .letterText{
    position: relative;
    left: 20px;
    top: 20px;
  }
  #return-address-window {
    position: absolute;
    left: .625in;
    top: .5in;
    width: 3.25in;
    height: .875in;
    background-color: rgba(255,0,0,0.5);
  }
  #return-address-text {
    position: absolute;
    left: .07in;
    top: .34in;
    width: 2.05in;
    height: .44in;
    background-color: white;
    font-size: .11in;
  }
  #return-logo {
    position: absolute;
    left: .07in;
    top: .02in;
    width: 2.05in;
    height: .3in;
    background-color: white;
  }
  #recipient-address-window {
    position: absolute;
    left: .625in;
    top: 1.75in;
    width: 4in;
    height: 1in;
    background-color: rgba(255,0,0,0.5);
  }
  #recipient-address-text {
    position: absolute;
    left: .07in;
    top: .05in;
    width: 2.92in;
    height: .9in;
    background-color: white;
  }
  .title {
    font-size:20px;
  }
//  .rowText{
//    font-size: 10px;
//  }
  .rowLabel{
    font-weight:bold;
  }
  .tleft{
    text-align:left;
  }
  .tright{
    text-align:right;
  }
  .tcenter{
    text-align:center;
  }
  .bordered{
    border: solid 1px black;
    border-style=solid;
    border-width=1px;
    border-color=black;
  }
  .tunderline{
    text-decoration:underline;
  }
  .tupper{
    text-transform: uppercase;
  }
  .tpascal{
    text-transform: capitalize;
  }
  table {
    font-size: .11in;
    table-layout:fixed;
    min-width: 7in;
    min-height: 6.5in;
  }
  tr > td:nth-child(4){
    text-align:right;
  }
  tr > td:nth-child(5){
    text-align:right;
  }
</style>
</head>
<body>
  <div class="page">
    <div style="float:left;margin-top:.25in;margin-left:.625in">
        <div>{{facility_name}}</div>
    </div>
    <div style="float:right;margin-top:.25in;margin-right:.25in">
        <div id="title">STATEMENT</div>
        <div class="rowText"><span class="rowLabel">Tax ID:</span> {{tax_id}}</div>
        <div class="rowText"><span class="rowLabel">Patient:</span> {{patient_name}}</div>
        <div class="rowText"><span class="rowLabel">Statement #:</span> {{statement_number}}</div>
        <div class="rowText">{{date}}</div>
    </div>
    <div class="page-content" style="margin-right:.25in">
      <div style="top:2.8in;position:relative">
        <table>
            <thead>
            <tr>
                <th class="tleft">Code</th>
                <th class="tleft">DOS</th>
                <th class="tleft">Provider/Note</th>
                <th class="tleft">Payment</th>
                <th class="tleft">Charge</th>
            </tr>
            <tr><td colspan="5">------------------------------------------------------------------------------------------------------</td></tr>
            </thead>
            <tbody>
                <tr>
                    <td width="1in"></td>
                    <td width="1.5in"></td>
                    <td width="3in"></td>
                    <td width="1in"></td>
                    <td width="1in"></td>
                </tr>
                {{rows}}
                <tr><td /><td /><td colspan="2">*** For Questions Call {{question_phone}} ***</td><td /></tr>
                <tr><td colspan="5">Note: {{note}}</td></tr>
                <tr><td colspan="5">------------------------------------------------------------------------------------------------------</td></tr>
                <tr><td colspan="2">COLUMN TOTALS</td><td /><td>{{payment}}</td><td class="tright">{{charge}}</td></tr>
                <tr><td colspan="5">------------------------------------------------------------------------------------------------------</td></tr>
                <tr><td /><td /><td class="tcenter">** PAY THIS AMOUNT **</td><td/></tr>
                <tr><td colspan="4" class="tcenter" /><td class="tupper">Balance Due</td></tr>
                <tr><td colspan="4" class="tcenter" /><td class="bordered tright">{{balance_due}}</td>
                <tr><td colspan="2" class="tunderline">Pay By Credit Card</td>
                    <td colspan="2"><span class="bordered">&nbsp;&nbsp;&nbsp;</span> American Express &nbsp;&nbsp;&nbsp;<span class="bordered">&nbsp;&nbsp;&nbsp;</span>&nbsp;Master Card &nbsp;&nbsp;<span class="bordered">&nbsp;&nbsp;&nbsp;</span> &nbsp;Visa</td>
                </tr>
                <tr><td colspan="2">Card Number:</td><td colspan="2">________ ________ ________ ________</td><td>Enclosed Amount</td></tr>
                <tr><td colspan="2">Expiration:</td><td>____ / ________ Card Code:________</td><td /><td rowspan="2" class="bordered"></td></tr>
                <tr />
                <tr><td colspan="2">Signature:</td><td>________________________________________________</td><td /><td>Post Account</td></tr>
                <tr><td colspan="4" /><td>{{xpm}}</td></tr>
            </tbody>
        </table>
      </div>
    </div>
    <div id="return-address-window">
      <div id="return-address-text">
        
      </div>
    </div>
    <div id="recipient-address-window">
      <div id="recipient-address-text">
      </div>
    </div>
  </div>
</body>
</html>"""


let createTemplate() = 
    Templates.createTemplate "ConversionStarted" Templates.html
    
let deleteTemplate tmplId = 
    Templates.deleteTemplate tmplId
    
module Letters = 
    type Address = {Line1:string; Line2:string; City:string; State:string; Zip:string}
    type MailLocation = {Id:string; Description:string; Name:string; Company:string; Phone:string; Email:string; Address:Address;}
    type AddresseeType = 
        | From
        | To
        
    type LetterType =
        //| Raw of string // needs special url encoding, let's not deal with this special case here
        | FromTemplate of string
    type LetterCreator = {Description:string option; To:MailLocation; From:MailLocation; LetterType:LetterType; Color:bool; MergeVariables: IDictionary<string,string>; MetaData:IDictionary<string,string>}
    let createLetterPost lc = 
        let toKeys (title:string) (x:IDictionary<string,string>) : (string*string) seq= 
            x
            |> Seq.map (|KeyValue|)
            |> Seq.map(fun (k,v) -> sprintf "%s[%s]" title k, v)
            
        let toAddrKeys t name a =
            let x = 
                match t with
                | To -> "to"
                | From -> "from"
            seq{
                yield (sprintf "%s[name]" x,name)
                yield (sprintf "%s[address_line1]" x, a.Line1)
                if not <| String.IsNullOrWhiteSpace a.Line2 then
                    yield (sprintf "%s[address_line2]" x, a.Line2)
                yield (sprintf "%s[address_city]" x, a.City)
                yield (sprintf "%s[address_state]" x, a.State)
                yield (sprintf "%s[address_zip]" x, a.Zip)
                yield (sprintf "%s[address_country]" x, "US")
            }
        Map [
            match lc.Description |> Option.bind Option.ofObj with
            | None -> ()
            | Some d -> yield ("description", d)
            yield! toAddrKeys To lc.To.Name lc.To.Address
            yield! toAddrKeys From lc.From.Name lc.From.Address
            match lc.LetterType with
            //|Raw html -> html
            | FromTemplate templId -> yield "file",templId
            if lc.MergeVariables.Count > 0 then
                yield! toKeys "merge_variables" lc.MergeVariables
            yield "color", if lc.Color then "true" else "false"
        ]
    let createLetter lc = 
        createLetterPost lc
        |> postIt (getUrl RequestType.CreateLetter |> UriAddress.Raw) u String.Empty 

        
open Letters

let xpress = {  MailLocation.Id="xpress"; Description = "XpressTechnologies office"; Name="Xpress Technologies"; Company="XpressTechnologies"; Phone="904-296-1189"; Email="brandond@xpte.com"
                Address={
                        Line1="6622 Southpoint Drive South, Suite 370"
                        Line2=null
                        City="Jacksonville"
                        State="FL"
                        Zip="32216"
                }
            }
            
let formatNoDollarCurrency (x:decimal) =
    sprintf "%.2f" x
let formatMdy (x:DateTime) = 
    x.ToString("MM/dd/yyyy")
    
type AccountLine = {Code:string; DOS:DateTime option;Provider_Note:string;Payment:decimal option;Charge:decimal option} with 
    member x.FormattedDOS = 
        match x.DOS with
        | Some dos -> formatMdy dos
        | None -> String.Empty
    member x.FormattedCharge = 
        match x.Charge with
        | None -> String.Empty
        | Some charge -> formatNoDollarCurrency charge
    member x.FormattedPayment = 
        match x.Payment with
        | None -> String.Empty
        | Some payment -> formatNoDollarCurrency payment

let makeLetter tmplId =
        
    let makeAccountRow x = 
        sprintf "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>" x.Code x.FormattedDOS x.Provider_Note x.FormattedPayment x.FormattedCharge
    let rows = 
        [   {Code="A0012305";DOS=Some <| DateTime(2016,5,18);Provider_Note="06/22/16 - BADANOWSKI, RALPH"; Payment=None; Charge=Some 59m}
            {Code="99213";DOS=Some <| DateTime(2016,7,8);Provider_Note="07/21/16 - BADANOWSKI, RALPH"; Payment=None; Charge=Some 50m}
            {Code=null;DOS=None;Provider_Note="11/11/16"; Payment=None; Charge=Some 1m}
            {Code=null;DOS=None;Provider_Note="11/11/16"; Payment=None; Charge=Some 0.23m}
            {Code=null;DOS=Some <| DateTime(2016,5,16);Provider_Note="02/27/17 - BADANOWSKI, RALPH"; Payment=None; Charge=Some 7.85m}
        ]
    let rowText = 
        rows
        |> Seq.map makeAccountRow
        |> delimit "\r\n"
    let mergeVars = 
        [
            "facility_name","myMachine2(R)"
            "tax_id", String.Empty
            "patient_name", "D'IMPERIO, CONNIE"
            "statement_number", "XS 213"
            "date", DateTime.Now.ToShortDateString()
            "balance_due", "$118.08"
            "payment", "0"
            "question_phone", "(904)867-5309"
            "charge", "118.08"
            "xpm", "XPM213"
            "note", String.Empty
            "rows", rowText
        ] |> List.map(function | (k,v) when String.IsNullOrWhiteSpace v -> k, "&nbsp;" | (k,v) -> k,v)
    {   Description = Some "Test letter 1"; To = xpress; From = xpress
        LetterType = FromTemplate tmplId
        Color = false
        MergeVariables = Map mergeVars
        MetaData = Map[
                    "testId","1"
        ]
    }

let createLetter tmplId = 
    makeLetter tmplId
    |> createLetter

let renderHtml html = 
    let path = sprintf "%s.htm" (Path.GetTempFileName())
    File.WriteAllText(path,html)
    Process.Start(path)
    |> ignore


(* did not work
//Templates.updateTemplate tmplId null Templates.html
//|> Dump
//|> ignore
*)

Templates.getTemplates()
|> Dump
|> Seq.filter(fun t -> t.Description = "ConversionStarted")
|> Seq.map(fun t ->
    deleteTemplate t.Id
)
|> List.ofSeq
|> Dump
|> ignore

let exampleTmplId = "tmpl_353f61315d79181"

let tmpl = createTemplate()
createLetter tmpl.Id
|> Dump
|> ignore