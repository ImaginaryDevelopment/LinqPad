<Query Kind="FSharpProgram" />

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
// c:\windows\System32\Drivers\etc\hosts
let uri = """http://localhost:16280/dynamicoffers/trialpay-test?extMemberId=STEPHAN-20140902&subid=STEPHAN-20140902-1&extCurrencyName=Interstellar%20Kredits&extCurrencyRate=1000&offeredRevenue=1&offeredQuotaGoup=352100&age=4&gender=2&postalCode=32256&iso2=US"""
type QueryParam = 
	|RequiredString of string
	|String of string option
	|Number of int option
		
let formatIt host port dynamicName (session:string option) (fallbackQgId:int) (age:int option) (gender:int option) (postalCode:string option) (iso2:string option) = 
	let currency = "Interstellar Kredits"
	//let expr = <@ (session) @>
	//match expr with | PropertyGet(a,b,c) -> b.Name.Dump("session") | b -> failwithf "%A" b
	let url = sprintf "http://%s:%i/dynamicoffers/trialpay-test" host port
	let queryParams: (string*string) list = [ 
		yield "extMemberId",dynamicName
		yield "subid",dynamicName + "-" + if session.IsSome then session.Value else "1"
		yield "extCurrencyName",currency
		yield "extCurrencyRate","1000"
		yield "offeredRevenue","1"
		yield "offeredQuotaGoup",fallbackQgId.ToString()
		if age.IsSome then yield "age",age.Value.ToString()
		if gender.IsSome then yield "gender", gender.Value.ToString()
		if postalCode.IsSome then yield "postalCode", postalCode.Value
		if iso2.IsSome then yield "iso2", iso2.Value
		]
	let encode x = System.Net.WebUtility.UrlEncode(x)
	let queryString = queryParams |> Seq.map (fun (e,x) -> sprintf "%s=%s" e (encode x)) |> fun e-> String.Join("&",e)
	url + "?" + queryString
let formatted = formatIt "localhost" 16280 ("STEPHAN-"+DateTime.Now.ToString("yyyyMMdd")) (Some "1")352100 (Some 4) (Some 2) (Some "32256") (Some "US")
formatted.Dump()
Uri(formatted).Dump()
LINQPad.Hyperlinq(formatted).Dump()