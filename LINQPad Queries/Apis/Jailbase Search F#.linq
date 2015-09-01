<Query Kind="FSharpProgram" />

open System.Net.Http
let dump c =
	c.Dump()
	c
let sample = """
	{"status":1,
	"next_page":2,
	"records":
		[
			{"name":"COREY S HAYES",
			"charges":["VIOLATION OF PROBATION OR COMMUNITY CONTROL - MISDEMEANOR OFFENSE (Warrant Case)"],
			"id":"/fl-jso/2015-02-01/corey-scott-hayes-2015002942",
			"book_date_formatted":"Feb 01, 2015",
			"details":[
				["Gender","M"],
				["Race","W"],
				["Height","5 10"],
				["Weight","240"],
				["Eyes","GREEN"],
				["Hair","BLONDE/STRAWBERRY"],
				["Facility","PDF"],
				["Notes","Age: 26"],
				["Ref #","2015002942"]
				],
			"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif",
			"book_date":"2015-02-01",
			"jail":
				{"city":"Jacksonville",
				"name":"Jacksonville Sheriff's Office",
				"url":"http://www.jailbase.com/en/sources/fl-jso/",
				"address1":"500 E Adams St",
				"address2":"",
				"state":"FL"},
			"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-02-01/corey-scott-hayes-2015002942"
		},{"name":"AUSTIN J BROWN","charges":["PETIT THEFT - RETAIL - LESS THAN $100"],"id":"/fl-jso/2015-02-01/austin-joseph-brown-2015002944","book_date_formatted":"Feb 01, 2015","details":[["Gender","M"],["Race","W"],["Height","5 08"],["Weight","159"],["Eyes","HAZEL"],["Hair","BROWN"],["Facility","PDF"],["Notes","Age: 28"],["Ref #","2015002944"]],"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif","book_date":"2015-02-01","jail":{"city":"Jacksonville","name":"Jacksonville Sheriff's Office","url":"http://www.jailbase.com/en/sources/fl-jso/","address1":"500 E Adams St","address2":"","state":"FL"},"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-02-01/austin-joseph-brown-2015002944"},{"name":"NICHOLAS J DAVIS","charges":["TRESPASS ON PROPERTY OTHER THAN STRUCTURE OR CONVEY./NOTICE GIVEN 0R FENCED"],"id":"/fl-jso/2015-02-01/nicholas-jermaine-davis-2015002945","book_date_formatted":"Feb 01, 2015","details":[["Gender","M"],["Race","B"],["Height","6 01"],["Weight","145"],["Eyes","BROWN"],["Hair","BLACK"],["Facility","PDF"],["Notes","Age: 28"],["Ref #","2015002945"]],"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif","book_date":"2015-02-01","jail":{"city":"Jacksonville","name":"Jacksonville Sheriff's Office","url":"http://www.jailbase.com/en/sources/fl-jso/","address1":"500 E Adams St","address2":"","state":"FL"},"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-02-01/nicholas-jermaine-davis-2015002945"},{"name":"MICHAEL P MURPHY","charges":["DUI - UNDER INFLUENCE OF ALCOHOL OR CHEMICAL SUBSTANCE; FACULTIES IMPAIRED"],"id":"/fl-jso/2015-02-01/michael-patrick-murphy-2015002946","book_date_formatted":"Feb 01, 2015","details":[["Gender","M"],["Race","W"],["Height","5 11"],["Weight","190"],["Eyes","BLUE"],["Hair","BROWN"],["Facility","PDF"],["Notes","Age: 24"],["Ref #","2015002946"]],"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif","book_date":"2015-02-01","jail":{"city":"Jacksonville","name":"Jacksonville Sheriff's Office","url":"http://www.jailbase.com/en/sources/fl-jso/","address1":"500 E Adams St","address2":"","state":"FL"},"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-02-01/michael-patrick-murphy-2015002946"},{"name":"ROBLERO A HUGO","charges":["NO DRIVER LICENSE - NEVER HAD ONE ISSUED"],"id":"/fl-jso/2015-01-31/roblero-arreaga-hugo-2015002937","book_date_formatted":"Jan 31, 2015","details":[["Gender","M"],["Race","W"],["Height","5 06"],["Weight","135"],["Eyes","BROWN"],["Hair","BLACK"],["Facility","PDF"],["Notes","Age: 29"],["Ref #","2015002937"]],"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif","book_date":"2015-01-31","jail":{"city":"Jacksonville","name":"Jacksonville Sheriff's Office","url":"http://www.jailbase.com/en/sources/fl-jso/","address1":"500 E Adams St","address2":"","state":"FL"},"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-01-31/roblero-arreaga-hugo-2015002937"},{"name":"FREDERIC SEJOUR","charges":["KNOWINGLY OPERATE VEH WHILE DL SUSPEND/CANCEL/REVOKED - 1ST OFFENSE","DUI - UNDER INFLUENCE OF ALCOHOL OR CHEMICAL SUBSTANCE; FACULTIES IMPAIRED"],"id":"/fl-jso/2015-01-31/frederic-sejour-2015002883","book_date_formatted":"Jan 31, 2015","details":[["Gender","M"],["Race","B"],["Height","5 09"],["Weight","141"],["Eyes","BROWN"],["Hair","BLACK"],["Facility","PDF"],["Notes","Age: 24"],["Ref #","2015002883"]],"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif","book_date":"2015-01-31","jail":{"city":"Jacksonville","name":"Jacksonville Sheriff's Office","url":"http://www.jailbase.com/en/sources/fl-jso/","address1":"500 E Adams St","address2":"","state":"FL"},"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-01-31/frederic-sejour-2015002883"},{"name":"DERRICK E ANDERSON","charges":["DUI - UNDER INFLUENCE OF ALCOHOL OR CHEMICAL SUBSTANCE; FACULTIES IMPAIRED","FAIL TO STOP AT STEADY RED LIGHT"],"id":"/fl-jso/2015-01-31/derrick-eugene-anderson-2015002941","book_date_formatted":"Jan 31, 2015","details":[["Gender","M"],["Race","B"],["Height","6 00"],["Weight","230"],["Eyes","GREEN"],["Hair","BLACK"],["Facility","PDF"],["Notes","Age: 37"],["Ref #","2015002941"]],"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif","book_date":"2015-01-31","jail":{"city":"Jacksonville","name":"Jacksonville Sheriff's Office","url":"http://www.jailbase.com/en/sources/fl-jso/","address1":"500 E Adams St","address2":"","state":"FL"},"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-01-31/derrick-eugene-anderson-2015002941"},{"name":"MICHAEL A HUTCHINSON","charges":["KNOWINGLY OPERATE VEH WHILE DL SUSPEND/CANCEL/REVOKED - 1ST OFFENSE (Warrant Case)"],"id":"/fl-jso/2015-01-31/michael-angelow-hutchinson-2015002915","book_date_formatted":"Jan 31, 2015","details":[["Gender","M"],["Race","B"],["Height","5 07"],["Weight","240"],["Eyes","BROWN"],["Hair","BLACK"],["Facility","PDF"],["Notes","Age: 26"],["Ref #","2015002915"]],"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif","book_date":"2015-01-31","jail":{"city":"Jacksonville","name":"Jacksonville Sheriff's Office","url":"http://www.jailbase.com/en/sources/fl-jso/","address1":"500 E Adams St","address2":"","state":"FL"},"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-01-31/michael-angelow-hutchinson-2015002915"},{"name":"DETRA L PARRISH","charges":["CARRYING CONCEALED WEAPON","AFFRAY (FIGHTING)","RESISTING OFFICER WITHOUT VIOLENCE TO HIS OR HER PERSON"],"id":"/fl-jso/2015-01-31/detra-latrice-parrish-2015002891","book_date_formatted":"Jan 31, 2015","details":[["Gender","F"],["Race","B"],["Height","5 05"],["Weight","250"],["Eyes","BROWN"],["Hair","BLACK"],["Facility","PDF"],["Notes","Age: 48"],["Ref #","2015002891"]],"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif","book_date":"2015-01-31","jail":{"city":"Jacksonville","name":"Jacksonville Sheriff's Office","url":"http://www.jailbase.com/en/sources/fl-jso/","address1":"500 E Adams St","address2":"","state":"FL"},"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-01-31/detra-latrice-parrish-2015002891"},{"name":"CAMERON R COSBY","charges":["FUGITIVE WARRANT OF EXTRADITION (FUGITIVE FROM JUSTICE)"],"id":"/fl-jso/2015-01-31/cameron-ray-cosby-2015002884","book_date_formatted":"Jan 31, 2015","details":[["Gender","M"],["Race","W"],["Height","5 08"],["Weight","150"],["Eyes","BROWN"],["Hair","BROWN"],["Facility","PDF"],["Notes","Age: 39"],["Ref #","2015002884"]],"mugshot":"http://imgstore.jailbase.com/widgets/NoMug.gif","book_date":"2015-01-31","jail":{"city":"Jacksonville","name":"Jacksonville Sheriff's Office","url":"http://www.jailbase.com/en/sources/fl-jso/","address1":"500 E Adams St","address2":"","state":"FL"},"more_info_url":"http://www.jailbase.com/en/arrested/fl-jso/2015-01-31/cameron-ray-cosby-2015002884"}],"current_page":1,"total_records":100,"msg":""}"""
let kvpOpt key (value:_ option) = 
	if value.IsSome then sprintf "%s=%s" key (value.Value.ToString()) else String.Empty

let getFromApi debug (key:string) lastname firstname source page = 
	//sources @ http://www.jailbase.com/api/#sources_list or http://www.jailbase.com/api/1/sources/
	use client=new HttpClient()
	client.DefaultRequestHeaders.Add("X-Mashape-Key",key);
	
	let uri:string = sprintf "https://jailbase-jailbase.p.mashape.com/search/?last_name=%s%s%s%s" lastname <| kvpOpt "&first_name" firstname <| kvpOpt "&source_id" source <| kvpOpt "&page" page
	// page=1
	uri.Dump("uri")
	let json = client.GetStringAsync(uri).Result;
	if debug then json.Dump("raw");
	json

//Newtonsoft.Json.JsonConvert.DeserializeObject(json).Dump("object");
type ArresteeDetail = {
	Gender:string
	Race:string
	Height:string
}
type Arrest = {
	Name:string
	Charges: string list
	Id:string
	Book_Date_Formatted:string
	Details: string list list
	Mugshot:Uri
	Book_Date:DateTime
	More_Info_Url: string
}
type JailResult = {
	Status:string
	Next_Page:int
	Records: Arrest list
	Current_Page:int
	Total_Records:int
	Msg:string
}
let toTypedResult input = 
	Newtonsoft.Json.JsonConvert.DeserializeObject<JailResult>(input)

// try to deserialize the sample first to see if it is working
let sampleInfo = toTypedResult sample

"attempting real api call".Dump();
let typed,raw = 
	// fl-jso, fl-ccso, fl-
	getFromApi false (Util.GetPassword("X-Mashape-Key")) 
	<|"Ames" 
	<| None // firstname
	<| None //Some "fl-ccso"  // source
	<| None // Page: Some 10
	|> fun raw -> (toTypedResult raw,raw)
	
typed.Dump()
raw.Dump("raw")

