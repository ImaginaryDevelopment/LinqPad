<Query Kind="FSharpProgram" />

// no entity types up top only value types (no db persistance concerns)
let isLinqPad = true


//type DoB = 
type DoB = | DoB of DateTime

let DoB (dt:DateTime) = // shadow constructor
	if(dt.Year>1915) //what does the business say a min valid dob should be? 
	then Some(DoB dt) 
	else None

let StringOfLengthConstructor<'c> (input:string, length:int, defaultConstructor:string->'c) = 	
	match input with
	| null -> None
	| x when x.Length = length -> Some(defaultConstructor(input))
	| _ -> None
	
type String4 = | String4 of string
let String4 input= StringOfLengthConstructor<String4>(input,4,String4)

type String5 = | String5 of string
let String5 input = StringOfLengthConstructor<String5>(input,5,String5)

type ZipCode = 
	| US of String5
	| Canada of string
	
type Email = | Email of string

let Email (input) = //shadow the constructor
	if Regex.IsMatch(input,@"^\S+@\S+\.\S+$")
		then Some (Email input)
		else None
type Gender = 
			| M
			| F

type Member = { Gender: Gender option; ZipCode:ZipCode; DoB:DoB; Email:Email option} // member site registration always gets lots of info

type SurveyTaker = // user
	| Member of Member
	| VendorMember //may have nothing about them

type InvitationType = //vendor members can't be invited(?)
	| Dynamic // dynamic controllers in the member site (dealspot/trialpay (facebook)) 
	| MemberEmail of Email
	| AvailableSurvey //links in member site (dynamic invitation?)
	| VendorLink // we send a link to a vendor, who may give it (or email it, or show it on a available page, who knows) to their members
	| ApiLink // takes to survey start page for survey (Andy's API)

type SurveyInvitationSource = |Source of InvitationType
type SurveyInvitationResponseSource =  |Response of InvitationType

type QuestionType = 
	|DropDown
	|Radio
	|Checkbox
	
type AnswerType = 
	| OnlyOne of string
	| MultipleChoice of seq<string>	
	
type Country = | Country of string

type QuotaGroupType = // onenote:https://d.docs.live.net/c9ad6e2c19de9959/OneNote Notebooks/ClearVoice/ClearVoice/CVS Product Documentation/Manage Site - CVR/Manage Site - CVR.one#Quota%20Group%20Types&section-id={CB361946-105C-493D-92A0-86E3A884BB9B}&page-id={C7ED0142-F754-4C94-B840-8534F6D7BBD6}&end
						// https://skydrive.live.com/edit.aspx/OneNote Notebooks/ClearVoice/ClearVoice?cid=c9ad6e2c19de9959&id=documents?&wd=target%28CVS%20Product%20Documentation%2fManage%20Site%20-%20CVR%2fManage%20Site%20-%20CVR.one%7cCB361946-105C-493D-92A0-86E3A884BB9B%2fQuota%20Group%20Types%7cC7ED0142-F754-4C94-B840-8534F6D7BBD6%2f%29
	| Demographic of (string*QuestionType*string list) list
	| Vendor of Country option list  // list of countries to ip2location filter on, and  vendor member survey we only have a link to provide 
	| Members of seq<Member> // accounts for members and vendors
	| PanelistNetwork // ??

type ProjectType =
	| PanelOwner 
	| Research // most common
	| QuickSurvey
	| MedicalResearch // medical panels are the only ones that can be targeted?
	| PanelBuild
	| ProfileCompletion
	| Complimentary // no revenue associated 
	
type Project = {ProjectType:ProjectType; Name:string; CvrProjectManager: string option}

//let project1 = {ProjectType=ProjectType.PanelOwner; Name="project1"; CvrProjectManager = None}
//let project2 = {project1 with Name="project2"}
type QuotaGroup = { Project:Project; Name:string; QuotaGroupType:QuotaGroupType}

type UserInvitation = {User:User; InvitationType: InvitationType; QuotaGroup:QuotaGroup}

	
type SurveyStartResult =
	| RestrictedIP
	| InvalidProject
	| InvalidQuotaGroup
	| AlreadyTaken
	| RestrictedCountry
	| InvalidIdentity // fingerprinting and entries in the identity table
	| DuplicateIp
	| InvalidDemographic // ip2location
	| Start


// ------------------- UI ---------------------------------

 
type SurveyStartPoints =
	| SurveyInvitationAspx
	| SurveyInvitationAspxMobile
	| VendorSurveyInvitation
	
// ------------------- Entities should be below ---------------------
//type InvitationType = //value
//	| Dealspot of UserInvitation
//	| Member of Member
//	| Api of VendorMember
//	
type MemberEntity = //entity
	| MemberId of int
	| MemberGuid of Guid
	
//type VendorMember = | VendorMemberId of User.VendorMember*int //entity

type UserInvitationEntity = //entity
	| UserInvitationId of int 
	| UserInvitationGuid of Guid

type Org = // Organization
	|Panel
	|Client
	|Vendor


// ****************************************************************************

// ---- tests --------------------------------------------------
//type stringExpectation = {Input:string;Expected:bool};

// does not compile, as it should not
// let emailAddress1 = Email 1
let validateConstructor<'i,'o> (f:'i->'o option, seq ) =
	let map = fun (input:'i,expected) -> 
		let aOption:'o option = f(input)
		match (aOption,expected) with
		| (Some(actual),false) -> 
			if isLinqPad then Util.Highlight(actual).Dump("should not be valid")
			failwithf "%A should not be valid" actual
		| (Some(actual),true) -> (actual :> obj,"is valid")
		| (None,true) ->
			if isLinqPad then Util.Highlight(input).Dump("should be valid")
			failwithf "%A should be valid" input
		| (None,false) -> (input :> obj,"is invalid")
	Seq.map map seq

(String5,[|("hello",true);("hell",false);(null,false);("",false)|]) |> validateConstructor
		|> if isLinqPad then Dump else ignore
(String4,[|("hello",false);("hell",true)|]) |> validateConstructor
	|> if isLinqPad then Dump else ignore
//validateConstructor(Email,[|("hello",false);("hello@",false);("hello@goodbye.com",true);("@nospam.net",false)|]).Dump()
(Email,[|("hello",false);("hello@nospam",false);("hello@goodbye.com",true);("@nospam.net",false)|]) |> validateConstructor 
	|> if isLinqPad then Dump else ignore
	
let dob1= DoB (DateTime(1910,12,1))


(DoB,[|(DateTime(1910,12,1),true);(DateTime(1900,1,1),false)|]) |> validateConstructor 
	|> if isLinqPad then Dump else ignore