<Query Kind="FSharpProgram" />

// no entity types up top only value types (no db persistance concerns)
//type Month = 
//	|January = 1
//	|February = 2
//	|March = 3
//	|April = 4
//	|May = 5
//	|June = 6
//	|July = 7
//	|August = 8
//	|September = 9
//	|October = 10
//	|November= 11
//	|December= 12
DateTime.MinValue.Dump()
type DoB = | DoB of System.DateTime
let DoB (dt:DateTime) = // shadow constructor
	if(dt.Year>1900) 
	then Some(DoB dt) 
	else None

type String5 = | String5 of string

let String5 (input:string) = //shadow constructor
	match input with
	| null -> None
	| x when x.Length =5 -> Some(String5 x)
	| _ -> None
	
type ZipCode = 
	| US5 of String5
	| US9 of String5*string
	| Canada of string
	
type Email = | Email of string

let Email (input:string) = //shadow the constructor
	if Regex.IsMatch(input,@"^\S+@\S+\.\S+$")
		then Some (Email input)
		else None

type Gender = 
			| M
			| F

type Member = { Gender: Gender option; ZipCode:ZipCode; DoB:DoB; Email:Email option}

type User = 
	| Member of Member
	| VendorMember //may have nothing about them

type InvitationType = //vendor members can't be invited(?)
	| Dynamic // dynamic controllers in the member site (dealspot/trialpay (facebook))
	| MemberEmail of Email
	| AvailableSurvey //links in member site
	| VendorLink // we send a link to a vendor, who may give it (or email it, or show it on a available page, who knows) to their members
	
type QuestionType = 
	| OnlyOne of string
	| OneOrMore of seq<string>	
	
type Country = | Country of string

type QuotaGroupType = // onenote:https://d.docs.live.net/c9ad6e2c19de9959/OneNote Notebooks/ClearVoice/ClearVoice/CVS Product Documentation/Manage Site - CVR/Manage Site - CVR.one#Quota%20Group%20Types&section-id={CB361946-105C-493D-92A0-86E3A884BB9B}&page-id={C7ED0142-F754-4C94-B840-8534F6D7BBD6}&end
						// https://skydrive.live.com/edit.aspx/OneNote Notebooks/ClearVoice/ClearVoice?cid=c9ad6e2c19de9959&id=documents?&wd=target%28CVS%20Product%20Documentation%2fManage%20Site%20-%20CVR%2fManage%20Site%20-%20CVR.one%7cCB361946-105C-493D-92A0-86E3A884BB9B%2fQuota%20Group%20Types%7cC7ED0142-F754-4C94-B840-8534F6D7BBD6%2f%29
	| Demographic of (string*QuestionType) list
	| Vendor of Country option list  // list of countries to ip2location filter on, and  vendor member survey we only have a link to provide 
	| Members of seq<Member> // accounts for members and vendors
	| PanelistNetwork // ??

type ProjectType =
	| PanelOwner
	| Research
	| QuickSurvey
	| MedicalResearch
	| PanelBuild
	| ProfileCompletion
	
type Project = {ProjectType:ProjectType; Name:string; CvrProjectManager: string option}
type QuotaGroup = { Project:Project; Name:string; QuotaGroupType:QuotaGroupType}
type UserInvitation = {User:User; InvitationType: InvitationType; QuotaGroup:QuotaGroup}

	
type SurveyStartResult =
	| InvalidProject
	| InvalidQuotaGroup
	| AlreadyTaken
	| RestrictedIP
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

// ---- tests --------------------------------------------------
//type stringExpectation = {Input:string;Expected:bool};

// does not compile, as it should not
// let emailAddress1 = Email 1
let validateConstructor (f, seq ) = 
	for (input,expected) in seq do
		let actual = f input
		match (actual,expected) with
		| (Some(email),true) -> actual.Dump("is valid")
		| (Some(email),false) -> Util.Highlight(actual).Dump("should not be valid")
		| (None,true) -> Util.Highlight(input).Dump("should be valid")
		| (None,false) -> input.Dump("is invalid")

let emailConstructors = [|("hello",false);("hello@",false);("hello@goodbye.com",true);("@nospam.net",false)|];
validateConstructor(Email,emailConstructors)
//for (ec,expected) in emailConstructors do
//	let em = Email ec
//	match (em,expected) with 
//	| (Some(email),true) -> email.Dump("is valid")
//	| (Some(email),false) -> Util.Highlight(email).Dump("should not be valid")
//	| (None,true) -> Util.Highlight(ec).Dump("should be valid")
//	| (None,false) -> ec.Dump("is invalid")

let dobConstructors = [(DateTime(1910,12,1),true);(DateTime(1900,1,1),false)]
for (dobC,expected) in dobConstructors do
	let dob = DoB dobC
	match (dob,expected) with
	| (Some(dt),true) -> dt.Dump("is valid")
	| (Some(dt),false) -> Util.Highlight(dt).Dump("should not be valid")
	| (None,true) -> Util.Highlight(dobC).Dump("should be valid")
	| (None,false) -> dobC.Dump("is invalid")
