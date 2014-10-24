<Query Kind="FSharpProgram">
  <Connection>
    <ID>282ab884-278b-4d31-a865-51e85288681f</ID>
    <Persist>true</Persist>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

// http://coding.infoconex.com/post/2012/06/01/Getting-LINQPad-to-read-your-applications-AppConfig-settings
// let dc = new TypedDataContext()
// AppDomain.CurrentDomain.SetupInformation.ConfigurationFile.Dump()
// System.Configuration.ConfigurationManager.ConnectionStrings.Dump()

assert (System.Configuration.ConfigurationManager.ConnectionStrings.["LocalSqlConnection"] <> null && System.Configuration.ConfigurationManager.ConnectionStrings.["LocalSqlConnection"].ConnectionString.Contains("SQLEXPRESS") = false)
assert (System.Configuration.ConfigurationManager.ConnectionStrings.["LocalLINQSqlConnection"] <> null && System.Configuration.ConfigurationManager.ConnectionStrings.["LocalLINQSqlConnection"].ConnectionString.Contains("SQLEXPRESS") = false)


let toNull<'a when 'a:null> (x:'a option) = match x with |None -> null | Some x -> x
let fromNull<'a when 'a:null and 'a:equality> (x:'a) = match x with | y when y <>null -> Some x | _ -> None

type PathsToSurvey = 
	|MemberRouter 
	|OfferClickRouter
	|DirectInvite

type TakeSurveyErrors = 
	| NotValidatedForStart
	| NotSmsValidated
	
type TakeSurveyPath = 
	| Error of TakeSurveyErrors
	| RedirectToSurveyStart
	
type Panel = | Panel | MedicalPanel

type InitializationResult =
	| FailureRedirect of string
	| SuccessRedirect of string
	| UnknownRedirect of string
	| NeedsValidation
	| Success of Member*UserInvitation 
	| Halt of Exception
	
let getBoolOrDefault s = 
	if not (String.IsNullOrEmpty(s)) then
		let success,value = bool.TryParse(s)
		success && value
	else
		false
		
let ``surveyinvitation.aspx.page_load`` (args:PageLoadArgs) :InitializationResult =
	//store vid in session
	let embed = getBoolOrDefault args.Embed 
	if args.UserInvitationGuid <> null then
		let initializationInfo = 
			initialize
			<| args.UserInvitationGuid
			<| if args.CurrentUser.IsSome then Some args.CurrentUser.Value.MemberId else None
			<| args.Src
			<| embed
			<| args.Vid
			<| args.IsAuthenticated
			<| args.GetUserIpAddress
			<| args.CaptureFlashCookie
			<| args.SurveyClosedUrl
			
		match initializationInfo with
		| FailureRedirect _ | UnknownRedirect _ | SuccessRedirect _ | NeedsValidation | Halt _ -> initializationInfo
		| Success( currentMember,userInvitation) -> 
