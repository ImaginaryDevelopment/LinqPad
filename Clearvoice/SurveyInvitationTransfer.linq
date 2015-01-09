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

open CVS.DataAccess.BusinessObjects

type ValidationBO = CVS.DataAccess.BusinessObjects.Members.ValidationBO
type UserInvitation = 	CVS.DataAccess.Entities.Projects.UserInvitation
type ProjectBO = CVS.DataAccess.BusinessObjects.Projects.ProjectBO
type Member = CVS.DataAccess.Entities.Member.Member
type Org = CVS.DataAccess.Entities.Admin.Org

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
		
let setupCaptureFlash capture embed userInviteId isAuthenticated vid src getUserIpFunc projectClosedUrl:InitializationResult = 
	if not <| getBoolOrDefault capture then
		UnknownRedirect(SurveyInvitationBO.Instance.GetRedirectToSurveyUrl("SurveyInvitation.aspx", vid, src, null, projectClosedUrl, getUserIpFunc))
	else
		let userInvitation = 
			try
				Some (UserInvitationBO.Instance.GetByUserInvitationGuid(userInviteId))
			with | _ -> None
		if userInvitation.IsNone then
			FailureRedirect(projectClosedUrl)
		else	
			let userInviteMember = MemberBO.Instance.GetMember(userInvitation.Value.MemberId)
			let userAuthenticatedId = if userInviteMember <> null then Nullable(userInviteMember.MemberId) else Nullable()
			match MemberBO.IsValid(userInviteMember,userInvitation.Value, userAuthenticatedId, isAuthenticated) with
			| MemberBO.MemberValidationResults.Valid -> 
				Success(userInviteMember, userInvitation.Value)
			| x -> FailureRedirect(x.ToString())
	
let initialize (userInvitationGuid:string) (memberId:int option) src (embed:bool)  (queryStringVid:string) (isAuthenticated:bool) (getUserIp:unit ->string) capture projectClosedUrl: InitializationResult = 
	let getUserIpFunc = System.Func<string>(getUserIp)
	getUserIpFunc.Invoke().Dump("got the func!")
	if String.IsNullOrEmpty(capture) then
		FailureRedirect(SurveyInvitationBO.Instance.GetRedirectToSurveyUrl("SurveyInvitation.aspx", queryStringVid, src, null, projectClosedUrl, getUserIpFunc))
	else
		setupCaptureFlash capture embed userInvitationGuid isAuthenticated queryStringVid src getUserIpFunc projectClosedUrl
	
type PageLoadArgs = { 
	Vid:string // was in query string
	UserInvitationGuid:string // was in session or else set by vid
	CurrentUser: Member option // was in session
	Src:string // query string
	Embed:string // query string
	IsAuthenticated:bool // provided via IIS
	GetUserIpAddress: unit -> string // extension method off of request
	IsBeingRedirectedFunc: unit->bool // property off of response
	CaptureFlashCookie:string // appSettings
	SurveyClosedUrl:string
	Auto:string
	CurrentOrg: Org
}

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
			let isValidated = ValidationBO.Instance.CheckIfValidated(null, currentMember, ValidationBO.VerificationType.BeforeSurveyStart);
			
			if not isValidated then
				NeedsValidation
			else
				let project = ProjectBO.Instance.GetProjectByProjectQuotaId(userInvitation.ProjectQuotaId)
				

				match SurveyInvitationBO.IsValidProjectForSurvey(project, userInvitation, currentMember) with
				| SurveyInvitationBO.IsValidProjectForSurveyResult.ProjectNotFound -> FailureRedirect("ProjectNotFound")
				| SurveyInvitationBO.IsValidProjectForSurveyResult.ProjectNotOpen -> FailureRedirect("ProjectNotOpen")
				| SurveyInvitationBO.IsValidProjectForSurveyResult.Valid
				| _ -> 
					
					let setupResult, redirectUrl, qg, qgps, isMedical = 
						SurveyInvitationBO.Instance.SetupMemberOrGetRedirectUrl(
							args.UserInvitationGuid,
							project,
							null,
							currentMember,
							userInvitation,
							args.Auto,
							embed,
							args.IsAuthenticated,
							args.Src,
							System.Func<string>(args.GetUserIpAddress),
							args.CurrentOrg,
							args.SurveyClosedUrl
							
						)
					FailureRedirect("not implemented")
	else
		if args.Vid = null then
			FailureRedirect(SurveyInvitationBO.SurveyStartReferenceCode.InvitationGuidNotInQueryString.ToString())
		elif args.UserInvitationGuid = null && args.Vid = null then 
				FailureRedirect(SurveyInvitationBO.SurveyStartReferenceCode.InvitationGuidNotInSession.ToString())
		else UnknownRedirect("Mobile or recurse")
			
			
			
// begin testing calls
let emptyArgs = {
	PageLoadArgs.Vid=String.Empty
	UserInvitationGuid=String.Empty
	CurrentUser = None
	Src = String.Empty
	Embed = String.Empty
	IsAuthenticated = false
	GetUserIpAddress = (fun () -> null)
	IsBeingRedirectedFunc= (fun () -> false)
	CaptureFlashCookie = String.Empty
	SurveyClosedUrl = String.Empty
	Auto = String.Empty
	CurrentOrg = null
}

let happyPathArgs = { 
	emptyArgs with 
		GetUserIpAddress = (fun () -> String.Empty)
		SurveyClosedUrl = "SurveyClosedUrl"
}
