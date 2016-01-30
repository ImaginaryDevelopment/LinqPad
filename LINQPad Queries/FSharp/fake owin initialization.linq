<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <NuGetReference>Microsoft.AspNet.Identity.Core</NuGetReference>
  <NuGetReference>Microsoft.AspNet.Identity.Owin</NuGetReference>
  <NuGetReference>Microsoft.Owin</NuGetReference>
  <Namespace>Microsoft.Owin</Namespace>
  <Namespace>Owin</Namespace>
</Query>

open Microsoft.Owin
open Owin
open Microsoft.AspNet.Identity
open Microsoft.AspNet.Identity.Owin

open Microsoft.Owin.Security.OAuth
open Microsoft.Owin.Security.Cookies

// dummy for compilation
type AppUser(name) =
    member val UserName:string = name with get,set
    member val Id:string = "" with get, set
    interface Microsoft.AspNet.Identity.IUser with
        member x.UserName with get() = x.UserName and set v = x.UserName <- v
        member x.Id with get() = x.Id
// dummy for compilation        
type ApplicationDbContext () =
    interface IDisposable with
        member x.Dispose() = "disposing".Dump() |> ignore
    static member Create() = ApplicationDbContext ()

// dummy for compilation
type ApplicationUserManager (store) =
    inherit Microsoft.AspNet.Identity.UserManager<AppUser>(store)
    
    static member Create(options:IdentityFactoryOptions<ApplicationUserManager>,context:IOwinContext) = 
        let emptyTask = Tasks.Task.Run(Action(fun () -> ()))
        let userFromStringTask s = Tasks.Task.FromResult (AppUser(s))
        // create new fake object that implements interface to satisfy compiler
        new ApplicationUserManager({
                                    new IUserStore<AppUser> with
                                        member x.CreateAsync _user = emptyTask
                                        member x.DeleteAsync _user = emptyTask
                                        member x.UpdateAsync _user = emptyTask
                                        member x.FindByIdAsync userId = userFromStringTask userId
                                        member x.FindByNameAsync name = userFromStringTask name
                                        
                                    interface IDisposable with
                                        member x.Dispose() = () })
                                        
// dummy for compilation
type ApplicationSignInManager(userManager,authManager) =
    inherit SignInManager<AppUser,string>(userManager,authManager)
    static member Create(options:IdentityFactoryOptions<ApplicationSignInManager>, context:IOwinContext) = 
        ApplicationSignInManager(null,null)
        
let mutable oAuthBearerOptions: OAuthBearerAuthenticationOptions = null
let configureAuth (app:IAppBuilder) =
    // all of these compile
    app.CreatePerOwinContext(ApplicationDbContext.Create) |> ignore
    
    let funcd: System.Func<ApplicationDbContext> = System.Func<ApplicationDbContext>(ApplicationDbContext.Create)
    app.CreatePerOwinContext(funcd) |> ignore
    
    let funcIt f = System.Func<_>(f)
    app.CreatePerOwinContext(funcIt ApplicationDbContext.Create) |> ignore
    
    // -------------------------------------------------
    // next section was a real doozy 
    // -------------------------------------------------
    // does not compile
    //app.CreatePerOwinContext<ApplicationUserManager>(ApplicationUserManager.Create) |> ignore
    // also does not compile (tupled arguments required of the create method freak it out):
    //let funcd: System.Func<IdentityFactoryOptions<ApplicationUserManager>,IOwinContext,ApplicationUserManager> = System.Func<_,_,_>(ApplicationUserManager.Create)
    
    /// create non-tupled method for Func constructor
    let create options context = ApplicationUserManager.Create(options,context)
    let funcIt3 f = System.Func<_,_,_>(f)
    let funcd = funcIt3 create
    app.CreatePerOwinContext<ApplicationUserManager>(funcd) |> ignore
    
    let create options context = ApplicationSignInManager.Create(options,context)
    let funcd = funcIt3 create
    
    app.CreatePerOwinContext<ApplicationSignInManager>(funcd) |> ignore
    
    oAuthBearerOptions <- OAuthBearerAuthenticationOptions()
    app.UseOAuthBearerAuthentication(oAuthBearerOptions)
    
    app.UseCookieAuthentication(
        CookieAuthenticationOptions( // property initialization looks just almost the same as constructor call
            AuthenticationType= DefaultAuthenticationTypes.ApplicationCookie,
            LoginPath= PathString("/User/Login"),
            Provider=CookieAuthenticationProvider(
                OnValidateIdentity = 
                    SecurityStampValidator.OnValidateIdentity<ApplicationUserManager,AppUser>(
                        validateInterval= TimeSpan.FromMinutes 30.0 ,
                        // we don't have the same AppUser so we don't have the generateUserIdentityAsync method to pretend to have
                        // notice that the fun delegate in this instance doesn't use any commas, it takes one argument then the other, not a tuple
                        regenerateIdentity= (Func<_,_,_>(fun manager user -> Tasks.Task.FromResult(Security.Claims.ClaimsIdentity())))
            ))))
    
    ()
    

//