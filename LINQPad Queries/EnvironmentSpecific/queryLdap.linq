<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.AccountManagement.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.Protocols.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <NuGetReference>System.DirectoryServices.AccountManagement</NuGetReference>
  <Namespace>System.DirectoryServices</Namespace>
  <Namespace>System.DirectoryServices.AccountManagement</Namespace>
</Query>

open System.DirectoryServices.Protocols

Environment.UserName.Dump("UN")
Environment.UserDomainName.Dump("domain")
//using (var context = new PrincipalContext(ContextType.ApplicationDirectory ))
let domain = Util.Cache((fun () -> Util.ReadLine "Domain?"), "domain") // |> Option.ofValueString |> Option.defaultValue Environment.UserDomainName
//let domainIdentifier = Util.GetPassword("DomainQualifier?")
//type KnownQueries =
    
let isValueString x = String.IsNullOrWhiteSpace x |> not

module Option =
    let ofValueString value =
        if isValueString value then
            Some value else None
let creds =
    match Util.Cache((fun () -> Util.ReadLine "Username?"), "username") |> Option.ofValueString with
    | Some un -> Some(un, Util.GetPassword(domain + "\\" + un + " password"))
    | None -> None

type LdapType =
    | NoVerify
let doLdap authType ldapTypeOpt = // port for LDAPS is 636
    let path = $"LDAP://{domain}"
    match ldapTypeOpt, authType with
    | Some NoVerify, Some (un,pwd:string) ->
        let timeout = TimeSpan.FromMinutes(5.0)
        let ldapIdentifier = LdapDirectoryIdentifier(domain + ":636", 636, fullyQualifiedDnsHostName=false, connectionless=false)
        let credentials = System.Net.NetworkCredential(un,pwd,domain= "DCPS")
        use ldapConn = new LdapConnection(ldapIdentifier, credentials)
        ldapConn.SessionOptions.VerifyServerCertificate <- VerifyServerCertificateCallback(fun _ _ -> true)
        let getUserRequest =
            let dcs = domain.Split(".") |> Seq.map(sprintf "DC=%s") |> String.concat ","
            dcs.Dump("dcs")
            //let dn = "OU=IdentityAccessManagement,"+ dcs
            SearchRequest(dcs,"(objectCategory=organizationalUnit)", System.DirectoryServices.Protocols.SearchScope.Subtree)
        // throws if the result is larger than requested
        //getUserRequest.SizeLimit <- 5
        getUserRequest.TimeLimit <- timeout
        let searchControl = new SearchOptionsControl(System.DirectoryServices.Protocols.SearchOption.DomainScope)
        getUserRequest.Controls.Add(searchControl) |> ignore<int>
        let userResponse = ldapConn.SendRequest(getUserRequest, timeout) :?> SearchResponse

        //Now, I have an object that operates very similarly to DirectoryEntry, mission accomplished
        if (userResponse.Entries.Count < 1) then
            raise <| Exception("User not found");

        let resultEntry = userResponse.Entries[0];
        // works
        resultEntry.Attributes["name"].GetValues(typeof<string>).Dump("name")
        for an in resultEntry.Attributes.AttributeNames do
            try
                let an = an :?> string
                resultEntry.Attributes[an].GetValues(typeof<string>).Dump(an)
            with ex ->
                eprintfn "Failed to read: '%A'" an
        
        resultEntry.Dump("re")
        
    | None, Some(un,pwd) ->
        printfn "Searching %s:%s" path un
        let dEntry = new DirectoryEntry(path, un, pwd)
        let dSearcher = new DirectorySearcher(dEntry)
        dSearcher.Filter <- "(&(objectClass=user))"
        dSearcher.FindAll().Dump("all")
        
        ()
    | None, None ->
        printfn "Searching %s" path
        let dEntry = new DirectoryEntry(path)
        let dSearcher = new DirectorySearcher(dEntry)
        dSearcher.Filter <- "(&(objectClass=user))"
        dSearcher.FindAll().Dump("all")
        
    ()
    
doLdap (creds ) (Some LdapType.NoVerify)
//}