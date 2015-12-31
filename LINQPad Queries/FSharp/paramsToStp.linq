<Query Kind="FSharpProgram" />

let testData = """zeroToNullable user.UserID, user.UserName, user.UserPassword, user.UserFirstName.Trim(), user.UserMiddleInitial, user.UserLastName.Trim(), zeroToNullable user.UserLevel, zeroToNullable user.UserFacilityID, Nullable user.UserPreviouslyLoggedIn, Nullable user.ResetPassword, Nullable user.LoginAttempts, Nullable user.IsLockedOut"""
let toReplace = "user"
let terms = testData.Split([|','|])
terms.Dump()
terms
|> Seq.map (fun s -> 
    let s = s.Trim ()
    let replaceIndex = s.IndexOf(toReplace)
    let memberAccessIndex = s.IndexOf(".",replaceIndex + 1 + toReplace.Length)
    match memberAccessIndex >= 0 with
    | false -> s.Replace(toReplace + ".","(^a : (member ") + ": _) user)"
    | true -> 
        let makeMemberAccess memberType remainder=
            let beginning = s.Substring(0,memberAccessIndex).Replace(toReplace + ".","((^a : (member ")
            sprintf "%s: %s) %s) %s" beginning memberType toReplace remainder
        let term = s.Substring(memberAccessIndex)
        // a better approach is/was to make an F# trim function, that removes the necessity to specify the type, and the extra ()
        match term with // needs work figuring out if it needs to specify the type
        |".Trim()" |".Trim ()" |".TrimStart()" | ".TrimStart ()" -> makeMemberAccess "string" (s.Substring(memberAccessIndex)) |> sprintf "%s)"
        | _ -> makeMemberAccess "_" (s.Substring(memberAccessIndex))
)
|> fun s -> String.Join (", ", Array.ofSeq s)
|> Dump