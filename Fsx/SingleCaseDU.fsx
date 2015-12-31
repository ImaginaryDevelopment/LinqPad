open System

module EmailAddress = 

    type _T = EmailAddress of string 

    // wrap
    let create (s:string) = 
        if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\S+@\S+\.\S+$") 
            then Some (EmailAddress s)
            else None
    
    // unwrap
    let value (EmailAddress e) = e

module ConsumerSamples = 
    // create email addresses
    let address1 = EmailAddress.create "x@example.com"
    let address2 = EmailAddress.create "example.com"

    // does not compile
    //let badAddress = EmailAddress "hello"

    // bad code, would love to make it not compile
    let badAddress = EmailAddress.EmailAddress "hello"

    // unwrap an email address
    match address1 with
    | Some e -> EmailAddress.value e |> printfn "the value is %s"
    | None -> ()

module PhoneNumber =
    type _T = USPhoneNumber of string
    let regex= "[^\d]*1?(\d{3})[^\d]*(\d{3})[^\d]*(\d{4})[^\d]*"
    let create (s: string) =
        if System.Text.RegularExpressions.Regex.IsMatch(s,regex)
            then Some (USPhoneNumber s)
            else None
    let value (USPhoneNumber e) = e
    let pretty (USPhoneNumber e) = 
        let values = System.Text.RegularExpressions.Regex.Match(e, regex)
        sprintf "(%s) %s-%s"  values.Groups.[1].Value values.Groups.[2].Value values.Groups.[3].Value

module PhoneConsumerSamples = 
    let tryCreate = PhoneNumber.create
    let bind p f = p |> Option.bind (fun x -> f x )
    let phone1 = tryCreate "904-999-9999"
    let badphone1 = tryCreate "failwhale"
    let phonepretty = match phone1 with | Some x -> phone1.Value |> PhoneNumber.pretty | None -> failwithf "bad phone1 %A" phone1
    let phonepretty' = bind phone1 (fun x -> Some <| PhoneNumber.pretty x)
    let phonepretty'' = bind phone1 ( Some << PhoneNumber.pretty)
    let phonepretty''' = bind phone1 (PhoneNumber.pretty >> Some)
    let phone2 = tryCreate "903-123-4567extA"
    let badphone2 = PhoneNumber.create

