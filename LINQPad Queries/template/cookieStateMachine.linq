<Query Kind="FSharpProgram" />

// cookie test

type Cookie = {mutable Expired:bool; mutable Discard:bool; Name:string;Value:string}
let getName c = c.Name
let flip f x y = f y x

let cookieState = 
    [
        { Expired = false; Discard = false; Name="DiscardMe"; Value="no"}
        { Expired = false; Discard = true; Name="DiscardMe2"; Value="no"}
        { Expired = false; Discard = false; Name="ExpireMe"; Value="no"}
        { Expired = true; Discard = false; Name="ExpireMe2"; Value="no"}
        { Expired = false; Discard = false; Name="KeepMeInResponse"; Value="no"}
        { Expired = false; Discard = false; Name="KeepMeNotInResponse"; Value="yay"}
        
    ]
module CookieCrumbles = 
    // add back oldState items that serverState didn't discard or expire, and that weren't already discarded or expired themselves
    let addMissing oldState serverState nextState = 
        let getNameEquality a b = getName a = getName b
        let getContainsCookie x = List.exists(getNameEquality x)
        nextState
        |> Seq.groupBy getName
        |> Seq.map(fun (k,v) -> k, v |> Seq.head)
        |> Map.ofSeq
        |> fun m ->
            let toAdd = 
                oldState
                // filter out items the server has sent 
                |> List.filter(flip getContainsCookie nextState >> not)
                |> List.filter(flip getContainsCookie serverState >> not)
                |> List.filter(fun c -> (c.Expired || c.Discard) |> not)
    //        (nextState,toAdd).Dump("adding")
            // add back items that remain and aren't exp/disc
            nextState
            |> List.append toAdd
                
                
        
        
    let replaceFromServer rc c =
    //    (getName rc, getName c).Dump("replace from server")
        if getName rc = getName c then
            if rc.Discard || rc.Expired then
                c.Expired <- rc.Expired
                c.Discard <- rc.Discard
                None
            else 
                Some rc
        elif c.Expired || c.Discard then 
            None
        else
            Some c
    let updateCookieState cookieState serverRespState = 
        serverRespState // take each server cookie and apply it to the cookieState
        |> Seq.fold(fun nextCookieState rc -> // apply each 
            nextCookieState |> List.choose(replaceFromServer rc)
        ) cookieState
        |> addMissing cookieState serverRespState
        |> addMissing serverRespState cookieState
let serverRespState = 
    [ 
        {Expired=false; Discard=true; Name="DiscardMe"; Value="no"}
        {Expired=true; Discard=false; Name="ExpireMe"; Value="no"}
        {Expired=false; Discard=true; Name="DiscardMe3"; Value="no"}
        {Expired=true; Discard=false; Name="ExpireMe3"; Value="no"}
        {Expired=false; Discard=false; Name="KeepMeInResponse"; Value="yay"}
        {Expired=false; Discard=false; Name="I'mNewHere"; Value="yay"}
    ]
CookieCrumbles.updateCookieState cookieState serverRespState
|> Dump
|> ignore

    
