<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>FsCheck</NuGetReference>
</Query>

open FsCheck
open System

type Page<'T> = {Index:int; PageSize:int; PageItems:'T list}
    with 
        member x.Page = x.Index + 1
//        member x.Pages = float x.Total / float x.PageSize |> ceil |> round
        member x.StartIndex = x.Index * x.PageSize
        member x.MaxIndex = x.StartIndex + x.PageSize
        member x.PageCount = x.PageItems.Length

let items = [ 0..100 ]

type PageSize = int
type Index = int
let config =
    let x = {Config.QuickThrowOnFailure with MaxTest=900}
    x.Dump("config")
    x
let test name x= Check.One({config with Name=name},x)
module PageMath = 
    let getLastPageIndex pageSize count = 
        if count < pageSize || pageSize < 1 then
            0
        else
            
            let x = Math.DivRem(count,pageSize)
            let result = 
                match x with
                | quot,rem when rem > 0 -> 
                    quot
                | quot,_ -> quot - 1
            //printfn "DivRem(%i:count,%i:pageSize) = %A -> %i:lastPageIndex" count pageSize x result
            result
        
    let lastPageCalcProperties pageSize count = 
        let lastPageIndex = getLastPageIndex pageSize count
        let quot = if pageSize > 0 then count / pageSize else 0
        if pageSize < 1 || count < 1 || pageSize >= count then 0 = lastPageIndex
        elif pageSize = 1 then lastPageIndex = quot - 1
        else // domain is pageSize >= 1, count >= 1, pageSize < count
            // not property based at all, but used to figure out properties of the equation
            let patternforming =
                match pageSize, count with
                | 1,2 -> lastPageIndex = 1
                | 1,3 -> lastPageIndex = 1
                | 2,4 -> lastPageIndex = 1
                | 10,11 -> lastPageIndex = 1
                | 10,19 -> lastPageIndex = 1
                | 10,21 -> lastPageIndex = 2
                | 10,20 -> lastPageIndex = 1
                | _ -> true
  
            let mustbeWithin1 = lastPageIndex = quot - 1 || lastPageIndex = quot
            let checkRule2 = 
                match float count / float pageSize = float quot with
                | true -> lastPageIndex = quot - 1
                | false -> lastPageIndex = quot
            
            let result = mustbeWithin1 && checkRule2 && patternforming
            if not result then
                (pageSize,count, lastPageIndex).Dump("Failing")
            result
    test "LastPageCalc" lastPageCalcProperties
        
        
type PaginateResult<'T> = {AllItems: 'T list; Page: Page<'T>}
module Pagination = 
    
    type PaginateError = 
        | BadInput of string
        | FailTest of string
        with 
            static member IsFail =
                function
                | FailTest _ -> true
                | _ -> false
            static member HasFail =
                List.exists(PaginateError.IsFail)
        
    let propSetup (pr:PaginateResult<_>) = 
        List.groupBy(PaginateError.IsFail)
        >> List.choose(
            function
            | true, items ->
                items 
                |> List.choose(function | FailTest msg -> Some msg | _ -> None)
                |> function
                    | [] -> None
                    | x -> Some x
            | false, badInputItems ->
                badInputItems 
                |> List.choose(function | BadInput msg -> Some msg | _ -> None)
                |> function
                    | [] -> ()
                    | badMsgs -> (pr,badMsgs).Dump()
                None
        )
        >> List.exists(fun _ -> true)
        >> not
        
    let anyPageProperties<'T> pr = 
        [
            if pr.Page.PageSize <= 0 then
                yield sprintf "bad page size %i" pr.Page.PageSize
            if pr.Page.Index < 0 then
                yield sprintf "bad index %i" pr.Page.Index
        ]
        |> List.map BadInput
        
    // f2 depends on f1 having passed
    let composeDep f1 f2 pr = 
        let x1 = f1 pr
        if PaginateError.HasFail x1 then
            x1
        else x1@(f2 pr)
    let compose f1 f2 pr = 
        (f1 pr)@(f2 pr)
        
    let firstPageProperties<'T> = 
        let f = 
            function
            | {Page={Index = x}} when x <> 0 -> List.Empty
            | pr ->    
                let total = pr.AllItems.Length
                let rLen = pr.Page.PageItems.Length
                [
                    if total >= 1 && pr.Page.PageSize >= total && total <> pr.Page.PageCount then
                        yield sprintf "page should have been all items lengths(%i vs %i)" total rLen |> FailTest
                ]
        composeDep anyPageProperties f

//        
//    
//    
//    let lastPageProperties pr =
//        let total = List.length pr.AllItems
//        let rLen = pr.Paginated.Length
//        let lastPage = 
//        let f pr = 
//            [
//            
//            ]    
    let paginatedProperties pr = 
        firstPageProperties pr
        |> propSetup pr

    

    
// not LINQ friendly
let paginate pageSize index (items:_ seq) = 
    if index < 0 || pageSize < 1 then
        List.empty
    else
        let x = items |> Seq.skip (index * pageSize) |> Seq.truncate pageSize |> List.ofSeq
        x
let prop<'T> x = 
    let p = {AllItems=x;Page={Index = 0; PageSize=20; PageItems = List.empty}}
    {p with Page = {p.Page with PageItems = paginate p.Page.PageSize p.Page.Index x}}
    |> Pagination.paginatedProperties
test "Pagination" prop

// Check.Verbose prop
