<Query Kind="FSharpProgram">
  <Namespace>Microsoft.FSharp.Reflection</Namespace>
</Query>

// reflect du cases, 3 different ways
let dumpt t x = x.Dump(description=t); x
module SimpleUnionCaseInfoReflection =
    open Microsoft.FSharp.Reflection

  // will crash if 'T contains members which aren't only tags
    let Construct<'T> (caseInfo: UnionCaseInfo)                   = FSharpValue.MakeUnion(caseInfo, [||]) :?> 'T

    let GetUnionCaseInfoAndInstance<'T> (caseInfo: UnionCaseInfo) = (caseInfo, Construct<'T> caseInfo)

    let AllCases<'T> = 
        FSharpType.GetUnionCases(typeof<'T>)
        |> Seq.map GetUnionCaseInfoAndInstance<'T>
    let GetAllCaseNames t =
        FSharpType.GetUnionCases(t)
        |> Seq.map (fun u -> u.Name, u.GetFields())
    let GetAllCaseNamesT<'T> = 
        GetAllCaseNames typeof<'T>
        
// didn't work in release mode, or on the build server for some reason
let rec getAllDUCases fNonUnionArg t : obj list =
        let getAllDUCases = getAllDUCases fNonUnionArg
        // both of the following are taken from http://stackoverflow.com/questions/6497058/lazy-cartesian-product-of-multiple-sequences-sequence-of-sequences
        let cartesian_product sequences =
            let step acc sequence = seq {
                for x in acc do
                for y in sequence do
                yield seq { yield! x; yield y}}
            Seq.fold step (Seq.singleton Seq.empty) sequences

        // only works with no arg cases I bet
        let makeCaseTypes (fUnion:Type-> obj list) (fNonUnionArg:Type -> obj) (uc: UnionCaseInfo) : UnionCaseInfo*(obj list list) =
            let constructorArgs =
                uc.GetFields()
                |> Seq.map (fun f ->
                    if FSharpType.IsUnion f.PropertyType then
                        let childTypes = fUnion f.PropertyType
                        if
                            childTypes
                            |> Seq.exists (fun ct -> FSharpType.IsUnion (ct.GetType()) |> not) then
                                failwithf "fUnion returned a bad type in list %A" childTypes
                        childTypes
                    else [ fNonUnionArg f.PropertyType] )
                |> List.ofSeq
            let allCombinationsOfFieldPossibles =
                cartesian_product constructorArgs
                |> Seq.map List.ofSeq
                |> List.ofSeq
            uc, allCombinationsOfFieldPossibles
        // with help from http://stackoverflow.com/a/4470670/57883
        // trying to write this one
        let result =
            FSharpType.GetUnionCases t
            |> Seq.map (makeCaseTypes getAllDUCases fNonUnionArg)
            |> List.ofSeq
        let result =
            result
            |> Seq.map (fun (uc,allFieldComboCases) -> allFieldComboCases |> Seq.map (fun args-> FSharpValue.MakeUnion(uc,args |> Array.ofList)))
            |> Seq.collect id
            |> Seq.map box
            |> List.ofSeq
        result

type Hello = 
    |World of string
    |Bye of bool
    |Three of int
type PtRespType = |Deductible |CoPay |CoInsurance with override x.ToString() = match x with |CoInsurance -> "CoIns" | _ -> sprintf "%A" x
type PaymentItemType = |EraPayment |EraAdjustment of createJournalEntry:bool | PtResp of PtRespType | OtherPayment
let f t = if t = typeof<bool> then box false else null
let getAll t = 
    getAllDUCases f t |> dumpt ("getAllDUCases:" + t.Name)
let testCases = [typeof<Hello>;typeof<PtRespType>;typeof<PtRespType>]
testCases
|> Seq.iter (getAll >> (fun x -> printfn "%A" x))
open SimpleUnionCaseInfoReflection

testCases
|> Seq.map GetAllCaseNames
|> dumpt "GetAllCaseNames t"
|> ignore

[
    box SimpleUnionCaseInfoReflection.AllCases<Hello>
    box SimpleUnionCaseInfoReflection.AllCases<PtRespType>
    box SimpleUnionCaseInfoReflection.AllCases<PaymentItemType>
]
|> Dump
|> ignore

