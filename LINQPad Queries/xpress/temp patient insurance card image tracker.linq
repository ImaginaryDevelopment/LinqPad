<Query Kind="FSharpProgram">
  <Connection>
    <ID>6d9036c3-dd08-4e55-8c56-0d7d31c5ebfc</ID>
    <Persist>true</Persist>
    <Server>prog7-pc</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>ApplicationDatabase</Database>
    <UserName>xpu10</UserName>
  </Connection>
</Query>

open Microsoft.FSharp.Linq.NullableOperators

let dc = new TypedDataContext()
module Xxx = 
    type PICInfo = {PCId:int; PatientId:int; PayerId:int; ImageLength:int; BackLength:int}

open Xxx
let srcId, targetId = 1, 300
let getData() =
    dc.PatientInsuranceCardImages
        .Where(fun x -> x.PatientID ?= srcId || x.PatientID ?= targetId)
        .Select(fun x -> x.PatientInsuranceCardID, x.PatientID.Value,x.PayerID.Value, x.PatientImage.Length, x.PatientBackImage.Length)
        .ToList()
        |> List.ofSeq
        |> List.map (fun (piid,pId,pyId, piL, piBL) -> {PCId = piid;PatientId=pId; PayerId= pyId; ImageLength=piL;BackLength=piBL})

try
    let cachedData = Util.Cache (Func<_>(getData))
    cachedData.Dump("cached")
with ex -> ex.Dump("cache failed")
    
let current =  getData()
current.Dump("new")
let targetHasCard = current |> Seq.exists(fun x -> x.PatientId = targetId)
if not targetHasCard then
    let src = dc.PatientInsuranceCardImages.First(fun x -> x.PatientID.HasValue && x.PayerID.HasValue)
    let x = PatientInsuranceCardImage(PatientID=Nullable targetId,PayerID=src.PayerID, PatientImage=src.PatientImage, 
                PatientBackImage=src.PatientBackImage)
    dc.PatientInsuranceCardImages.InsertOnSubmit(x)
    dc.SubmitChanges()
    x.Dump()
    getData().Dump("after insert")


