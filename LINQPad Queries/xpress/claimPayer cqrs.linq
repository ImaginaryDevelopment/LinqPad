<Query Kind="FSharpProgram" />

// model of RCM -> PayerProfileAdd/edit screen
type CreationGuid = System.Guid // http://blog.ploeh.dk/2014/08/11/cqs-versus-server-generated-ids/
type Identity = int
type Entity<'t> = {Id: Identity; Value:'t}
type PayerValue = { Name:string; }
type PayerEntity = PayerValue Entity
type Payer = 
    | PValue of PayerValue
    | PEntity of PayerEntity

type PayerType = |Primary |Secondary | Tertiary |Workmans | ThirdParty

type PayerProfileInfoDataModel = { Payer:Payer; PatientId: Identity; PatientImageData: byte[] }
type PayerProfileInfoEntity = Entity<PayerProfileInfoDataModel>

type PayerProfileInfo = | PpiEntity of PayerProfileInfoEntity | PpiValue of PayerProfileInfoDataModel

type ClaimValue = 
    {   CurrentLevel: int option
        PatientId: Identity
        PrimaryPayer: PayerProfileInfoEntity option
        SecondaryPayer: PayerProfileInfoEntity option
        TertiaryPayer: PayerProfileInfoEntity option
        WorkmansPayer: PayerProfileInfoEntity option
        } with
        member x.MaxLevel = 
            match x.PrimaryPayer, x.SecondaryPayer, x.TertiaryPayer, x.WorkmansPayer, x.CurrentLevel with
            | _, _,_, Some wp, Some 99 -> Some 99
            | Some _, Some _, Some _, _wp, _cl -> Some 3
            | Some _, Some _, None, _wp, _cl -> Some 2
            | Some _, None, _, _wp, _cl -> Some 1
            | None, None, None, None, None -> None
        member x.GetPayerByType pt =
            match pt with
            | Primary -> x.PrimaryPayer
            | Secondary -> x.SecondaryPayer
            | Tertiary -> x.TertiaryPayer
            | Workmans -> x.WorkmansPayer
            | ThirdParty -> None
            
type ClaimEntity = ClaimValue Entity

type PrepClaimCommand =
    | AddPayer of forFuture:bool * PayerProfileInfoDataModel * PayerType
    | ChangePayer of forFuture:bool * PayerType * payerId:int * updatedPayer:Payer
    | RemovePayer of payerId:int
    
type PrepClaimEvent =
    //| CurrentPayerProfileModified of payerId:Identity
    | PayerAdded of CreationGuid // you can then query with the creation guid to get the shiny new identity
    | PayerInfoCreated of CreationGuid
    | PayerChanged of forFuture:bool // in this situation the actual identity was available
    | PayerRemoved // payer deleted is not available from RCM
    
    //| ProfileSaved

//SavePayerProfile
let savePayerProfile updatePayerProfile forFuture (state:ClaimValue) (p:PayerProfileInfoDataModel,pt:PayerType) =
    let oldPayerProfile = state.GetPayerByType pt
    let isInsert = 
        match oldPayerProfile with 
        | Some pp -> pp.Id = 0
        | None -> true
    seq {
    
        if isInsert then
            yield PayerInfoCreated (Guid.NewGuid())
        if updatePayerProfile then
            if Option.isSome oldPayerProfile then
                yield PayerChanged forFuture
            else
                yield PayerAdded (Guid.NewGuid())
        // not modeling the modification of max level //
        
    }
//SavePayerProfileInfo
let savePayerProfileInfo (state:ClaimValue) (p:PayerProfileInfoDataModel, pt:PayerType) =
    seq {
        PayerInfoCreated
    }
    
let decide (claimEntity:ClaimEntity) command =
    seq{
    
        match command with
        | AddPayer (forFuture, newPayerInfo,pt) ->
            // save profile
            if forFuture then // line 1073
                //save payerProfile
                yield! savePayerProfile (* updatePayerProfile= *) true true claimEntity.Value (newPayerInfo,pt)
                00
            else
                //save payerProfileInfo
                yield! savePayerProfile (* updatePayerProfile= *) false false claimEntity.Value (newPayerInfo,pt)
                ()
        
    }
        


    

    