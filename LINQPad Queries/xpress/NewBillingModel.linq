<Query Kind="FSharpProgram" />

// double entry accounting

type AppointmentIdentifier = int
type ChargeIdentifier = int
type PaymentIdentifier = int
type EraItemIdentifier = int
type PatientIdentifier = int
type PayerIdentifier = int
type UserIdentifier = int
type AccountIdentifier = int
type CheckNumber = string
type TransactionNumber = string
type Money = decimal
type Check = Check of CheckNumber

type PatientPayment= |Cash |Lockbox |Check of Check | CreditCard
type ThirdPartyPayment = | PatientPayment of PatientPayment | Ach

type EraPaymentMethod = 
    |Check of CheckNumber
    |Ach of TransactionNumber
    |Other // when ERA is 0

type PtRespType = | Deductible |CoPay|CoInsurance

type PaymentItemType = |EraPayment |EraAdjustment |PtResp of PtRespType |Other
type PaymentTier = 
    |Primary //1 
    |Secondary //2
    |Tertiary //3
    |Workman'sComp // 4?
    
type EraStatus = |New | Complete |Partial
type PaymentType = 
    |Patient of PatientIdentifier * PatientPayment 
    |ThirdParty of PayerIdentifier * ThirdPartyPayment 
    |Era of PayerIdentifier * EraPaymentMethod

type Payment= { // branched from payments for legacy data and migration
    //PayerId: PayerIdentifier (included in PaymentType)
    PaymentTypeId: PaymentType // 3 columns in db
    Created:DateTime
    Recv'd:DateTime
    UserId:UserIdentifier
    TotalAmount: Money // must be >= 0
    PaymentMethodId: int // db only (In model PaymentType encapsulates this)
    PaymentStatusId: EraStatus
    IsElectronic:bool
    Comments:string
    CCItemID:int
    //apptId removed
    //isReconciled (wasn't in use)
    // chargereconciled (wasn't being used)
    // is copay remove
    // timestamp -> Created
    // Amount -> not nullable (6,2)
    // CreditCardPaymentResultID remove
    // PatientId keep (consider recv'd patientid)
    // TransactioNumber -> remove
    // CheckNumber -> TransactionNumber // also will store Ach number
    // Comments keep
    // apptDate remove
    
    // credit card stuff (consider making into own table):
    // responseCode keep
    // response description
    // transactionId
    // transactionType
    // card type
    // maskedacctnum
    // expDate
    // acctnumsource
    //cardholdname
    //alias
    //..
    // approvalcode
    
    //charge id migrates into payment items
    //insurance becomes payment items and remove the source column
    //paymentdate -> Rcd (recv'd) datetime
    //deductible,coins,copay -> remove
    //adjustment, adjustmentcode, remarks code -> payment items
    //billbanlanceto, denied, writeoff balance -> remove
    // PtRespons amt -> amount in payment items
    // ptRespons type -> determines payment item type
    // payment tier stay
    // allowedamount -> payment item
    // payerinitiated cob -> claims -> DidPrimaryInitiateCob, DidSecondaryInitiateCob
    }
    
type PaymentItemStatus = 
    |Posted // on change to posted create journal entries
    |Unposted
    
type PaymentItem =
    {
    PaymentID:EraItemIdentifier
    PaymentItemTypeId :PaymentItemType 
    Created:DateTime
    Amount: Money
    //PtRespType: PtRespType option
    ChargeId: ChargeIdentifier option // provider level adjustments are the only case where the option would be empty
    PaymentTier: PaymentTier
    RemarkCode:string
    AdjustmentReasonCode:string
    PaymentItemStatus : PaymentItemStatus
    Comments : string
    }

type Charge = { 
    Amount:Money
    ApptId:AppointmentIdentifier option
    //CodeUnique -> Modifier1
    // ModCodeId -> remove
    // CodeId -> ProcedureCode CPT codes
    //Charge -> not nullable
    // Adjusted -> remove
    // Allowed -> remove
    // PayerId -> deprecated
    // ProviderId -> RenderingProviderId
    // ChargeDateTime -> rename 
    // Units -> decimal(4,1)
    // isautoinsert -> remove
    //total charge -> decimal(6,2)
    //isbundled .. adjudicatedcodeworkmanscomp -> remove
    //balance -> remove
    }
    
type PtPayment = |Payment of Money * PatientIdentifier * AppointmentIdentifier option
type Patient = {PatientId:PatientIdentifier; AccountId:AccountIdentifier}

type AccountType = 
    |PatientAcct of Patient list // empty is valid
    |PayerAcct of PayerIdentifier
    |SystemAcct // users can add/edit system accounts (our write off account,cash account/etc
    |ThirdParty 
    
type JournalEntryType = 
    |Charge of ChargeIdentifier
    |Payment of PaymentIdentifier
    |EraItemID of EraItemIdentifier
    
type Account = {Name:string;AccountType:AccountType;}   // must have account number different from ID column (?) 
type JournalEntry = {
                        Source:Account 
                        Destination:Account
                        Amount:decimal
                        Entered:DateTime 
                        UserId:UserIdentifier 
                        // begin derived/related
                        Type:JournalEntryType
                        ApptId:AppointmentIdentifier option
                        Note:string
                    }

//TODO: 
// 1. table changes
// 2. 
    // a.link patients to accounts ui
    // b. link patientInfo guarantor to parent patient
    //    1. give option on parent payer change to migrate child balance or keep at the same account
// 3. on payer create -> create account
// 
// 4 or 1. account chart -> ledger entries

// Guarantors -> Payers
//pending and review stages will become 1
// need ui to search for pt account, search for payer account
// config -> add payer will add account and payer
