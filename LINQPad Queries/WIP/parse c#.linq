<Query Kind="FSharpProgram">
  <NuGetReference>FParsec</NuGetReference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

// fparsec practice
// not working in linqpad see FsInteractive MacroRunner FParsecTry2.fsx
let target = // change this to use %devroot% or create %practicemanagement% ?
    @"D:\Projects\PracticeManagement\Source-dev-rewrite\PracticeManagement\Db\Schema Objects\Schemas\dbo\Tables\Payment.table.sql"
let test p str = 
    match run p str with
    |Success(result, _, _)   -> printfn "Success: %A" result
    |Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
// following http://trelford.com/blog/post/parsecsharp.aspx (http://fssnip.net/lf)
module Ast =
    // Base type abbreviations
    type Name = string
    type VarName = Name
    type TypeName = Name
    type MemberName = Name
    type LabelName = Name
    type Value = obj
    type Literal = Literal of Value
    // Expressions
    type ArgType = ValueArg | RefArg | OutArg
    type Expr = 
        | Value of Literal
        | Variable of VarName
        | MethodInvoke of MemberName * Arg list
        | PropertyGet of MemberName
        | Cast of TypeName * Expr
        | InfixOp of Expr * string * Expr
        | PrefixOp of string * Expr
        | PostfixOp of Expr * string
        | TernaryOp of Expr * Expr * Expr
    and Arg = Arg of ArgType * Expr
    // Statements
    type Define = Define of TypeName * VarName
    type Init = 
        | Assign of Name * (* =,+=, etc. *) Expr
        | Construct of TypeName * Name * Expr
    type Condition = Expr
    type Iterator = Expr
    type Statement =
        | Definition of Define
        | Assignment of Init
        | PropertySet of MemberName * Expr
        | Action of Expr
        //| Block/Scope of Statement list
        | If of Expr * Block
        | IfElse of Expr * Block * Block
        | Switch of Expr * Case list
        | For of Init list * Condition * Iterator list * Block
        | ForEach of Define * Expr * Block
        | While of Expr * Block
        | DoWhile of Block * Expr
        | Throw of Expr
        | Try of Block
        | Catch of TypeName * Block
        | Finally of Block
        | Lock of Expr * Block    
        | Using of Expr * Block
        | Label of LabelName
        | Goto of LabelName
        | Break
        | Continue
        | Return of Expr
        //| Directive of Name
    and Case = 
        | Case of Literal * Block
        | Default of Block
    and Block = Statement list
    // Modifiers
    type Access = Public | Private | Protected | Internal
    type Modifier = Static | Sealed | Override | Virtual | Abstract
    // Members
    type ReturnType = TypeName
    type MemberInfo = MemberInfo of Access * Modifier option * ReturnType * Name
    type IsReadOnly = bool
    type ParamType = ByValue | ByRef | Out | Params
    type Param = Param of ParamType * TypeName * VarName
    type PreConstruct = PreConstruct of Name * Param list
    type Member =
        | Field of Access * Modifier option * IsReadOnly * 
                   ReturnType * Name * Expr option
        | Property of MemberInfo * Block option * Block option
        | Method of MemberInfo * Param list * Block
        | Constructor of Access * Modifier option * Name * Param list * 
                         PreConstruct option * Block
    // Types
    type Members = Member list
    type Implements = Name list
    type EnumValue = EnumValue of Name * Value
    type CSharpType = 
        | Class of Access * Modifier option * Name * Implements * Members
        | Struct of Access * Name * Member list
        | Interface of Access * Name * Implements * Member list
        | Enum of Access * TypeName * EnumValue list
        | Delegate of Access * Name * ReturnType * Param list    
    // Namespace scopes
    type Import = 
        | Import of Name list
        | Alias of Name * Name list
    type NamespaceScope =
        | Namespace of Import list * Name list * NamespaceScope list
        | Types of Import list * CSharpType list
        
module Parser =  
    open Ast
    let maxCount = System.Int32.MaxValue
    let pcomment = pstring "//" >>. many1Satisfy ((<>) '\n') 
    let pspaces = spaces >>. many (spaces >>. pcomment >>. spaces)
    let pmlcomment = pstring "/*" >>. skipCharsTillString "*/" true (maxCount)
    
    let ws = pspaces >>. many (pspaces >>. pmlcomment >>. pspaces) |>> (fun _ -> ())
    let ws1 = spaces1
    let str_ws s = pstring s .>> ws
    let str_ws1 s = pstring s .>> ws1
    
    // Literals
    
    type Lit = NumberLiteralOptions
    let numberFormat = Lit.AllowMinusSign ||| Lit.AllowFraction ||| Lit.AllowExponent
    let pnumber : Parser<Literal, unit> =
        numberLiteral numberFormat "number"
        |>> fun nl ->
                if nl.IsInteger then Literal(int nl.String)
                else Literal(float nl.String)
    let ptrue = str_ws "true" |>> fun _ -> Literal(true)
    let pfalse = str_ws "false" |>> fun _ -> Literal(false)
    let pbool = ptrue <|> pfalse
    let pstringliteral =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        between (pstring "\"") (pstring "\"")
                (manyChars (normalChar <|> escapedChar))
        |>> fun s -> Literal(s)
    
    let pliteral = pnumber <|> pbool <|> pstringliteral
    
    // Expressions
    
    let pexpr, pexprimpl = createParserForwardedToRef ()
    
    let reserved = ["for";"do"; "while";"if";"switch";"case";"default";"break" (*;...*)]
    let pidentifierraw =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    let pidentifier =
        pidentifierraw 
        >>= fun s -> 
            if reserved |> List.exists ((=) s) then fail "keyword" 
            else preturn s
    let pidentifier_ws = pidentifier .>> ws
    let pvar = pidentifier |>> fun x -> Variable(x)
    
    let pargref = str_ws1 "ref" |>> fun _ -> RefArg
    let pargout = str_ws1 "out" |>> fun _ -> OutArg
    let pargtype = (opt pargref <|> opt pargout) 
                   |>> function Some x -> x | None -> ValueArg
    let parg = pargtype .>>. pexpr |>> fun (by,e) -> Arg(by,e)
    let pinvoke =
        pidentifier_ws .>>.
        between (str_ws "(") (str_ws ")") (many parg)
        |>> fun (name,args) -> MethodInvoke(name,args)
    
    let pcast =
        let ptypecast = between (str_ws "(") (str_ws ")") pidentifier_ws
        ptypecast .>>. pexpr |>> fun (name,e) -> Cast(name,e)
    
    let pvalue = (pliteral |>> fun x -> Value(x)) <|> 
                 attempt pinvoke <|> attempt pvar <|> attempt pcast
    
    type Assoc = Associativity
    
    let opp = OperatorPrecedenceParser<Expr,unit,unit>()
    pexprimpl := opp.ExpressionParser
    let term = pvalue .>> ws <|> between (str_ws "(") (str_ws ")") pexpr
    opp.TermParser <- term
    let inops = ["+";"-";"*";"/";"%"
                 "&&"; "||"; ">>"; "<<"; "&"; "|"; "^"
                 "==";"!=";"<=";">=";"<";">";"??"
                 "."]
    for op in inops do
        opp.AddOperator(InfixOperator(op, ws, 1, Assoc.Left, fun x y -> InfixOp(x, op, y)))
    let preops = ["-";"++";"--"]
    for op in preops do
        opp.AddOperator(PrefixOperator(op, ws, 1, true, fun x -> PrefixOp(op, x)))
    opp.AddOperator(PrefixOperator("new", ws1, 1, true, fun x -> PrefixOp("new", x)))
    let postops = ["++";"--"]
    for op in postops do
        opp.AddOperator(PostfixOperator(op, ws, 1, true, fun x -> PostfixOp(x, op)))
    
    let pexpr' = between (str_ws "(") (str_ws ")") pexpr
    
    // Statement blocks
    
    let pstatement, pstatementimpl = createParserForwardedToRef()
    let psinglestatement = pstatement |>> fun statement -> [statement]
    let pstatementblock =
        psinglestatement <|>
        between (str_ws "{") (str_ws "}") (many pstatement) 
    
    // Assignement statements
    
    let pdefine = pipe2 (pidentifier .>> ws1) (pidentifier)
                    (fun ty name -> Define(ty,name))
    
    let pdefinition = pdefine |>> fun d -> Definition(d)
    
    let passign = pipe3 pidentifier_ws (str_ws "=") pexpr
                   (fun var _ expr -> Assign(var,expr))
    
    let pconstruct = 
        pipe4
            (pidentifier .>> ws1)
            pidentifier_ws
            (str_ws "=")
            pexpr
            (fun ty name _ e -> Construct(ty, name, e))
    
    let passignment = attempt passign <|> attempt pconstruct |>> fun c -> Assignment(c)
    
    // Selection statements
    
    let pif =
        pipe2 (str_ws "if" >>. pexpr') pstatementblock
            (fun e block -> If(e,block))
    
    let pifelse =
        pipe3 (str_ws "if" >>. pexpr') pstatementblock (str_ws "else" >>. pstatementblock)
            (fun e t f -> IfElse(e,t,f))
    
    let pcase = str_ws1 "case" >>. pliteral .>> str_ws ":" 
    let pcaseblock = pipe2 pcase (many pstatement) (fun case block -> Case(case,block))
    let pdefault = str_ws "default" >>. str_ws ":" 
    let pdefaultblock = pdefault >>. (many pstatement) |>> fun block -> Default(block)
    let pcases' = many pcaseblock .>>. opt pdefaultblock 
                  |>> fun (cases,d) -> cases@(Option.toList d)
    let pcases = between (str_ws "{") (str_ws "}") pcases'
    
    let pswitch =
        pipe2 (str_ws "switch" >>. pexpr') pcases
            (fun e cases -> Switch(e, cases))
    
    // Iteration statements
    
    let pforargs =
        let pinit = attempt passign <|> attempt pconstruct
        pipe3 
            (sepBy pinit (str_ws ",") .>> str_ws ";")
            (pexpr .>> str_ws ";")
            (sepBy pexpr (str_ws ","))
            (fun from until steps -> from, until, steps)
    
    let pfor =
        pipe2 
            (str_ws "for" >>. between (str_ws "(") (str_ws ")") pforargs)
            pstatementblock
            (fun (inits,until,iterators) block -> For(inits,until,iterators,block))
    
    let pforeachargs =
        pipe3 pdefine (str_ws1 "in") pexpr
            (fun define _ collection -> define, collection)
    
    let pforeach =
        pipe2 (str_ws "foreach" >>. pforeachargs) pstatementblock
            (fun (define,collection) block -> ForEach(define,collection,block))
    
    let pwhile = 
        pipe2 (str_ws "while" >>. pexpr') pstatementblock
            (fun e block -> While(e,block))
    
    let pdowhile =
        pipe2
            (str_ws "do" >>. pstatementblock)
            (str_ws "while" >>. pexpr')
            (fun block e -> DoWhile(block, e))
    
    // Jump statements
    
    let preturn = str_ws1 "return" >>. pexpr |>> fun e -> Return(e)
    
    let pbreak = str_ws "break" |>> fun _ -> Break
    
    let pcontinue = str_ws "continue" |>> fun _ -> Continue
    
    let pgoto = str_ws1 "goto" >>. pidentifier_ws |>> fun label -> Goto(label)
    
    let plabel = 
        pidentifier_ws .>> str_ws ":" 
        |>> fun label -> Label(label)
    
    // Exception statements
    
    let pthrow = str_ws1 "throw" >>. pexpr |>> fun e -> Throw(e)
    
    let ptry = str_ws "try" >>. pstatementblock |>> fun block -> Try(block)
    
    let pfinally = str_ws "finally" >>. pstatementblock |>> fun block-> Finally(block)
    
    let pexception = between (str_ws "(") (str_ws ")") pidentifier_ws
    let pcatch = str_ws "catch" >>. pexception .>>. pstatementblock 
                 |>> fun (ex,block) -> Catch(ex, block)
    
    // Lock statement
    
    let plock = 
        str_ws "lock" >>. pexpr' .>>. pstatementblock 
        |>> (fun (e,block) -> Lock(e,block))
    
    // Statement implementation
    
    let paction = pexpr |>> fun e -> Action(e)
    
    pstatementimpl :=
        attempt (preturn .>> str_ws ";") <|>
        attempt (pbreak .>> str_ws ";") <|>
        attempt (pcontinue .>> str_ws ";") <|>
        attempt (pgoto .>> str_ws ";") <|>  
        attempt (pdefinition .>> str_ws ";") <|>
        attempt (passignment .>> str_ws ";") <|> 
        attempt (paction .>> str_ws ";") <|>
        attempt plabel <|>
        attempt pifelse <|> attempt pif <|> 
        attempt pswitch <|>
        attempt pfor <|> attempt pforeach <|>
        attempt pwhile <|> attempt pdowhile <|>
        attempt pthrow <|>
        attempt ptry <|> attempt pcatch <|> attempt pfinally
        attempt plock
    
    // Access
    
    let ppublic = str_ws1 "public" |>> fun _ -> Public
    let pprivate = str_ws1 "private" |>> fun _ -> Private
    let pprotected = str_ws1 "protected" |>> fun _ -> Protected
    let pinternal = str_ws1 "internal" |>> fun _ -> Internal
    let paccess = 
        opt (ppublic <|> pprivate <|> pprotected <|> pinternal)
        |>> (fun access -> defaultArg access Internal)
    
    // Modifiers
    
    let psealed = str_ws1 "sealed" |>> fun _ -> Sealed
    let pstatic = str_ws1 "static" |>> fun _ -> Static
    let pmodifier = psealed <|> pstatic
    
    // Parameters
    
    let pref = str_ws "ref" |>> fun _ -> ByRef
    let pout = str_ws1 "out" |>> fun _ -> Out
    let pparams = str_ws1 "params" |>> fun _ -> Params
    let pby = (opt pout <|> opt pref <|> opt pparams) 
              |>> function Some x -> x | None -> ByValue
    let pparam = 
        pipe3 pby pidentifier_ws pidentifier_ws 
          (fun by ty name -> Param(by,ty,name)) 
    let pparamlist= str_ws "(" >>. sepBy pparam (str_ws ",") .>> str_ws ")"
    
    // Members
    
    let pmemberinfo = 
        pipe4 paccess (opt pmodifier) pidentifier_ws pidentifier_ws
         (fun access modifier ty name -> MemberInfo(access,modifier,ty,name))
    
    let preadonly = str_ws1 "readonly"
    let pfieldpreamble =
        pipe3 paccess (opt pmodifier) (opt preadonly)
         (fun access modifier ro -> (access, modifier, Option.isSome ro))
    let pfield = 
        pipe4 pfieldpreamble pidentifier_ws pidentifier_ws (str_ws ";")
         (fun (access,modifier,ro) rt name _ -> Field(access, modifier, ro, rt, name, None))
    let pget = str_ws "get" >>. pstatementblock
    let pset = str_ws "set" >>. pstatementblock
    let ppropertyblock =
        between (str_ws "{") (str_ws "}") ((opt pget) .>>. (opt pset))
    let pproperty = 
        pipe2 pmemberinfo ppropertyblock
         (fun mi (gblock,sblock) -> Property(mi,gblock,sblock))
    let pmethod =
        pipe3 pmemberinfo pparamlist pstatementblock 
         (fun mi ps block -> Method(mi,ps,block))
    let pconstructor = 
        pipe5 paccess (opt pmodifier) pidentifier_ws pparamlist pstatementblock
         (fun access modifier name ps block ->
           Constructor(access, modifier, name, ps, None, block))
    
    let pmember = 
        attempt pfield <|> attempt pmethod <|> attempt pproperty <|> attempt pconstructor
    
    let pmembersblock = between (str_ws "{") (str_ws "}") (many pmember) 
                       |>> (fun members -> members)
    let penumblock = 
        between (str_ws "{") (str_ws "}") (sepBy pidentifier_ws (str_ws ","))
        |>> fun names -> names |> List.mapi (fun i name -> EnumValue(name,i))
    
    // Types
    
    let pclasspreamble =
        paccess .>>. (opt pmodifier) .>> (str_ws1 "class")
    let pimplements =
        opt (str_ws ":" >>. sepBy1 (pidentifier_ws) (str_ws ","))
        |>> function Some xs -> xs | None -> []
    let pclass = 
        pipe4 pclasspreamble pidentifier_ws pimplements pmembersblock
         (fun (access,modifier) name implements block -> 
            Class(access, modifier, name, implements, block))
    let pstruct =
        pipe4 paccess (str_ws1 "struct") pidentifier_ws pmembersblock
         (fun access _ name block -> Struct(access, name, block))
    let pinterface =
        pipe5 paccess (str_ws1 "interface") pidentifier_ws pimplements pmembersblock
         (fun access _ name implements block -> 
            Interface(access, name, implements, block))
    let penum =
        pipe4 paccess (str_ws1 "enum") pidentifier_ws penumblock
         (fun access _ name block -> Enum(access, name, block))
    let pdelegate =
        pipe5 paccess (str_ws1 "delegate") pidentifier_ws pidentifier_ws pparamlist
         (fun access _ ty name ps -> Delegate(access, ty, name, ps))
    
    let ptypedeclaration = 
        pclass <|> pstruct <|> pinterface <|> penum <|> pdelegate
    
    // Scopes
    
    let pnsscope, pscopeimpl = createParserForwardedToRef()
    let pnsscopesblock = between (str_ws "{") (str_ws "}") (many pnsscope)
    
    let pns = sepBy1 pidentifier_ws (str_ws ".")
    let palias = str_ws1 "using" >>. pidentifier_ws >>. str_ws "=" .>>. pns
                 |>> fun (alias,name) -> Alias(alias,name)
    let popen = str_ws1 "using" >>. pns |>> fun name -> Import(name)
    let pimport = (attempt popen <|> attempt palias) .>> str_ws ";"
    
    let pnsblock =
        pipe3 (many pimport) (str_ws1 "namespace" >>. pns) pnsscopesblock
         (fun imports name block -> 
            let types = Types([],[])
            Namespace(imports,name,block))
    
    let ptypes = 
        pipe2 (many pimport) (many1 ptypedeclaration)
         (fun imports classes -> Types(imports, classes))
    
    pscopeimpl := ws >>. (pnsblock <|> ptypes)

let text = """
        {
            new TableInfo
            {
                Name="Payment",
                Schema="dbo",
                Columns = new []
                {
                    new ColumnInfo{Name="PaymentID", Type = typeof(int), 
                        Attributes = new []{"identity","primary key"},
                        },
                    CreateFKeyedColumn<int>("AppointmentId", new FKeyInfo{Schema="dbo",Table="Appointments",Column="AppointmentId"}, /* allowNull= */ true),
                    new ColumnInfo
                    {
                        Name="PaymentTypeId", Type= typeof(string), Length=50,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentType",Column="PaymentTypeId" },
                        GenerateReferenceTable = true,
                        ReferenceValuesWithComment= new Dictionary<string,string>{
                            {"Patient",null},{"ThirdParty",null},
                            {"Era",null}
                            },
                        Comments= new[]{
                            "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                            }
                        },
                    new ColumnInfo{
                        Name="PaymentMethodId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentMethod"},
                        ReferenceValuesWithComment = new Dictionary<string,string>{
                            {"Cash",null},{"CC",null},{"Check",null},{"Ach",null},{"Fsa",null},{"Other","for when Era amount is 0 or a catch-all"}
                            },
                        },
                    new ColumnInfo{
                        Name="PaymentStatusId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentStatus"},
                        ReferenceValuesWithComment = new []{"New", "Partial", "Complete"}.ToDictionary(f=>f,f=> (string)null),
                        },
                    new ColumnInfo{
                        Name="TotalAmount", Type = typeof(decimal),
                        Precision=12,Scale=2, // see: http://stackoverflow.com/questions/2377174/how-do-i-interpret-precision-and-scale-of-a-number-in-a-database
                        Comments = new[]{ "was Amount (18,2)"}
                        },
                    CreateUserIdColumn(null, true, "null to allow system inserts/adjustments that aren't done by a user"),
                    CreateFKeyedColumn<int>("PayerID", new FKeyInfo{ Schema="dbo", Table="Payers" }, /* allowNull= */ true),
                    CreatePatientIdColumn(null, true,null),
                    new ColumnInfo{
                        Name="Created", Type = typeof(DateTime),
                        AllowNull=true,
                        Comments = new[]{ "was timestamp"}
                        },
                    new ColumnInfo{
                        Name="TransactionNumber", Type = typeof(string),
                        Length=30,
                        AllowNull=true,
                        Comments = new[]{ "was checkNumber now will store check number or ACH number (when applicable)"}
                        },
                    new ColumnInfo{
                        Name="Rcd", Type = typeof(DateTime),
                        Comments = new []{"Payment Recvd"},
                        AllowNull=true,
                        },
                    new ColumnInfo{
                        Name="IsElectronic", Type = typeof(bool),
                        },
                    CreateFKeyedColumn<int>("CCItemID", new FKeyInfo{ Schema="Accounts", Table="CCItem"},true),
                    new ColumnInfo{
                        Name="Comments", Type = typeof(string),
                        UseMax=true,
                        AllowNull=true,
                        },
                    }
                },
            new TableInfo{
                Schema="Accounts",
                Name="CCItem",
                Columns = new []
                {
                    new ColumnInfo{
                        Name="CCItemID", Type = typeof(int), Attributes = new []{"identity","primary key"},
                        },
                    MakeNullable50("ResponseCode"),
                    MakeNullable50("ResponseDescription"),
                    MakeNullable50("TransactionID"),
                    MakeNullable50("TransactionType"),
                    MakeNullable50("CardType"),
                    MakeNullable50("MaskedAcctNum"),
                    MakeNullable50("ExpDate"),
                    MakeNullable50("AcctNumSource"),
                    MakeNullable50("CardholderName"),
                    MakeNullable50("Alias"),
                    MakeNullable50("ProcessorResponse"),
                    MakeNullable50("BatchNum"),
                    MakeNullable50("BatchAmount"),
                    MakeNullable50("ApprovalCode"),
                    }
                },
            new TableInfo{
                Schema="Accounts",
                Name="PaymentItem",
                Columns = new []{
                    new ColumnInfo{ Name = "PaymentItemID", Type = typeof(int), Attributes = new []{"identity","primary key"}},
                    new ColumnInfo{ Name = "PaymentID", Type = typeof(int), FKey= new FKeyInfo{Schema="dbo",Table="Payment"}},
                    new ColumnInfo{ Name = "PaymentItemTypeId", Type = typeof(string), Length=50,
                        AllowNull=true,
                        GenerateReferenceTable=true, FKey= new FKeyInfo{Schema="Accounts", Table="PaymentItemType", Column="PaymentItemTypeId"},
                        ReferenceValuesWithComment = new []{"EraPayment", "EraAdjustment", "PtRespDeductible", "PtRespCoPay","PtRespCoIns","Other"}.ToDictionary(f => f, f => (string)null),
                    },
                    new ColumnInfo{
                        Name="PaymentTierId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        AllowNull=true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentTier",Column="PaymentTierId" },
                        ReferenceValuesWithComment = new []{"Primary", "Secondary", "Tertiary", "Worker'sComp"}.ToDictionary(f=>f,f=> (string)null),
                        },
                    new ColumnInfo{
                        Name="PtRespTypeId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        AllowNull=true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PtRespType",Column="PtRespTypeId" },
                        ReferenceValuesWithComment = new []{"Deductible", "CoIns", "CoPay"}.ToDictionary(f=>f,f=> (string)null),
                        },
                    new ColumnInfo{ Name = "Created", Type= typeof(DateTime)},
                    new ColumnInfo{ Name = "Amount", Type= typeof(decimal), Precision=8, Scale=2},
                    new ColumnInfo{ Name = "PatientResponsiblityAmt", Type = typeof(decimal), Precision=8, Scale=2},
                    CreateFKeyedColumn<int>("ChargeID", new FKeyInfo{Schema="dbo",Table="Charge"},true),
                    MakeNullable50("RemarkCode"),
                    MakeNullable50("AdjustmentCode"),
                    new ColumnInfo{ Name = "PaymentItemStatusId", Type = typeof(string), Length=50,
                        GenerateReferenceTable=true, FKey= new FKeyInfo{Schema="Accounts", Table="PaymentItemStatus"},
                        ReferenceValuesWithComment = new []{"Posted", "Unposted"}.ToDictionary(f=>f,f=> (string)null),
                    },
                    new ColumnInfo{
                        Name="Comments", Type = typeof(string),
                        UseMax=true,
                        AllowNull=true,
                    },
                }
            }
        }
"""
open Parser
run pnsscope text
|> sprintf "%A"
|> Dump
|> ignore