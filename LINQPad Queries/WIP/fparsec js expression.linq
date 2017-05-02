<Query Kind="FSharpProgram">
  <NuGetReference>FParsec</NuGetReference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

// fparsec javascript

//let target = "C:\TFS\PracticeManagement\dev\PracticeManagement\Pm.Web\Scripts\extensions.js"


// fparsec practice

let target = // change this to use %devroot% or create %practicemanagement% ?
    @"D:\Projects\PracticeManagement\Source-dev-rewrite\PracticeManagement\Db\Schema Objects\Schemas\dbo\Tables\Payment.table.sql"
let test p str = 
    match run p str with
    |Success(result, _, _)   -> printfn "Success: %A" result
    |Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
// following http://trelford.com/blog/post/parsecsharp.aspx (http://fssnip.net/lf)
// this parser was originally based of a C# parser, modifying it for javascript
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
        // example: a + b
        | InfixOp of Expr * string * Expr
        // example: ++i
        | PrefixOp of string * Expr
        // example: i++
        | PostfixOp of Expr * string
        // example: boolExpr ? Expr<'t> : Expr<'t>
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
        let isIdentifierFirstChar c = isLetter c || c = '_' || c = '$'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '$'
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
    
    let pvalue = (pliteral |>> fun x -> Value(x)) <|> 
                 attempt pinvoke <|> attempt pvar
    
    type Assoc = Associativity
    
    let opp = OperatorPrecedenceParser<Expr,Expr,unit>()
    pexprimpl := opp.ExpressionParser
    let term = pvalue .>> ws <|> between (str_ws "(") (str_ws ")") pexpr
    opp.TermParser <- term
    let pexprInParens = (between (str_ws "(") (str_ws ")") pexpr)
    let pexprParenthesized = between (str_ws "(") (str_ws ")") pexpr
    // ternary is hard!
    // a non operator option : https://github.com/stephan-tolksdorf/fparsec/blob/master/Samples/FSharpParsingSample/FParsecVersion/parser.fs#L88-L93
    // a sample ternaryOperator option: https://github.com/stephan-tolksdorf/fparsec/blob/69dd75043a7d3f77b276b55f4830bb59947fcb97/Test/OperatorPrecedenceParserTests.fs#L267
    let pexpr2 = spaces >>. (pexprParenthesized <|> pexpr) .>> spaces
    let tern = TernaryOperator<Expr,Expr,unit>("?", pexpr2, ":", pexpr2,1, Associativity.Left, fun (condition:Expr) left right -> TernaryOp( condition, left, right))
    opp.AddOperator(tern)
    opp.MissingTernary2ndStringErrorFormatter <- fun (_, _, op, _) -> expected op.TernaryRightString
    // Statement blocks
    
    
open Parser
// start off slower with simple ternaries?
let ternSamples =[
    "false ? 0 : 1"
    "false ? b : a"
    "c?b:a"
    "condition ? true : false"
    "1 + 2 == 5 ? 1 : 0"
]
ternSamples
|> Seq.iter (run (spaces >>. pexpr .>> spaces) >> sprintf "%A\r\n\r\n" >> Dump >> ignore)
