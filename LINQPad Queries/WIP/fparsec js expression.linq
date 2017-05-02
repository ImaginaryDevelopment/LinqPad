<Query Kind="FSharpProgram">
  <NuGetReference>FParsec</NuGetReference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

// fparsec javascript
// just expressions right now
// excluding es6, and '' strings for now

let dumpt t x = x.Dump(description=t)
let hoist f x = f x |> ignore; x

// fparsec practice
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
    
    // this should only be returning number literals, bool literals, strings
    type RLiteral = 
        // [(+|-)][digits][.digits][(E|e)[(+|-)]digits] per mdn
        |Number of obj 
        | Bool of bool 
        | StringLiteral of string 
        
    // Expressions
    and Expr = 
        // {abc:123}
        | ObjectLiteral of string
        // leaving this here for convience
        // regular expressions can be literals /ab+c/
        | RegexLiteral of string
        | RegularLiteral of RLiteral
        | Variable of VarName
        // arguments to methods can be expressions
        | MethodInvoke of MemberName * Expr list
        // examples: a.b; (a.b).c; a["b"]; a['b'];a[1])
        | PropertyGet of Expr * RLiteral
        // for a[Expr]
        | DynamicPropertyGet of Expr * Expr
        | Cast of TypeName * Expr
        // example: a + b
        | InfixOp of Expr * string * Expr
        // example: ++i
        | PrefixOp of string * Expr
        // example: i++
        | PostfixOp of Expr * string
        // example: boolExpr ? Expr<'t> : Expr<'t>
        | TernaryOp of Expr * Expr * Expr
    
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
    let punsignedint = 
        numberLiteral Lit.None "punsignedint"
        |>> fun nl ->
            int nl.String
    let numberFormat = Lit.AllowMinusSign ||| Lit.AllowFraction ||| Lit.AllowExponent
    let pnumber : Parser<RLiteral, unit> =
        numberLiteral numberFormat "number"
        |>> fun nl ->
            if nl.IsInteger then RLiteral.Number(int nl.String)
            else RLiteral.Number(float nl.String)
                    
    let ptrue = 
        str_ws "true" 
        |>> fun _ -> RLiteral.Bool(true)
        //<?> "true"
    let pfalse = str_ws "false" |>> fun _ -> RLiteral.Bool(false)
    let pbool = ptrue <|> pfalse
    type EscapedChar = 
        |Reescape of char
        |Plain of char
    let pstringliteral =
        let normalChar enclosure = satisfy (fun c -> c <> '\\' && c <> enclosure)
        // add back the backslash for n r t, leave it out for \\ and \"
        let escapes = 
            [
                yield! [ 
                    'n'
                    'r'
                    't'
                    'b'
                    'f'
                    'v'
                ] |> Seq.map Reescape
                yield! [
                    '\\'
                    '\''
                    '"'
                ] |> Seq.map Plain
            ]
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let latin1 = 
            pstring "\\" >>. punsignedint
            |>> (fun x -> 
                if 0 <= x && x <=377 then
                    sprintf "\\%i" x
                else failwithf "bad latin number"
            )
        //where/how do we handle multi-char escapes?
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"0bf" |>> unescape)
        let stringLiteral escapee = (manyChars (normalChar escapee <|> escapedChar)) <|> latin1
        let stringLiteral2 escapee =
            let strL = pstring (string escapee)
            between strL strL (stringLiteral escapee)
        stringLiteral2 '"' 
        <|> stringLiteral2 '\''
        |>> RLiteral.StringLiteral
    
    let pliteral = pnumber <|> pbool <|> pstringliteral
    
    // Expressions
    // should this be named pOperations? I'm not sure why it is based on a OperatorPrecedenceParser
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
    let pinvoke =
        pidentifier_ws .>>.
        between (str_ws "(") (str_ws ")") (many pexpr)
        |>> fun (name,args) -> MethodInvoke(name,args)
    // should a method call really be included in 'pvalue' ?
    let pvalue = (pliteral |>> fun x -> Ast.RegularLiteral x) <|> 
                 attempt pinvoke <|> attempt pvar
    
    type Assoc = Associativity
    
    let opp = OperatorPrecedenceParser()
    // Operator Precedence Parser apparently handles trailing spaces, and surrounding something with () per http://www.quanttec.com/fparsec/reference/operatorprecedenceparser.html
    pexprimpl := opp.ExpressionParser
    let term = pvalue .>> ws <|> between (str_ws "(") (str_ws ")") pexpr // (pexpr <|> between (str_ws "(") (str_ws ")") pexpr) .>> ws
    opp.TermParser <- term

    // ternary is hard!
    // a non operator option : https://github.com/stephan-tolksdorf/fparsec/blob/master/Samples/FSharpParsingSample/FParsecVersion/parser.fs#L88-L93
    // a sample ternaryOperator option: https://github.com/stephan-tolksdorf/fparsec/blob/69dd75043a7d3f77b276b55f4830bb59947fcb97/Test/OperatorPrecedenceParserTests.fs#L267
    // better sample? https://github.com/stephan-tolksdorf/fparsec/blob/69dd75043a7d3f77b276b55f4830bb59947fcb97/Test/OperatorPrecedenceParserTests.fs#L360
    let posWS = getPosition .>> ws
    let tern = TernaryOperator("?", posWS, ":", posWS,1, Associativity.Left, fun (condition:Expr) left right -> TernaryOp( condition, left, right))
    opp.AddOperator(tern)
    opp.MissingTernary2ndStringErrorFormatter <- fun (_, _, op, _) -> expected op.TernaryRightString
    // Statement blocks
type Railway<'T> = 
    |RSuccess of 'T
    |RFailure of string

let getUnionCaseName (x:'A) = 
    match FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'A>) with
    | case, _ -> case.Name
type ComparisonAssertion = { Expected: Ast.Expr; ExpectedRaw : string; Input:string}
type Assertion = 
    |Equals of ComparisonAssertion
    |NotEquals of ComparisonAssertion
    |ShouldFail

let fSamples title p items = 
    items
    |> Seq.map(fun (x,assertion) ->
        // temporarily return all everything on a failure
        let er,input,r = // make it effectiveInput, (RailWay<Success,FailureMsg> option)
            match assertion, run p x with
            |Equals ca, Success(result,_,_) when result = ca.Expected -> 
            
                (ca.ExpectedRaw, ca.Input, None)
            |NotEquals ca, Success(result,_,_) when result <> ca.Expected -> 
                (ca.ExpectedRaw, ca.Input, None)
            |ShouldFail, Failure _ ->
                null, null, None
            |_, Failure(errorMsg, _,_) -> null,null, (RFailure errorMsg |> Some)
            |ShouldFail, Success(result,_,_) -> null, null, RSuccess(result) |> Some
            |Equals ca, Success(result,_,_)
            |NotEquals ca, Success(result,_,_) ->
                ca.ExpectedRaw, ca.Input, RSuccess result |> Some
        
        x,er,getUnionCaseName assertion,input, r
    )
    |> List.ofSeq
    |> List.rev
    |> dumpt title
    |> ignore
    
open Parser

let valueSamples = 
    let quote = sprintf "'%s'"
    let quotes = sprintf "\"%s\""
    let rl x = Ast.RegularLiteral x
    [
    yield! [
        yield! [0..13] |> Seq.map (fun x -> string x, None, (Ast.RLiteral.Number(box x)))
        yield "true", None, (Ast.RLiteral.Bool true)
        yield "false", None, (Ast.RLiteral.Bool false)
        yield! 
            [
                yield "helloworld"
                
                yield " hello world"
                yield "hello world"
                yield " hello $%&^&* "
                // Latin-1 encoding 0-377 are valid
                yield "\\123"
                yield "\\0"
                // multi-line strings are allowed
                yield "\"this string \\\r\nis broken"
            ] 
            |> Seq.map (fun x ->
                [
                    if x.Contains "'" then
                        yield x |> replace "'" @"\'" |> quote
                    else 
                        yield x |> quote
                    if x.Contains "\"" then
                        yield x |> replace "\"" "\\\"" |> quotes
                    else yield x |> quotes
                ] |> Seq.map (fun quotedIdent -> x,quotedIdent)
            )
            |> Seq.concat
            |> Seq.map (fun (x:string,expectedRaw:string) -> expectedRaw, Some x,  Ast.RLiteral.StringLiteral x)
            
        
        yield "3.1415926", None, (Ast.RLiteral.Number 3.1415926)
        yield "-.123456789", None, (Ast.RLiteral.Number -0.123456789)
        yield "-3.1E+12", None, (Ast.RLiteral.Number -3.1E+12)
        yield ".1e-23", None, (Ast.RLiteral.Number 0.1e-23)
        
    ] |> Seq.map (fun (x,erOpt, exp) -> 
        x,Equals {  Expected= rl exp
                    ExpectedRaw=(match erOpt with |Some er -> er | None -> x)
                    Input=x})
    yield! [
        yield "hello(world)", Ast.MethodInvoke("hello",[Ast.Variable "world"])
    ] |> Seq.map (fun (x,exp) -> x,Equals {Expected= exp; ExpectedRaw=x;Input=x})
    // non generated quote cases
    // '\0' should become \0
    // "\0" should become \0
    yield "'\\0'", Equals {Expected= Ast.RLiteral.StringLiteral "\\0" |> rl; ExpectedRaw= "'\\0'"; Input="'\\0'"}
    yield "\"\\0\"", Equals {Expected= Ast.RLiteral.StringLiteral"\\0" |> rl; ExpectedRaw="\\\\\"\\0\\\\\""; Input= "\"\\0\""}
    yield "'\\0'", Equals {Expected= Ast.RLiteral.StringLiteral "\\0" |> rl; ExpectedRaw="'\\0'"; Input= "'\\0'"}
    yield! [
        yield "0e"
        yield "0a"
        yield "1a"
        
    ] |> Seq.map (fun x -> x,ShouldFail)
]
valueSamples
|> fSamples "valueSamples" pvalue
//|> Seq.iter (run (spaces >>. pvalue .>> spaces) >> fOut "pvalue")
// start off slower with simple ternaries?
let exprNumber x = Ast.Expr.RegularLiteral (Ast.RLiteral.Number x)
let exprBool x = Ast.Expr.RegularLiteral (Ast.RLiteral.Bool x)
let exprVar x = Ast.Variable x
let ternSamples = [
    yield! [
        // not valid C# perhaps, but valid js!
        "5 ? 1 : 0", Ast.TernaryOp(exprNumber 5,exprNumber 1,exprNumber 0)
        "5?1:0", Ast.TernaryOp(exprNumber 5,exprNumber 1,exprNumber 0)
        "5 ?1:0", Ast.TernaryOp(exprNumber 5,exprNumber 1,exprNumber 0)
        "5?1 :0", Ast.TernaryOp(exprNumber 5,exprNumber 1,exprNumber 0)
        "5?1: 0", Ast.TernaryOp(exprNumber 5,exprNumber 1,exprNumber 0)
        //"1 + 2 == 5 ? 1 : 0", Ast.TernaryOp(Ast.Expr(Ast.MethodInvoke("+", [ Ast.Arg(Ast.Literal
        "false ? 0 : 1", Ast.TernaryOp(exprBool false, exprNumber 0, exprNumber 1)
        "false ? b : a", Ast.TernaryOp(exprBool false, exprVar "b", exprVar "a")
        "c?b:a", Ast.TernaryOp(exprVar "c", exprVar "b", exprVar "a")
        "condition ? true : false", Ast.TernaryOp(exprVar "condition", exprBool true, exprBool false)
        "hello(world) ? world : false", Ast.TernaryOp(Ast.MethodInvoke("hello",[exprVar "world"]), exprVar "world", exprBool false)
    ] |> Seq.map (fun (x,exp) -> x,Equals {Expected= exp;ExpectedRaw=x;Input=x})
]

//ternSamples
//|> Seq.iter (run (spaces >>. pexpr .>> spaces) >> fOut "tern as pexpr")
ternSamples
|> fSamples "pexpr:ternSamples" pexpr

