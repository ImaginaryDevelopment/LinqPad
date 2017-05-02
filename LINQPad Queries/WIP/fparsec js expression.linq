<Query Kind="FSharpProgram">
  <NuGetReference>FParsec</NuGetReference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

// fparsec javascript
let dumpt t x = x.Dump(description=t)
let hoist f x = f x |> ignore; x
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
    type Expr = 
        | Value of Literal
        | Variable of VarName
        // arguments to methods can be expressions
        | MethodInvoke of MemberName * Expr list
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
    let ptrue = 
        str_ws "true" 
        |>> fun _ -> Literal(true)
        //<?> "true"
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
    let pinvoke =
        pidentifier_ws .>>.
        between (str_ws "(") (str_ws ")") (many pexpr)
        |>> fun (name,args) -> MethodInvoke(name,args)
    // should a method call really be included in 'pvalue' ?
    let pvalue = (pliteral |>> fun x -> Value(x)) <|> 
                 attempt pinvoke <|> attempt pvar
    
    type Assoc = Associativity
    
    let opp = OperatorPrecedenceParser<Expr,Expr,unit>()
    pexprimpl := ((spaces >>. opp.ExpressionParser .>> spaces) <|> (spaces >>. (between (str_ws "(") (str_ws ")") opp.ExpressionParser) .>> spaces))
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
type Railway<'T> = 
    |RSuccess of 'T
    |RFailure of string
let fSamples title p items = 
    items
    |> Seq.map(fun (x,expected) ->
        let r = 
            match run p x with
            |Success(result,_,_) -> 
                if result = expected then 
                    None
                else Some (RSuccess result)
            |Failure(errorMsg, _,_) -> RFailure errorMsg |> Some
        
        sprintf "%A\r\n" x, r
    )
    |> dumpt title
    |> ignore
    
open Parser
let valueSamples = [
    yield! [0..13] |> Seq.map (fun x -> string x, Ast.Value(Ast.Literal(box x)))
    yield "true", Ast.Value (Ast.Literal true)
    yield "false", Ast.Value (Ast.Literal false)
    // yield "'helloworld'", Ast.Value(
    yield "\"helloworld\"", Ast.Value (Ast.Literal "helloworld")
    // yield "' hello world'"
    yield "\"hello world\"", Ast.Value (Ast.Literal "hello world")
    yield "\" hello $%&^&* \"", Ast.Value(Ast.Literal " hello $%&^&* ")
    yield "hello(world)", Ast.MethodInvoke("hello",[Ast.Variable "world"])
]
valueSamples
|> fSamples "valueSamples" pvalue
//|> Seq.iter (run (spaces >>. pvalue .>> spaces) >> fOut "pvalue")
// start off slower with simple ternaries?
let exprLiteral (x:obj) = Ast.Value(Ast.Literal x)
let exprVar (x:string) = Ast.Variable x
let ternSamples = [
    // not valid C# perhaps, but valid js!
    "5 ? 1 : 0", Ast.TernaryOp(exprLiteral 5,exprLiteral 1,exprLiteral 0)
    //"1 + 2 == 5 ? 1 : 0", Ast.TernaryOp(Ast.Expr(Ast.MethodInvoke("+", [ Ast.Arg(Ast.Literal
    "false ? 0 : 1", Ast.TernaryOp(exprLiteral false, exprLiteral 0, exprLiteral 1)
    "false ? b : a", Ast.TernaryOp(exprLiteral false, exprVar "b", exprVar "a")
    "c?b:a", Ast.TernaryOp(exprVar "c", exprVar "b", exprVar "a")
    "condition ? true : false", Ast.TernaryOp(exprVar "condition", exprLiteral true, exprLiteral false)
]

//ternSamples
//|> Seq.iter (run (spaces >>. pexpr .>> spaces) >> fOut "tern as pexpr")
ternSamples
|> fSamples "pexpr:ternSamples" pexpr