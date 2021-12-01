<Query Kind="FSharpProgram" />

open System

let random = new Random()
let flip f x y = f y x

type UnaryOps =
    | Abs
    with static member unary = function |"ABS" -> Abs | x -> failwithf "Invalid Unary '%s" x
    
type BinomialOps =
    | Add
    | Multiply
    | Divide
    | Choose
with
    static member binomial =
        function
        | "+" -> Add
        | "*" -> Multiply
        | "/" -> Divide
        | "choose" -> Choose
        | x -> failwithf "Invalid Binomial '%s" x
        
type expr =
    | CstI of int
    | Prim1 of UnaryOps * expr
    | PrimX of BinomialOps * expr list
    
type CstEval<'t> = int -> 't
type OpEval1<'t> = UnaryOps -> int -> 't   
type OpEvalX<'t> = BinomialOps -> int list -> 't

type Eval1<'t> = UnaryOps -> expr -> 't
type EvalX<'t> = BinomialOps -> expr list -> 't
type Eval<'t> = expr -> 't

type Cata<'t> = CstEval<'t> -> OpEval1<'t> -> OpEvalX<'t> -> Eval<'t>

type IExprBuilder<'t> =
    abstract member Bind: expr -> 't
    abstract member Return: int -> 't
    abstract member ReturnFrom: expr -> 't
// monads?
//type OpEvalOpt<'t> = expr -> 't option

[<NoComparison;NoEquality>]
type Evaluator<'t> = 
    {
        // building blocks
        OpEval1 : OpEval1<'t>
        OpEvalX : OpEvalX<'t>
        //Wrap : int -> 't
        //Unwrap: 't -> int
        // full handlers
        Eval: Eval<'t>
        // derived, but here to force us to code it anyway
        //Cata: Cata<'t>
        //BuildEval1: IExprBuilder<'t> -> 
        //BuildEvalX: IExprBuilder<'t> -> OpEvalX<'t>
    }
//with
//    member x.Eval:Eval<'t> =
//        function
//        | CstI i -> x.Wrap i
//        | Prim1(op,e) -> 
//            let v = x.Eval e
//            x.OpEval1 op v
            
    
type IdentityEvaluator<'t> = Evaluator<'t>    
type OptionEvaluator<'t> = Evaluator<'t option>
type SetEvaluator<'t when 't : comparison > = Evaluator<Set<'t>>
        
module Ops =
    let rec cata fcst fprim1 fprimx =
        let recurse = cata fcst fprim1 fprimx
        function
        | CstI x -> fcst x
        | Prim1(op,e) ->
            let v = recurse e
            fprim1 op v
        | PrimX(op,es) ->
            let vs = es |> List.map recurse
            fprimx op vs
        
    let opEval1 op (v1:int) =
        match op with
        | Abs -> Math.Abs v1
    let rec opEvalX op vs =
        let opEval op v1 v2 =
            match op with
            | BinomialOps.Add -> v1 + v2
            | BinomialOps.Multiply -> v1 * v2
            | BinomialOps.Divide -> v1 / v2
            | BinomialOps.Choose -> if random.NextDouble() > 0.5 then v1 else v2
        match vs with
        | [] -> 0
        | h :: [] -> h
        | h :: tl ->
            (h,tl)
            ||> List.fold(opEval op)
    let rec eval e =
        match e with
        | CstI i -> i
        | Prim1(op,e) ->
            let v = eval e
            opEval1 op v
        | PrimX (op,es) ->
            let vs = es |> List.map eval
            opEvalX op vs
            
let eval =
    {
        OpEval1 = Ops.opEval1
        OpEvalX = Ops.opEvalX
        Eval = Ops.eval
        //Cata = Ops.cata
    }
    
module IdentityE =         
    type IdentityBuilder() =
        member this.Bind(x, f) = f x
        member this.Return x = x // identity builder is a simple identity so no actual wrapper
        member this.ReturnFrom x = x
        member this.For(xs,f) =
            this.Bind(xs,f)
    let identM = IdentityBuilder()
    
    let opEval1 op v =
        identM { 
                 return! Ops.opEval1 op v }
    let opEvalX op vs =
        identM {
            return! Ops.opEvalX op vs
        }
        
    
    let rec eval e : int =
        match e with
        | CstI i -> identM { return i }
        | Prim1(op, e1) ->
            identM { let! v1 = eval e1
                     return! Ops.opEval1 op v1 }
        | PrimX(op, es) ->
            let vs = es |> List.map(fun e ->
                identM{
                    return! eval e
                }
            )
            identM  {  return! Ops.opEvalX op vs }
    let eval: IdentityEvaluator<_> =
        {
            OpEval1 = opEval1
            OpEvalX = opEvalX
            Eval = eval
            //Cata = 
        }
module OptionE =
    let opEval1:OpEval1<int option> = fun op v ->
        match op with
        | Abs -> Ops.opEval1 Abs v |> Some
    let opEvalX:OpEvalX<int option> = fun op vs ->
        match op,vs with
        | _, (h::[]) -> h |> Some
        | Divide, vs when vs.[1..] |> List.exists((=) 0) -> None 
        | _,vs -> Ops.opEvalX op vs |> Some
    let rec eval =
        function
        | CstI i -> Some i
        | Prim1(op,e) ->
            let vOpt = eval e
            vOpt |> Option.bind (opEval1 op)
        | PrimX(op,es) ->
            match es |> List.choose eval with
            | vs when vs.Length = es.Length -> opEvalX op vs
            | _ -> None
             
            
    
    
    let eval: OptionEvaluator<int>=
        {
            OpEval1 = opEval1
            OpEvalX = opEvalX
            Eval = eval
        }

    // Evaluator that may fail, return type: int option
    type OptionBuilder() =
        member this.Bind(x, f) =
            match x with
            | None   -> None
            | Some v -> f v
        member this.Return x = Some x
        member this.ReturnFrom x = x
        
    
    let opEvalOptX op vs =
        match vs with
        | [] -> None
        | v::[] -> Some v
        | v1Opt::vs ->
            (Some v1Opt,vs)
            ||> List.fold(fun vOpt v2 ->
                match op,vOpt, v2 with
                | Divide,_,0 -> None
                | _, Some v1, _ -> Ops.opEvalX op [v1;v2] |> Some
                | _, None, _ -> None
            )

     
    let optionM = OptionBuilder()

    let rec eval e : int option =
        match e with
        | CstI i -> optionM { return i }
        | Prim1(op,e1) ->
            optionM { let! v1 = eval e1
                      return Ops.opEval1 op v1 }
        | PrimX(op, es) ->
            let vOpts = es |> List.map(fun e ->
                optionM {
                    return! eval e
                }
            )
            match vOpts |> List.choose id with
            | vs when vOpts.Length = vs.Length ->
                optionM {  return! opEvalOptX op vs }
            | _ -> None
    ()

()
module SetE =
    let opEvalSet op v1 v2 : int Set =
        OptionE.opEvalOpt2 op v1 v2
        |> Option.map Set.singleton
        |> Option.defaultValue Set.empty
    let opEvalSet1 op (v1: int) : int Set =
        opEval1 op v1
        |> Set.singleton

    let rec setEval e : int Set =
        match e with
        | CstI i -> Set [i]
        | Prim1(op, e1) ->
            let s1 = setEval e1
            let yss = Set.map (opEvalSet1 op) s1
            Set.unionMany yss
        | Prim2(op, e1, e2) ->
            let s1 = setEval e1
            let yss = Set.map (fun v1 ->
                               let s2 = setEval e2
                               let xss = Set.map (fun v2 -> opEvalSet op v1 v2) s2
                               Set.unionMany xss)
                              s1
            Set.unionMany yss


    let setFlatMap (f : 'a -> 'b Set) (x : 'a Set) : 'b Set =
        Set.unionMany (Set.map f x)

    type SetBuilder() =
        member this.Bind(x, f) =
            Set.unionMany (Set.map f x)
        member this.Return x = Set [x]
        member this.ReturnFrom x = x
     
    let setM = SetBuilder()

    let rec setEval3 e : int Set =
        match e with
        | CstI i -> setM { return i }
        | Prim1(op, e1) ->
            setM  { let! v1 = setEval3 e1
                    return! opEvalSet1 op v1}
        | Prim2(op, e1, e2) ->
            setM { let! v1 = setEval3 e1
                   let! v2 = setEval3 e2
                   return! opEvalSet op v1 v2 }


// ------------------------------------------------------------

// Evaluator that records sequence of operators used,
// return type: int trace
type 'a Traceable = string list * 'a
module TraceableE =
    let getOp1Trace =
        function
        | Abs -> "Abs"
    let getOp2Trace =
        function
        | Add -> "+"
        | Multiply -> "*"
        | Divide -> "/"
        | Choose -> "choose"
        
    let opEvalTrace1 op (v1: int) : int Traceable =
        [getOp1Trace op], opEval1 op v1
        
    let opEvalTrace2 op v1 v2 : int Traceable =
        [getOp2Trace op], opEval2 op v1 v2
    let opEvalTrace3 op v1 v2 v3 : int Traceable =
        let result = opEval2 op v1 v2
        let result = opEval2 op result v3
        [getOp2Trace op],result

    let rec traceEval e : int Traceable =
        match e with
        | CstI i -> ([], i)
        | Prim1(op, e1) ->
            let trace1, v1 = traceEval e1
            let trace2, res = opEvalTrace1 op v1
            (trace1 @ trace2, res)
        | Prim2(op, e1, e2) ->
            let trace1, v1 = traceEval e1
            let trace2, v2 = traceEval e2
            let trace3, res = opEvalTrace2 op v1 v2
            (trace1 @ trace2 @ trace3, res)

    let traceFlatMap (f : 'a -> 'b Traceable) (x : 'a Traceable) : 'b Traceable =
        let trace1, v = x
        let trace2, res = f v
        (trace1 @ trace2, res)

    type TraceBuilder() =
        member this.Bind(x, f) =
            let trace1, v = x
            let trace2, res = f v
            (trace1 @ trace2, res)
        member this.Return x = ([], x)
        member this.ReturnFrom x = x
     
    let traceM = TraceBuilder()

    let rec traceEval3 e : int Traceable =
        match e with
        | CstI i -> traceM { return i }
        | Prim1(op, e1) ->
        traceM { let! v1 = traceEval3 e1
                 return! opEvalTrace1 op v1}
        | Prim2(op, e1, e2) ->
            traceM { let! v1 = traceEval3 e1
                     let! v2 = traceEval3 e2
                     return! opEvalTrace2 op v1 v2 }
        | Prim3(op,e1,e2,e3) ->
            traceM { let! v1 = traceEval3 e1
                     let! v2 = traceEval3 e2
                     let! v3 = traceEval3 e3
                     return! opEvalTrace3 op v1 v2 v3
                 }
            

// ------------------------------------------------------------

let expr1 = Prim2(BinomialOps.binomial "+", CstI(7), Prim2(BinomialOps.binomial"*", CstI(9), CstI(10)))
let expr2 = Prim2(BinomialOps.binomial"+", CstI(7), Prim2(BinomialOps.binomial"/", CstI(9), CstI(0)))
let expr3 = Prim2(BinomialOps.binomial"+", CstI(7), Prim2(BinomialOps.binomial"choose", CstI(9), CstI(10)))
let expr4 = Prim2(BinomialOps.binomial"choose", CstI(7), Prim2(BinomialOps.binomial"choose", CstI(9), CstI(13)))
let expr5 = Prim2(BinomialOps.binomial"*", expr4, Prim2(BinomialOps.binomial"choose", CstI(2), CstI(3)))


let expr10 = Prim1(UnaryOps.unary "ABS", Prim2(BinomialOps.binomial"+", CstI(7), Prim2(BinomialOps.binomial"*", CstI(-9), CstI(10))))
let expr11 = Prim1(UnaryOps.unary "ABS", Prim2(BinomialOps.binomial"+", CstI(7), Prim2(BinomialOps.binomial"/", CstI(9), CstI(0))))
let expr12 = Prim2(BinomialOps.binomial"+", CstI(7), Prim2(BinomialOps.binomial"choose", Prim1(UnaryOps.unary"ABS", CstI(-9)), CstI(10)))
let expr13 = Prim3(Divide, CstI 10, CstI 5, CstI 2)
let expr14 = PrimX(Add, [CstI 3; CstI 4; CstI 5])

let rec toPrimX =
    function
    | CstI i -> CstI i
    | Prim1 (op,x) -> Prim1(op,toPrimX x)
    | Prim2(op,x1,x2) -> PrimX(op, [x1;x2])
    | Prim3(op,x1,x2,x3) -> PrimX(op,[x1;x2;x3])
    | PrimX(op,vs) -> PrimX(op,vs)

[
    OptionE.optEval>> sprintf "Opt:%A" // ignore<int option>
    SetE.setEval>>sprintf "Set: %A" //ignore<int Set>
    TraceableE.traceEval>>sprintf "Trace:%A" // ignore<int Traceable>
]
|> Seq.map(fun f ->
    fun e ->
        try
            f e
        with ex -> ex.Message
)
|> Seq.collect(fun f ->
    [   expr10;expr11;expr12
        expr13;expr14]
    |> List.map f
)
|> List.ofSeq
|> Dump
|> ignore
//[
//    expr10
//    expr11
//    expr12
//]
//|> List.iter (OptionE.optEval>>ignore<int option>)

//[
//    expr10
//    expr11
//    expr12
//]
//|> List.iter (SetE.setEval>>ignore<int Set>)


//[
//    expr10
//    expr11
//    expr12
//]
//|> List.iter(TraceableE.traceEval>>ignore<int Traceable>)
//