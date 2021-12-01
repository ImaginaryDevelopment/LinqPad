<Query Kind="FSharpProgram" />


open System

type UnaryOps =
    | Abs
    with static member unary = function | "ABS" -> Abs | x -> failwithf "Invalid Unary '%s" x
    
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
    | Prim2 of BinomialOps * expr * expr
    | Prim3 of BinomialOps * expr * expr * expr
    | PrimX of BinomialOps * expr list
    
let f3ToX f op e1 e2 e3 =
    f op [e1;e2;e3]
    
//type Evaluator<'t> = {
//    OpEval1: UnaryOpts -> int -> 't
//    OpEvalX: BinomialOps -> int list -> 't
//    Eval: expr -> 't
//}
//type OptionEvaluator<'t> = Evaluator<Option<'t>>
//
//let optEval:OptionEvaluator<'t> = {
//    
//}

let opEval1 op (v1:int) =
    match op with
    | Abs -> Math.Abs v1
    
let random = new Random()
let flip f x y = f y x

let opEval2 op v1 v2 =
    match op with
    | BinomialOps.Add -> v1 + v2
    | BinomialOps.Multiply -> v1 * v2
    | BinomialOps.Divide -> v1 / v2
    | BinomialOps.Choose -> if random.NextDouble() > 0.5 then v1 else v2
    
let opEval3 op v1 v2 v3 = opEval2 op v1 v2 |> fun v2' -> opEval2 op v2' v3


module CatE =    
        
    //type 'a Traceable = string list * 'a
    let rec cata fcst fprim1 fprim2 fprim3 fprimx item =
        let inline recurse x = cata fcst fprim1 fprim2 fprim3 fprimx x
        match item with
        | CstI i -> fcst i
        | Prim1(op,e) ->
            let v = recurse e
            fprim1 op v
        | Prim2(op,e1,e2) ->
            let v1,v2 = recurse e1, recurse e2
            fprim2 op v1 v2
        | Prim3(op,e1,e2,e3) ->
            let v1,v2,v3 = recurse e1, recurse e2, recurse e3
            fprim3 op v1 v2 v3
        | PrimX(op,es) ->
            let vs = es |> List.map recurse
            fprimx op vs
    //let eval : expr -> _ = cata id opEval1 opEval2 opEval3 
    let optEval : expr -> int option =
        let opEval2' op v1 v2 : int option =
            match op,v2 with
            | Divide, 0 -> None
            | _ -> opEval2 op v1 v2 |> Some
            
        let opEvalOpt2 op o1 o2 =
            match o1,o2 with
            | Some v1, Some v2 -> opEval2' op v1 v2
            | _ -> None
            
        let opEvalOptX op (os:int option list) =
            match os |> List.choose id with
            | [] -> None
            | h :: [] -> Some h
            | h::tl when tl.Length + 1 = os.Length ->
                (Some h,tl)
                ||> List.fold(fun state v -> state |> Option.bind (opEval2' op v))
            | _ -> None
            
        cata Some (fun op -> Option.map (opEval1 op)) opEvalOpt2 (fun o e1 e2 e3 -> opEvalOptX o [e1;e2;e3]) opEvalOptX
    let setEval:expr -> int Set =
        let opEvalSet2 op s1 s2 =
            s1 |> Set.map (fun v1 -> s2 |> Set.map(opEval2 op v1)) |> Set.unionMany
        let opEvalSetX op (ss:int Set list) =
            match ss with
            | [] -> Set.empty
            | h :: [] -> h
            | h::tl ->
                let result =
                    (h,tl)
                    ||> List.fold(opEvalSet2 op)
                result
        cata Set.singleton (fun op -> Set.map (opEval1 op)) opEvalSet2 (f3ToX opEvalSetX) opEvalSetX
    //let traceEval =
    //    let opEvalTrace2 op t1 t2 =
    //        
    //    cata
    let rec cataX fcst fprim1 fprimX item =
        let inline recurse x = cataX fcst fprim1 fprimX x
        match item with
        | CstI i -> fcst i
        | Prim1(op,e) ->
            let v = recurse e
            fprim1 op v
        | Prim2(op,e1,e2) ->
            recurse (PrimX(op,[e1;e2]))
        | PrimX(op,es) ->
            let vs = es |> List.map recurse
            fprimX op vs
// Plain evaluator, return type int

let rec eval1 e : int =
    match e with
    | CstI i -> i
    | Prim1(op, e1) ->
        let v1 = eval1 e1
        opEval1 op v1
    | Prim2(op, e1, e2) ->
        let v1 = eval1 e1
        let v2 = eval1 e2
        opEval2 op v1 v2
    | Prim3(op,e1,e2,e3) ->
        let v1 = eval1 e1
        let v2 = eval1 e2
        let v3 = eval1 e3
        opEval3 op v1 v2 v3
    | PrimX(op,es) ->
        match es |> List.map eval1 with
        | [] -> 0
        | v::[] -> v
        | v::tl ->
            (v,tl)
            ||> List.fold(fun v1 v2 ->
                opEval2 op v1 v2
            )

let rec eval2 e : int =
    match e with
    | CstI i -> i
    | Prim1(op, e1) ->
        let v1 = eval2 e1
        opEval1 op v1
    | Prim2(op, e1, e2) ->
        let v1 = eval2 e1
        let v2 = eval2 e2
        opEval2 op v1 v2

type IdentityBuilder() =
    member this.Bind(x, f) = f x
    member this.Return x = x
    member this.ReturnFrom x = x

let identM = IdentityBuilder();;

let rec eval3 e : int =
    match e with
    | CstI i -> identM { return i }
    | Prim1(op, e1) ->
        identM { let! v1 = eval3 e1
                 return! opEval1 op v1 }
    | Prim2(op, e1, e2) ->
        identM  { let! v1 = eval3 e1
                  let! v2 = eval3 e2
                  return! opEval2 op v1 v2 }
                  

module OptionE = // Evaluator that may fail, return type: int option
    let opEvalOpt1 op (v1: int) : int option = opEval1 op v1 |> Some
    let opEvalOpt2 op v1 v2 : int option =
        match op,v2 with
        | Divide,0 -> None
        | _ -> opEval2 op v1 v2 |> Some
        
    let rec optEval e : int option =
        match e with
        | CstI i -> Some i
        | Prim1(op, e1) ->
            optEval e1
            |> Option.bind(opEvalOpt1 op)
        | Prim2(op, e1, e2) ->
            match optEval e1, optEval e2 with
            | Some v1, Some v2 ->
                opEvalOpt2 op v1 v2
            | _ -> None

    let optionFlatMap (f : 'a -> 'b option) (x : 'a option) : 'b option =
        match x with
        | None   -> None
        | Some v -> f v

    type OptionBuilder() =
        member this.Bind(x, f) =
            match x with
            | None   -> None
            | Some v -> f v
        member this.Return x = Some x
        member this.ReturnFrom x = x
     
    let optionM = OptionBuilder()

    let rec optionEval3 e : int option =
        match e with
        | CstI i -> optionM { return i }
        | Prim1(op,e1) ->
            optionM { let! v1 = optionEval3 e1
                      return! opEvalOpt1 op v1 }
        | Prim2(op, e1, e2) ->
            optionM { let! v1 = optionEval3 e1
                      let! v2 = optionEval3 e2
                      return! opEvalOpt2 op v1 v2 }
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
let expr4 = Prim2(BinomialOps.binomial "choose", CstI(7), Prim2(BinomialOps.binomial"choose", CstI(9), CstI(13)))
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