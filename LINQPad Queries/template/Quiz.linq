<Query Kind="FSharpProgram" />

// question bank, randomize questions, grade applicable responses.
// for more questions see https://stackoverflow.com/questions/365489/questions-every-good-net-developer-should-be-able-to-answer
let debug = true
let rSeed = 2

let delimit d (items:string seq) = String.Join(separator=d,values=items)

type MultipleChoiceQ = { Question:string; CorrectAnswer: string; OtherChoices:string list}
type TrueFalseQ = {Question:string; IsTrue:bool}
type Question = 
    | MultipleChoice of MultipleChoiceQ
    | TrueFalse of TrueFalseQ
type Answer = Correct | Incorrect

let randomizeItems (r:Random) =
    List.fold(fun result item -> 
        if r.NextBool() then
            item :: result
        else item :: (result |> List.rev)
    ) List.empty
let questions =
    [
        MultipleChoice {Question = "You can override methods _______"; CorrectAnswer="that are virtual"; OtherChoices=["that are in sealed classes"; "with only the protected modifier"; "that are non-virtual"; "that aren't abstract or virtual"]}
        TrueFalse {Question = "You can override some protected methods."; IsTrue=true}
        TrueFalse {Question = "You can inherit from sealed classes."; IsTrue=false}
    ]
type FoldState<'T> = 
    | Working
    | Done
let askNumber text l = 
    Seq.unfold(
        function
        | Done -> None
        | Working ->        
            let answer = Util.ReadLine<int>(text)
            if answer > 0 && answer < l + 1 then
                Some(Some answer,Done)
            else Some(None, Working)
    ) Working
    |> Seq.choose id
    |> Seq.head
let r = (Random(rSeed))
let quizResults = 
    questions
    |> randomizeItems r
    |> Seq.map(
        function
        | TrueFalse q -> 
            if not debug then Util.ClearResults()
            Util.ReadLine<bool>(q.Question)
            |> fun x -> if x = q.IsTrue then Correct else Incorrect
        | MultipleChoice q -> 
            if not debug then Util.ClearResults()
            printfn "%s" q.Question
            [1 .. q.Question.Length + 5] |> List.map(fun _ -> "-") |> delimit String.Empty |> printfn "%s"
            let answers = q.CorrectAnswer::q.OtherChoices |> randomizeItems r
            let correct = answers |> List.findIndex(fun x -> x = q.CorrectAnswer) |>  (+) 1
            answers
            |> Seq.iteri(fun i t -> printfn "%i. %s" (i + 1) t)
            let number,result = 
                let number = askNumber "Select the number of your answer" answers.Length
                if number = correct then
                    number,Correct
                else number,Incorrect
            if debug then (number,correct,sprintf "%A" result,answers |> List.mapi(fun i t -> i + 1,t)).Dump()
            result
            
    )
    |> Seq.mapi(fun i r ->
        i,r
    )
    |> List.ofSeq
let getCorrect = Seq.filter(function | Correct -> true | _ -> false) >> Seq.length
let getPercent correct total = 
    (float correct) / (float total)
type Grade = {Score:float; Correct:int; Answers:(int*Answer) list; Seed:int}
let scoreIt x =
    let l = List.length x
    let c = getCorrect (x |> Seq.map snd)
    let score = getPercent c l
    {Score=score; Correct=c; Answers=quizResults; Seed = rSeed}
(scoreIt quizResults |> sprintf "%A")
|> Dump
|> ignore