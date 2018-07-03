<Query Kind="FSharpExpression" />

let letters = "endeid" //.ToCharArray() |> List.ofArray

let dicUrl = @"https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt"
let containsI (c:string) (word:string) = word.Contains(string c, comparison=StringComparison.InvariantCultureIgnoreCase)
let flip f x y = f y x
let getText url = 
    use wc = new System.Net.WebClient()
    wc.DownloadString(Uri url)
let isInWordBounds (x:string) =
    x.Length > 2 && x.Length <= letters.Length
let splitLines x = 
    //|> fun x -> x.Split([| Environment.NewLine|],StringSplitOptions.RemoveEmptyEntries)
    seq {
        use r = new System.IO.StringReader(x)
        yield! 
            r |> Seq.unfold(fun r -> if r.Peek() > -1 then Some <| (r.ReadLine(),r) else None)
    }
let usesOnly (letters:string) (word:string) = 
    word.ToCharArray()
    |> Seq.forall(string>>flip containsI letters)
    
let limitedBy (word:string) = 
    if word |> usesOnly letters && isInWordBounds word then
        let failing = Some (false,(String.Empty,String.Empty))
        // check that it only uses the same number of each letter that is available
        (word,letters)
        |> Seq.unfold(fun (word,letters) -> 
            if letters.Length > 0 && word.Length > 0 then
                //consume a letter from word, if letters contains it then replace that letter one time in both else Some(false,String.Empty,
                if containsI word.[..0] letters then
                    Some(true,(word.[1..], letters.Remove(letters.IndexOf(word.[0]),1)))
                else failing
            elif word.Length > 0 then failing
            else None
        )
        |> Seq.forall id
    else false
let words = 
    Util.Cache(fun () -> getText dicUrl)
    |> splitLines
let allwordsGrouped = 
    words
    |> Seq.filter(isInWordBounds)
    |> Seq.filter(usesOnly letters)
    |> Seq.filter(limitedBy)
    |> Seq.groupBy(fun x -> x.Length)
    |> Seq.sortByDescending fst
    |> Seq.map(fun (x,items) -> x, items |> Seq.sort)
// for verification of the above
let _finder word = 
    words
    |> Seq.filter(isInWordBounds)
    |> Seq.filter(usesOnly letters)
    |> Seq.filter(limitedBy)
    |> Seq.find((=) word)
allwordsGrouped

