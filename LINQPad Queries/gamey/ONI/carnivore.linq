<Query Kind="FSharpExpression" />

// carnivore calc

let dupes = 11
// assume no bottomless and normal difficulty
let consumption = dupes * 1000
let cycle = 85
let eatenAlready = 323000//




// how much do you need to eat and how many dupes do you need to have
let getCaloriesByCycle100 target cycle =
    let cyclesRem = 100 - cycle
    target / cyclesRem
    
// assume no added dupes    
let getCyclesNeeded consumption target =
    target / consumption
    
let getCrittersNeeded target caloriesPer =
    target / caloriesPer
let getHatchesNeeded target = // assumes cooked
    getCrittersNeeded target 4000
let barbeque calories =
    let leftovers = calories % 3200
    let bbq = calories / 3200 * 4000
    bbq+leftovers
    

let target = 400000

let remaining = target - eatenAlready

getCaloriesByCycle100 remaining cycle
|> printfn "You need to consume %A calories per day"

100 - getCyclesNeeded consumption remaining
|> printfn "Or switch over to pure meat by cycle %A"
getHatchesNeeded remaining
|> printfn "You could use %A hatches to stop breeding" 

getCrittersNeeded remaining (barbeque 16000)
|> printfn "You could have %A voles"