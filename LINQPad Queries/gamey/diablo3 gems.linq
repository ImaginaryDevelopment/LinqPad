<Query Kind="FSharpProgram" />

//type [<Measure>] Marquise
//type [<Measure>] Imperial
//type [<Measure>] FlawlessImperial
//type [<Measure>] Royal
//type [<Measure>] FlawlessRoyal

type GemCache = 
    {
        Marquise:int//<Marquise>; 
        Imperial:int //<Imperial>; 
        FlawlessImperial:int //<FlawlessImperial>; 
        Royal:int //<Royal>; 
        FlawlessRoyal:int //<FlawlessRoyal>
    }
let upconvert cache = 
    let upconvertGem count = 
        count / 3, if count > 3 then count % 3 else count
    let imperial,marquise = upconvertGem cache.Marquise //cache.Marquise / 3, if cache.Marquise > 3 then cache.Marquise % 3 else cache.Marquise
    let cache = {cache with Marquise = marquise; Imperial = imperial + cache.Imperial}
    let fi,imperial = upconvertGem cache.Imperial
    let cache = {cache with Imperial=imperial; FlawlessImperial = fi}
    let r, fi = upconvertGem cache.FlawlessImperial
    let cache = {cache with FlawlessImperial= fi; Royal = r}
    let fr, r = upconvertGem cache.Royal
    let cache = {cache with Royal = r; FlawlessRoyal=fr}
    cache
//    
//    {cache with Imperial = imperial'; Marquise=marquise; FlawlessImperial=fi}
    
{
    Marquise = 176; Imperial=27; FlawlessImperial=0; Royal = 0; FlawlessRoyal=1
}
|> upconvert
|> Dump