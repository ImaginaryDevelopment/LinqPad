<Query Kind="FSharpProgram" />


type JoinResult<'tLeft,'tRight> =
    {
        LeftOnly:'tLeft list
        RightOnly:'tRight list
        Both: ('tLeft*'tRight) list
    } 
    
//type SetJoinResult<'t> = { Both: 't list; LeftOnly: 't list; RightOnly: 't list}
type SetJoinType<'t> =
    | LeftOnly of 't
    | Both of 't
    | RightOnly of 't
    
type MapJoinType<'tKey,'tLeft,'tRight when 'tKey : comparison> =
    | LeftOnly of 'tKey * 'tLeft
    | Both of 'tKey * ('tLeft * 'tRight)
    | RightOnly of 'tKey * 'tRight

    
    
let forceJoinSet (l,r): SetJoinType<_> list =    
    let both = Set.intersect l r
    let lOnly = l - both |> Set.toList |> List.map SetJoinType.LeftOnly
    let rOnly = r - both |> Set.toList |> List.map SetJoinType.RightOnly
    rOnly
    |> List.append ( both |> Set.toList |> List.map SetJoinType.Both)
    |> List.append lOnly
    
let forceJoin (l,r): SetJoinType<_> list =    
    let l = Set.ofList l
    let r = Set.ofList r
    let both = Set.intersect l r
    let lOnly = l - both |> Set.toList |> List.map SetJoinType.LeftOnly
    let rOnly = r - both |> Set.toList |> List.map SetJoinType.RightOnly
    rOnly
    |> List.append ( both |> Set.toList |> List.map SetJoinType.Both)
    |> List.append lOnly
    
let accumulateMapKeys f values =
    ((Set.empty,Map.empty), values)
    ||> Seq.fold(fun (keys,m) v ->
        let key = f v
        Set.add key keys, Map.add key v m
    )
    
let forceJoinOn (l:'Left list,getLeftKey:'Left -> _) (r:'Right list, getRightKey:'Right -> _) =
    let lk, l = accumulateMapKeys getLeftKey l
    let rk, r = accumulateMapKeys getRightKey r
    let joined = forceJoinSet (lk,rk)
    
    joined
    |> Seq.map(
        function
        | SetJoinType.Both k ->
            MapJoinType<_,'Left,'Right>.Both(k,(l.[k],r.[k]))
        | SetJoinType.LeftOnly k ->
            MapJoinType<_,'Left,_>.LeftOnly(k, l.[k])
        | SetJoinType.RightOnly k -> 
            MapJoinType<_,_,'Right>.RightOnly(k, r.[k])
    )
    
( // forceJoin tests
    [
        [3;5;0;2], [3;2;1], [SetJoinType.LeftOnly 0;SetJoinType.LeftOnly 5;SetJoinType.Both 2; SetJoinType.Both 3; SetJoinType.RightOnly 1]
    ]
    |> Seq.map(fun (l,r,expected) ->
        let actual = forceJoin (l,r)
        let display = l,r,expected,actual
        Util.HighlightIf(display,fun _ -> expected <> actual)
    )
    |> Dump
    |> ignore
)

