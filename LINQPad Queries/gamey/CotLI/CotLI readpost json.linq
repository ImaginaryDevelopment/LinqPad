<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

open Newtonsoft.Json
open Newtonsoft.Json.Linq

//type PascalPropertyNamesContractResolver() =
//    inherit Newtonsoft.Json.Serialization.DefaultContractResolver()
//    static let distinctResolutionNames = HashSet<string>()
//    static member GetResolutionNames () = distinctResolutionNames :> seq<_>
//    override x.ResolvePropertyName s =
//        let result = Regex.Replace(s, @"_(\w)","\U\1")
//        distinctResolutionNames.Add s |> ignore
//        //printfn "resolved %s to %s" s result
//        result
        
module ReverseDumper2 = 
    // reverse dumper with raw html support
    // linqpad helper (for when you can't .Dump(description), but still want the html)
    // also of note: there is an extra open div tag, because the ToHtmlString closes one that it didn't open
    let titleize t (x:obj) = 
        let objHtml = Util.ToHtmlString(enableExpansions=true, noHeader=true, objectsToDump= ([ x ] |> Array.ofList))
        let result = sprintf """<table class="headingpresenter">
        <tr>
        <th class="">%s</th>
        </tr>
        <tr>
        <td class="headingpresenter"><div>%s</td>
        </tr>
        </table>"""                 t objHtml
        Util.RawHtml result
    type DumpType = 
        | Raw of string
        | DumpObj of obj
    // consider taking in obj, downcast to string, check for "<" starting the string to autodetect a raw html string? nah.    
    let dumpReverse :  DumpType -> unit =
        let dc = DumpContainer()
        dc.Dump() |> ignore
        (fun o -> 
            let o = 
                match o with
                | Raw s -> Util.RawHtml s
                | DumpObj o -> o
                
            match dc.Content with
            | :? (obj list) as items -> List.Cons(o,items)
            | _ -> [ o ]
            |> fun content -> dc.Content <- content
        )
    let dumptRev t x = 
        titleize t x |> DumpObj |> dumpReverse
        x
    // override/overwrite existing dump methods, no direct dumpReverse calls required
    type System.Object with
        member x.Dump() = printfn "override hit!";  x |> DumpObj |> dumpReverse |> ignore
        member x.Dump description = printfn "override2 hit! %s" description; x |> titleize description |> DumpObj |> dumpReverse |> ignore
    let dumpt t x = x.Dump(description=t); x
open ReverseDumper2        
let serializerSettings = JsonSerializerSettings(ContractResolver=Serialization.DefaultContractResolver(NamingStrategy=Serialization.SnakeCaseNamingStrategy()))
let serializeRaw x = Newtonsoft.Json.JsonConvert.SerializeObject x
let serialize x = Newtonsoft.Json.JsonConvert.SerializeObject(x, Newtonsoft.Json.Formatting.Indented)        
let deserialize<'T> x: 'T = Newtonsoft.Json.JsonConvert.DeserializeObject<'T>(x, serializerSettings)
let getProperty name (x:JObject) = x.Property name |> Option.ofObj
let getPropertyValue (x:JProperty) = x.Value |> Option.ofObj
let asJObject (x:obj) = x :?> JObject
let asJArray (x:obj) = x :?> JArray
let hoist f x =
    f x |> ignore
    x

let text = File.ReadAllText @"C:\projects\cotlicheatsheet\js\sampleHeroData.json"
type CrusaderData = {Slot:string; Id:int;Name:string}
let crusaders = [
    {Slot="01";Id=1;Name="TheBushWhacker"}
    {Slot="01a";Id=38;Name="RoboRabbit"}
    {Slot="01b";Id=66;Name="GrahamTheDriver"}
    {Slot="01c";Id=75;Name="WarwickTheWarlock"}
    {Slot="02";Id=2;Name="JimTheLumberjack"}
    {Slot="02a";Id=29;Name="PilotPam"}
    {Slot="02b";Id=51;Name="VeronicaTheAndroidArcher"}
    {Slot="03";Id=3;Name="EmoWerewolf"}
    {Slot="03a";Id=34;Name="SallyTheSuccubus"}
    {Slot="03b";Id=53;Name="KarenTheCatTeenager"}
    {Slot="04";Id=4;Name="SashaTheFierceWarrior"}
    {Slot="04a";Id=31;Name="GroklokTheOrc"}
    {Slot="04b";Id=62;Name="MindiTheMimic"}
    {Slot="05";Id=5;Name="TheWashedUpHermit"}
    {Slot="05a";Id=36;Name="KyleThePartyBro"}
    {Slot="05b";Id=46;Name="SerpentKingDraco"}
    {Slot="05c";Id=64;Name="HenryTheScaredyGhoul"}
    {Slot="06";Id=6;Name="DetectiveKaine"}
    {Slot="06a";Id=21;Name="MisterTheMonkey"}
    {Slot="06b";Id=35;Name="LarryTheLeprechaun"}
    {Slot="06c";Id=71;Name="BernardTheBartender"}
    {Slot="07";Id=7;Name="ThePrincess"}
    {Slot="07a";Id=25;Name="RoboTurkey"}
    {Slot="07b";Id=49;Name="RangerRayna"}
    {Slot="08";Id=8;Name="NatalieDragon"}
    {Slot="08a";Id=24;Name="JackOLantern"}
    {Slot="08b";Id=47;Name="PresidentBillySmithsonian"}
    {Slot="08c";Id=73;Name="KarlTheKicker"}
    {Slot="09";Id=9;Name="JasonMasterOfShadows"}
    {Slot="09a";Id=22;Name="PeteTheCarney"}
    {Slot="09b";Id=41;Name="Broot"}
    {Slot="09c";Id=67;Name="PaulThePilgrim"}
    {Slot="10";Id=10;Name="ArtaxesTheLion"}
    {Slot="10a";Id=32;Name="DrizzleTheDarkElf"}
    {Slot="10b";Id=52;Name="BubbaTheSwimmingOrc"}
    {Slot="11";Id=11;Name="KhouriTheWitchDoctor"}
    {Slot="11a";Id=26;Name="MommaKaine"}
    {Slot="11b";Id=44;Name="BrogonPrinceOfDragons"}
    {Slot="11c";Id=63;Name="HalfBloodElf"}
    {Slot="12";Id=12;Name="DarkGryphon"}
    {Slot="12a";Id=33;Name="RockyTheRockstar"}
    {Slot="12b";Id=45;Name="MontanaJames"}
    {Slot="12c";Id=69;Name="TheDarkHelper"}
    {Slot="13";Id=13;Name="SarahTheCollector"}
    {Slot="13a";Id=40;Name="TheMetalSoldierette"}
    {Slot="13b";Id=77;Name="SnicketteTheSneaky"}
    {Slot="14";Id=14;Name="GoldPanda"}
    {Slot="14a";Id=27;Name="RoboSanta"}
    {Slot="14b";Id=43;Name="LeerionTheRoyalDwarf"}
    {Slot="14c";Id=76;Name="KatieTheCupid"}
    {Slot="15";Id=15;Name="PrinceSalTheMerman"}
    {Slot="15a";Id=23;Name="WendyTheWitch"}
    {Slot="15b";Id=42;Name="RobbieRaccoon"}
    {Slot="15c";Id=72;Name="PrincessValTheMermaid"}
    {Slot="16";Id=16;Name="FirePhoenix"}
    {Slot="16a";Id=37;Name="AlanTheArchAngel"}
    {Slot="16b";Id=65;Name="FrightOTron4000"}
    {Slot="17";Id=17;Name="KingReginaldIV"}
    {Slot="17a";Id=30;Name="QueenSiri"}
    {Slot="17b";Id=54;Name="Mr_BogginsTheSubstitute"}
    {Slot="17c";Id=78;Name="SquigglesTheClown"}
    {Slot="18";Id=18;Name="ThaliaTheThunderKing"}
    {Slot="18a";Id=28;Name="FrostyTheSnowman"}
    {Slot="18b";Id=50;Name="Littlefoot"}
    {Slot="18c";Id=74;Name="CindyTheCheerOrc"}
    {Slot="19";Id=19;Name="MerciTheMadWizard"}
    {Slot="19a";Id=39;Name="TheBatBillionaire"}
    {Slot="19b";Id=68;Name="PetraThePilgrim"}
    {Slot="20";Id=20;Name="NateDragon"}
    {Slot="20a";Id=48;Name="KizlblypTheAlienTraitor"}
    {Slot="20b";Id=70;Name="RoboRudolph"}
    {Slot="21";Id=55;Name="TheExterminator"}
    {Slot="21a";Id=58;Name="GloriaTheGoodWitch"}
    {Slot="22";Id=56;Name="TheShadowQueen"}
    {Slot="22a";Id=59;Name="IlsaTheInsaneWizard"}
    {Slot="23";Id=57;Name="GreyskullThePirate"}
    {Slot="23a";Id=60;Name="EiralonTheBloodMage"}
    {Slot="24";Id=61;Name="PriestessOfTime"}  ]
type HeroRaw={HeroId:int; Level:int; Health:int; Disenchant:int;Owned:int; InSeat:byte;Raw:obj}
let mapHero  (hero:JObject) : HeroRaw =
    hero |> serialize |> deserialize |> fun x -> {x with Raw = hero |> serializeRaw }
let mapData (data:JObject) =
    getProperty "details" data
    |> Option.bind getPropertyValue
    |> Option.map asJObject
    |> Option.bind (getProperty "heroes")
    |> Option.bind getPropertyValue
    |> Option.map (asJArray>> fun x -> x.AsJEnumerable())
    |> Option.map (Seq.map asJObject)
    |> Option.map (Seq.map mapHero)
type Mating =
    |Matched of HeroRaw*CrusaderData
    |OrphanH of HeroRaw
    |OrphanC of CrusaderData
let mapCrusaders (x:HeroRaw list) =
    let mating = 
        crusaders 
        |> Seq.map (fun c -> 
            match x |> List.tryFind(fun h -> h.HeroId= c.Id), c with 
                | Some h,c -> Matched(h,c) 
                | None,c -> OrphanC c)
        |> List.ofSeq
    let mating = 
        x
        |> List.filter(fun h ->
            mating |> Seq.exists(function |Matched(h2,_) -> h.HeroId = h2.HeroId | _ -> false) |> not)
        |> List.map OrphanH
        |> (@) mating
    mating
    |> List.sortBy(function |OrphanH h -> (0,Some (h.HeroId),None) | OrphanC c -> (1,None,Some c.Slot) | Matched _ -> (2,None,None))
type MatedCrusader = {Name:string; EP:int; Owned:bool; Slot:string; HeroId:int; Raw:obj}    
type TranslationType = 
    | GetForEPDoc
    | GetForGithub


let formatData t (x:MatedCrusader seq) = 
    match t with
    // make a paster for going into google docs
    | GetForEPDoc ->
        x
        |> Seq.map (fun m -> if m.Owned then sprintf "%i" m.EP else String.Empty) 
        |> delimit "\r\n" 
        |> dumpt "EP doc pastable" 
        |> ignore
    | GetForGithub ->
        x
        |> Seq.map (fun m ->
            sprintf "\"%s\"" m.Slot
        )
        |> delimit ","
        |> sprintf "\"ownedCrusaderIds\":[%s]"
        |> dumpt "github site pastable" 
        |> ignore
        
let mode = GetForGithub

deserialize<JObject>(text)
|> mapData
//|> Dump
|> Option.map (List.ofSeq>>mapCrusaders)
//|> hoist (mapCrusaders >> Dump)
|> Option.map (hoist (Seq.choose (function |OrphanH _ | OrphanC _ as x -> Some x | _ -> None) >> (dumpt "orphans")))
|> Option.map (Seq.choose (function |Matched(h,c) -> Some(h,c) | _ -> None))
|> Option.map (Seq.map (fun (h,c) -> {Name=c.Name; HeroId=h.HeroId; EP=h.Disenchant; Owned=h.Owned = 1; Slot=c.Slot; Raw=h.Raw}))
|> Option.map (hoist (dumpt "MatedData"))
//|> Option.map (hoist (serialize >> dumpt "serializedMated"))
|> Option.iter (
    formatData mode
)
