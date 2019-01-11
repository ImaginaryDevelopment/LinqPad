<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <NuGetReference>FsHtml</NuGetReference>
</Query>

// generate html from inputs
// blog items not finished
open FsHtml

let replace d r =
    function
    | null | "" as x -> x
    | x -> x.Replace(d,newValue=r)
    
[<RequireQualifiedAccess>]
module A =
    let id x = "id"%=x
    let href x = "href"%=x
    let src x = "src"%=x
    let title x = "title"%=x
    let className x = "class"%=x
    module Style =
        let hidden = "style"%="visibility:hidden"
let elem tag attr content = FsHtml.Element(tag,attr,content)
let meta attr = elem "meta" attr List.empty
module Link =
    let css href = elem "link" [A.href href; "rel"%="stylesheet";"type"%="text/css"] []
module Script =
    let asyncScript src = script ["async"%="";A.src src] []
    let text txt = script [] %(txt)
    // the type attribute is unnecessary for js resources
    let src src = script [A.src src][]
    
let comment text = elem "!--" [] %(text)


    
type GoogleAd={TagId:string;SlotId:string;Comment:string;Width:int;Height:int;ExtraStyle:string}
module Google =
    let gaScript = """(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
      (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
      m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
      })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
    
      ga('create', 'UA-98200285-1', 'auto');
      ga('send', 'pageview');"""
    let ad {TagId=tagId;SlotId=slotId;Comment=commentText;Width=w;Height=ht;ExtraStyle=extraStyle} =
        div[A.id tagId][
            Script.asyncScript "//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"
            comment commentText
            ins[
                A.className "adsbygoogle"
                "style"%= sprintf "display: inline-block; width: %ipx; height: %ipx;%s" w ht extraStyle
                "data-ad-client"%="ca-pub-7924380187053536"
                "data-ad-slot"%=slotId ][]
            Script.text "(adsbygoogle = window.adsbygoogle || []).push({});"
            
        ]
// let (~%) s = [Text(s.ToString())]
let head = 
    head [] [
        meta ["charset" %= "utf-8"]
        Link.css "css/my.css"
        Link.css "css/menu.css"
        title [] %"PoeAffix"
        script [] %(Google.gaScript)
    ]
let navItem title pageMap =
    li [] [
        FsHtml.Text title
        ul [] (pageMap |> List.map(fun (title,href) -> li [] [a ["href"%=href] %(title)] ))
    ]
let siteNav =
    ul [] [
        navItem "One Hand" [
            "Axe", "1h-axe.html"
            "Claw", "1h-claw.html"
            "Dagger", "1h-dagger.html"
            "Mace", "1h-mace.html"
            "Sceptre", "1h-sceptre.html"
            "Sword", "1h-sword.html"
            "Wand", "1h-wand.html"
        ]
        navItem "Two Hand" [
            "Axe", "2h-axe.html"
            "Bow", "2h-bow.html"
            "Mace", "2h-mace.html"
            "Staff", "2h-staff.html"
            "Sword", "2h-sword.html"
            "Fishing", "2h-fish.html"
        ]
        navItem "Body Armour" [
            "Armour", "ch-ar.html"
            "Evasion", "ch-ev.html"
            "Energy", "ch-es.html"
            "Armour", "ch-ares.html"
            "Armour", "ch-arev.html"
            "Evasion", "ch-eves.html"
            "Sacrificial", "ch-garb.html"
        ]
        navItem "Helmet" [
            "Armour", "hm-ar.html"
            "Evasion", "hm-ev.html"
            "Energy", "hm-es.html"
            "Armour", "hm-ares.html"
            "Armour", "hm-arev.html"
            "Evasion", "hm-eves.html"
            "Enchantment", "hm-enchant.html"
        ]
        navItem "Gloves" [
            "Armour", "gl-ar.html"
            "Evasion", "gl-ev.html"
            "Energy", "gl-es.html"
            "Armour", "gl-ares.html"
            "Armour", "gl-arev.html"
            "Evasion", "gl-eves.html"
            "Enchantment", "gl-enchant.html"
        ]
        navItem "Boots" [
            "Armour", "bt-ar.html"
            "Evasion", "bt-ev.html"
            "Energy", "bt-es.html"
            "Armour", "bt-ares.html"
            "Armour", "bt-arev.html"
            "Evasion", "bt-eves.html"
            "Enchantment", "gl-enchant.html"
        ]
        navItem "Shield" [
            "Armour", "sh-ar.html"
            "Evasion", "sh-ev.html"
            "Energy", "sh-es.html"
            "Armour", "sh-ares.html"
            "Armour", "sh-arev.html"
            "Evasion", "sh-eves.html"
        ]
        navItem "Accessories" [
            "Amulet", "ac-amulet.html"
            "Belt", "ac-belt.html"
            "Ring", "ac-ring.html"
            "Quiver", "ac-quiver.html"
            "Flask", "ac-flask.html"
            "Cobalt", "../jw-cobalt.html"
            "Crimson", "../jw-crimson.html"
            "Viridian", "../jw-viridian.html"
            "Murderous", "jw-murderous.html"
            "Searching", "jw-searching.html"
            "Hypnotic", "jw-hypnotic.html"
            "Ghastly", "jw-ghastly.html"
            "Jewel", "../jw-all.html"
        ]
        navItem "Other" [
            "Map", "ot-map.html"
            "Strongbox", "ot-box.html"
        ]                        
    ]
let blogItems =
    let blogItem (date:DateTime,x)=
        // deviate, wrap each blurb in an element
        div[] [
            br [] []
            div[A.className "seperatorINDEX";"align"%="center"][
                strong [] %(date.ToLongDateString())
            ]
            div[A.className "affix index"] x
        ]
    [
        DateTime(2019,9,1),[center [] %"Working on new betrayal affixes"]
        DateTime(2018,8,4),[    center[] [
                                    Text "Incorrect or missing information should be reported at" 
                                    u [] [a[A.href "https://github.com/poeaffix/poeaffix.github.io"] %"github.com/poeaffix"]
                                ]
                                br [] []
                                Text "Added new Vaal orb corruptions."
                                br [] []
                                br [] []
                                Text """If a piece of gear has more life, energy shield, evasion, or armour value than a single listed mod, it's
        because the item has two mods that combine that stat. This is also the case for physical and spell damage on
        weapons, maybe more. Thanks for all the support."""
        ]
        DateTime(2017,12,11), %"Added new Shaped/Elder mods, updated the ilvl requirements of the Abyss jewels, and fixed some mods GGG updated."
        DateTime(2017,12,8), %"Added new Abyss jewel mods. In process of updating Shaper/Elder mods along with anything else that has changed. The ilvl of the jewel mods are incorrect atm."
        DateTime(2017,8,9), %"Added new jewel and map mods, updated energy shield essences. Fixed some helmet enchants not showing"
        DateTime(2017,8,4), %"""This update changed the mod values to the new 3.0 values (Beta wave 4). I did not include legacy values like
            before because there are too many of them with this update (Sorry standard players). Updates to Essences,
            jewels, and anything else changed after beta wave 4 will be addressed later this week. Thanks for all the
            support."""
        DateTime(2017,5,12),%"""This update is to add multi-mod viewing and further improve the layout of mods. It should be a little easier to
        identify what can be crafted on each item. I will be making a more thorough update in the near future, possibly
        even converting it to be mobile friendly. I have not cross checked every mod value, but they seem to be mostly
        correct from what short time I spent looking. I have disabled the Helmet enchant page. I haven't had time to
        get to it yet. Any missing or incorrect information should be reported to poeaffix@gmail.com. 3.0 information
        will be updated after the beta balance changes are final. I would also like to add a crafting guide section,
        you can email me any crafting techniques that you think should be included. I would like to thank all the
        people that have shown support for this site. Special shout out to Twitch streamers that have continually
        promoted the site."""
    ]
    |> List.map blogItem
let body =
    body [] [
        div [A.id "wrapper"] [
            header [A.id "header"] [
                div [A.id "logo"] [
                    a [A.href "index.html"] [
                        img[A.src "images/header.png"; "alt"%="header"] []
                    ]
                ]
                Google.ad {TagId="ad";SlotId="5241339200";Comment="728x90 Banner";Width=728;Height=90;ExtraStyle=null}
                nav [A.id "mainav"] [ siteNav ]
            ]
            div[A.id "paypal"] [
                a[A.href "https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=S6L2QZULXFK7E"][
                    img [A.src "images/paypal.png";"alt"%="paypal"] []
                ]
            ]
            div[A.id "pageinfo"][
                article [A.id "main"][
                    h2 [A.Style.hidden] %"xx"
                ]
                article [A.id "main2"][
                    h2 [A.Style.hidden] %"x"
                ]
                article [A.id "main3"][
                    h2 ["style" %= "text-indent: 522px; margin-left: 0px; margin-right: 215px;"] %"Path of Exile Item Affixes"
                ]
            ]
            aside[A.id "corruption"][
                div[A.Style.hidden] %"xx"
                div[A.id "item"][
                    div[A.id "openModal1000";A.className "close"][
                        a[A.href "#close";A.title "Close";A.className "close"][]
                        div[A.id "1haxecorr"] %" Content "
                    ]
                ]
            ]
            aside[A.id "left";A.className "left list"][
                Google.ad {TagId="uppermiddlesidebaradINDEX";Width=300;Height=600;Comment="300x600 Display Only";ExtraStyle="margin-left: 0px";SlotId="2287872807"}
                ]
            aside[A.id "right";A.className "right list"][
                yield br [] []
                yield! blogItems
                yield Script.src "../js/mod.js"
            ]
            div [A.id"ADBAR"][
                Google.ad {TagId= "TopSideBarAd";Comment= "300x250 Text Only";Width=300;Height=250;SlotId="9811139608";ExtraStyle=null}
                Google.ad {TagId= "lowermiddlesidebarad";Comment= "300x250 Display";Width=300;Height=250;SlotId="3764606006";ExtraStyle=null}
                Script.src "js/closemodal.js"
                Script.src "js/mod.js"
            ]
        ]
        
    ]
html [] [
    head
    body
]
|> toString 
|> replace "async=\"\"" "async"
|> replace "<!-->" "<!-- "
|> replace "</!-->" " -->"
|> replace "></meta>" "/>"
|> replace "></link>" "/>"
|> replace "></img>" "/>"
|> replace "<br></br>" "<br />"
|> sprintf "<!DOCTYPE html>\r\n%s"
|> Dump
|> ignore
