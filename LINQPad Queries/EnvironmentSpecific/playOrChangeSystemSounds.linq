<Query Kind="FSharpProgram" />

// WIP: play/change system sounds
open System.Runtime.InteropServices

let playSimpleSystemSound() =
    System.Media.SystemSounds.Asterisk.Play()
    
//http://www.codeproject.com/Articles/2740/Play-Windows-sound-events-defined-in-system-Contro    
module SystemSounds =
    [<Flags>]
    type Snd =
        |SYNC         = 0x0000  (* play synchronously (default) *)
        |ASYNC        = 0x0001 (* play asynchronously *)
        |NODEFAULT    = 0x0002 (* silence (!default) if sound not found *)
        |MEMORY       = 0x0004 (* pszSound points to a memory file *)
        |LOOP         = 0x0008 (* loop the sound until next sndPlaySound *)
        |NOSTOP       = 0x0010 (* don't stop any currently playing sound *)
        |NOWAIT       = 0x00002000(* don't wait if the driver is busy *)
        |ALIAS        = 0x00010000 (* name is a registry alias *)
        |ALIAS_ID     = 0x00110000(* alias is a pre d ID *)
        |FILENAME     = 0x00020000(* name is file name *)
        |RESOURCE     = 0x00040004(* name is resource name or atom *)
        |PURGE        = 0x0040 (* purge non-static events for task *)
        |APPLICATION  = 0x0080 (* look for application specific association *)
    [<DllImport("winmm.dll", EntryPoint="PlaySound", CharSet= CharSet.Auto)>]
    extern int private PlaySound(string pszSound, int hmod, int flags)
    // untested
    let playSound wavPath = 
        PlaySound(wavPath, 0, (int Snd.ASYNC ||| int Snd.FILENAME ||| int Snd.NOWAIT))
    let playSoundEvent sndAlias = 
        PlaySound(sndAlias, 0, (int Snd.ASYNC ||| int Snd.ALIAS ||| int Snd.NOWAIT))
        
    type SoundSchemeApp = {App:string; Event:string; CurrentOpt: string; DefaultOpt: string; ModifiedOpt:string}
    type PlayableSoundSchemeApp = {SchemeSound:SoundSchemeApp; PlayCurrent: obj option; PlayDefault: obj option; PlayModified: obj option} with 
        static member fromSoundSchemeApp (x:SoundSchemeApp) = 
            let makePlayLink path = if not <| String.IsNullOrWhiteSpace path then Util.OnDemand("Play", fun () -> playSound path) |> box else null
            {
                SchemeSound = x; 
                PlayCurrent = x.CurrentOpt |> Option.ofObj  |> Option.map makePlayLink
                PlayDefault= x.DefaultOpt |> Option.ofObj   |> Option.map makePlayLink
                PlayModified= x.ModifiedOpt |> Option.ofObj |> Option.map makePlayLink
            }
    let sounds = 
        use cu = Microsoft.Win32.Registry.CurrentUser
        use ae = cu.OpenSubKey("AppEvents")
        use el = ae.OpenSubKey("EventLabels")
        use cuAeAppsKey = ae.OpenSubKey("Schemes\\Apps")
            
        let apps = 
            cuAeAppsKey.GetSubKeyNames()
            |> Seq.map (fun appName ->
                                use appKey = cuAeAppsKey.OpenSubKey appName
                                let names = appKey.GetSubKeyNames()
                                names 
                                |> Seq.map (fun sn -> 
                                                use eventSubKey = appKey.OpenSubKey sn
                                                let eventValues = eventSubKey.GetSubKeyNames()
                                                let getEv n = 
                                                    if eventValues |> Seq.contains n then 
                                                        use eSSK = eventSubKey.OpenSubKey n
                                                        eSSK.GetValue String.Empty |> string |> Some 
                                                    else None
                                                let x = {App = appName; Event=sn; CurrentOpt =  getEv ".Current" |> Option.toObj; DefaultOpt= getEv ".Default"|> Option.toObj; ModifiedOpt = getEv ".Modified"|> Option.toObj}
                                                PlayableSoundSchemeApp.fromSoundSchemeApp x
                                                //appName, sn, eventSubKey
                                                //x
//                                                { App = n; Event = sn; CurrentOpt = sn,
//                                                            sk.OpenSubKey(sn)
                                            ) 
                                |> List.ofSeq
                        )
            |> List.ofSeq
            
        let allEventLabels = 
            el.GetSubKeyNames()
            |> Seq.map (fun n -> n,el.OpenSubKey(n))
            |> Seq.map (fun (n,sk) -> n, 
                                        //sk, 
                                        //sk.GetSubKeyNames(), 
                                        //sk.GetValueNames() |> Seq.map (fun vn -> vn, sk.GetValue(vn)) |> List.ofSeq
                                        sk.GetValueNames() |> Seq.tryFind((=) String.Empty) |> Option.map (fun n -> sk.GetValue(n))
                        )
            |> Seq.map (fun (n, lblOpt) -> n, lblOpt)
            |> List.ofSeq
        apps,allEventLabels
//        let openAppSubKey = appsKey.OpenSubKey
        
open SystemSounds    
sounds
|> Dump
|> ignore

