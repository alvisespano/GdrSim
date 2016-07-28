
module GdrSim.Weapon

open GdrSim.Globals
open FSharp.Common
open FSharp.Data.UnitSystems.SI.UnitSymbols

// weapon type hierarchy
//

type hand = OneHand | MainHand | TwoHand
with
    override this.ToString () =
        match this with
        | OneHand -> "1H"
        | MainHand -> "MH"
        | TwoHand -> "2H"

type dmg_scale = S | A | B | C | Z
with
    member this.to_float =
        match this with
        | S -> 2.0
        | A -> 1.5
        | B -> 1.0
        | C -> 0.50
        | Z -> 0.0 

    static member op_Implicit (this : dmg_scale) = this.to_float

    override this.ToString () =
        match this with
        | S -> "S"
        | A -> "A"
        | B -> "B"
        | C -> "C"
        | Z -> "Z"

type stats_dmg_scale = dmg_scale stats_bundle

let pretty_stats_dmg_scale (stats : stats_dmg_scale) =
    let p name = function
        | Z -> ""
        | d -> sprintf "%s:%O " name d
    let r =
        sprintf "%s%s%s%s%s"
            (p "str" stats.str)
            (p "dex" stats.dex)
            (p "con" stats.con)
            (p "per" stats.per)
            (p "int" stats.int)
    in
        r.Trim [|' '|]  // trim because there might be a trailing space

type [< AbstractClass >] weapon (name_, hand_, min_range_, max_range_, hit_mod_, base_dmg_, stats_dmg_scale_ : stats_dmg_scale) =
    member val hit_mod = hit_mod_
    member val hand = hand_
    member val base_dmg = base_dmg_
    member val stats_dmg_scale = stats_dmg_scale_
    member val min_range : int<m> = min_range_
    member val max_range : int<m> = max_range_
    member this.typee = this.GetType().Name
    member val name = name_
    member this.dmg (stats : stats) = this.base_dmg + this.scaled_dmg stats

    member this.scaled_dmg (stats : stats) =
        let ds = this.stats_dmg_scale
        in
            float stats.str * !> ds.str
                + float stats.dex * !> ds.dex
                + float stats.con * !> ds.con
                + float stats.int * !> ds.int
                + float stats.per * !> ds.per    
            |> Operators.round |> int |> LanguagePrimitives.Int32WithMeasure<hp>

    override this.ToString () =
        sprintf "\"%s\" %O %s hit:%+g%% range:%dm-%dm base:%d + {%s}" this.name this.hand this.typee this.hit_mod this.min_range this.max_range this.base_dmg (pretty_stats_dmg_scale this.stats_dmg_scale) 

    abstract pretty_with_dmg : stats -> string
    default this.pretty_with_dmg (stats : stats) =
        sprintf "%O scale:+%d tot:%d" this (this.scaled_dmg stats) (this.dmg stats)


type [< AbstractClass >] ranged_weapon (name, hand, hit_mod, base_dmg, stats_dmg_scale) =
    inherit weapon (name, hand, Config.Weapon.Ranged.default_min_range, Config.Weapon.Ranged.default_max_range, hit_mod, base_dmg, stats_dmg_scale)

type [< AbstractClass >] melee_weapon (name, hand, hit_mod, base_dmg, stats_dmg_scale) =
    inherit weapon (name, hand, Config.Weapon.Melee.default_min_range, Config.Weapon.Melee.default_max_range, hit_mod, base_dmg, stats_dmg_scale)

type [< AbstractClass >] firearm (name, hand, hit_mod, base_dmg, stats_dmg_scale, reload_ : float) =
    inherit ranged_weapon (name, hand, hit_mod, base_dmg, stats_dmg_scale)
    new (name, hand, hit_mod, base_dmg, stats_dmg_scale, reload_every, reload_time) = new firearm (name, hand, hit_mod, base_dmg, stats_dmg_scale, 1. - float reload_every / (float reload_every + float reload_time))
    member this.reload = reload_
    member this.avg_dpca stats = (this.dmg stats |> float |> LanguagePrimitives.FloatWithMeasure<hp>) * this.reload
    override this.ToString () = sprintf "%s reload:%s" (base.ToString ()) (pretty_percent this.reload)
        


// standard weapon subclasses
//

type gun (name, base_dmg, ?stats_dmg_scale) =
    inherit firearm (name, OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = Z; dex = Z; int = Z; per = B; con = Z }, 6, 1)

type shotgun (name, base_dmg, ?stats_dmg_scale) =
    inherit firearm (name, TwoHand, -0.25, base_dmg, defaultArg stats_dmg_scale { str = C; dex = Z; int = Z; per = B; con = Z }, 4, 1)

type assault_rifle (name, base_dmg, ?stats_dmg_scale) =
    inherit firearm (name, TwoHand, 0., base_dmg, defaultArg stats_dmg_scale { str = Z; dex = C; int = Z; per = B; con = Z }, 20, 1)

type sniper_rifle (name, base_dmg, ?stats_dmg_scale) =
    inherit firearm (name, TwoHand, 0.20, base_dmg, defaultArg stats_dmg_scale { str = Z; dex = C; int = C; per = B; con = Z }, 3, 1)

type pump_shotgun (name, base_dmg, ?stats_dmg_scale) =
    inherit firearm (name, MainHand, -0.30, base_dmg, defaultArg stats_dmg_scale { str = C; dex = C; int = Z; per = C; con = Z }, 2, 2)

type sawnoff_shotgun (name, base_dmg, ?stats_dmg_scale) =
    inherit firearm (name, TwoHand, -0.40, base_dmg, defaultArg stats_dmg_scale { str = Z; dex = Z; int = Z; per = C; con = C }, 1, 1)

type bolt_action_rifle (name, base_dmg, ?stats_dmg_scale) =
    inherit firearm (name, TwoHand, 0.30, base_dmg, defaultArg stats_dmg_scale { str = C; dex = Z; int = Z; per = B; con = Z }, 1, 1)

type bow (name, base_dmg, ?stats_dmg_scale) =
    inherit ranged_weapon (name, TwoHand, -0.10, base_dmg, defaultArg stats_dmg_scale { str = C; dex = Z; int = Z; per = B; con = C })

type unarmed () =
    inherit melee_weapon ("Unarmed", OneHand, 0., 1<hp>, { str = B; dex = C; int = Z; per = Z; con = C })

type knucles (name, base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (name, OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = A; dex = Z; int = Z; per = Z; con = Z })

type katana (name, base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (name, OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = B; dex = B; int = Z; per = Z; con = Z })

type dagger (name, base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (name, OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = A; dex = Z; int = Z; per = Z; con = Z })



// some weapon instances
//

let cia_pistol = gun ("CIA Pistol", 5<hp>)
let cia_shotgun = shotgun ("CIA Shotgun", 10<hp>)
let cia_pump_shotgun = pump_shotgun ("CIA Pump Shotgun", 13<hp>)

let epic_gun = gun ("Magnum", 8<hp>, { str = B; dex = C; int = C; per = A; con = Z })
let epic_dagger = dagger ("US Navy Dagger", 15<hp>, { str = S; dex = C; int = Z; per = Z; con = Z })
let legendary_knucles = knucles ("Adamantium Knucles", 10<hp>, { str = S; dex = A; int = Z; per = Z; con = C })

