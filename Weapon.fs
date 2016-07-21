﻿
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

type 'a stats_bundle = {
    str : 'a
    dex : 'a
    con : 'a
    per : 'a
    int : 'a
}
with
    member this.pretty p sep =
        sprintf "str:%s%sdex:%s%scon:%s%sper:%s%sint:%s%s"
            (p this.str) sep
            (p this.dex) sep
            (p this.con) sep
            (p this.per) sep
            (p this.int) sep

type stats = int stats_bundle

type dmg_scale = S | A | B | C | D
with
    member this.to_float =
        match this with
        | S -> 2.0
        | A -> 1.5
        | B -> 1.0
        | C -> 0.50
        | D -> 0.10

    static member op_Implicit (this : dmg_scale) = this.to_float

    override this.ToString () =
        match this with
        | S -> "S"
        | A -> "A"
        | B -> "B"
        | C -> "C"
        | D -> "D"

type stats_dmg_scale = dmg_scale stats_bundle

type [< AbstractClass >] weapon (hand, min_range, max_range, hit_mod, base_dmg, stats_dmg_scale : stats_dmg_scale) =
    member val hit_mod = hit_mod
    member val hand = hand
    member val base_dmg = base_dmg
    member val stats_dmg_scale = stats_dmg_scale
    member val min_range : int<m> = min_range
    member val max_range : int<m> = max_range
    member this.scaled_dmg (stats : stats) =
        let d = stats_dmg_scale
        in
            float stats.str * !> d.str
                + float stats.dex * !> d.dex
                + float stats.con * !> d.con
                + float stats.int * !> d.int
                + float stats.per * !> d.per    
            |> Operators.round |> int |> LanguagePrimitives.Int32WithMeasure<hp>

    member this.dmg (stats : stats) = this.base_dmg + this.scaled_dmg stats

    override this.ToString () =
        let name = this.GetType().Name
        sprintf "[%s] %O %+g%% range:%dm-%dm base:%d + {%s}" name this.hand this.hit_mod this.min_range this.max_range this.base_dmg (this.stats_dmg_scale.pretty (sprintf "%O") " ") 

    member this.pretty_with_dmg (stats : stats) =
        sprintf "%O scale:+%d tot:%d" this (this.scaled_dmg stats) (this.dmg stats)


type [< AbstractClass >] ranged_weapon (hand, hit_mod, base_dmg, stats_dmg_scale) =
    inherit weapon (hand, 1<m>, 100<m>, hit_mod, base_dmg, stats_dmg_scale)

type [< AbstractClass >] melee_weapon (hand, hit_mod, base_dmg, stats_dmg_scale) =
    inherit weapon (hand, 0<m>, 3<m>, hit_mod, base_dmg, stats_dmg_scale)

type [< AbstractClass >] firearm (hand, hit_mod, base_dmg, stats_dmg_scale, fire_rate : float) =
    inherit ranged_weapon (hand, hit_mod, base_dmg, stats_dmg_scale)
    new (hand, hit_mod, base_dmg, stats_dmg_scale, reload_every, reload_time) = new firearm (hand, hit_mod, base_dmg, stats_dmg_scale, float reload_every / (float reload_every + float reload_time))
    member this.fire_rate = fire_rate
    member this.avg_dpca stats = (this.dmg stats |> float |> LanguagePrimitives.FloatWithMeasure<hp>) * this.fire_rate


// standard weapon subclasses
//

type gun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = D; dex = D; int = D; per = B; con = D }, 6, 1)

type shotgun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, -0.25, base_dmg, defaultArg stats_dmg_scale { str = C; dex = D; int = D; per = B; con = D }, 4, 1)

type assault_rifle (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, 0., base_dmg, defaultArg stats_dmg_scale { str = D; dex = C; int = D; per = B; con = D }, 20, 1)

type sniper_rifle (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, 0.20, base_dmg, defaultArg stats_dmg_scale { str = D; dex = C; int = C; per = B; con = D }, 3, 1)

type pump_shotgun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (MainHand, -0.30, base_dmg, defaultArg stats_dmg_scale { str = C; dex = C; int = D; per = C; con = D }, 2, 2)

type sawnoff_shotgun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, -0.40, base_dmg, defaultArg stats_dmg_scale { str = D; dex = D; int = D; per = C; con = C }, 1, 2)

type bolt_action_rifle (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, 0.30, base_dmg, defaultArg stats_dmg_scale { str = C; dex = D; int = D; per = B; con = D }, 1, 2)

type bow (base_dmg, ?stats_dmg_scale) =
    inherit ranged_weapon (TwoHand, -0.10, base_dmg, defaultArg stats_dmg_scale { str = C; dex = D; int = D; per = B; con = C })

type unarmed () =
    inherit melee_weapon (OneHand, 0., 1<hp>, { str = B; dex = C; int = D; per = D; con = C })

type knucles (base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = A; dex = D; int = D; per = D; con = D })

type katana (base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = B; dex = B; int = D; per = D; con = D })

type knife (base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = A; dex = D; int = D; per = D; con = D })



// some weapon instances
//

let cia_gun = gun (5<hp>)
let cia_shotgun = shotgun (10<hp>)
let cia_pump_shotgun = pump_shotgun (13<hp>)

let epic_gun = gun (8<hp>, { str = B; dex = C; int = C; per = A; con = D })
let epic_katana = katana (15<hp>, { str = S; dex = C; int = D; per = D; con = D })
let legendary_shotgun = shotgun (20<hp>, { str = B; dex = C; int = C; per = S; con = D })
let legendary_knucles = knucles (10<hp>, { str = S; dex = A; int = D; per = D; con = C })

