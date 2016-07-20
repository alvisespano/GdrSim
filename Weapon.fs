
module GdrSim.Weapon

open GdrSim.Globals
open FSharp.Common

// weapon type hierarchy
//

type hand = OneHand | MainHand | TwoHand

type 'a stats_bundle = {
    mutable str : 'a
    mutable dex : 'a
    mutable con : 'a
    mutable int : 'a
    mutable per : 'a
}

type stats = int stats_bundle

type dmg_scale = S | A | B | C | D | E
with
    member this.to_float =
        match this with
        | S -> 1.20
        | A -> 1.0
        | B -> 0.80
        | C -> 0.50
        | D -> 0.30
        | E -> 0.10

    static member op_Implicit (this : dmg_scale) = this.to_float

type stats_dmg_scale = dmg_scale stats_bundle

type [< AbstractClass >] weapon (hand, hit_mod, base_dmg, stats_dmg_scale : stats_dmg_scale) =
    member val hit_mod = hit_mod
    member val hand = hand
    member val base_dmg = base_dmg
    member val stats_dmg_scale = stats_dmg_scale
    member this.dmg (stats : stats) =
        let d = stats_dmg_scale
        in
            float base_dmg + float stats.str * !> d.str
                           + float stats.dex * !> d.dex
                           + float stats.con * !> d.con
                           + float stats.int * !> d.int
                           + float stats.per * !> d.per
            |> Operators.round |> int |> LanguagePrimitives.Int32WithMeasure<hp>

type [< AbstractClass >] ranged_weapon (hand, hit_mod, base_dmg, stats_dmg_scale) =
    inherit weapon (hand, hit_mod, base_dmg, stats_dmg_scale)

type [< AbstractClass >] melee_weapon (hand, hit_mod, base_dmg, stats_dmg_scale) =
    inherit weapon (hand, hit_mod, base_dmg, stats_dmg_scale)

type [< AbstractClass >] firearm (hand, hit_mod, base_dmg, stats_dmg_scale, fire_rate : float) =
    inherit ranged_weapon (hand, hit_mod, base_dmg, stats_dmg_scale)
    new (hand, hit_mod, base_dmg, stats_dmg_scale, reload_every, reload_time) = new firearm (hand, hit_mod, base_dmg, stats_dmg_scale, float reload_every / (float reload_every + float reload_time))
    member this.fire_rate = fire_rate
    member this.avg_dpca stats = (this.dmg stats |> float |> LanguagePrimitives.FloatWithMeasure<hp>) * this.fire_rate


// standard weapon subclasses
//

type gun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = D; dex = D; int = E; per = B; con = E }, 6, 1)

type shotgun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, -0.25, base_dmg, defaultArg stats_dmg_scale { str = C; dex = E; int = E; per = B; con = E }, 4, 1)

type assault_rifle (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, 0., base_dmg, defaultArg stats_dmg_scale { str = E; dex = C; int = E; per = B; con = E }, 20, 1)

type sniper_rifle (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, 0.20, base_dmg, defaultArg stats_dmg_scale { str = E; dex = C; int = C; per = B; con = E }, 3, 1)

type pump_shotgun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (MainHand, -0.30, base_dmg, defaultArg stats_dmg_scale { str = C; dex = C; int = E; per = C; con = E }, 2, 2)

type sawnoff_shotgun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, -0.40, base_dmg, defaultArg stats_dmg_scale { str = D; dex = E; int = E; per = C; con = C }, 1, 2)

type bolt_action_rifle (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, 0.30, base_dmg, defaultArg stats_dmg_scale { str = C; dex = E; int = D; per = B; con = E }, 1, 2)

type bow (base_dmg, ?stats_dmg_scale) =
    inherit ranged_weapon (TwoHand, -0.10, base_dmg, defaultArg stats_dmg_scale { str = C; dex = E; int = E; per = B; con = C })

type unarmed () =
    inherit melee_weapon (OneHand, 0., 1, { str = B; dex = C; int = D; per = D; con = C })

type knucles (base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = A; dex = D; int = E; per = D; con = E })

type katana (base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = B; dex = B; int = E; per = E; con = E })

type knife (base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = A; dex = D; int = E; per = E; con = E })



// some weapon instances
//

let cia_gun = gun (5)
let cia_shotgun = shotgun (10)
let cia_pump_shotgun = pump_shotgun (13)

let epic_gun = gun (8, { str = B; dex = C; int = C; per = A; con = E })
let epic_katana = katana (15, { str = S; dex = C; int = E; per = E; con = E })
let legendary_shotgun = shotgun (20, { str = B; dex = C; int = C; per = S; con = E })
let legendary_knucles = knucles (10, { str = S; dex = A; int = E; per = E; con = C })

