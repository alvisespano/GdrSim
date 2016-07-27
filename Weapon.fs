
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

type [< AbstractClass >] weapon (hand_, min_range_, max_range_, hit_mod_, base_dmg_, stats_dmg_scale_ : stats_dmg_scale) =
    member val hit_mod = hit_mod_
    member val hand = hand_
    member val base_dmg = base_dmg_
    member val stats_dmg_scale = stats_dmg_scale_
    member val min_range : int<m> = min_range_
    member val max_range : int<m> = max_range_
    member this.scaled_dmg (stats : stats) =
        let ds = this.stats_dmg_scale
        in
            float stats.str * !> ds.str
                + float stats.dex * !> ds.dex
                + float stats.con * !> ds.con
                + float stats.int * !> ds.int
                + float stats.per * !> ds.per    
            |> Operators.round |> int |> LanguagePrimitives.Int32WithMeasure<hp>

    member this.dmg (stats : stats) = this.base_dmg + this.scaled_dmg stats

    override this.ToString () =
        let name = this.GetType().Name
        in
            sprintf "%s %O %+g%% range:%dm-%dm base:%d + {%s}" name this.hand this.hit_mod this.min_range this.max_range this.base_dmg (pretty_stats_dmg_scale this.stats_dmg_scale) 

    abstract pretty_with_dmg : stats -> string
    default this.pretty_with_dmg (stats : stats) =
        sprintf "%O scale:+%d tot:%d" this (this.scaled_dmg stats) (this.dmg stats)


type [< AbstractClass >] ranged_weapon (hand, hit_mod, base_dmg, stats_dmg_scale) =
    inherit weapon (hand, 1<m>, 100<m>, hit_mod, base_dmg, stats_dmg_scale)

type [< AbstractClass >] melee_weapon (hand, hit_mod, base_dmg, stats_dmg_scale) =
    inherit weapon (hand, 0<m>, 3<m>, hit_mod, base_dmg, stats_dmg_scale)

type [< AbstractClass >] firearm (hand, hit_mod, base_dmg, stats_dmg_scale, fire_rate_ : float) =
    inherit ranged_weapon (hand, hit_mod, base_dmg, stats_dmg_scale)
    new (hand, hit_mod, base_dmg, stats_dmg_scale, reload_every, reload_time) = new firearm (hand, hit_mod, base_dmg, stats_dmg_scale, float reload_every / (float reload_every + float reload_time))
    member this.fire_rate = fire_rate_
    member this.avg_dpca stats = (this.dmg stats |> float |> LanguagePrimitives.FloatWithMeasure<hp>) * this.fire_rate
    override this.ToString () = sprintf "%s rate:%s" (base.ToString ()) (pretty_percent this.fire_rate)
        


// standard weapon subclasses
//

type gun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = Z; dex = Z; int = Z; per = B; con = Z }, 6, 1)

type shotgun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, -0.25, base_dmg, defaultArg stats_dmg_scale { str = C; dex = Z; int = Z; per = B; con = Z }, 4, 1)

type assault_rifle (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, 0., base_dmg, defaultArg stats_dmg_scale { str = Z; dex = C; int = Z; per = B; con = Z }, 20, 1)

type sniper_rifle (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, 0.20, base_dmg, defaultArg stats_dmg_scale { str = Z; dex = C; int = C; per = B; con = Z }, 3, 1)

type pump_shotgun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (MainHand, -0.30, base_dmg, defaultArg stats_dmg_scale { str = C; dex = C; int = Z; per = C; con = Z }, 2, 2)

type sawnoff_shotgun (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, -0.40, base_dmg, defaultArg stats_dmg_scale { str = Z; dex = Z; int = Z; per = C; con = C }, 1, 2)

type bolt_action_rifle (base_dmg, ?stats_dmg_scale) =
    inherit firearm (TwoHand, 0.30, base_dmg, defaultArg stats_dmg_scale { str = C; dex = Z; int = Z; per = B; con = Z }, 1, 2)

type bow (base_dmg, ?stats_dmg_scale) =
    inherit ranged_weapon (TwoHand, -0.10, base_dmg, defaultArg stats_dmg_scale { str = C; dex = Z; int = Z; per = B; con = C })

type unarmed () =
    inherit melee_weapon (OneHand, 0., 1<hp>, { str = B; dex = C; int = Z; per = Z; con = C })

type knucles (base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = A; dex = Z; int = Z; per = Z; con = Z })

type katana (base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = B; dex = B; int = Z; per = Z; con = Z })

type knife (base_dmg, ?stats_dmg_scale) =
    inherit melee_weapon (OneHand, 0., base_dmg, defaultArg stats_dmg_scale { str = A; dex = Z; int = Z; per = Z; con = Z })



// some weapon instances
//

let cia_gun = gun (5<hp>)
let cia_shotgun = shotgun (10<hp>)
let cia_pump_shotgun = pump_shotgun (13<hp>)

let epic_gun = gun (8<hp>, { str = B; dex = C; int = C; per = A; con = Z })
let epic_katana = katana (15<hp>, { str = S; dex = C; int = Z; per = Z; con = Z })
let legendary_shotgun = shotgun (20<hp>, { str = B; dex = C; int = C; per = S; con = Z })
let legendary_knucles = knucles (10<hp>, { str = S; dex = A; int = Z; per = Z; con = C })

