
module GdrSim.Weapon

open GdrSim.Globals

// weapon type hierarchy
//

type [< AbstractClass >] weapon (hit_mod, dmg) =
    abstract hit_mod : float
    abstract dmg : int<hp>
    default val hit_mod = hit_mod
    default val dmg = dmg
    abstract can_dualwield : bool

type [< AbstractClass >] ranged_weapon (hit_mod, dmg) =
    inherit weapon (hit_mod, dmg)

type [< AbstractClass >] bow (hit_mod, dmg) =
    inherit ranged_weapon (hit_mod, dmg)

type [< AbstractClass >] melee_weapon (hit_mod, dmg) =
    inherit weapon (hit_mod, dmg)

type [< AbstractClass >] unarmed (dmg) =
    inherit melee_weapon (1., dmg)

type [< AbstractClass >] firearm (hit_mod, dmg_mult : float, dmg_level : int, fire_rate : float) =
    inherit ranged_weapon (hit_mod, LanguagePrimitives.Int32WithMeasure<hp> (dmg_mult * float dmg_level |> Operators.round |> int))
    new (hit_mod, dmg, dmg_level, reload_every, reload_time) = new firearm (hit_mod, dmg, dmg_level, float reload_every / (float reload_every + float reload_time))
    override val can_dualwield = false
    member this.fire_rate = fire_rate
    member this.avg_dpca = (LanguagePrimitives.FloatWithMeasure<hp> (float this.dmg)) * this.fire_rate

type gun (hit_mod, dmg_level) =
    inherit firearm (hit_mod, 5., dmg_level, 6, 1)
    override val can_dualwield = true

type shotgun (hit_mod, dmg_level) =
    inherit firearm (hit_mod, 11., dmg_level, 4, 1)

type assault_rifle (hit_mod, dmg_level) =
    inherit firearm (hit_mod, 7.5, dmg_level, 20, 1)

type sniper_rifle (hit_mod, dmg_level) =
    inherit firearm (hit_mod, 8.5, dmg_level, 3, 1)

type pump_shotgun (hit_mod, dmg_level) =
    inherit firearm (hit_mod, 12., dmg_level, 2, 2)

type sawnoff_shotgun (hit_mod, dmg_level) =    // fucile a canne mozze
    inherit firearm (hit_mod, 14.5, dmg_level, 1, 2)
    override val can_dualwield = true

type bolt_action_rifle (hit_mod, dmg_level) =
    inherit firearm (hit_mod, 18.0, dmg_level, 1, 2)


// some basic weapon instances
//

let cia_pistol = gun (0., 1)
let cia_shotgun = shotgun (-0.10, 1)
let cia_pump_shotgun = pump_shotgun (-0.20, 1)

