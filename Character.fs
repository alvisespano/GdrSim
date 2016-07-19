

module GdrSim.Character

open GdrSim.Globals

type move = Shoot

type [< AbstractClass >] weapon () =
    abstract hit_mod : float
    abstract dmg : float

type [< AbstractClass >] ranged_weapon () =
    inherit weapon ()

type [< AbstractClass >] melee_weapon () =
    inherit weapon ()

type [< AbstractClass >] unarmed () =
    inherit melee_weapon ()

type gun () =
    inherit ranged_weapon ()
    override val hit_mod = 0.
    override val dmg = 10.

type build = (string * int) list

type pc = {
    build : build

    right_weapon : weapon
    left_weapon : weapon

    base_hit        : float
    hit_mod         : float
    aimed_hit_malus : float

    dmg_mod : float
}
with
    member this.total_hit (w : weapon) =
        this.base_hit + this.hit_mod + (assert (this.aimed_hit_malus < 0.); this.aimed_hit_malus) + w.hit_mod

    member this.std_attack npc (w : weapon) =
        let hit = this.total_hit
        let dmg = w.dmg + this.dmg_mod
        in
            match roll_d100 hit with
            | roll.Crit     -> 
            | roll.Success  -> dmg
            | roll.Fail     -> 0.
        
    