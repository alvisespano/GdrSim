

module GdrSim.Character

open GdrSim.Globals
open GdrSim.Weapon

type move = Shoot



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
            | roll.Crit     -> this.aimed_attack npc w
            | roll.Success  -> dmg
            | roll.Fail     -> 0.
        
    