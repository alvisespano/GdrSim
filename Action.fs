
module GdrSim.Action

open GdrSim.Globals
open GdrSim.Character
open GdrSim.Skill
open GdrSim.Weapon
open FSharp.Common

type action =
    | Attack
    | Cast of string
with
    member this.perform (pc : pc) =        
        match this with
        | Attack ->
            let target = pc.target
            let roll_attack (w : weapon) =
                match roll_d100 (pc.hit w) with
                | roll.Crit    // TODO: implementare il crit
                | roll.Success  -> w.dmg pc.stats
                | roll.Fail     -> 0<hp>
            let rdmg = roll_attack pc.right_weapon
            let ldmg =
                match pc.right_weapon.hand with
                | OneHand | MainHand -> roll_attack pc.left_weapon
                | TwoHand -> 0<hp>
            let dmg = ldmg + rdmg
            target.health <- target.health - dmg
            1<ca>

        | Cast name ->
            let abi = find_ability name
            match abi.rule with
            | Passive _ -> unexpected "cannot cast a passive ability: %s" __SOURCE_FILE__ __LINE__ name
            | Active (n, on_use) ->
                let i = find_in_build name pc.build
                let out = on_use i n pc
                out.after i n pc
                out.ca
                

