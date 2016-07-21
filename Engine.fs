
module GdrSim.Engine

open FSharp.Common
open Character
open Globals
open Skill
open Weapon
open FSharp.Common.Log

let apply_build (pc : pc) (build : build) = 
    for name, i in build do
        let abi = find_ability name
        match abi.rule with
        | Active _            -> ()     // deal only with passive abilities
        | Passive (n, on_use) -> on_use i n pc

type action with
    member this.perform (pc : pc) =        
        let pc = pc.clone_and_apply_buffs 
        match this with
        | Attack ->
            let target = pc.target
            let roll_attack base_hit (w : weapon) =
                if w.can_reach pc target then 
                    match roll_d100 (pc.hit base_hit w) with
                    | roll.Crit    // TODO: implementare il crit
                    | roll.Success  -> w.dmg pc.stats
                    | roll.Fail     -> 0<hp>
                else 0<hp>
            let rdmg = roll_attack pc.base_right_hit pc.right_weapon
            let ldmg =
                match pc.right_weapon.hand with
                | OneHand | MainHand -> roll_attack pc.base_left_hit pc.left_weapon
                | TwoHand -> 0<hp>
            let dmg = ldmg + rdmg
            target.health <- target.health - dmg
            L.msg Normal "[Attack][target hp = %d][Ldmg = %d][Rdmg = %d][dmg = %d]" target.health ldmg rdmg dmg
            1<ca>

        | UseAbility name ->
            let abi = find_ability name
            let r =
                match abi.rule with
                | Passive _ -> unexpected "cannot use a passive ability: %s" __SOURCE_FILE__ __LINE__ name
                | Active (n, on_use) ->
                    let i = pc.build.lookup name
                    let out = on_use i n pc
                    out.ca
            L.msg Normal "[Use(%s)][CA = %d]" name r
            r


type single_player (pc) =
    let mutable current_ca = 0<ca>
    let mutable current_round = 0<round>
    
    member this.perform_actions (actions : action seq) =
        for a in actions do
            this.perform_action a

    member this.perform_action (a : action) =
        let dca = a.perform pc
        current_ca <- current_ca + dca
        // update buffs whose duration is in CAs
        pc.filter_buffs (fun (buff : buff) ->
            match buff.duration with
            | Ca n -> if n <= dca then false else buff.duration <- Ca (n - dca); true
            | _ -> true)
        if current_ca >= pc.ca_per_round then
            // start new round
            current_ca <- current_ca % pc.ca_per_round
            let dround = dca / (pc.ca_per_round / 1<round>) // trick for converting unit of measure to ca/round
            current_round <- current_round + dround
            // update buffs whose duration is in rounds
            pc.filter_buffs (fun (buff : buff) ->
                match buff.duration with
                | Round n -> if n <= dround then false else buff.duration <- Round (n - dround); true
                | _ -> true)

                