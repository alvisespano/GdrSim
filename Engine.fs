
module GdrSim.Engine

open Character
open Action
open Globals
open Skill

let apply_build_to_pc (build : build) (pc : pc) =    
    for name, i in build do
        let abi = find_ability name
        match abi.rule with
        | Active _            -> ()     // deal only with passive abilities
        | Passive (n, on_use) -> on_use i n pc

let sim_actions (pc : pc) (actions : action seq) =
    let mutable current_ca = 0<ca>
    let mutable current_round = 0<round>
    for action in actions do
        let ca = action.perform pc
        current_ca <- current_ca + ca
        if current_ca >= pc.ca_per_round then
            current_ca <- 0<ca>
            current_round <- current_round + 1<round>
