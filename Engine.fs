
module GdrSim.Engine

open FSharp.Common
open Character
open Globals
open Skill
open Weapon
open FSharp.Common.Log
open FSharp.Data.UnitSystems.SI.UnitSymbols


let apply_build (pc : pc) (build : build) = 
    for name, i in build do
        let abi = find_ability name
        match abi.rule with
        | Active _            -> ()     // deal only with passive abilities
        | Passive (n, on_use) -> on_use i n pc



type action =
    | Attack
    | UseAbility of string
    | GoMelee
    | GoRanged
with
    override this.ToString () =
        match this with
        | Attack -> "atk"
        | UseAbility name -> sprintf "use(%s)" name
        | GoMelee -> "goMelee"
        | GoRanged -> "goRanged"

    member this.perform (pc : pc) =        
        let pc = pc.clone_and_apply_buffs 
        match this with
        | Attack ->
            let target = pc.target
            let roll_attack (a : arm) =
                if a.weapon.can_reach pc target then 
                    match roll_d100 a.hit with
                    | roll.Crit    // TODO: implementare il crit
                    | roll.Success  -> a.weapon.dmg pc.stats
                    | roll.Fail     -> 0<hp>
                else 0<hp>
            let rdmg = roll_attack pc.R
            let ldmg =
                match pc.R.weapon.hand with
                | OneHand | MainHand -> roll_attack pc.L
                | TwoHand -> 0<hp>
            let dmg = ldmg + rdmg
            target.health <- target.health - dmg
            L.msg Normal "[%O][target hp = %d][Ldmg = %d][Rdmg = %d][dmg = %d]" this target.health ldmg rdmg dmg
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
            L.msg Normal "[%O][CA = %d]" this r
            r

        | GoMelee ->
            pc.position <- pc.target.position
            L.msg Normal "[%O]" this
            1<ca>

        | GoRanged ->
            pc.position <- pc.target.position - 100<m>
            L.msg Normal "[%O]" this
            1<ca>



type sim = {
    current_ca : int<ca>
    current_round : int<round>
    ca_cnt : int<ca>
}
with
    static member start = { current_ca = 0<ca>; current_round = 0<round>; ca_cnt = 0<ca> }

    member this.perform_action pc (a : action) =
//        L.debug Low "[perform][%O] %O" a this
        let dca = a.perform pc
        let current_ca = this.current_ca + dca
        // update buffs whose duration is in CAs
        pc.filter_buffs (fun (buff : buff) ->
            match buff.duration with
            | Ca n -> if n <= dca then false else buff.duration <- Ca (n - dca); true
            | _ -> true)
        let current_ca, current_round =
            if current_ca >= pc.ca_per_round then
                // start new round
                let dround = current_ca / (pc.ca_per_round / 1<round>) // trick for converting unit of measure to ca/round
                let current_round = this.current_round + dround
                let current_ca = current_ca % pc.ca_per_round
                // update buffs whose duration is in rounds
                pc.filter_buffs (fun (buff : buff) ->
                    match buff.duration with
                    | Round n -> if n <= dround then false else buff.duration <- Round (n - dround); true
                    | _ -> true)
                current_ca, current_round
            else current_ca, this.current_round
        in
            { current_ca = current_ca; current_round = current_round; ca_cnt = this.ca_cnt + 1<ca> }
                
    member this.perform_actions pc (actions : action seq) =
        Seq.fold (fun (this : sim) a -> this.perform_action pc a) this actions

    override this.ToString () =
        sprintf "CA:%d round:%d totCA:%d" this.current_ca this.current_round this.ca_cnt