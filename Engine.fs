
module GdrSim.Engine

open FSharp.Common
open Character
open Globals
open Skill
open Weapon
open FSharp.Common.Log
open FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Collections.Generic


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

    member this.performer (pc : pc) =        
        let pc = pc.clone_and_apply_buffs
        match this with
        | Attack ->            
            1<ca>, (fun _ ->
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
                L.msg Low "[%O][target hp = %d][Ldmg = %d][Rdmg = %d][dmg = %d]" this target.health ldmg rdmg dmg)

        | UseAbility name ->            
            let abi = find_ability name            
            match abi.rule with
            | Passive _ -> unexpected "cannot use a passive ability: %s" __SOURCE_FILE__ __LINE__ name
            | Active (n, on_use) ->
                let i = pc.build.lookup name
                let out = on_use i n pc
                out.ca, (fun cai ->
                    L.msg Low "[%O][CA = #%d/%d]" this cai out.ca
                    out.performer cai)

        | GoMelee ->
            1<ca>, (fun _ ->
                pc.position <- pc.target.position
                L.msg Low "[%O]" this)

        | GoRanged ->
            1<ca>, (fun _ ->
                pc.position <- pc.target.position - 100<m>
                L.msg Low "[%O]" this)



type sim (pc : pc) as this =
    let mutable current_ca = 0<ca>
    let mutable current_round = 0<round>
    let mutable total_ca_cnt = 0<ca>
    let before_ca_cbs = new LinkedList<sim -> unit> ()
    let after_ca_cbs = new LinkedList<sim -> unit> ()
    let start_of_new_round_cbs = new LinkedList<sim -> unit> ()
    let end_of_round_cbs = new LinkedList<sim -> int<ca> -> unit> ()

    do
        let decrement_buff_durations (pc : pc) (c : int<'u> -> duration) ((|C|_|) : duration -> int<'u> option) =    // type annotation required for polymorphic inference of measures
            pc.filter_buffs (fun (buff : buff) ->
                    match buff.duration with
                    | C n -> if n <= LanguagePrimitives.Int32WithMeasure<'u> 0 then false else buff.duration <- c (n - LanguagePrimitives.Int32WithMeasure<'u> 1); true
                    | _ -> true)
        this.register_after_ca <| fun _ -> decrement_buff_durations pc Ca (function Ca n -> Some n | _ -> None)
        this.register_end_of_round <| fun _ _ -> decrement_buff_durations pc Round (function Round n -> Some n | _ -> None)
        
    member val pc = pc

    member this.call_before_ca = for cb in before_ca_cbs do cb this
    member this.call_after_ca = for cb in after_ca_cbs do cb this
    member this.call_start_of_new_round = for cb in start_of_new_round_cbs do cb this
    member this.call_end_of_round cas = for cb in end_of_round_cbs do cb this cas

    member this.register_before_ca (f : sim -> unit) = before_ca_cbs.AddLast f |> ignore
    member this.register_after_ca (f : sim -> unit) = after_ca_cbs.AddLast f |> ignore
    member this.register_start_of_new_round (f : sim -> unit) = start_of_new_round_cbs.AddLast f |> ignore
    member this.register_end_of_round (f : sim -> _ -> unit) = end_of_round_cbs.AddLast f |> ignore        

    member this.perform_action (a : action) =
        let dca, perform = a.performer pc
        for i = 1 to int dca do
            if current_round = 0<round> then this.call_start_of_new_round   // start-of-new-round callback must be invoked before any CA-related callback
            this.call_before_ca
            perform (LanguagePrimitives.Int32WithMeasure<ca> i)
            this.call_after_ca
            total_ca_cnt <- total_ca_cnt + 1<ca>
            current_ca <- current_ca + 1<ca>
            if current_ca >= pc.ca_per_round then
                this.call_end_of_round current_ca   // pass the number of CAs executed in the round just ended
                current_ca <- 0<ca>
                current_round <- 0<round>
               
    member this.perform_actions (actions : action seq) =
        for a in actions do
            this.perform_action a

    override this.ToString () =
        sprintf "CA:%d round:%d totCA:%d" current_ca current_round total_ca_cnt


type meter_value = {
    avg : float
    min : float
    max : float
    var : float
    rms : float
    med : float
}
with
    static member of_seq sq =
        let sq = Seq.cache
        { avg = Seq.average sq
          min = Seq.min sq
          max = Seq.max sq
          var = Seq.sum (seq { for x in sq do  })
        }

type dmg_done_meter (sim : sim) =
    let dmgtab = new LinkedList<LinkedList<float>> ()
    
    do
        let mutable last_target_health_ca = 0<hp>
        sim.register_start_of_new_round <| fun (sim : sim) -> dmgtab.AddLast (new LinkedList<_> ()) |> ignore
        sim.register_before_ca <| fun (sim : sim) -> last_target_health_ca <- sim.pc.target.health
        sim.register_after_ca <| fun (sim : sim) -> dmgtab.Last.Value.AddLast (last_target_health_ca - sim.pc.target.health |> float) |> ignore

    member this.dpca n =
        seq { for dmgrow in dmgtab do
                for l in Seq.chunkBySize n dmgrow do
                    yield Seq.average l }
        |> Seq.average                

    member this.pretty_report =
        sprintf "Damage Done:\n\tDP1CA = " (this.dpca 1) (this.dpca 2) (this.dpca 3)

