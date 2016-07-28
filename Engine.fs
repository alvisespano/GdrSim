
module GdrSim.Engine

open FSharp.Common
open Character
open Globals
open Skill
open Weapon
open FSharp.Common.Log
open FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Collections.Generic
open Printf


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
    | EquipR of weapon
    | EquipL of weapon
with
    override this.ToString () =
        match this with
        | Attack -> "atk"
        | UseAbility name -> sprintf "use(%s)" name
        | GoMelee -> "goMelee"
        | GoRanged -> "goRanged"
        | EquipR w -> sprintf "equipR(%s)" w.name
        | EquipL w -> sprintf "equipL(%s)" w.name

    member this.performer (pc : pc) : int<ca> * action_performer =        
        let pc = pc.clone_and_apply_buffs
        let log l cai can (fmt : StringFormat<'a, 'r>) =
            let fmt = StringFormat<action -> int<ca> -> int<ca> -> 'a, 'r> ("[%O][CA# %d/%d]" + fmt.Value)
            l Min fmt this (cai + 1<ca>) can  // print CA# incremented by 1 
        let msg cai can fmt = log L.msg cai can fmt
        let warn cai can fmt = log L.warn cai can fmt
        match this with
        | Attack ->            
            1<ca>, (fun cai can ->
                let target = pc.target
                let arm_attack (a : arm) =
                    if a.weapon.can_reach pc target then 
                        match roll_d100 a.hit with
                        | roll.Crit    // TODO: implementare il crit
                        | roll.Success  -> a.weapon.dmg pc.stats
                        | roll.Fail     -> 0<hp>
                    else 0<hp>
                let rdmg = arm_attack pc.R
                let ldmg = if pc.can_dual_wield then arm_attack pc.L else 0<hp>
                let dmg = ldmg + rdmg
                target.health <- target.health - dmg
                msg cai can "[target hp = %d][L-dmg = %d][R-dmg = %d][dmg = %d]" target.health ldmg rdmg dmg)

        | EquipR w ->
            pc.equip_weapon_time, (fun cai can ->
                    pc.R.weapon <- w
                    if not pc.can_dual_wield then pc.L.unequip_weapon
                    msg cai can "[R-weapon = %O][L-weapon = %O]" pc.R.weapon pc.L.weapon)

        | EquipL w ->
            pc.equip_weapon_time, (fun cai can ->
                    if pc.can_dual_wield && w.hand = OneHand then
                        pc.L.weapon <- w
                        msg cai can "[R-weapon = %O][L-weapon = %O]" pc.R.weapon pc.L.weapon
                    else warn cai can "cannot equip [%O] in left weapon" w)

        | UseAbility name ->            
            let abi = find_ability name            
            match abi.rule with
            | Passive _ -> unexpected "cannot use a passive ability: %s" __SOURCE_FILE__ __LINE__ name
            | Active (n, on_use) ->
                let i = pc.build.lookup name
                let out = on_use i n pc
                out.ca, (fun cai can ->
                    msg cai can "[out = %O]" out
                    out.performer cai can)

        | GoMelee ->
            1<ca>, (fun cai can ->
                pc.position <- pc.target.position
                msg cai can "ok")

        | GoRanged ->
            1<ca>, (fun cai can ->
                pc.position <- pc.target.position + Config.Pc.default_ranged_position
                msg cai can "ok")





type sim (pc_ : pc) as this =
    let mutable current_ca = 0<ca>
    let mutable current_round = 0<round>
    let mutable total_ca_cnt = 0<ca>
    let before_ca_cbs = new LinkedList<sim -> unit> ()
    let after_ca_cbs = new LinkedList<sim -> unit> ()
    let start_of_new_round_cbs = new LinkedList<sim -> unit> ()
    let end_of_round_cbs = new LinkedList<sim -> int<ca> -> unit> ()

    do
        let decrement_buff_durations (c : int<'u> -> duration) ((|C|_|) : duration -> int<'u> option) =    // type annotation required for polymorphic inference of measures
            this.pc.filter_buffs (fun (buff : buff) ->
                    match buff.duration with
                    | C n -> if n <= LanguagePrimitives.Int32WithMeasure<'u> 0 then false else buff.duration <- c (n - LanguagePrimitives.Int32WithMeasure<'u> 1); true
                    | _ -> true)
        this.register_after_ca <| fun _ -> decrement_buff_durations Ca (function Ca n -> Some n | _ -> None)
        this.register_end_of_round <| fun _ _ -> decrement_buff_durations Round (function Round n -> Some n | _ -> None)
        
    member __.pc : pc = pc_

    member this.call_before_ca = for cb in before_ca_cbs do cb this
    member this.call_after_ca = for cb in after_ca_cbs do cb this
    member this.call_start_of_new_round = for cb in start_of_new_round_cbs do cb this
    member this.call_end_of_round cas = for cb in end_of_round_cbs do cb this cas

    member this.register_before_ca (f : sim -> unit) = before_ca_cbs.AddLast f |> ignore
    member this.register_after_ca (f : sim -> unit) = after_ca_cbs.AddLast f |> ignore
    member this.register_start_of_new_round (f : sim -> unit) = start_of_new_round_cbs.AddLast f |> ignore
    member this.register_end_of_round (f : sim -> _ -> unit) = end_of_round_cbs.AddLast f |> ignore        

    member this.perform_action (a : action) =
        let dca, performer = a.performer this.pc
        if current_ca = 0<ca> then this.call_start_of_new_round   // start-of-new-round callback must be invoked before any CA-related callback
        if dca > 0<ca> then
            // perform action taking 1 or more CAs to execute
            for i = 0 to int dca - 1 do
                this.call_before_ca
                performer (LanguagePrimitives.Int32WithMeasure<ca> i) dca
                this.call_after_ca
                total_ca_cnt <- total_ca_cnt + 1<ca>
                current_ca <- current_ca + 1<ca>
                if current_ca >= this.pc.ca_per_round then
                    this.call_end_of_round current_ca   // pass the number of CAs the finishing round had
                    current_ca <- 0<ca>
                    current_round <- current_round + 1<round>
        else
            // perform action taking no time to execute
            performer 0<ca> dca
               
    member this.perform_actions (actions : action seq) =
        for a in actions do
            this.perform_action a

    override this.ToString () =
        sprintf "CA:%d round:%d totCA:%d" current_ca current_round total_ca_cnt



type meter_report = {
    data : float[]
    count : int
    total : float
    average : float
    min : float
    max : float
    median : float
    variance : float
    std_deviation : float   // sinonimo di scarto quadratico medio
    coefficient_of_variation : float
}
with
    override this.ToString () =
        sprintf "data = %s\ncount = %d\ntotal = %g\navg = %g\nmin = %g\nmax = %g\nmedian = %g\nvariance = %g\nstd deviation = %g\ncoefficient of variance = %s"
            (mappen_stringables (sprintf "%g") " " this.data)
            this.count this.total this.average this.min this.max this.median
            this.variance this.std_deviation (pretty_percent this.coefficient_of_variation)

    static member create sq =
        let a = Seq.toArray sq  // conversion to array should improve performance
        let μ = Array.average a
        let len = a.Length
        let σ2 = Array.fold (fun z x -> z + (x - μ) ** 2.) 0. a / float len
        let σ = sqrt σ2
        in
            { data = a
              count = len
              average = μ
              total = Array.sum a
              min = Array.min a
              max = Array.max a

              median =
                let a = Array.sort a
                let v = a.[len / 2]
                in
                    if len % 2 = 1 then v else (a.[len / 2 - 1] + v) / 2.

              variance = σ2
              std_deviation = σ
              coefficient_of_variation = σ / abs μ
            }

type damage_meter (sim : sim) =
    let dmgtab = new LinkedList<LinkedList<float>> ()
    
    do
        let mutable last_target_health_ca = 0<hp>
        sim.register_start_of_new_round <| fun (sim : sim) -> dmgtab.AddLast (new LinkedList<_> ()) |> ignore
        sim.register_before_ca <| fun (sim : sim) -> last_target_health_ca <- sim.pc.target.health
        sim.register_after_ca <| fun (sim : sim) -> dmgtab.Last.Value.AddLast (last_target_health_ca - sim.pc.target.health |> float) |> ignore

    member this.dpca n =
        seq { for dmgrow in dmgtab do
                for l in Seq.chunkBySize n dmgrow do
                    yield Seq.sum l }
        |> meter_report.create              

    member this.dpr n =
        seq { for rounds in Seq.chunkBySize n dmgtab do
                yield Seq.sum <| seq { for dmgrow in rounds do yield Seq.sum dmgrow } }
        |> meter_report.create              

    member this.round_length =
        seq { for dmgrow in dmgtab do
                yield Seq.length dmgrow |> float }
        |> meter_report.create              

    member this.report =
        let retab x = retab Config.retab_width (sprintf "%O" x)
        let body =
            Computation.B.string {
                yield! sprintf "Round Length:\n%s\n" (this.round_length |> retab)
                for i = Config.Report.Damage.dpca_min to Config.Report.Damage.dpca_max do
                    yield! sprintf "DP%dCA:\n%s\n" i (this.dpca i |> retab)
                for i = Config.Report.Damage.dpr_min to Config.Report.Damage.dpr_max do
                    yield! sprintf "DP%dR:\n%s\n" i (this.dpr i |> retab)
            }
        in
            sprintf "Damage Report:\n%s" (retab body)

    override this.ToString () = sprintf "%A" dmgtab