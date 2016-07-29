
module GdrSim.Config

open FSharp.Common
open FSharp.Data.UnitSystems.SI.UnitSymbols

// availabled compilation symbols
//
// SHOW_DATA_IN_METER_REPORT    include the 'data' field into the meter_report record


let logger_cfg =
    let cfg = new Log.config ()
    cfg.show_datetime <- false
    cfg.msg_header <- ""
    cfg

let retab_width = 4
let crit_percent = 0.10

module Pc =
    let base_max_health (stats : stats) = stats.con * 100<hp>
    let death_threshold (stats : stats) = -stats.con * 1<hp>
    let base_dodge (stats : stats) = proj_int_to_float (0, 20) (0.20, 0.90) stats.dex

    let base_ca_per_round = 4<ca>
    let base_arm_hit = 0.50
    let base_larm_hit_malus = -0.20
    let base_aimed_hit_malus = -0.40
    
    let dummy_max_health = 1000<hp>
    let dummy_position = StandingAt 20.<m>

    let default_position = StandingAt 0.<m>
    let default_melee_distance = 0.5<m> // below ranged min range, so equipped weapon cannot shoot
    let default_ranged_distance = 15.<m>

module Weapon =

    module Ranged =
        let default_min_range = 0.7<m>
        let default_max_range = 50.<m>

    module Melee =
        let default_min_range = 0.<m>
        let default_max_range = 2.5<m>


module Report =

    module Damage =
        let dpca_min = 1
        let dpca_max = Pc.base_ca_per_round / 1<ca>
        let dpr_min = 1
        let dpr_max = 4

