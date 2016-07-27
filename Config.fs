
module GdrSim.Config

open FSharp.Common


let logger_cfg =
    let cfg = new Log.config ()
    cfg.show_datetime <- false
    cfg

let crit_percent = 0.10

let max_health_by_con (con : int) = con * 100<hp>

let dodge_by_dex (dex : int) = proj_int_to_float (0, 20) (0.20, 0.90) dex



