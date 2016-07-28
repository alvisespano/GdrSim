
module GdrSim.Store

open Globals
open Weapon
open Character

module Pc =
    module Stats =
        let avg_str_and_per = {
            str = 10
            dex = 8
            per = 10
            int = 6
            con = 7
        }

    module Build =
        let empty = FSharp.Common.Env.empty

    open Stats

    let unarmed_fighter1 = pc (avg_str_and_per, Build.empty)

    let cia_gunner1 =
        let r = pc (avg_str_and_per, Build.empty)
        r.R.weapon <- cia_pistol
        r



