﻿
module GdrSim.Main

open Globals
open Weapon
open Engine
open FSharp.Common
open FSharp.Common.Log


let test_attacks (rounds : int) =
    let pc = Sample.Pc.cia_gun1
    L.msg High "%O" pc
    let thp = pc.target.health
    let sim = new sim (pc)
    let met1 = new dmg_done_meter (sim)
    let sim = sim.perform_actions <| seq { for i = 1 to int pc.ca_per_round * rounds do yield Attack }
    L.msg Normal "final state: %O" sim
    L.msg High "%s" met1.pretty_report
//    let dmg = thp - pc.target.health
//    let dp1ca = dmg / sim.ca_cnt
//    let dp2ca = dmg / sim.ca_cnt
//    let dpr = dmg / (sim.current_round + 1<round>)
//    L.msg High "total dmg = %d\ndpca = %d\ndpr = %d" dmg dp1ca dpr

[<EntryPoint>]
let main argv =
    let code =
        try
            test_attacks 10
            0
        with e -> L.fatal_error "%s\nStack trace:\n%s" (pretty_exn_and_inners e) e.StackTrace; 1
    
    #if DEBUG
    System.Console.ReadKey () |> ignore
    #endif
    code

