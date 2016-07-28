
module GdrSim.Main

open Globals
open Weapon
open Engine
open FSharp.Common
open FSharp.Common.Log
open System.Collections.Generic
open Character


let test_attacks (rounds : int) =
    let pc = Store.Pc.cia_gunner1
    L.msg Normal "%O" pc
    let thp = pc.target.health
    let sim = new sim (pc)
    let met1 = new damage_meter (sim)
    let sim = sim.perform_actions <| seq { for i = 1 to int pc.ca_per_round * rounds do yield Attack }
    L.debug Normal "final state: %O" sim
    L.msg Normal "%s" met1.report



[<EntryPoint>]
let main argv =
    let code =
        try
            test_attacks 10
            0
        with e -> L.fatal_error "%s\nStack trace:\n%s" (pretty_exn_and_inners e) e.StackTrace; 1
    
    #if DEBUG
    printfn "\nPress any key to exit..."
    System.Console.ReadKey () |> ignore
    #endif
    code

