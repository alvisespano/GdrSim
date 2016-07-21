
module GdrSim.Main

open Globals
open Weapon
open Character
open FSharp.Common
open FSharp.Common.Log


let test_attacks (rounds : int) =
    let pc = Sample.Pc.cia_gun1
    L.msg High "%O" pc
    let eng = Engine.single_player (pc)
    for i = 1 to rounds do
        eng.perform_actions <| seq { for i = 1 to int pc.ca_per_round do yield Attack }



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

