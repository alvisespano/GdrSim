

module GdrSim.Globals

open FSharp.Common
open FSharp.Common.Log

let L =
    let cfg = new Log.config ()
    cfg.show_datetime <- false
    new Log.console_logger (cfg)

// units of measure

type [< Measure >] ca           // combat action
type [< Measure >] round        // round
type [< Measure >] hp           // both damage and health is measured in hp


// basic types
//

type duration =
    | Ca of int<ca>
    | Round of int<round>


// random and rolls
//

let Rng = new System.Random ()

let rand_float (a, b) = (Rng.NextDouble () * (b - a)) + a
let rand_int (a, b) = rand_float (float a, float b) |> Operators.round |> int

[< RequireQualifiedAccessAttribute >]
type roll = Crit | Success | Fail

let roll_d100 (x : float) =
    assert (x > 0.)
    let y = rand_float (0., 1.)
    if y < x / 10. then roll.Crit
    elif y < x then roll.Success
    else roll.Fail

   

