

module GdrSim.Globals

// some units of measure

type [< Measure >] ca           // combat action
type [< Measure >] round        // round
type [< Measure >] hp           // both damage and health is measured in hp


// random and rolls
//

let Rng = new System.Random ()

let rand_float (a, b) = (Rng.NextDouble () * (b - a)) + a
let rand_int (a, b) = rand_float (float a, float b) |> Operators.round |> int

let is_percent x = x >= 0. && x <= 1.

[< RequireQualifiedAccessAttribute >]
type roll = Crit | Success | Fail

let roll_d100 (x : float) =
    assert (x > 0.)
    let y = rand_float (0., 1.)
    in
        if y < x / 10. then roll.Crit
        elif y < x then roll.Success
        else roll.Fail

   

