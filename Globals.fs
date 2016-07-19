

module GdrSim.Globals

let PRNG = new System.Random ()

let rand_float (a, b) = (PRNG.NextDouble () * (b - a)) + a
let rand_int (a, b) = rand_float (float a, float b) |> round |> int

let is_percent x = x >= 0. && x <= 1.

[< RequireQualifiedAccessAttribute >]
type roll = Crit | Success | Fail

let roll_d100 x =
    assert is_percent x
    let y = rand_float (0., 1.)
    in
        if y < x / 10. then roll.Crit
        elif y < x then roll.Success
        else roll.Fail

    | 