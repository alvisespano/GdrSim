
[< AutoOpen >]
module GdrSim.Prelude

open FSharp.Common

// units of measure

type [< Measure >] ca           // combat action
type [< Measure >] round        // round
type [< Measure >] hp           // both damage and health is measured in hp


// stats

type 'a stats_bundle = {
    str : 'a
    dex : 'a
    con : 'a
    per : 'a
    int : 'a
}
with
    member this.pretty p sep =
        sprintf "str:%s%sdex:%s%scon:%s%sper:%s%sint:%s%s"
            (p this.str) sep
            (p this.dex) sep
            (p this.con) sep
            (p this.per) sep
            (p this.int) sep

type stats = int stats_bundle


// some math tools

// TODO: consider promoting these to some FSharp.Common.Math module
let proj_float (a : float<'u>, b) (a' : float<'v>, b') i =
    let i = crop (a, b) i
    let k = (b' - a') / (b - a)
    in
        k * (i - a)

let floatU (n : int<'u>) = n |> float |> LanguagePrimitives.FloatWithMeasure<'u>
let intU (x : float<'u>) = x |> float |> Operators.round |> int |> LanguagePrimitives.Int32WithMeasure<'u>

let proj_int (a, b) (a', b') i =
    proj_float (floatU a, floatU b) (floatU a', floatU b') (floatU i) |> intU

let proj_int_to_float (a, b) (a', b') i =
    proj_float (floatU a, floatU b) (a', b') (floatU i)

let proj_float_to_int (a, b) (a', b') i =
    proj_float (a, b) (floatU a', floatU b') i |> intU

