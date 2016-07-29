
[< AutoOpen >]
module GdrSim.Prelude

open FSharp.Common
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

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

type [< NoComparison >] position =
    | StandingAt of float<m>
    | MovingAround of float<m> * float<m>
with
    override this.ToString () =
        match this with
        | StandingAt x -> sprintf "@%g" x
        | MovingAround (x, y) -> sprintf "@%g-%g" x y

    member this.approximately_at =
        match this with
        | StandingAt x -> x
        | MovingAround (x1, x2) -> abs (x1 - x2) / 2.

    static member (+) (p : position, d : float<m>) =
        match p with
        | StandingAt x -> StandingAt (x + d)
        | MovingAround (x, y) -> MovingAround (x + d, y + d)

    static member (-) (a : position, b : position) = abs (a.approximately_at - b.approximately_at)
        

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

