﻿

module GdrSim.Skill

open FSharp.Common
open GdrSim.Character
open Globals


type active_out = {
    ca : int<ca>
//    after : int -> int -> pc -> unit
}

type rule =
    | Active of int * (int -> int -> pc -> active_out)
    | Passive of int * (int -> int -> pc -> unit)

type req = (int * string) option

type ability = {
    name : string
    req : req
    rule : rule
}

let req n name = Some (n, name)
let no_req : req = None


// shortcuts for writing rule algorithmically
//

let proj (a, b) (a' : int<'u>, b') i =
    let i = crop (a, b) i
    let k = float (b' - a') / float (b - a)
    in
        k * float (i - a) |> Operators.round |> int |> LanguagePrimitives.Int32WithMeasure<'u>

let active n on_use = Active (n, on_use)
let passive n setter = Passive (n, setter)

let passive_stepped (a : float, b : float) (step : float) setter =
    let len = int ((b - a) / step) + 1
    in
        passive len (fun i n pc -> setter (a + step * float i) pc)



// abilities
//

//let UntilAttack = Until (fun () -> true)

let Abilities : ability list =
    [
        { name = "mira"
          req  = no_req
          rule = passive_stepped (0.10, 0.50) 0.10 (fun x pc -> pc.base_hit <- x)
        }

        { name = "mira da fermo"
          req  = req 3 "mira"
          rule = active 7 
                        (fun i n pc ->
                            pc.add_buff (buff ((fun pc -> pc.dmg_mult <- 1.0 + float i * 0.50), Ca 1<ca>))
                            { ca = proj (1, n) (1<ca>, 4<ca>) i }
                        )
        }

        { name = "precisione"
          req  = req 4 "mira"
          rule = passive_stepped (-0.50, -0.10) 0.10 (fun x pc -> pc.aimed_hit_malus <- x)
        }
    ]


let find_ability (name : string) =
    try List.find (fun { name = name' } -> name'.ToLower() = name.ToLower()) Abilities
    with :? System.Collections.Generic.KeyNotFoundException -> unexpected "ability \"%s\" does not exist" __SOURCE_FILE__ __LINE__ name


