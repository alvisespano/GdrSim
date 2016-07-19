

module GdrSim.Skill

open FSharp.Common
open GdrSim.Character


type rule_proc_out = {
    pc : pc
    ca : int
}

type rule_type = Active | Passive

type rule = {
    typee : rule_type
    cost : int                                   // cost in skill points
    effect : pc -> rule_proc_out  // stat processor function
}

type req = (int * string) option

type ability = {
    name : string
    req : req
    rules : rule list
}

let req n name = Some (n, name)
let no_req : req = None


// shortcuts for writing rules algorithmically
//

let proj (a, b) (a', b') i =
    let i = crop (a, b) i
    let k = float (b' - a') / float (b - a)
    in
        k * float (i - a) |> round |> int

let rules n rt cost f : rule list = [ for i = 1 to n do yield { typee = rt; cost = cost i n; effect = f i n } ]

let rules_fixed_cost n k rt f = rules n rt (fun _ _ -> k) f

let rules_cost1 n rt f = rules_fixed_cost n 1 rt f

let passive_stepped (a : float, b : float) (step : float) setter =
    let len = int ((b - a) / step) + 1
    in
        rules_cost1 len Passive (fun i n pc -> { pc = setter (a + step * float i) pc; ca = 0 })



// abilities
//

let abilities : ability list =
    [

        { name = "mira"
          req  = no_req
          rules = passive_stepped (0.05, 0.50) 0.05 (fun x pc -> { pc with hit_mod = x })
        }

        { name = "colpo mirato da fermo"
          req  = req 3 "mira"
          rules = rules_cost1 7 Active (fun i n pc ->
                            { pc = { pc with dmg_mod = 5 * float i }
                              ca = proj (1, n) (1, 5) i })
        }

        { name = "precisione"
          req  = req 4 "mira"
          rules = passive_stepped (-0.50, -0.10) 0.10 (fun x pc -> { pc with aimed_hit_malus = x })
        }

    ]




