

module GdrSim.Character

open GdrSim.Globals
open GdrSim.Weapon
open System
open FSharp.Common

type build = (string * int) list

let empty_build = []

let find_in_build (name : string) (build : build) =
    try List.find (fun (name' : string, n) -> name.ToLower() = name'.ToLower()) build |> snd
    with :? System.Collections.Generic.KeyNotFoundException -> unexpected "ability \"%s\" does not belong to build" __SOURCE_FILE__ __LINE__ name

type [< AbstractClass >] target () as this =
    abstract max_health : int<hp> with get, set
    member val health = this.max_health with get, set

type dummy (?max_health) =
    inherit target ()
    override val max_health = defaultArg max_health 1000000<hp> with get, set

type pc (stats, build) as this =
    inherit target ()

    member val stats : stats = stats

    override val max_health = this.stats.con * 10<hp> with get, set

    member val build : build = build with get, set
    member val target : target = upcast dummy () with get, set

    member val right_weapon : weapon = upcast unarmed () with get, set
    member val left_weapon : weapon = upcast unarmed () with get, set

    member val base_hit = 0.50 with get, set
    member val aimed_hit_malus = -0.40 with get, set
    member val dmg_mult = 1.0 with get, set

    member this.hit (w : weapon) = this.base_hit + this.aimed_hit_malus + w.hit_mod
            

        
    