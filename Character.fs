

module GdrSim.Character

open GdrSim.Globals
open GdrSim.Weapon
open FSharp.Common
open System
open FSharp.Data.UnitSystems.SI.UnitSymbols

type build = Env.t<string, int>
   
type [< AbstractClass >] target (max_health, position) =
    member val max_health : int<hp> = max_health with get, set
    member val health = max_health with get, set
    member this.is_alive = this.health > 0<hp>
    member val position : int<m> = position with get, set

type weapon with
    member this.can_reach (t1 : target) (t2 : target) = let d = abs (t2.position - t1.position) in d <= this.max_range && d >= this.min_range


type dummy (?max_health) =
    inherit target (defaultArg max_health 1000<hp>, 15<m>)

type pc (stats_, build_) =
    inherit target (stats_.con * 10<hp>, 0<m>)

    member val stats : stats = stats_ with get, set
    member val build : build = build_ with get, set

    member val target : target = upcast dummy () with get, set

    member val right_weapon : weapon = upcast unarmed () with get, set
    member val left_weapon : weapon = upcast unarmed () with get, set

    member val ca_per_round = 3<ca> with get, set
    member val base_hit = 0.50 with get, set
    member val left_hit_malus = -0.20 with get, set
    member this.base_right_hit = this.base_hit
    member this.base_left_hit = this.base_hit + this.left_hit_malus
    member val aimed_hit_malus = -0.40 with get, set
    member val dmg_mult = 1.0 with get, set

    member this.hit base_hit (w : weapon) = base_hit + w.hit_mod
    member this.aimed_hit base_hit (w : weapon) = this.hit base_hit w + this.aimed_hit_malus

//    member this.right_hit = this.base_right_hit + this.aimed_hit_malus + this.right_weapon.hit_mod
//    member this.left_hit = this.base_left_hit + this.aimed_hit_malus + this.left_weapon.hit_mod
            
    member val active_buffs : buff list = [] with get, set
    member this.add_buff (buff : buff) = this.active_buffs <- buff :: this.active_buffs
    member this.filter_buffs f = this.active_buffs <- List.filter f this.active_buffs

    interface ICloneable with
        member this.Clone () =
            let r = this.MemberwiseClone () :?> pc
            // no need to copy pure members (such as stats or active_buffs list etc) because any modification would create new objects anyway
            r :> obj

    member this.clone_and_apply_buffs =
        let r = (this :> ICloneable).Clone () :?> pc
        for buff in this.active_buffs do
            buff.apply this
        r

    override this.ToString () =
        let p bas w =
            let h bas w = this.hit bas w * 100. |> int
            in
                sprintf "hit:%d%% %s" (h bas w) (w.pretty_with_dmg this.stats)
        in
            sprintf "%s\n[R] %s\n[L] %s"
                (this.stats.pretty (sprintf "%d") "\n")
                (p this.base_right_hit this.right_weapon) 
                (p this.base_left_hit this.left_weapon)


and buff (f, duration_) =
    member this.apply (pc : pc) : unit = f pc        
    member val duration : duration = duration_ with get, set

        
type action =
    | Attack
    | UseAbility of string
