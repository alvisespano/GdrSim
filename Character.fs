

module GdrSim.Character

open GdrSim.Globals
open GdrSim.Weapon
open FSharp.Common
open System
open FSharp.Data.UnitSystems.SI.UnitSymbols

type build = Env.t<string, int>
   
type [< AbstractClass >] npc (max_health_, position_, dodge_) =
    member val max_health : int<hp> = max_health_ with get, set
    member val health = max_health_ with get, set
    member this.is_alive = this.health > 0<hp>
    member val position : int<m> = position_ with get, set
    member val dodge : float = dodge_ with get, set

type weapon with
    member this.can_reach (t1 : npc) (t2 : npc) = let d = abs (t2.position - t1.position) in d <= this.max_range && d >= this.min_range


type dummy (?max_health) =
    inherit npc (defaultArg max_health 1000<hp>, 15<m>, 0.)


type arm (base_hit_) =
    abstract base_hit : float with get, set
    default val base_hit = base_hit_ with get, set
    member val aimed_hit_malus = -0.40 with get, set
    member this.hit = this.base_hit + this.weapon.hit_mod
    member this.aimed_hit = this.hit + this.aimed_hit_malus
    member val weapon : weapon = upcast unarmed () with get, set
    interface ICloneable with
        member this.Clone () = this.MemberwiseClone ()

type larm (base_hit_) =
    inherit arm (base_hit_)
    member val hit_malus = -0.20 with get, set
    override this.base_hit with get () = base.base_hit + this.hit_malus // setter is not overridden


type pc (stats_, build_) =
    inherit npc (Config.max_health_by_con stats_.con, 0<m>, Config.dodge_by_dex stats_.dex)

    member val stats : stats = stats_ with get, set
    member val build : build = build_ with get, set
    member val target : npc = upcast dummy () with get, set
    member val ca_per_round = 3<ca> with get, set
    member val dmg_mult = 1.0 with get, set

    member val R = new arm (0.50) with get, set
    member val L = new larm (0.50) with get, set
            
    member this.base_hit with set x = this.R.base_hit <- x; this.L.base_hit <- x
    member this.aimed_hit_malus with set x = this.R.aimed_hit_malus <- x; this.L.aimed_hit_malus <- x

    member val active_buffs : buff list = [] with get, set
    member this.add_buff (buff : buff) = this.active_buffs <- buff :: this.active_buffs
    member this.add_buff (d, f) = this.add_buff (new buff (d, f))
    member this.filter_buffs f = this.active_buffs <- List.filter f this.active_buffs

    interface ICloneable with
        member this.Clone () =
            let r = this.MemberwiseClone () :?> pc
            r.L <- (this.L :> ICloneable).Clone () :?> larm
            r.R <- (this.R :> ICloneable).Clone () :?> arm
            // no need to copy pure members (such as stats or active_buffs list etc) because any modification would create new objects anyway
            r :> obj

    member this.clone_and_apply_buffs =
        let r = (this :> ICloneable).Clone () :?> pc
        for buff in this.active_buffs do
            buff.apply this
        r

    override this.ToString () =
        let p (a : arm) =
            let h bas w = pretty_percent a.hit
            in
                sprintf "hit:%s %s" (h a.base_hit a.weapon) (a.weapon.pretty_with_dmg this.stats)
        in
            sprintf "%s\n[R] %s\n[L] %s" (this.stats.pretty (sprintf "%d") "\n") (p this.R) (p this.L)


and buff (duration_ : duration, f : pc -> unit) =
    member this.apply pc = f pc        
    member val duration : duration = duration_ with get, set
