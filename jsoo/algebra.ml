open Optic
open Type

type 'a t = 'a Type.t

let empty : type a. a t = W ((), (), Eq.unit, fun x -> x, Dag.empty ())

let ( & ) (W (c0, s0, _, w0)) (W (c1, s1, _, w1)) =
  W
    ( (None, c0, c1)
    , (s0, s1)
    , Eq.create ()
    , fun ((x, (s0, s1), (cache, c0, c1)) as input) ->
        match cache with
        | Some (x', s0', s1', img) when x == x' && s0 == s0' && s1 == s1' -> input, img
        | _ ->
          let (x, s0, c0), img0 = w0 (x, s0, c0) in
          let (x, s1, c1), img1 = w1 (x, s1, c1) in
          let img = Dag.seq img0 img1 in
          let cache = Some (x, s0, s1, img) in
          (x, (s0, s1), (cache, c0, c1)), img )

let iso iso (W (c, s, _, w)) =
  let iso' =
    { Iso.ltor = (fun (x, y) -> Iso.ltor iso x, y)
    ; rtol = (fun (x, y) -> Iso.rtol iso x, y)
    }
  in
  let eq_iso = Eq.create () in
  W
    ( (None, c)
    , s
    , Eq.create ()
    , fun ((x, s, (cache, c)) as input) ->
        match cache with
        | Some (x', s', img) when x == x' && s == s' -> input, img
        | _ ->
          let y = Optic.Iso.ltor iso x in
          let (y, s, c), img = w (y, s, c) in
          let x = Optic.Iso.rtol iso y in
          let img = Dag.iso eq_iso iso' img in
          let cache = Some (x, s, img) in
          (x, s, (cache, c)), img )

let on lens (W (c, s, _, w)) =
  let eq_lens = Eq.create () in
  W
    ( (None, c)
    , s
    , Eq.create ()
    , fun ((x, s, (cache, c)) as input) ->
        match cache with
        | Some (x', s', img) when x == x' && s == s' -> input, img
        | _ ->
          let y = Optic.Lens.get lens x in
          let (y, s, c), img = w (y, s, c) in
          let x = Optic.Lens.put lens y x in
          let img = Dag.on eq_lens lens img in
          let cache = Some (x, s, img) in
          (x, s, (cache, c)), img )

let case prism (W (c, s0, _, w)) =
  let eq_prism = Eq.create () in
  W
    ( (None, c)
    , s0
    , Eq.create ()
    , fun ((x, s, (cache, c)) as input) ->
        match cache with
        | Some (x', s', img) when x == x' && s == s' -> input, img
        | _ ->
          (match Optic.Prism.extract prism x with
           | None ->
             let img = Dag.empty () in
             let cache = Some (x, s, img) in
             (x, s0, (cache, c)), img
           | Some y ->
             let (y, s, c), img = w (y, s, c) in
             let x = Optic.Prism.make prism y in
             let img = Dag.into eq_prism prism img in
             let cache = Some (x, s, img) in
             (x, s, (cache, c)), img) )

let into prism (W (c, s, _, w)) =
  let eq_prism = Eq.create () in
  W
    ( (None, c)
    , s
    , Eq.create ()
    , fun ((x, s, (cache, c)) as input) ->
        match cache with
        | Some (x', s', img) when x == x' && s == s' -> input, img
        | _ ->
          (match Optic.Prism.extract prism x with
           | None ->
             let img = Dag.empty () in
             let cache = Some (x, s, img) in
             (x, s, (cache, c)), img
           | Some y ->
             let (y, s, c), img = w (y, s, c) in
             let x = Optic.Prism.make prism y in
             let img = Dag.into eq_prism prism img in
             let cache = Some (x, s, img) in
             (x, s, (cache, c)), img) )

let cond predicate w = into (Prism.satisfy predicate) w
let cond_forget predicate w = case (Prism.satisfy predicate) w

let ifte predicate if_true if_false =
  cond predicate if_true & cond (fun x -> not (predicate x)) if_false

let reorder : type a b c. (a * (b * c), (b * a) * c) Optic.iso =
  { Optic.Iso.ltor = (fun (x, (s0, s1)) -> (s0, x), s1)
  ; rtol = (fun ((s0, x), s1) -> x, (s0, s1))
  }

let stateful s0 (W (c, s1, _, w)) =
  let eq_iso = Eq.create () in
  W
    ( (None, c)
    , (s0, s1)
    , Eq.create ()
    , fun ((x, (s0, s1), (cache, c)) as input) ->
        match cache with
        | Some (x', s0', s1', img) when x == x' && s0 == s0' && s1 == s1' -> input, img
        | _ ->
          let ((s0, x), s1, c), img = w ((s0, x), s1, c) in
          let img = Dag.iso eq_iso reorder img in
          let cache = Some (x, s0, s1, img) in
          (x, (s0, s1), (cache, c)), img )

let dynamic : type a. (a t * a) t =
  W
    ( ()
    , ()
    , Eq.unit
    , fun (wx, (), ()) ->
        let W (c, s, seq, w), x = wx in
        let (x, s, c), img = w (x, s, c) in
        let wx = W (c, s, seq, w), x in
        let img = Dag.dynamic seq img in
        (wx, (), ()), img )

(********************************************************************************)

let ( <*> ) a b = on Lens.fst a & on Lens.snd b

let initialize : type a b. (a -> b) -> (b option * a, b * a) Optic.iso =
  fun fn ->
  { Optic.Iso.ltor =
      (fun (s0, x) ->
        let s0 =
          match s0 with
          | None -> fn x
          | Some s0 -> s0
        in
        s0, x)
  ; rtol = (fun (s0, x) -> Some s0, x)
  }

let stateful_by fn w = stateful None (iso (initialize fn) w)
let of_lazy w = stateful_by (fun _ -> Lazy.force w) dynamic
let apply f x = of_lazy (lazy (f x))

let fix fn =
  let rec self = lazy (fn (of_lazy self)) in
  Lazy.force self

let of_list ws = List.fold_left ( & ) empty ws
let list w = fix (fun lst -> into Prism.cons (w <*> lst))
