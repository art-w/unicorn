open Type

let rec undirty : type a. a dag -> unit =
 fun t ->
  if not t.dirty
  then ()
  else (
    t.dirty <- false ;
    match t.s with
    | Iso (_, _, child) -> undirty child
    | On (_, _, child) -> undirty child
    | Into (_, _, child) -> undirty child
    | Seq (a, b) ->
      undirty a ;
      undirty b
    | Node (_, _, child) -> undirty child
    | _ -> ())

let rec recompute : type a s. (a * s) dag -> a * s -> a * s =
 fun t input ->
  if not t.dirty
  then input
  else (
    t.dirty <- false ;
    match t.s with
    | Empty -> failwith "recompute empty"
    | Text _ -> failwith "recompute text"
    | Node (_, _, child) -> recompute child input
    | Seq (a, b) ->
      let x, (s0, s1) = input in
      let x, s0 = recompute a (x, s0) in
      let x, s1 = recompute b (x, s1) in
      x, (s0, s1)
    | Iso (_, iso, child) ->
      let out = Optic.Iso.ltor iso input in
      let out = recompute child out in
      Optic.Iso.rtol iso out
    | On (_, lens, child) ->
      let x, s = input in
      let y = Optic.Lens.get lens x in
      let y, s = recompute child (y, s) in
      Optic.Lens.put lens y x, s
    | Into (_, prism, child) ->
      let x, s = input in
      (match Optic.Prism.extract prism x with
      | None ->
        undirty child ;
        input
      | Some y ->
        let y, s = recompute child (y, s) in
        Optic.Prism.make prism y, s)
    | Dynamic (eq, child) ->
      let (W (c, s', seq', w), x), () = input in
      (x, eq, s', seq', c, w, child)
      |>
      fun (type x s s2 c)
          ( x
          , (eq : s Eq.t)
          , (s' : s2)
          , (seq' : s2 Eq.t)
          , (c : c)
          , (w : (x, s2, c) render)
          , (child : (x * s) dag) ) ->
        (match Eq.check eq seq' with
        | Some (Refl : (s2, s) Eq.eq) ->
          let x, s = recompute child (x, s') in
          (W (c, s, seq', w), x), ()
        | None ->
          undirty child ;
          (W (c, s', seq', w), x), ())
    | Event (_, _, _, latest_event, handler) ->
      (match !latest_event with
      | None -> failwith "no event?"
      | Some ev ->
        latest_event := None ;
        handler ev input)
    | Always handler -> handler input)

let set_root ev fn = if ev.dirty then fn () else ev.parent <- Root fn
