open Js_of_ocaml
open Type
open Algebra

let text show =
  W
    ( None
    , ()
    , Eq.unit
    , fun ((x, (), cache) as input) ->
        match cache with
        | Some (x', img) when x == x' -> input, img
        | _ ->
          let str = show x in
          let img = Dag.text str in
          let cache = Some (x, img) in
          (x, (), cache), img )

let str s = text (fun _ -> s)

module H = struct
  let make name (W (c, s, seq, w)) =
    W
      ( (None, c)
      , s
      , seq
      , fun ((x, s, (cache, c)) as input) ->
          match cache with
          | Some (x', s', img) when x == x' && s == s' -> input, img
          | _ ->
            let (x, s, c), img = w (x, s, c) in
            let img = Dag.node name img in
            let cache = Some (x, s, img) in
            (x, s, (cache, c)), img )

  let button t = make "button" t
  let div t = make "div" t
  let h1 t = make "h1" t
  let h2 t = make "h2" t
  let h3 t = make "h3" t
  let h4 t = make "h4" t
  let h5 t = make "h5" t
  let h6 t = make "h6" t
  let input t = make "input" t
  let li t = make "li" t
  let p t = make "p" t
  let pre t = make "pre" t
  let span t = make "span" t
  let ul t = make "ul" t
  let leaf name children = make name (of_list children)
  let hr t = leaf "hr" t
end

module A = struct
  let make key show =
    W
      ( None
      , ()
      , Eq.unit
      , fun ((x, (), cache) as input) ->
          match cache with
          | Some (x', img) when x == x' -> input, img
          | _ ->
            let str = show x in
            let img = Dag.attr key str in
            let cache = Some (x, img) in
            (x, (), cache), img )

  let make_bool key show = make key (fun x -> Attr.Bool (show x))
  let make_string key show = make key (fun x -> Attr.String (show x))
  let value fn = make_string "value" fn
  let type_ fn = make_string "type" fn
  let class_ fn = make_string "class" fn
  let style fn = make_string "style" fn
  let checked fn = make_bool "checked" fn
  let disabled fn = make_bool "disabled" fn
end

module E = struct
  let make typ fn =
    let eq_event = Eq.create () in
    W
      ( None
      , ()
      , Eq.unit
      , fun ((x, (), cache) as input) ->
          match cache with
          | Some img -> input, img
          | _ ->
            let img = Dag.event eq_event typ (fun ev (x, ()) -> fn ev x, ()) in
            let cache = Some img in
            (x, (), cache), img )

  let click fn = make Dom_html.Event.click (fun _ -> fn)
  let change fn = make Dom_html.Event.change fn
  let input fn = make Dom_html.Event.input fn
  let delta_time = ref 0.0
  let current_time = ref nan

  let set_current_time t =
    (delta_time
       := match classify_float !current_time with
          | FP_nan -> 0.0
          | _ -> t -. !current_time) ;
    current_time := t

  let always fn =
    W
      ( ()
      , ()
      , Eq.unit
      , fun input ->
          let img = Dag.always (fun (x, ()) -> fn x, ()) in
          input, img )

  let now fn = always (fun x -> fn !current_time x)
  let dt fn = always (fun x -> fn !delta_time x)
end

let button = H.button

let checkbox =
  H.input
    (A.type_ (fun _ -> "checkbox")
    & E.input (fun ev _ ->
          Js.Opt.case
            ev##.target
            (fun () -> failwith "event without a target?")
            (fun t ->
              Js.Opt.case
                (Dom_html.CoerceTo.input t)
                (fun () -> failwith "not an input?")
                (fun t -> Js.to_bool t##.checked)))
    & A.checked (fun b -> b))

let input_string =
  H.input
    (A.type_ (fun _ -> "text")
    & A.value (fun s -> s)
    & E.input (fun ev _ ->
          Js.Opt.case
            ev##.target
            (fun () -> failwith "event without a target?")
            (fun t ->
              Js.Opt.case
                (Dom_html.CoerceTo.input t)
                (fun () -> failwith "not an input?")
                (fun t -> Js.to_string t##.value))))

let string_of_array t = String.init (Array.length t) (Array.get t)

let digits str =
  string_of_array
  @@ Array.of_list
  @@ List.rev
  @@ String.fold_left
       (fun acc chr ->
         if (chr >= '0' && chr <= '9') || chr = '-' then chr :: acc else acc)
       []
       str

let input_int =
  stateful_by (fun i -> i, string_of_int i)
  @@ iso
       { Optic.Iso.ltor = (fun ((i, str), j) -> if i = j then str else string_of_int j)
       ; rtol =
           (fun x ->
             let x = digits x in
             try
               let i = int_of_string x in
               (i, x), i
             with
             | _ -> (0, x), 0)
       }
  @@ input_string
