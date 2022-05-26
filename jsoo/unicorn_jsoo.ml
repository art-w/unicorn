open Js_of_ocaml
open Type
include Algebra
include Html

let get_element_by_id id =
  match Js.Opt.to_option (Dom_html.document##getElementById (Js.string id)) with
  | None -> failwith "id not found"
  | Some node -> node

type state =
  | State :
      ('a * 'state) Dag.t
      * ('a * 'state) Dag.t
      * 'a
      * 'state
      * 'cache
      * ('a, 'state, 'cache) render
      -> state

let run ?id w x =
  let parent =
    match id with
    | None -> Dom_html.document##.body
    | Some id -> get_element_by_id id
  in
  let (W (cache, state, _, render)) = w in
  let global_state = State (Dag.empty (), Dag.empty (), x, state, cache, render) in
  let rec rerender global_state =
    let (State (instance, old, x, state, cache, render)) = global_state in
    let (x, state, cache), latest = render (x, state, cache) in
    let instance = Dag.redraw ~parent ~instance ~old ~latest in
    let global_state = State (instance, latest, x, state, cache, render) in
    instance.parent <- Root (update global_state)
  and update global_state () =
    let _ =
      Dom_html.window##requestAnimationFrame
        (Js.wrap_callback (fun _t ->
             let (State (instance, old, x, state, cache, render)) = global_state in
             let x, state = Event.recompute instance (x, state) in
             let global_state = State (instance, old, x, state, cache, render) in
             rerender global_state))
    in
    ()
  in
  rerender global_state
