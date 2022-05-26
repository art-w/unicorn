module Eq = struct
  type !_ typ = ..
  type (_, _) eq = Refl : ('a, 'a) eq
  type !'state check = { eq : 'x. 'x typ -> ('x, 'state) eq option }
  type !'a t = 'a typ * 'a check

  let create (type a) () : a t =
    let open struct
      type _ typ += Proof : a typ
    end in
    let eq : type x. x typ -> (x, a) eq option = function
      | Proof -> Some Refl
      | _ -> None
    in
    Proof, { eq }

  let unit : unit t = create ()
  let check (type a b) ((_, eq) : a t) ((x, _) : b t) : (b, a) eq option = eq.eq x
end

open Js_of_ocaml

type elt = Dom_html.element Js.t
type node = Dom.node Js.t

type 'a s =
  | Empty : 'a s
  | Text : Dom.text Js.t option * string -> 'a s
  | Node : elt option * string * 'a dag -> 'a s
  | Seq : ('a * 's0) dag * ('a * 's1) dag -> ('a * ('s0 * 's1)) s
  | Iso :
      ('b * 's2) Eq.t * ('a * 's1, 'b * 's2) Optic.iso * ('b * 's2) dag
      -> ('a * 's1) s
  | On : 'b Eq.t * ('a, 'b) Optic.lens * ('b * 's) dag -> ('a * 's) s
  | Into : 'b Eq.t * ('a, 'b) Optic.prism * ('b * 's) dag -> ('a * 's) s
  | Dynamic : 's Eq.t * ('a * 's) dag -> (('a t * 'a) * unit) s
  | Event :
      (('v #Dom.event Js.t as 'e) Dom_html.Event.typ as 'ee) Eq.t
      * Dom_html.event_listener_id option
      * 'ee
      * 'e option ref
      * ('e -> 'a -> 'a)
      -> 'a s

and dag_parent =
  | No_parent
  | Any : _ dag -> dag_parent
  | Root : (unit -> unit) -> dag_parent

and 'a dag =
  { mutable dirty : bool
  ; mutable parent : dag_parent
  ; attributes : Attr.t
  ; s : 'a s
  }

and ('a, 'state, 'cache) render =
  'a * 'state * 'cache -> ('a * 'state * 'cache) * ('a * 'state) dag

and !'a t = W : 'cache * 'state * 'state Eq.t * ('a, 'state, 'cache) render -> 'a t
