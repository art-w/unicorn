open Js_of_ocaml
open Type

type 'a s = 'a Type.s
type 'a t = 'a Type.dag

let collect_attributes : type a. a s -> Attr.t = function
  | Empty | Text _ | Node _ | Event _ | Always _ -> Attr.empty
  | Seq (a, b) -> Attr.merge a.attributes b.attributes
  | On (_, _, t) -> t.attributes
  | Iso (_, _, t) -> t.attributes
  | Into (_, _, t) -> t.attributes
  | Dynamic (_, t) -> t.attributes

let child_is_dirty : type a. a s -> bool = function
  | Always _ -> true
  | Node (_, _, t) -> t.dirty
  | On (_, _, t) -> t.dirty
  | Iso (_, _, t) -> t.dirty
  | Into (_, _, t) -> t.dirty
  | Dynamic (_, t) -> t.dirty
  | Seq (a, b) -> a.dirty || b.dirty
  | _ -> false

let make s =
  { dirty = child_is_dirty s; parent = No_parent; attributes = collect_attributes s; s }

let empty () = make Empty
let text str = make (Text (None, str))
let node name child = make (Node (None, name, child))
let seq a b = make (Seq (a, b))
let iso eq lens t = make (Iso (eq, lens, t))
let on eq lens t = make (On (eq, lens, t))
let into eq prism t = make (Into (eq, prism, t))
let dynamic eq t = make (Dynamic (eq, t))
let event eq typ fn = make (Event (eq, None, typ, ref None, fn))
let always fn = make (Always fn)
let attr key value = { (empty ()) with attributes = Attr.single key value }

let attach : type a. a t -> a t =
  fun t ->
  (match t.s with
   | Empty -> ()
   | Text _ -> ()
   | Event _ -> ()
   | Always _ -> ()
   | Node (_, _, child) -> child.parent <- Any t
   | Iso (_, _, child) -> child.parent <- Any t
   | On (_, _, child) -> child.parent <- Any t
   | Into (_, _, child) -> child.parent <- Any t
   | Dynamic (_, child) -> child.parent <- Any t
   | Seq (a, b) ->
     a.parent <- Any t ;
     b.parent <- Any t) ;
  t

let create ~attributes s =
  attach { dirty = child_is_dirty s; parent = No_parent; attributes; s }

let rec delete : type a. parent:elt -> a t -> unit =
  fun ~parent t ->
  match t.s with
  | Empty -> ()
  | Text (None, _) -> () (* ? *)
  | Text (Some elt, _) -> ignore (parent##removeChild (elt :> node))
  | Seq (a, b) ->
    delete ~parent a ;
    delete ~parent b
  | Iso (_, _, t) -> delete ~parent t
  | On (_, _, t) -> delete ~parent t
  | Into (_, _, t) -> delete ~parent t
  | Dynamic (_, t) -> delete ~parent t
  | Event (_, Some eid, _, _, _) -> Dom_html.removeEventListener eid
  | Event (_, None, _, _, _) -> () (* ?? *)
  | Always _ -> ()
  | Node (Some elt, _, _) -> ignore (parent##removeChild (elt :> node))
  | Node (None, _, _) -> failwith "delete node: should have an instance"

let rec trigger target =
  match target with
  | No_parent -> failwith "no parent?"
  | Root fn -> fn ()
  | Any inst ->
    if not inst.dirty
    then (
      inst.dirty <- true ;
      trigger inst.parent)

let rec instantiate
  : type a. parent:elt -> previous:node option -> a t -> a t * node option
  =
  fun ~parent ~previous t ->
  let attributes = t.attributes in
  match t.s with
  | Empty -> t, previous
  | Text (_, str) ->
    let elt = Dom_html.document##createTextNode (Js.string str) in
    ignore
      ((parent :> Dom.node Js.t)##insertBefore
         (elt :> Dom.node Js.t)
         (Js.Opt.option previous)) ;
    create ~attributes (Text (Some elt, str)), Some (elt :> node)
  | Node (inst, name, child) ->
    assert (inst = None) ;
    let elt = Dom_html.document##createElement (Js.string name) in
    Attr.update ~node:elt ~old:Attr.empty ~latest:child.attributes ;
    ignore (parent##insertBefore (elt :> node) (Js.Opt.option previous)) ;
    let child, _ = instantiate ~parent:elt ~previous:None child in
    create ~attributes (Node (Some (elt :> elt), name, child)), Some (elt :> node)
  | Seq (a, b) ->
    let b, previous = instantiate ~parent ~previous b in
    let a, previous = instantiate ~parent ~previous a in
    create ~attributes (Seq (a, b)), previous
  | Iso (eq, iso, t) ->
    let t, previous = instantiate ~parent ~previous t in
    create ~attributes (Iso (eq, iso, t)), previous
  | On (eq, lens, t) ->
    let t, previous = instantiate ~parent ~previous t in
    create ~attributes (On (eq, lens, t)), previous
  | Into (eq, prism, t) ->
    let t, previous = instantiate ~parent ~previous t in
    create ~attributes (Into (eq, prism, t)), previous
  | Dynamic (eq, t) ->
    let t, previous = instantiate ~parent ~previous t in
    create ~attributes (Dynamic (eq, t)), previous
  | Event (eq, _, typ, _, handler) ->
    let latest_event = ref None in
    let target = ref No_parent in
    let js_handler =
      Dom.handler (fun ev ->
        latest_event := Some ev ;
        trigger !target ;
        Js._true)
    in
    let eid = Dom_html.addEventListener parent typ js_handler Js._false in
    let self = create ~attributes (Event (eq, Some eid, typ, latest_event, handler)) in
    target := Any self ;
    attach self, previous
  | Always fn ->
    let self = create ~attributes (Always fn) in
    attach self, previous

let rec first : type a. previous:node option -> a t -> node option =
  fun ~previous t ->
  match t.s with
  | Node (elt, _, _) ->
    assert (elt <> None) ;
    (elt :> node option)
  | Seq (a, b) ->
    (match first ~previous:None a with
     | None -> first ~previous b
     | found -> found)
  | Iso (_, _, t) -> first ~previous t
  | On (_, _, t) -> first ~previous t
  | Into (_, _, t) -> first ~previous t
  | _ -> previous

let rec reuse
  : type a.
    parent:elt
    -> previous:node option
    -> instance:a t
    -> old:a t
    -> latest:a t
    -> a t * node option
  =
  fun ~parent ~previous ~instance ~old ~latest ->
  if old == latest
  then instance, first ~previous instance
  else (
    let attributes = latest.attributes in
    let no_reuse () =
      delete ~parent instance ;
      instantiate ~parent ~previous latest
    in
    match instance.s, old.s, latest.s with
    | Empty, Empty, Empty -> instance, first ~previous instance
    | Always _, Always _, Always _ ->
      { instance with dirty = true }, first ~previous instance
    | Text (Some txt, _), Text (_, old_str), Text (_, new_str) ->
      if not (String.equal old_str new_str) then txt##.data := Js.string new_str ;
      instance, first ~previous instance
    | ( Node (Some parent, _, child_instantiated)
      , Node (_, name, child_old)
      , Node (_, name', child) )
      when name = name' ->
      let child =
        Attr.update
          ~node:parent
          ~old:child_instantiated.attributes
          ~latest:child.attributes ;
        let child, _ =
          reuse
            ~parent
            ~previous:None
            ~instance:child_instantiated
            ~old:child_old
            ~latest:child
        in
        child
      in
      let it = create ~attributes (Node (Some parent, name', child)) in
      it, first ~previous it
    | Seq (inst0, inst1), Seq (old0, old1), Seq (latest0, latest1) ->
      let inst1, previous =
        reuse ~parent ~previous ~instance:inst1 ~old:old1 ~latest:latest1
      in
      let inst0, previous =
        reuse ~parent ~previous ~instance:inst0 ~old:old0 ~latest:latest0
      in
      let it = create ~attributes (Seq (inst0, inst1)) in
      it, previous
    | Iso (eq0, _, instance), Iso (eq1, _, old), Iso (eq2, i2, latest) ->
      (match Eq.check eq0 eq1, Eq.check eq1 eq2 with
       | Some Refl, Some Refl ->
         let inst, previous = reuse ~parent ~previous ~instance ~old ~latest in
         let it = create ~attributes (Iso (eq2, i2, inst)) in
         it, previous
       | _ -> no_reuse ())
    | On (eq0, _, instance), On (eq1, _, old), On (eq2, l2, latest) ->
      (match Eq.check eq0 eq1, Eq.check eq1 eq2 with
       | Some Refl, Some Refl ->
         let inst, previous = reuse ~parent ~previous ~instance ~old ~latest in
         let it = create ~attributes (On (eq2, l2, inst)) in
         it, previous
       | _ -> no_reuse ())
    | Into (eq0, _, instance), Into (eq1, _, old), Into (eq2, l2, latest) ->
      (match Eq.check eq0 eq1, Eq.check eq1 eq2 with
       | Some Refl, Some Refl ->
         let inst, previous = reuse ~parent ~previous ~instance ~old ~latest in
         let it = create ~attributes (Into (eq2, l2, inst)) in
         it, previous
       | _ -> no_reuse ())
    | Dynamic (eq0, instance), Dynamic (eq1, old), Dynamic (eq2, latest) ->
      (match Eq.check eq0 eq1, Eq.check eq1 eq2 with
       | Some Refl, Some Refl ->
         let inst, previous = reuse ~parent ~previous ~instance ~old ~latest in
         let it = create ~attributes (Dynamic (eq2, inst)) in
         it, previous
       | _ -> no_reuse ())
    | Event (eq0, Some _, _, _, _), Event (eq1, _, _, _, _), Event (eq2, _, _, _, _) ->
      (match Eq.check eq0 eq1, Eq.check eq1 eq2 with
       | Some Refl, Some Refl -> instance, first ~previous instance
       | _ -> no_reuse ())
    | _ -> no_reuse ())

let redraw ~parent ~instance ~old ~latest =
  let img, _ = reuse ~parent ~previous:None ~instance ~old ~latest in
  img
