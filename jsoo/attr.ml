open! Js_of_ocaml
module M = Map.Make (String)

type value =
  | String of string
  | Bool of bool

type t = value M.t

let empty = M.empty
let single key value = M.singleton key value
let add key value t = M.add key value t

let merge a b =
  M.merge
    (fun _ ox oy ->
      match ox, oy with
      | _, Some _ -> oy
      | _, None -> ox)
    a
    b

let dom_remove ~(node : Dom_html.element Js.t) key =
  let key = Js.string key in
  node##removeAttribute key ;
  Js.Unsafe.delete node key

let dom_add ~(node : Dom_html.element Js.t) key value =
  let key = Js.string key in
  match value with
  | String value ->
    let value = Js.string value in
    Js.Unsafe.set node key value ;
    node##setAttribute key value
  | Bool value ->
    Js.Unsafe.set node key (Js.bool value) ;
    if value then node##setAttribute key (Js.string "") else node##removeAttribute key

let update ~(node : Dom_html.element Js.t) ~old ~latest =
  ignore
  @@ M.merge
       (fun key ox oy ->
         (match ox, oy with
         | Some _, None -> dom_remove ~node key
         | _, Some attr -> dom_add ~node key attr
         | _ -> ()) ;
         None)
       old
       latest
