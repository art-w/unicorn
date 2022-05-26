open Longident
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let make_lens ~loc name =
  let extract = Pat.record [ lid name, pvar "x" ] Open in
  let make = Exp.record [ lid name, evar "x" ] (Some (evar "v")) in
  Vb.mk
    (pvar name)
    [%expr { Optic.Lens.get = (fun [%p extract] -> x); put = (fun x v -> [%e make]) }]

let make_prism ~loc name = function
  | Pcstr_tuple [] ->
    Vb.mk
      (pvar (String.lowercase_ascii name))
      [%expr
        { Optic.Prism.extract =
            (function
            | [%p pconstr name []] -> Some ()
            | _ -> None)
        ; make = (fun () -> [%e constr name []])
        }]
  | Pcstr_tuple [ _ ] ->
    Vb.mk
      (pvar (String.lowercase_ascii name))
      [%expr
        { Optic.Prism.extract =
            (function
            | [%p pconstr name [ pvar "x" ]] -> Some x
            | _ -> None)
        ; make = (fun x -> [%e constr name [ evar "x" ]])
        }]
  | Pcstr_tuple args ->
    let pargs = List.mapi (fun i _ -> pvar ("x" ^ string_of_int i)) args in
    let eargs = List.mapi (fun i _ -> evar ("x" ^ string_of_int i)) args in
    Vb.mk
      (pvar (String.lowercase_ascii name))
      [%expr
        { Optic.Prism.extract =
            (function
            | [%p pconstr name pargs] -> Some [%e tuple eargs]
            | _ -> None)
        ; make = (fun [%p ptuple pargs] -> [%e constr name eargs])
        }]
  | Pcstr_record _ -> failwith "optic: unsupported record constructor in prism"

let str_of_type type_decl =
  match type_decl.ptype_kind, type_decl.ptype_manifest with
  | Ptype_record labels, _ ->
    List.map
      (fun { pld_name = { txt = name; _ }; pld_loc = loc; _ } -> make_lens ~loc name)
      labels
  | Ptype_variant constrs, _ ->
    List.map
      (fun { pcd_name = { txt = name; _ }; pcd_args = args; pcd_loc = loc; _ } ->
        make_prism ~loc name args)
      constrs
  | _ -> []

let () =
  Ppx_deriving.(
    register
      (create
         "optic"
         ~type_decl_str:(fun ~options:_ ~path:_ type_decls ->
           let type_decls =
             List.map Astlib.Migrate_412_413.copy_type_declaration type_decls
           in
           let result =
             Str.value Nonrecursive (List.concat (List.map str_of_type type_decls))
           in
           let result = Astlib.Migrate_413_412.copy_structure_item result in
           [ result ])
         ()))
