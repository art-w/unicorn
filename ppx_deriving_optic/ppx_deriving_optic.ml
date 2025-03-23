module Past = Ppxlib.Ast_builder.Default

let lid ~loc s = Past.Located.mk ~loc (Ppxlib.lident s)
let pvar ~loc s = Past.ppat_var ~loc (Past.Located.mk ~loc s)
let evar ~loc s = Past.pexp_ident ~loc (lid ~loc s)

let make_lens ~loc name =
  let extract = Past.ppat_record ~loc [ lid ~loc name, pvar ~loc "x" ] Open in
  let make =
    Past.pexp_record ~loc [ lid ~loc name, evar ~loc "x" ] (Some (evar ~loc "v"))
  in
  Past.value_binding
    ~loc
    ~pat:(pvar ~loc name)
    ~expr:
      [%expr { Optic.Lens.get = (fun [%p extract] -> x); put = (fun x v -> [%e make]) }]

let make_prism ~loc name constructor =
  let expr =
    match constructor with
    | Ppxlib.Pcstr_tuple [] ->
      [%expr
        { Optic.Prism.extract =
            (function
              | [%p Past.ppat_construct ~loc (lid ~loc name) None] -> Some ()
              | _ -> None)
        ; make = (fun () -> [%e Past.pexp_construct ~loc (lid ~loc name) None])
        }]
    | Pcstr_tuple [ _ ] ->
      [%expr
        { Optic.Prism.extract =
            (function
              | [%p Past.ppat_construct ~loc (lid ~loc name) (Some (pvar ~loc "x"))] ->
                Some x
              | _ -> None)
        ; make =
            (fun x ->
              [%e Past.pexp_construct ~loc (lid ~loc name) (Some (evar ~loc "x"))])
        }]
    | Pcstr_tuple args ->
      let pargs =
        Past.ppat_tuple ~loc
        @@ List.mapi (fun i _ -> pvar ~loc ("x" ^ string_of_int i)) args
      in
      let eargs =
        Past.pexp_tuple ~loc
        @@ List.mapi (fun i _ -> evar ~loc ("x" ^ string_of_int i)) args
      in
      [%expr
        { Optic.Prism.extract =
            (function
              | [%p Past.ppat_construct ~loc (lid ~loc name) (Some pargs)] ->
                Some [%e eargs]
              | _ -> None)
        ; make =
            (fun [%p pargs] -> [%e Past.pexp_construct ~loc (lid ~loc name) (Some eargs)])
        }]
    | Pcstr_record _ -> failwith "optic: unsupported record constructor in prism"
  in
  Past.value_binding ~loc ~pat:(pvar ~loc (String.lowercase_ascii name)) ~expr

let str_of_type type_decl =
  let open Ppxlib in
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
  let open Ppxlib in
  Ppx_deriving.(
    register
      (create
         "optic"
         ~type_decl_str:(fun ~options:_ ~path:_ type_decls ->
           [ Past.pstr_value
               ~loc:Location.none
               Nonrecursive
               (List.concat (List.map str_of_type type_decls))
           ])
         ()))
