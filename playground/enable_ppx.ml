module Ppx_deriving_optic = Ppx_deriving_optic
module Ppx_deriving_show = Ppx_deriving_show

(* from https://github.com/ocaml-ppx/ppx_deriving/blob/master//src/ppx_deriving_main.cppo.ml *)
module Ast_mapper = Ocaml_common.Ast_mapper
module From_current = Ppxlib_ast.Selected_ast.Of_ocaml
module To_current = Ppxlib_ast.Selected_ast.To_ocaml

let mapper _argv =
  let module Current_ast = Ppxlib_ast.Selected_ast in
  let structure s =
    match s with
    | [] -> []
    | _ -> Ppxlib.Driver.map_structure s
  in
  let structure _ st =
    Current_ast.of_ocaml Structure st |> structure |> Current_ast.to_ocaml Structure
  in
  let signature _ si =
    Current_ast.of_ocaml Signature si
    |> Ppxlib.Driver.map_signature
    |> Current_ast.to_ocaml Signature
  in
  { Ast_mapper.default_mapper with structure; signature }

let init () = Ast_mapper.register "ppx_deriving" mapper
