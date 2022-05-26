(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Js_of_ocaml
open Js_of_ocaml_toplevel
module Ppx_deriving_optic = Ppx_deriving_optic

let by_id s = Dom_html.getElementById s

let by_id_coerce s f =
  Js.Opt.get (f (Dom_html.getElementById s)) (fun () -> raise Not_found)

let exec' s =
  let res : bool = JsooTop.use Format.std_formatter s in
  if not res then Format.eprintf "error while evaluating %s@." s

let setup_toplevel () =
  JsooTop.initialize () ;
  Enable_ppx.init () ;
  Sys.interactive := false

let highlight_location loc =
  let _, line1, col1 = Location.get_pos_info loc.Location.loc_start in
  let _, line2, col2 = Location.get_pos_info loc.Location.loc_end in
  let hl = Js.Unsafe.global##.highlight in
  Js.Unsafe.fun_call hl
  @@ Array.map Js.Unsafe.inject [| line1 - 1; col1; line2 - 1; col2 |]

let append output s =
  Dom.appendChild output (Dom_html.document##createTextNode (Js.string s))

let textbox () = by_id_coerce "userinput" Dom_html.CoerceTo.textarea
let caml_chan = open_out "/dev/null1"
let caml_ppf = Format.formatter_of_out_channel caml_chan

let init_playground =
  {|module Unicorn_jsoo = struct
      include Unicorn_jsoo
      let run w x = run ~id:"unicorn" (H.div (A.class_ (fun _ -> "test") & w)) x
    end ;;|}

let execute () =
  let textbox = textbox () in
  let _ = JsooTop.use caml_ppf init_playground in
  let content = Js.to_string textbox##.value##trim in
  let content' =
    let len = String.length content in
    if try content <> "" && content.[len - 1] <> ';' && content.[len - 2] <> ';' with
       | _ -> true
    then content ^ ";;"
    else content
  in
  JsooTop.execute true ~highlight_location caml_ppf content' ;
  Lwt.return_unit

let setup_async_hook () =
  Lwt.async_exception_hook
    := fun exc ->
         Format.eprintf "exception during Lwt.async: %s@." (Printexc.to_string exc)

let setup () =
  let div_unicorn = by_id "unicorn" in
  let div_stdout = by_id "output" in
  let div_stderr = by_id "errors" in
  div_unicorn##.innerHTML := Js.string "" ;
  div_stdout##.innerHTML := Js.string "" ;
  div_stderr##.innerHTML := Js.string "" ;
  let append_caml s =
    let fn = Js.Unsafe.global##.logCaml in
    let _ = Js.Unsafe.fun_call fn [| Js.Unsafe.inject (Js.string s) |] in
    ()
  in
  Sys_js.set_channel_flusher caml_chan append_caml ;
  Sys_js.set_channel_flusher stdout (append div_stdout) ;
  Sys_js.set_channel_flusher stderr (append div_stderr) ;
  setup_toplevel ()

let refresh () =
  let _ = Js.Unsafe.(fun_call global##.resetCaml) [||] in
  setup () ;
  Lwt.async execute ;
  Js._false

let _ =
  Dom_html.window##.onload
    := Dom_html.handler (fun _ ->
           let btn = by_id "run" in
           let _ =
             Dom_html.addEventListener
               btn
               Dom_html.Event.click
               (Dom.handler (fun _ -> refresh ()))
               Js._false
           in
           refresh ())
