(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*              Mark Shinwell and Leo White, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Dynamic loading of .cmx files *)

module Config = Dynlink_config

open Dynlink_cmxs_format

module DC = Dynlink_common
module DT = Dynlink_types
module DN = Dynlink_nat

module Native = struct
  module Unit_header = struct
    type t = dynunit

    let name (t : t) = t.dynu_name
    let crc (t : t) = Some t.dynu_crc

    let interface_imports (t : t) = t.dynu_imports_cmi
    let implementation_imports (t : t) = t.dynu_imports_cmx

    let defined_symbols (t : t) = t.dynu_defines
    let unsafe_module _t = false
  end

  let init () = ()

  let is_native = true
  let adapt_filename f = Filename.chop_extension f ^ ".cmxs"

  let num_globals_inited () = DN.ndl_globals_inited ()

  let fold_initial_units ~init ~f =
    let rank = ref 0 in
    List.fold_left (fun acc ({ name; crc_intf; crc_impl; syms; } : DN.global_map) ->
        rank := !rank + List.length syms;
        let implementation =
          match crc_impl with
          | None -> None
          | Some _ as crco -> Some (crco, DT.Check_inited !rank)
        in
        f acc ~compunit:name ~interface:crc_intf
            ~implementation ~defined_symbols:syms)
      init
      (DN.ndl_getmap ())

  let run_shared_startup handle =
    match handle with
    | DT.Native_handle (nh, _) -> DN.ndl_run nh "_shared_startup"
    | DT.Bytecode_handle (_, _) -> ()

  let run _lock handle ~unit_header ~priv:_ =
    match handle with
    | DT.Native_handle (nh, _) ->
        List.iter (fun cu ->
            try DN.ndl_run nh cu
            with exn ->
              Printexc.raise_with_backtrace
                (DT.Error (Library's_module_initializers_failed exn))
                (Printexc.get_raw_backtrace ()))
          (Unit_header.defined_symbols unit_header)
    | DT.Bytecode_handle (_, _) ->
        failwith "Not implemented"

  let load ~filename ~priv =
    (* Check if it's a bytecode plugin (.cmo) by file extension *)
    if Filename.check_suffix filename ".cmo" then begin
      (* Load bytecode plugin *)
      failwith "Not implemented"
    end else begin
      (* Load native plugin (.cmxs) *)
      let handle, header =
        try DN.ndl_open filename (not priv)
        with exn -> raise (DT.Error (Cannot_open_dynamic_library exn))
      in
      if header.dynu_magic <> Config.cmxs_magic_number then begin
        raise (DT.Error (Not_a_bytecode_file filename))
      end;
      let syms =
        "_shared_startup" ::
        List.concat_map Unit_header.defined_symbols header.dynu_units
      in
      try
        DN.ndl_register handle (Array.of_list syms);
        let unit_names = List.map (fun unit -> unit.dynu_name) header.dynu_units in
        DT.Native_handle (handle, unit_names), header.dynu_units
      with exn -> raise (DT.Error (Cannot_open_dynamic_library exn))
    end

  let unsafe_get_global_value ~bytecode_or_asm_symbol =
    match DN.ndl_loadsym bytecode_or_asm_symbol with
    | exception _ -> None
    | obj -> Some obj

  let finish handle =
    match handle with
    | DT.Native_handle (_, _) -> ()
    | DT.Bytecode_handle (_, _) -> failwith "Not implemented"
end

include DC.Make (Native)

type linking_error = DT.linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error = DT.error =
  | Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | Cannot_open_dynamic_library of exn
  | Library's_module_initializers_failed of exn
  | Inconsistent_implementation of string
  | Module_already_loaded of string
  | Private_library_cannot_implement_interface of string

exception Error = DT.Error
let error_message = DT.error_message
