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

module Symtable = Dynlink_symtable
module Config = Dynlink_config
open Dynlink_cmo_format
open Dynlink_cmxs_format

module DC = Dynlink_common
module DT = Dynlink_types

module Compression = struct (* Borrowed from utils/compression.ml *)
  external zstd_initialize: unit -> bool = "caml_zstd_initialize"
  let input_value = Stdlib.input_value
end

let _compression_supported = Compression.zstd_initialize ()

(* Module for native plugin support in bytecode *)
module Native_bridge = struct
  external ndl_open : string -> bool -> DT.native_handle * dynheader
    = "caml_natdynlink_open"
  external ndl_register : DT.native_handle -> string array -> unit
    = "caml_natdynlink_register"
  external ndl_run : DT.native_handle -> string -> unit = "caml_natdynlink_run"
end

(* Main bytecode dynlink implementation *)
module Bytecode = struct
  module Unit_header = struct
    type t = compilation_unit

    let name (t : t) = Symtable.Compunit.name t.cu_name
    let crc _t = None

    let interface_imports (t : t) = t.cu_imports
    let implementation_imports (t : t) =
      let required =
        t.cu_required_compunits
        @ Symtable.required_compunits t.cu_reloc
      in
      let required =
        List.filter
          (fun cu -> not (Symtable.Compunit.is_packed cu))
          required
      in
      List.map
        (fun (Compunit cu) -> cu, None)
        required

    let defined_symbols (t : t) =
      List.map (fun (Compunit cu) -> cu)
        (Symtable.initialized_compunits t.cu_reloc)

    let unsafe_module (t : t) = t.cu_primitives <> []
  end

  let default_crcs = ref []
  let default_global_map = ref Symtable.empty_global_map

  let init () =
    if !Sys.interactive then begin (* PR#6802 *)
      invalid_arg "The dynlink.cma library cannot be used \
        inside the OCaml toplevel"
    end;
    default_crcs := Symtable.init_toplevel ();
    default_global_map := Symtable.current_state ()

  let is_native = false
  let adapt_filename f = f

  let num_globals_inited () =
    failwith "Should never be called for bytecode dynlink"

  let fold_initial_units ~init ~f =
    List.fold_left (fun acc (compunit, interface) ->
        let global =
          Symtable.Global.Glob_compunit (Compunit compunit)
        in
        let defined =
          Symtable.is_defined_in_global_map !default_global_map global
        in
        let implementation =
          if defined then Some (None, DT.Loaded)
          else None
        in
        let defined_symbols =
          if defined then [compunit]
          else []
        in
        f acc ~compunit ~interface ~implementation ~defined_symbols)
      init
      !default_crcs

  let run_shared_startup handle =
    match handle with
    | DT.Native_handle (nh, _units) ->
        Native_bridge.ndl_run nh "_shared_startup"
    | DT.Bytecode_handle _ ->
        (* Bytecode doesn't have shared startup *)
        ()

  let with_lock lock f =
    Mutex.lock lock;
    Fun.protect f
      ~finally:(fun () -> Mutex.unlock lock)

  let really_input_bigarray ic ar st n =
    match In_channel.really_input_bigarray ic ar st n with
      | None -> raise End_of_file
      | Some () -> ()

  type instruct_debug_event
  external reify_bytecode :
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    instruct_debug_event list array -> string option ->
    Obj.t * (unit -> Obj.t)
    = "caml_reify_bytecode"

  let run lock handle ~unit_header ~priv:_ =
    match handle with
    | DT.Native_handle (nh, unit_names) ->
        (* For native plugins, run the entry point for each unit *)
        List.iter (fun unit_name ->
          try Native_bridge.ndl_run nh unit_name
          with exn ->
            Printexc.raise_with_backtrace
              (DT.Error (Library's_module_initializers_failed exn))
              (Printexc.get_raw_backtrace ()))
          unit_names
    | DT.Bytecode_handle ((ic, file_name, file_digest, _old_st), _) ->
    (* if true then failwith "Ident foo run" else (); *)
    let clos = with_lock lock (fun () ->
        let compunit : compilation_unit = unit_header in
        seek_in ic compunit.cu_pos;
        let code =
          Bigarray.Array1.create Bigarray.Char Bigarray.c_layout
            compunit.cu_codesize
        in
        really_input_bigarray ic code 0 compunit.cu_codesize;
        begin try
          Symtable.patch_object code compunit.cu_reloc;
          Symtable.check_global_initialized compunit.cu_reloc;
          Symtable.update_global_table ()
        with Symtable.Error error ->
          let new_error : DT.linking_error =
            match error with
            | Symtable.Undefined_global global ->
              let desc = Symtable.Global.description in
              Undefined_global (Format.asprintf "%a" desc global)
            | Symtable.Unavailable_primitive s -> Unavailable_primitive s
            | Symtable.Uninitialized_global global ->
              Uninitialized_global (Symtable.Global.name global)
            | Symtable.Wrong_vm _ -> assert false
          in
          raise (DT.Error (Linking_error (file_name, new_error)))
        end;
        (* PR#5215: identify this code fragment by
           digest of file contents + unit name.
           Unit name is needed for .cma files, which produce several code
           fragments. *)
        let unit_name = Symtable.Compunit.name compunit.cu_name in
        let digest = Digest.string (file_digest ^ unit_name) in
        let events =
          if compunit.cu_debug = 0 then [| |]
          else begin
            seek_in ic compunit.cu_debug;
            [| (Compression.input_value ic : instruct_debug_event list) |]
          end in
        let _, clos = reify_bytecode code events (Some digest) in
        clos
      )
    in
    (* We need to release the dynlink lock here to let the module initialization
       code dynlinks plugins too.
    *)
    try ignore ((clos ()) : Obj.t);
    with exn ->
      Printexc.raise_with_backtrace
        (DT.Error (Library's_module_initializers_failed exn))
        (Printexc.get_raw_backtrace ())

  let load ~filename:file_name ~priv =
    (* Check if it's a native plugin (.cmxs) by file extension *)
    if Filename.check_suffix file_name ".cmxs" then begin
      (* Load native plugin using the bridge *)
      try
        let handle, header = Native_bridge.ndl_open file_name (not priv) in
        if header.dynu_magic <> Config.cmxs_magic_number then
          raise (DT.Error (Not_a_bytecode_file file_name));
        (* Register the native symbols *)
        let syms =
          "_shared_startup" ::
          List.concat_map (fun unit -> unit.dynu_defines) header.dynu_units
        in
        Native_bridge.ndl_register handle (Array.of_list syms);
        (* Convert native units to bytecode Unit_header format *)
        let units = List.map (fun (unit : dynunit) ->
          (* Create a fake compilation_unit for compatibility *)
          let cu : compilation_unit = {
            cu_name = Compunit unit.dynu_name;
            cu_pos = 0;
            cu_codesize = 0;
            cu_reloc = [];
            cu_imports = unit.dynu_imports_cmi;
            cu_required_compunits = [];
            cu_primitives = [];
            cu_force_link = false;
            cu_debug = 0;
            cu_debugsize = 0;
          } in
          cu
        ) header.dynu_units in
        let unit_names = List.map (fun (unit : dynunit) -> unit.dynu_name) header.dynu_units in
        DT.Native_handle (handle, unit_names), units
      with exn ->
        raise (DT.Error (Cannot_open_dynamic_library exn))
    end else begin
      (* Load bytecode plugin (.cmo/.cma) *)
      let ic =
        try open_in_bin file_name
        with exc -> raise (DT.Error (Cannot_open_dynamic_library exc))
      in
      try
        let file_digest = Digest.channel ic (-1) in
        seek_in ic 0;
        let buffer =
          try really_input_string ic (String.length Config.cmo_magic_number)
          with End_of_file -> raise (DT.Error (Not_a_bytecode_file file_name))
        in
        let old_symtable =
          if priv then
            Some (Symtable.current_state ())
          else
            None
        in
        let bytecode_data = (ic, file_name, file_digest, old_symtable) in
        if buffer = Config.cmo_magic_number then begin
          let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
          seek_in ic compunit_pos;
          let cu = (input_value ic : compilation_unit) in
          DT.Bytecode_handle (bytecode_data, [Symtable.Compunit.name cu.cu_name]), [cu]
        end else
        if buffer = Config.cma_magic_number then begin
          let toc_pos = input_binary_int ic in  (* Go to table of contents *)
          seek_in ic toc_pos;
          let lib = (input_value ic : library) in
          Symtable.open_dlls lib.lib_dllibs;
          let unit_names = List.map (fun (cu : compilation_unit) -> Symtable.Compunit.name cu.cu_name) lib.lib_units in
          DT.Bytecode_handle (bytecode_data, unit_names), lib.lib_units
        end else begin
          raise (DT.Error (Not_a_bytecode_file file_name))
        end
      with
      (* Wrap all exceptions into Cannot_open_dynamic_library errors except
         Not_a_bytecode_file ones, as they bring all the necessary information
         already
         Use close_in_noerr since the exception we really want to raise is exc *)
      | DT.Error _ as exc ->
        close_in_noerr ic;
        raise exc
      | exc ->
        close_in_noerr ic;
        raise (DT.Error (Cannot_open_dynamic_library exc))
    end

  let unsafe_get_global_value ~bytecode_or_asm_symbol =
    let global =
      Symtable.Global.Glob_compunit (Compunit bytecode_or_asm_symbol)
    in
    match Symtable.get_global_value global with
    | exception _ -> None
    | obj -> Some obj

  let finish handle =
    match handle with
    | DT.Native_handle (_nh, _units) ->
        (* Native handles don't need explicit cleanup *)
        ()
    | DT.Bytecode_handle ((ic, _filename, _digest, restore_symtable), _) ->
        begin match restore_symtable with
        | Some old_state ->
          Symtable.hide_additions old_state
        | None -> ()
        end;
        close_in ic
end

include DC.Make (Bytecode)

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
