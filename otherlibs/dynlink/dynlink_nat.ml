type handle

type global_map = {
  name : string;
  crc_intf : Digest.t option;
  crc_impl : Digest.t option;
  syms : string list
}

external ndl_open : string -> bool -> handle * Dynlink_cmxs_format.dynheader
  = "caml_natdynlink_open"
external ndl_register : handle -> string array -> unit
  = "caml_natdynlink_register"
external ndl_run : handle -> string -> unit = "caml_natdynlink_run"
external ndl_getmap : unit -> global_map list = "caml_natdynlink_getmap"
external ndl_globals_inited : unit -> int = "caml_natdynlink_globals_inited"
external ndl_loadsym : string -> Obj.t = "caml_natdynlink_loadsym"
