/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Bridge for loading native plugins from bytecode            */
/*                                                                        */
/*   Copyright 2024 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/intext.h"

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <dlfcn.h>


// Missing symbols needed to satisfy linker

value caml_exn_Stack_overflow = 0;
void caml_garbage_collection(void) {}
void caml_program(void) {}
value caml_apply2 = 0;
value caml_apply3 = 0;

void caml_array_bound_error_asm(void) {
    caml_array_bound_error();
}

extern value caml_ml_open_descriptor_out(value fd);
extern value caml_ml_output_bytes(value vchannel, value buff, value start, value length);
extern value caml_ml_output_char(value vchannel, value ch);
extern value caml_ml_flush(value vchannel);
extern value caml_ml_string_length(value s);
extern value caml_blit_string(value s1, value ofs1, value s2, value ofs2, value n);

value camlStdlib$print_endline_369(value arg) {
  // movq arg, %rax
  __asm__ ("" : "=a"(arg));
  value stdout_channel = caml_ml_open_descriptor_out(Val_int(1));
  value len = caml_ml_string_length(arg);
  caml_ml_output_bytes(stdout_channel, arg, Val_int(0), len);
  caml_ml_output_char(stdout_channel, Val_int('\n'));
  caml_ml_flush(stdout_channel);
  
  return Val_unit;
}


value camlStdlib$$$5e_139(value s1, value s2) {
  // movq s1, %rax
  // movq s2, %rbx
  __asm__ ("" : "=a"(s1), "=b"(s2));

  value l1 = caml_ml_string_length(s1);
  value l2 = caml_ml_string_length(s2);
  mlsize_t len = Long_val(l1) + Long_val(l2);
  
  value res = caml_alloc_string(len);

  caml_blit_string(s1, Val_int(0), res, Val_int(0), l1);
  caml_blit_string(s2, Val_int(0), res, l1, l2);
  
  return res;
}

#define Handle_val(v) (*((void **) Data_abstract_val(v)))

static value Val_handle(void* handle) {
  value res = caml_alloc_small(1, Abstract_tag);
  Handle_val(res) = handle;
  return res;
}

static void *getsym(void *handle, const char *module, const char *name) {
  char *fullname = caml_stat_strconcat(4, "caml", module, "$", name);
  void *sym = caml_dlsym(handle, fullname);
  caml_stat_free(fullname);
  return sym;
}

// stub
CAMLprim value caml_natdynlink_getmap(value unit) {
  return Val_emptylist;
}

static intnat bytecode_globals_inited = 0;

CAMLprim value caml_natdynlink_globals_inited(value unit) {
  return Val_int(bytecode_globals_inited);
}

static void export_bridge_symbols(void) {
  static int symbols_exported = 0;
  if (!symbols_exported) {
    // Make symbols available to dynamically loaded code
    void *self = dlopen(NULL, RTLD_NOW | RTLD_GLOBAL);
    if (self) dlclose(self);
    symbols_exported = 1;
  }
}

// Open a native plugin from bytecode
CAMLprim value caml_natdynlink_open(value filename, value global) {
  CAMLparam2(filename, global);
  CAMLlocal3(res, handle, header);
  void *dlhandle;
  const void *sym;
  char_os *p;
  
  // Ensure bridge symbols available
  export_bridge_symbols();
  
  p = caml_stat_strdup_to_os(String_val(filename));
  caml_enter_blocking_section();
  // Force RTLD_GLOBAL so plugin can see symbols
  // TODO implement priv
  dlhandle = caml_dlopen(p, RTLD_NOW | RTLD_GLOBAL);
  caml_leave_blocking_section();
  caml_stat_free(p);
  
  if (dlhandle == NULL) {
    caml_failwith(caml_dlerror());
  }
  
  // Check if valid OCaml plugin
  sym = caml_dlsym(dlhandle, "caml_plugin_header");
  if (sym == NULL) {
    caml_dlclose(dlhandle);
    caml_failwith("not an OCaml plugin");
  }
  
  handle = Val_handle(dlhandle);
  header = caml_input_value_from_block(sym, INT_MAX);
  
  res = caml_alloc_tuple(2);
  Field(res, 0) = handle;
  Field(res, 1) = header;
  CAMLreturn(res);
}

CAMLprim value caml_natdynlink_register(value handle_v, value symbols) {
  return Val_unit;
}

CAMLprim value caml_natdynlink_run(value handle_v, value symbol) {
  CAMLparam2(handle_v, symbol);
  CAMLlocal1(result);
  void* handle = Handle_val(handle_v);
  const char *unit = String_val(symbol);
  void (*entrypoint)(void);
  
  if (strcmp(unit, "_shared_startup") == 0) {
    // printf("DEBUG: SKIP _shared_startup\n");
    // fflush(stdout);
    result = Val_unit;
  } else {
    entrypoint = getsym(handle, unit, "entry");
    if (entrypoint != NULL) {
      result = caml_callback_native((value)&entrypoint, 0);
    } else {
      printf("DEBUG: No entry point found for unit %s\n", unit);
      fflush(stdout);
      result = Val_unit;
    }
  }
  
  CAMLreturn(result);
}

/* Load symbol by name */
CAMLprim value caml_natdynlink_loadsym(value symbol) {
  CAMLparam1(symbol);
  /* For bytecode, we can't directly load native symbols */
  /* This would need special handling */
  caml_failwith("caml_natdynlink_loadsym not implemented for bytecode");
  CAMLreturn(Val_unit);
}

/* Toplevel loading - not used in bytecode */
CAMLprim value caml_natdynlink_run_toplevel(value filename, value symbol) {
  caml_failwith("caml_natdynlink_run_toplevel not supported in bytecode");
  return Val_unit;
}

void init_symbol_bridge(void) {
  /* Placeholder for future symbol patching initialization */
}




