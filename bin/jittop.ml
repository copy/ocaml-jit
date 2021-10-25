let outcome_global : Topcommon.evaluation_outcome option ref = ref None

(* Copied from opttoploop.ml *)
module Backend = struct
  let symbol_for_global' = Compilenv.symbol_for_global'

  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx

  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int

  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end

let backend = (module Backend : Backend_intf.S)

let jit_load_body ppf phrase_name program =
  let open Config in
  let dll =
    if !Clflags.keep_asm_file then phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ phrase_name) ext_dll
  in
  let filename = Filename.chop_extension dll in
  let middle_end =
    if Config.flambda then Flambda_middle_end.lambda_to_clambda
    else Closure_middle_end.lambda_to_clambda
  in
  Asmgen.compile_implementation ~toplevel:Tophooks.need_symbol ~backend
    ~prefixname:filename ~middle_end ~ppf_dump:ppf program;
  match !outcome_global with
  | None -> failwith "No evaluation outcome"
  | Some res ->
      outcome_global := None;
      res

let jit_load ppf phrase_name program =
  Jit.with_jit_x86 (fun () -> jit_load_body ppf phrase_name program) phrase_name outcome_global

let jit_lookup_symbol symbol =
  Printf.printf "lookup_symbol %s\n" symbol;
  match Jit.Symbols.find !Jit.Globals.symbols symbol with
  | None -> Tophooks.default_lookup symbol
  | Some x -> Some (Jit.Address.to_obj x)

let set_debug () =
  match Sys.getenv_opt "OCAML_JIT_DEBUG" with
  | Some ("true" | "1") -> Jit.Globals.debug := true
  | None | Some _ -> Jit.Globals.debug := false

let init_top () =
  set_debug ();
  Tophooks.register_assembler ~lookup:jit_lookup_symbol ~load:jit_load

let () =
  Clflags.native_code := true;
  Toploop.set_paths ();
  Toploop.initialize_toplevel_env ();
  init_top ();
  Clitop.main ~name:"jittop" ~eval_phrase:Toploop.execute_phrase
    ~loop:Toploop.loop ()
