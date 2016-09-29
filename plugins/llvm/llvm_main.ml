open Core_kernel.Std
open Bap.Std
include Self()

external init : unit -> int = "disasm_llvm_init_stub" "noalloc"

let loaded = ref false

let library_load (syntax : string) : unit =
  if not !loaded then (
    Unix.putenv "BAP_LLVM_OPTIONS" ("-x86-asm-syntax=" ^ syntax);
    let r = init () in
    if r < 0 then
      eprintf "LLVM initialization failed with error %d\n" r
    else
      loaded := true
  )

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Uses LLVM library to provide disassembler and file loader" ;
      `S "SEE ALSO";
      `P "$(b,bap-plugin-elf-loader)(1)";
    ] in
  let x86_syntax =
    let names = ["att", "att"; "intel", "intel"] in
    let doc = sprintf "Choose style of code for x86 syntax between %s"
      @@ Config.doc_enum names in
    Config.(param (enum names) ~default:"att" "x86-syntax" ~doc) in
  Config.when_ready (fun {Config.get=(!)} -> library_load !x86_syntax)

let () = library_load "att"
