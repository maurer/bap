module SetM = Set.Make (* Csg.E does not have sexp *)
open Core_kernel.Std
open Bap.Std
open OUnit2

module Csg = Flow.Csg

type csg_case = {
  program : disasm;
  funcs   : mem list;
  result  : Csg.t;
}

module S = SetM(Csg.E)

let graph_repr (csg : Csg.t) : S.t =
  let () = Csg.iter_edges_e (fun (s, _, d) -> Printf.printf "%s -> %s\n" (Addr.to_string s) (Addr.to_string d)) csg in
  Csg.fold_edges_e S.add csg S.empty

let cmp_graphs (csg : Csg.t) (csg' : Csg.t) : bool =
  print_endline "lhs";
  let repr  = graph_repr csg in
  print_endline "rhs";
  let repr' = graph_repr csg' in
  S.equal repr repr'

let test_csg case ctxt =
  let csg = Csg.of_disasm case.program case.funcs in
  assert_equal ~ctxt ~cmp:cmp_graphs case.result csg

let make_mem s = Bigstring.of_string s |>
  Memory.create LittleEndian (Addr.of_int64 0L) |>
  ok_exn

(* main -> f -> g *)
let linear_mem = make_mem
  "\xe8\x01\x00\x00\x00\xc3\xe8\x01\x00\x00\x00\xc3\xc3"

(* main -> f -> g -> f *)
let recursive_mem = make_mem
  "\232\001\000\000\000\195\232\001\000\000\000\195\232\245\255\255\255\195"
  (*
"\xe8\x01\x00\x00\x00\xc3\xe8\x01\x00\x00\x00\xe8\xf5\xff\xff\xff\xc3\xc3"
*)
(* main -> f -> g -> h -> i -> g; h -> f *)
let natural_mem = make_mem
  "\xe8\x01\x00\x00\x00\xc3\xe8\x01\x00\x00\x00\xc3\xe8\x01\x00\x00\x00\xc3\xe8\x06\x00\x00\x00\xe8\xea\xff\xff\xff\xc3\xe8\xea\xff\xff\xff\xc3"

let make_graph e = List.fold_left e ~f:Csg.add_edge_e ~init:Csg.empty

let rec make_mem mem = let view = Bap_memory.view in function
  | (s::e::rs) ->
        let len = (Addr.(to_int @@ e - s) |> ok_exn) - 1 in
        (view ~from:s ~words:len mem |> ok_exn)
      :: make_mem mem (e::rs)
  | [v] -> [view ~from:v mem |> ok_exn]
  | [] -> []

let linear = 
  let main = Addr.of_int64 0x0L in
  let f    = Addr.of_int64 0x6L in
  let g    = Addr.of_int64 0xcL in
  {
    program = disassemble `x86_64 linear_mem;
    funcs   = make_mem linear_mem [main; f; g];
    result  = make_graph [
      (main, main, f);
      (f, f, g);
    ];
  }

let recursive =
  let main = Addr.of_int64 0x0L in
  let f    = Addr.of_int64 0x6L in
  let g    = Addr.of_int64 0xcL in
  {
    program = disassemble `x86_64 recursive_mem;
    funcs   = make_mem recursive_mem [main; f; g];
    result  = make_graph [
      (main, main, f);
      (f, f, g);
      (g, g, f);
    ];
  }

let natural =
  let main = Addr.of_int64 0x00L in
  let f    = Addr.of_int64 0x06L in
  let g    = Addr.of_int64 0x0cL in
  let h    = Addr.of_int64 0x12L in
  let i    = Addr.of_int64 0x1dL in
  {
    program = disassemble `x86_64 natural_mem;
    funcs   = make_mem natural_mem [main; f; g; h; i];
    result  = make_graph [
      (main, main, f);
      (f, f, g);
      (g, g, h);
      (h, h, i);
      (i, i, g);
      (h, Addr.of_int64 0x17L, f);
    ]
  }

let suite = "Flow.Csg" >::: [
    "linear"    >:: test_csg linear;
    "recursive" >:: test_csg recursive;
    "natural"   >:: test_csg natural;
  ]
