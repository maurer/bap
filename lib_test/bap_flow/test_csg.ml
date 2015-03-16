open Core_kernel.Std
open Bap.Std
open OUnit2

(* Fake until CSG is implemented *)
module CSG = struct
  module CallSite = struct
    include Addr
    let default = zero 0
  end
  
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Addr)(CallSite)
  include G
  type csg = t
  let of_disasm _ _ = empty
end
type csg = CSG.csg
(* End fake *)

type csg_case = {
  program : disasm;
  funcs   : addr list;
  result  : csg;
}

let test_csg case ctxt =
  let csg = CSG.of_disasm case.program case.funcs in
  assert_equal ~ctxt case.result csg

let make_disasm s = Bigstring.of_string s |>
  Memory.create LittleEndian (Addr.of_int64 0L) |>
  ok_exn |>
  disassemble `x86_64

(* main -> f -> g *)
let linear_prog = make_disasm
  "\xe8\x01\x00\x00\x00\xc3\xe8\x01\x00\x00\x00\xc3\xc3"

(* main -> f -> g -> f *)
let recursive_prog = make_disasm
  "\xe8\x01\x00\x00\x00\xc3\xe8\x01\x00\x00\x00\xe8\xf5\xff\xff\xff\xc3\xc3"

(* main -> f -> g -> h -> i -> g; h -> f *)
let natural_prog = make_disasm
  "\xe8\x01\x00\x00\x00\xc3\xe8\x01\x00\x00\x00\xc3\xe8\x01\x00\x00\x00\xc3\xe8\x06\x00\x00\x00\xe8\xea\xff\xff\xff\xc3\xe8\xea\xff\xff\xff\xc3"

let make_graph e = List.fold_left e ~f:CSG.add_edge_e ~init:CSG.empty

let linear = 
  let main = Addr.of_int64 0x0L in
  let f    = Addr.of_int64 0x6L in
  let g    = Addr.of_int64 0xcL in
  {
    program = linear_prog;
    funcs   = [main; f; g];
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
    program = recursive_prog;
    funcs   = [main; f; g];
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
    program = natural_prog;
    funcs   = [main; f; g; h; i];
    result  = make_graph [
      (main, main, f);
      (f, f, g);
      (g, g, h);
      (h, h, i);
      (i, i, g);
      (h, Addr.of_int64 0x17L, f);
    ]
  }

let suite = "Flow.CSG" >::: [
    "linear"    >:: test_csg linear;
    "recursive" >:: test_csg recursive;
    "natural"   >:: test_csg natural;
  ]
