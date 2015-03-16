open Core_kernel.Std
open Bap_disasm
module Block = Bap_disasm_block
module Insn  = Bap_disasm_insn
open Bap_types.Std
open Image_internal_std

module CallSite = struct
  include Addr
  let default = Addr.zero 0
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Addr)(CallSite)
include G

let add_func (disasm : disasm) (csg : t) (mem : mem) : t =
  Seq.fold (Table.intersections (Disasm.blocks disasm) mem) ~init:csg ~f:(fun csg (mem, block) ->
      let func_id = Memory.min_addr mem in
      (* If there is to be a call, it must be the terminator *)
      (* I cannot use Block.terminator to access it because I need the memory address as well *)
      let (insn_mem, insn) = List.hd_exn @@ Seq.to_list_rev @@ Block.insns block in
      if Insn.is_call insn
      then (print_endline "Hit call";
           Seq.fold (Block.dests block) ~init:csg ~f:(fun csg dest ->
          match dest with
          (* Resolved jump or conditional jump in a call insn is assumed a call target *)
          | `Block(tgt, `Jump)
          | `Block(tgt, `Cond) -> (print_endline "Adding edge";
            add_edge_e csg (func_id, (Memory.min_addr insn_mem), (Block.addr tgt)))
          | `Block(tgt, `Fall) -> (print_endline "Call fallthrough"; csg)
          (* Anything else is unresolved or a fallthrough, ignore it *)
          | `Unresolved(_) -> (print_endline "Call no edge"; csg)
        ))
      else csg
    )

let of_disasm (disasm : disasm) (funcs : mem list) : t =
  List.fold_left funcs ~f:(add_func disasm) ~init:empty
