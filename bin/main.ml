(* let usage = "gen [options]"

let safe = ref ""
let relaxed = ref ""
let dir_name = ref "."
let size = ref 6
let nprocs = ref 4

let args = [
  "-safe", Arg.Set_string safe, "<SafeList> Comma-separated list of safe relations";
  "-relaxed", Arg.Set_string relaxed, "<Relaxed> Relation to be relaxed";
  "-dir", Arg.Set_string dir_name, "<dir> Directory to put generated tests in (default: .)";
  "-size", Arg.Set_int size, "<n> Maximum size of generated cycles";
  "-nprocs", Arg.Set_int nprocs, "<n> Maximum number of threads in generated tests";
]
let () = Arg.parse args (fun _ -> raise (Arg.Bad "Extra arguments given")) "Usage here"

(* let select_arch : string -> (module Arch.Sig) = function
  | "AArch64" -> (module AArch64)
  | s -> raise (Invalid_argument ("Unrecognised architecture: " ^ s)) *)

let () =
  let safe = String.split_on_char ',' !safe in
  let safe = List.map Cycle.parse_relation safe in
  let relaxed = Cycle.parse_relation !relaxed in
  let _tests = CycleGen.gen_cycles safe relaxed !size !nprocs in
  () *)