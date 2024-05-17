open Gen

(*let cycle =
  (*[FromRead (`Same, Internal); ProgramOrder (`Same, Write, Read)]*) (* CoWR? *)
  (*[FromRead (`Aliased, Internal); ProgramOrder (`Aliased, Write, Read)]*) (* CoWR.alias *)
  (*[FromRead (`Aliased, External); ReadsFrom (`Same, External); ProgramOrder (`Aliased, Read, Read)]*) (* CoRR0.alias+po *)
  (*[FromRead (`Same, External); ReadsFrom (`Same, External); ProgramOrder (`Same, Read, Read)]*) (* CoRR0+po? *)
  (*[ProgramOrder (`Same, Write, Read); FromRead (`Same, External); WriteSerialisation (`Same, External)]*)

let test = test_from_cycle cycle*)

let usage = "genonce [options] <cycle>"
let orig = ref ""
let test_name = ref "PLACEHOLDER"
let use_stdout = ref false
let dir_name = ref "generated"
let overwrite = ref false
let arch = ref "AArch64"
let args = [
  "-stdout", Arg.Set use_stdout, "Output the generated test to stdout";
  "-overwrite", Arg.Set overwrite, "Overwrite the test if it already exists (default: false)";
  "-name", Arg.Set_string test_name, "<name> The name of the test";
  "-dir", Arg.Set_string dir_name, "<dir> Directory to put the generated test in";
  "-arch", Arg.Set_string arch, "<arch> The architecture to use for the generated instructions";
]
let () = Arg.parse args (fun arg -> if !orig = "" then orig := arg else orig := !orig ^ " " ^ arg) usage

let select_arch : string -> (module Arch.Sig) = function
  | "AArch64" -> (module AArch64)
  | s -> raise (Invalid_argument ("Unrecognised architecture: " ^ s))

let () =
  let cycle = Cycle.parse !orig in
  let test = test_from_cycle !test_name !orig cycle in
  let file_name = !dir_name ^ "/" ^ !test_name ^ ".litmus.toml" in
  let out = if !use_stdout || (Sys.file_exists file_name && not !overwrite) then stdout else open_out file_name in
  let formatter =  Format.formatter_of_out_channel out in
  let (module SelectedArch) = select_arch !arch in
  let module TestPrinter = Test.Printer(SelectedArch) in
  Format.fprintf formatter "%a\n" TestPrinter.pp_test test