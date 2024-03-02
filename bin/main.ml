open Gen

(*let cycle =
  (*[FromRead (`Same, Internal); ProgramOrder (`Same, Write, Read)]*) (* CoWR? *)
  (*[FromRead (`Aliased, Internal); ProgramOrder (`Aliased, Write, Read)]*) (* CoWR.alias *)
  (*[FromRead (`Aliased, External); ReadsFrom (`Same, External); ProgramOrder (`Aliased, Read, Read)]*) (* CoRR0.alias+po *)
  (*[FromRead (`Same, External); ReadsFrom (`Same, External); ProgramOrder (`Same, Read, Read)]*) (* CoRR0+po? *)
  (*[ProgramOrder (`Same, Write, Read); FromRead (`Same, External); WriteSerialisation (`Same, External)]*)

let test = test_from_cycle cycle*)

let usage = "testgen [-name <name>] -cycle <cycle>"
let cycle = ref []
let orig = ref ""
let test_name = ref "PLACEHOLDER"
let use_stdout = ref false
let args = [
  "-stdout", Arg.Set use_stdout, "Output the generated test to stdout";
  "-name", Arg.Set_string test_name, "The name of the test";
  "-cycle", Arg.Rest_all (fun tokens ->
    cycle := List.map Cycle.parse_relation tokens;
    orig := String.concat " " tokens),
    "The cycle to generate the test from";
]
let () = Arg.parse args (fun arg -> print_endline ("Ignored anonymous argument: " ^ arg)) usage

module TestPrinter = Test.Printer(AArch64)
let () =
  let test = test_from_cycle !test_name !orig !cycle in
  let file_name = "generated/" ^ !test_name ^ ".litmus.toml" in
  let out = if !use_stdout || Sys.file_exists file_name then stdout else open_out file_name in
  let formatter =  Format.formatter_of_out_channel out in
  Format.fprintf formatter "%a\n" TestPrinter.pp_test test